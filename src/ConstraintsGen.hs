{-# LANGUAGE GADTs #-}
module ConstraintsGen where

import Control.Monad.Trans.Reader
import Control.Monad.State.Lazy
import Control.Applicative

import Script.AST
import Data.Bitcoin.Script.Types

import qualified Data.ByteString as BS

genConstraints :: ScriptAST -> BConstraints
genConstraints script
  = foldl1 OrConstr
  $ map cnstrs
  $ genBuildStates script

genBuildStates :: ScriptAST -> [BuildState]
genBuildStates script
  = map (\s -> execState s initBuildState)
  $ runReader (genCnstrs script) (return ())

type Ident = Int
type OpIdent = String
data Expr where
  ConstInt :: Int -> Expr
  ConstBS  :: BS.ByteString -> Expr
  Var   :: Ident -> Expr
  Hash  :: Expr -> Expr
  Op    :: Expr -> OpIdent -> Expr -> Expr
  deriving (Show)

data BConstraints where
  ExprConstr :: Expr -> BConstraints
  AndConstr  :: BConstraints -> BConstraints -> BConstraints
  OrConstr   :: BConstraints -> BConstraints -> BConstraints
  LeafConstr :: BConstraints

instance Show BConstraints where
  show (ExprConstr e) = show e
  show (AndConstr b0 b1) = show b0 ++ " && " ++ show b1
  show (OrConstr  b0 b1) = show b0 ++ " ||\n" ++ show b1
  show LeafConstr = "True"

type Stack = [Expr]
data BuildState =
  BuildState {
    cnstrs   :: BConstraints,
    stack    :: Stack,
    altStack :: Stack,
    freshV   :: Ident
  }
initBuildState =
  BuildState {
    cnstrs   = LeafConstr,
    stack    = [],
    altStack = [],
    freshV   = 0
  }

instance Show BuildState where
  show s = "BuildState {\n\tcnstrs: " ++ show (cnstrs s) ++
           ",\n\tstack: " ++ show (stack s) ++
           ",\n\taltStack: " ++ show (altStack s) ++
           "}\n"

type BranchBuilder a = State BuildState a
type ConstraintBuilder a = Reader (BranchBuilder ()) a

genV :: BranchBuilder Expr
genV = do
  st <- get
  put $ st {freshV = freshV st + 1}
  return $ Var (freshV st)


cnstrsMod :: (BConstraints -> BConstraints) -> BranchBuilder ()
cnstrsMod f = do
  st <- get
  put $ st {cnstrs = f $ cnstrs st}

popStack :: BranchBuilder Expr
popStack = do
  st <- get
  put $ st {stack = tail $ stack st}
  return $ head (stack st)

safePopStack :: BranchBuilder Expr
safePopStack = do
  st <- get
  if null (stack st)
    then genV
    else popStack

pushStack :: Expr -> BranchBuilder ()
pushStack e = do
  st <- get
  put $ st {stack = e : stack st}

pushsStack :: [Expr] -> BranchBuilder ()
pushsStack es = mapM_ pushStack es

--withReader_ :: r' -> Reader r' a -> Reader r a
withReader_ x m =
  withReader (const x) m

genCnstrs :: ScriptAST -> ConstraintBuilder [BranchBuilder ()]
genCnstrs (ScriptITE l b0 b1 cont) = do
  ss0 <- withReader (>> stModITE True)  (genCnstrs b0)
  ss1 <- withReader (>> stModITE False) (genCnstrs b1)
  concat <$> mapM (\s -> local (const s) (genCnstrs cont)) (ss0 ++ ss1)
genCnstrs (ScriptOp l op cont) = do
  withReader (>> stModOp op) $ genCnstrs cont
genCnstrs ScriptTail = do
  s <- ask
  return [s]


stModITE :: Bool -> BranchBuilder ()
stModITE b = do
  v <- safePopStack
  let nc = ExprConstr $ if b
                          then Op v "/=" (ConstInt 0)
                          else Op v "==" (ConstInt 0)
  cnstrsMod (AndConstr nc)

stModOp :: ScriptOp -> BranchBuilder ()
stModOp (OP_PUSHDATA bs _) = do
  pushStack (ConstBS bs)
stModOp OP_DROP = do
  safePopStack
  return ()
stModOp OP_DUP = do
  v <- safePopStack
  pushStack v
  pushStack v
stModOp _ = do
  return ()
