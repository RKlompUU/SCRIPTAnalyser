{-# LANGUAGE GADTs #-}
module ConstraintsGen where

import Control.Monad.Trans.Reader

import Script.AST


genConstraints :: ScriptAST -> BConstraints
genConstraints script = foldl1 OrConstr $ map cnstrs (runReader (genCnstrs script) initBuildState)

type Ident = Int
type OpIdent = String
data Expr where
  Const :: Int   -> Expr
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

type ConstraintBuilder a = Reader BuildState a

genV :: BuildState -> (BuildState, Expr)
genV s = (s {freshV = freshV s + 1}, Var $ freshV s)

cnstrsMod :: (BConstraints -> BConstraints) -> BuildState -> BuildState
cnstrsMod f st =
  st {cnstrs = f $ cnstrs st}

popStack :: BuildState -> (BuildState,Expr)
popStack st = (st {stack = tail $ stack st}, head (stack st))

stModITE :: Bool -> BuildState -> BuildState
stModITE b = \st ->
  let (st', v) = if null (stack st)
                  then genV st
                  else popStack st
      nc = ExprConstr $ if b
                          then Op v "/=" (Const 0)
                          else Op v "==" (Const 0)
  in cnstrsMod (AndConstr nc) st'


genCnstrs :: ScriptAST -> ConstraintBuilder [BuildState]
genCnstrs (ScriptITE l b0 b1 cont) = do
  ss0 <- withReader (stModITE True)  (genCnstrs b0)
  ss1 <- withReader (stModITE False) (genCnstrs b1)
  concat <$> mapM (\s -> local (const s) (genCnstrs cont)) (ss0 ++ ss1)
genCnstrs (ScriptOp l op cont) = do
  genCnstrs cont
genCnstrs ScriptTail = do
  s <- ask
  return [s]
