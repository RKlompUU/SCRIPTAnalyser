{-# LANGUAGE GADTs #-}
module ConstraintsSolver where

import ConstraintsGen
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Applicative
import qualified Data.ByteString as BS
import Data.Maybe

data VConstraint =
  VConstraint {
    minI :: Int, -- Minimal integer bound
    maxI :: Int, -- Maximal integer bound
    minL :: Int, -- Minimal bytestring length bound
    maxL :: Int  -- Maximal bytestring length bound
  }
  | VSubst OpTy Ident   -- v_i op v_j
  deriving (Show,Eq)
type VConstraints = [(Ident,VConstraint)]
type Contradiction = String

validVConstr :: VConstraint -> Bool
validVConstr c =
  minI c <= maxI c &&
  minL c <= maxL c

type Solver a = ExceptT Contradiction (State VConstraints) a

-- iota = no knowledge
iota =
  VConstraint {
      minI = -maxN
    , maxI = maxN
    , minL = 0
    , maxL = maxBSL
  }

maxN = undefined
maxBSL = 520 -- bytes
maxIntBSL = 4

pushVConstraint :: Ident -> VConstraint -> Solver ()
pushVConstraint x c = lift $ do
  cs <- get
  undefined

getVConstr :: Ident -> Solver VConstraint
getVConstr x = lift $ do
  maybeC <- lookup x <$> get
  case maybeC of
    Just c  -> return c
    Nothing -> return $ iota

solveConstraints :: BConstraints -> Either Contradiction VConstraints
solveConstraints constrs =
  case flip runState [] $ runExceptT (constrsSolver constrs) of
    (Left e,_)          -> Left e
    (Right (),vConstrs) -> Right vConstrs

constrsSolver :: BConstraints -> Solver ()
constrsSolver (AndConstr b1 b2) =
  constrsSolver b1 >> constrsSolver b2
constrsSolver (OrConstr b1 b2) =
  constrsSolver b1 <|> constrsSolver b2
constrsSolver (ExprConstr e) =
  genVConstraints e >> return ()

genVConstraints :: Expr -> Solver VConstraint
genVConstraints (ConstInt i) =
  return $ VConstraint {
      minI = i
    , maxI = i
    , minL = 0
    , maxL = maxIntBSL
  }
genVConstraints (ConstBS bs) =
  return $ VConstraint {
      minI = -maxN
    , maxI = maxN
    , minL = BS.length bs
    , maxL = BS.length bs
  }
genVConstraints (Op (Var x) op (Var y)) = do
  pushVConstraint x (VSubst op y)
  return $ VConstraint {
      minI = -maxN
    , maxI = maxN
    , minL = 0
    , maxL = maxIntBSL
  }
genVConstraints (Op (Var x) op e) = do
  c1  <- genVConstraints e
  c2  <- getVConstr x
  c2' <- combineVConstr c2 op c1
  pushVConstraint x c2'
  return $ VConstraint {
      minI = -maxN
    , maxI = maxN
    , minL = 0
    , maxL = maxIntBSL
  }
genVConstraints (Op e op (Var x))
  | isJust op' = genVConstraints (Op (Var x) op e)
  where op' = lookup op flipOpSet
genVConstraints (Op e "-" (Var x)) =
  fail "e - x not implemented yet in genVConstraints"
genVConstraints (Op e1 op e2) = do
  c1 <- genVConstraints e1
  c2 <- genVConstraints e2
  combineVConstr c1 op c2

combineVConstr :: VConstraint -> OpTy -> VConstraint -> Solver VConstraint
combineVConstr c1 "==" c2 = do
  let c' = VConstraint {
      minI = max (minI c1) (minI c2)
    , maxI = min (maxI c1) (maxI c2)
    , minL = max (minL c1) (minL c2)
    , maxL = min (maxL c1) (maxL c2)
  }
  if validVConstr c'
    then return c'
    else fail $ show c1 ++ ", cannot be combined with: " ++ show c2
combineVConstr _ op _ =
  fail $ "combineVConstr not implemented yet for operator: " ++ show op
