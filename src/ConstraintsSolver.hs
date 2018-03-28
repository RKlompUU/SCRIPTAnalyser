{-# LANGUAGE GADTs #-}
module ConstraintsSolver where

import ConstraintsGen
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Applicative
import qualified Data.ByteString as BS
import Data.Maybe
import qualified Data.Map.Lazy as M

import qualified Data.Range.Range as R
import qualified Data.Range.Algebra as RA

data VConstraint =
  VConstraint {
    intRanges :: [R.Range Int], -- Integer bounds
    bsRanges :: [R.Range Int]
  }
  | VSubst OpTy Ident   -- v_i op v_j
  deriving Show
type VConstraints = M.Map Ident VConstraint
type Contradiction = String

validVConstr :: VConstraint -> Bool
validVConstr c =
  (not . null) (bsRanges c) &&
  ((not . null) (intRanges c) ||
   (not . null) (R.intersection [R.SpanRange 5 maxBSL] (bsRanges c)))

data VConstraintsBuilder =
  VConstraintsBuilder {
    vconstrs :: VConstraints,
    info :: String
  }

type Solver a = ExceptT Contradiction (State VConstraintsBuilder) a

-- iota = no knowledge
vconstraint =
  VConstraint {
      intRanges = [R.SpanRange (-maxN) (maxN)]
    , bsRanges = [R.SpanRange 0 maxBSL]
  }
cFalse = vconstraint { intRanges = [R.SingletonRange 0],
                       bsRanges = [R.SingletonRange 1] }
cTrue = vconstraint { intRanges = R.invert [R.SingletonRange 0],
                      bsRanges = [R.SingletonRange 1] }


maxN = 0x7fffffff -- 32 bit signed int
maxBSL = 520 -- bytes
maxIntBSL = 4
hashOutBL = 32
sigBL = 71
pubBL = 65




debug :: String -> Solver a -> Solver a
debug str f = do
  cs <- lift $ get
  let oldInfo = info cs
  lift $ put $ cs { info = oldInfo ++ "\n,<+log+>,\n" ++ str }
  x <- f
  lift $ put $ cs { info = oldInfo }
  return x

getDebug :: Solver String
getDebug = lift $ do
  info <$> get

pushVConstraint :: Expr -> VConstraint -> Solver ()
pushVConstraint (Var x) c = lift $ do
  cs <- get
  put $ cs { vconstrs = M.insert x c (vconstrs cs) }
pushVConstraint e c2 = do
  c1 <- genVConstraints e
  debug ("Pushing VConstraint {\n" ++ show c2 ++ "\n} on: \n" ++ show e ++ "\n, which has constr: \n" ++ show c1) $ combineVConstr c1 "==" c2
  return ()

getVConstr :: Ident -> Solver VConstraint
getVConstr x = lift $ do
  maybeC <- M.lookup x <$> vconstrs <$> get
  case maybeC of
    Just c  -> return c
    Nothing -> return $ vconstraint

solveConstraints :: BConstraints -> Either Contradiction VConstraints
solveConstraints constrs =
  case flip runState (VConstraintsBuilder M.empty "") $ runExceptT (constrsSolver constrs) of
    (Left e,_)          -> Left e
    (Right (),st) -> Right (vconstrs st)

constrsSolver :: BConstraints -> Solver ()
constrsSolver (AndConstr b1 b2) =
  constrsSolver b1 >> constrsSolver b2
constrsSolver (OrConstr b1 b2) =
  constrsSolver b1 <|> constrsSolver b2
constrsSolver (ExprConstr e) =
  genVConstraints e >> return ()
constrsSolver LeafConstr =
  return ()

genVConstraints :: Expr -> Solver VConstraint
genVConstraints (ConstInt i) =
  return $ vconstraint { intRanges = [R.SingletonRange i],
                         bsRanges = [R.SpanRange 0 4] }
genVConstraints (ConstBS bs) = do
  let bsI = e2i <$> convert2Int (ConstBS bs)
  return $ VConstraint {
      intRanges = [if isJust bsI
                    then R.SingletonRange (fromJust bsI)
                    else R.SpanRange (-maxN) maxN]
    , bsRanges = [R.SingletonRange (BS.length bs)]
  }
genVConstraints (Hash x) =
  return $ vconstraint { bsRanges = [R.SingletonRange hashOutBL] }
genVConstraints (Sig sig pub) = do
  let sigC = vconstraint { bsRanges = [R.SingletonRange sigBL] }
      pubC = vconstraint { bsRanges = [R.SingletonRange pubBL] }
  pushVConstraint sig sigC
  pushVConstraint pub pubC
  return $ vconstraint
genVConstraints (Op (Var x) op (Var y)) = do
  pushVConstraint (Var x) (VSubst op y)
  return $ vconstraint { bsRanges = [R.SpanRange 0 maxIntBSL] }
genVConstraints e_@(Op (Var x) op e) = do
  c1  <- genVConstraints e
  c2  <- getVConstr x
  c2' <- debug ("combining vconstrs for: " ++ show e_) $ combineVConstr c2 op c1
  pushVConstraint (Var x) c2'
  return $ vconstraint { bsRanges = [R.SpanRange 0 maxIntBSL] }
genVConstraints (Op e op (Var x))
  | isJust op' = genVConstraints (Op (Var x) op e)
  where op' = lookup op flipOpSet
genVConstraints (Op e "-" (Var x)) =
  throwError' "e - x not implemented yet in genVConstraints"
genVConstraints (Op e1 op e2) = do
  c1 <- genVConstraints e1
  c2 <- genVConstraints e2
  combineVConstr c1 op c2

combineRule :: VConstraint ->
               ([R.Range Int] -> [R.Range Int] -> [R.Range Int]) ->
               ([R.Range Int] -> [R.Range Int] -> [R.Range Int]) ->
               VConstraint ->
               VConstraint
combineRule c1 f1 f2 c2 =
  VConstraint {
      intRanges = f1 (intRanges c1) (intRanges c2)
    , bsRanges = f2 (bsRanges c1) (bsRanges c2)
  }

combineVConstr :: VConstraint -> OpTy -> VConstraint -> Solver VConstraint
combineVConstr c1 "/\\" c2 = do
  verifyTrue c1
  verifyTrue c2
combineVConstr c1 "\\/" c2 = do
  verifyTrue c1 <|> verifyTrue c2
combineVConstr c1 op c2 = do
  let c' = combineRule c1 (fst $ f op) (snd $ f op) c2
  if validVConstr c'
    then return c'
    else throwError' $ "**********\ninvalid combine result: " ++ show c' ++ ", created on:\n" ++ show c1 ++ "\nON OP[" ++ op ++ "]\n" ++ show c2
  where f :: OpTy -> ([R.Range Int] -> [R.Range Int] -> [R.Range Int], [R.Range Int] -> [R.Range Int] -> [R.Range Int])
        f "==" = (R.intersection, R.intersection)
        f "/=" = (R.difference, R.union)
combineVConstr _ op _ =
  throwError' $ "combineVConstr not implemented yet for operator: " ++ show op

verifyTrue :: VConstraint -> Solver VConstraint
verifyTrue c = do
  combineVConstr c "/=" cFalse

throwError' :: String -> Solver a
throwError' str = do
  str' <- getDebug
  throwError (str ++ "\n************" ++ str')
