{-# LANGUAGE GADTs #-}
module Constraints.Solver where

import Constraints.Types
import Constraints.Gen
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Applicative
import qualified Data.ByteString as BS
import Data.Maybe
import qualified Data.Map.Lazy as M

import qualified Data.Range.Range as R
import qualified Data.Range.Algebra as R
import qualified Debug.Trace as D

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
  } deriving Show

type Solver a = ExceptT Contradiction (State VConstraintsBuilder) a

-- iota = no knowledge
vconstraint =
  VConstraint {
      intRanges = [R.SpanRange (-maxN) (maxN)]
    , bsRanges = [R.SpanRange 0 maxBSL]
  }
cFalse = vconstraint { intRanges = [R.SingletonRange 0],
                       bsRanges = [R.SingletonRange 1] }
cTrue = vconstraint { intRanges = R.intersection [R.SpanRange (-maxN) maxN] (R.invert [R.SingletonRange 0]),
                      bsRanges = [R.SpanRange 0 maxIntBSL] }
cInt = vconstraint { bsRanges = [R.SpanRange 0 maxIntBSL] }
cBot = vconstraint { bsRanges = [] }





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


forceInt :: Expr -> Solver VConstraint
forceInt e = do
  c <- genVConstraints e
  c' <- combineVConstr c "==" cInt
  updateVConstraint e c'
  return c'

updateVConstraint :: Expr -> VConstraint -> Solver ()
updateVConstraint (Var x) c = lift $ do
  cs <- get
  put $ cs { vconstrs = M.insert x c (vconstrs cs) }
updateVConstraint _ _ = return ()

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

debugEConstraint :: Expr -> Either Contradiction (VConstraint, VConstraintsBuilder)
debugEConstraint e =
  case flip runState (VConstraintsBuilder M.empty "") $ runExceptT (genVConstraints e) of
    (Left e,_)          -> Left e
    (Right c,st) -> Right (c, st)


constrsSolver :: BConstraints -> Solver ()
constrsSolver (AndConstr b1 b2) =
  constrsSolver b1 >> constrsSolver b2
constrsSolver (OrConstr b1 b2) =
  constrsSolver b1 <|> constrsSolver b2
constrsSolver (ExprConstr e) =
  genVConstraints e >> return ()
constrsSolver TrueConstr =
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
genVConstraints (Var x) =
  getVConstr x

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
  return $ cTrue

genVConstraints (Op e1 "-" e2) = do
  c1 <- forceInt e1
  c2 <- forceInt e2
  let l = R.lowestBound (intRanges c1)
      h = R.highestBound (intRanges c1)
  let c' = c1 {
      intRanges = R.shiftLows (\x -> x - h) $ R.shiftHighs (\x -> x - l) $ intRanges c2
  }
  verifyC c'
genVConstraints (Op e1 "<=" e2) = do
  c1 <- forceInt e1
  c2 <- forceInt e2
  let c' = cInt --{
 --     We really don't know this yet. Unsafe
 --     intRanges = [R.SpanRange (-maxN) (R.lowestBound (intRanges c2))]
  --}
  pushVConstraint e1 c'
  return $ cTrue
genVConstraints (Op e1 "<" e2) =
  genVConstraints (Op e1 "<=" (Op e2 "-" (ConstInt 1)))
genVConstraints e_@(Op (Var x) op e) = do
  c1  <- genVConstraints e
  c2  <- getVConstr x
  c2' <- combineVConstr c2 op c1
  pushVConstraint (Var x) c2'
  return $ c2' -- Both <- and -> are wrong -- vconstraint { bsRanges = [R.SpanRange 0 maxIntBSL] }

genVConstraints (Op e op (Var x))
  | isJust op' = genVConstraints (Op (Var x) op e)
  where op' = lookup op flipOpSet
genVConstraints (Op e "-" (Var x)) =
  throwError' "e - x not implemented yet in genVConstraints"
genVConstraints (Op e1 op e2) = do
  c1 <- genVConstraints e1
  c2 <- genVConstraints e2
  combineVConstr c1 op c2

genVConstraints (Length e) = do
  c <- genVConstraints e
  return $ c {
    intRanges = [R.SpanRange (R.lowestBound $ bsRanges c)
                             (R.highestBound $ bsRanges c)]
  }
genVConstraints (Not e) = do
  genVConstraints (Op e "/=" (ConstInt 0))
genVConstraints e =
  throwError' $ "genVConstraints " ++ show e ++ " not implemented"

verifyC :: VConstraint -> Solver VConstraint
verifyC c =
  if validVConstr c
    then return c
    else throwError' $ "**********\ninvalid combine result: " ++ show c

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


debugCombineVConstr :: VConstraint -> OpTy -> VConstraint -> Either Contradiction VConstraint
debugCombineVConstr c1 op c2 =
  case flip runState (VConstraintsBuilder M.empty "") $ runExceptT (combineVConstr c1 op c2) of
    (Left e,_)   -> Left e
    (Right c,st) -> Right c

combineVConstr :: VConstraint -> OpTy -> VConstraint -> Solver VConstraint
combineVConstr c1 "/\\" c2 = verifyTrue c1 >> verifyTrue c2 >> return cTrue
combineVConstr c1 "\\/" c2 = (verifyTrue c1 <|> verifyTrue c2) >> return cTrue
combineVConstr c1 op c2 = do -- The boolean base variants
  let c' = combineRule c1 (fst $ f op) (snd $ f op) c2
  if validVConstr c'
    then return c'
    else throwError' $ "**********\ninvalid combine result: " ++ show c' ++ ", created on:\n" ++ show c1 ++ "\nON OP[" ++ op ++ "]\n" ++ show c2
  where f :: OpTy -> ([R.Range Int] -> [R.Range Int] -> [R.Range Int], [R.Range Int] -> [R.Range Int] -> [R.Range Int])
        f "==" = (R.intersection, R.intersection)
        f "/=" = (\r1 r2 -> R.intersection (intRanges vconstraint) $ R.difference r1 r2, R.union)
        f op   = (R.intersection, (const . const) [])
combineVConstr _ op _ =
  throwError' $ "combineVConstr not implemented yet for operator: " ++ show op

verifyTrue :: VConstraint -> Solver VConstraint
verifyTrue c = do
  combineVConstr c "/=" cFalse

throwError' :: String -> Solver a
throwError' str = do
  str' <- getDebug
  throwError (str ++ "\n************" ++ str')
