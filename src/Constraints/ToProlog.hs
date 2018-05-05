module Constraints.ToProlog where

import Constraints.Types
import Data.List
import Data.Maybe
import Control.Monad.Except
import Control.Monad.Trans.Reader
import Control.Monad.Writer
import Control.Monad
import qualified Data.Map as M
import qualified Data.Range.Range as R

type PL = String
type PrologWriter a = ReaderT (BuildState,Expr -> Ident) (ExceptT String (Writer PL)) a

askTy :: Expr -> PrologWriter AnnotTy
askTy e = do
  i <- (\f -> f e) <$> snd <$> ask
  (\m -> (i, m M.! e)) <$> ty_cnstrs <$> fst <$> ask

plFact :: PL -> PrologWriter ()
plFact pl =
  if null pl
    then return ()
    else tell $ pl ++ ",\n"

branchToProlog :: BuildState -> Either String PL
branchToProlog b =
  let eIdents = \e -> M.findIndex e $ ty_cnstrs b
  in case runWriter (runExceptT (runReaderT bToProlog (b,eIdents))) of
    (Left e,_)   -> Left e
    (Right _,pl) -> Right pl

bToProlog :: PrologWriter ()
bToProlog = do
  tell $ ":- use_module(library(clpfd)).\n\n"
  css <- val_cnstrs <$> fst <$> ask
  mapM_ (\(i,c) -> addStmt i c) (zip [0..] css)

addStmt :: Int -> ValConstraint -> PrologWriter ()
addStmt i c = do
  tell $ "s" ++ show i ++ " :-\n"
  cToProlog c
  tell "true.\n"

cToProlog :: ValConstraint -> PrologWriter ()
cToProlog (C_IsTrue e) = do
  mapM_ (\e_ -> op2Prolog e_) (opsInE e)
cToProlog (C_Not c) = do
  tell "#\\ "
  cToProlog c
cToProlog c =
  throwError $ "cToProlog not implemented for: " ++ show c


opsInE :: Expr -> [Expr]
opsInE e@(Op _ _ _) = [e]
opsInE (Sig e1 e2) = opsInE e1 ++ opsInE e2
opsInE (MultiSig es1 es2) = concat $ map opsInE es1 ++ map opsInE es2
opsInE (Hash e _) = opsInE e
opsInE (Length e) = opsInE e
opsInE _ = []


op2Prolog :: Expr -> PrologWriter ()
op2Prolog (Op e1 "==" e2) = do
  t1 <- askTy e1
  t2 <- askTy e2
  relateTys t1 "==" t2
op2Prolog _ = return ()


boolE2Prolog :: Expr -> PrologWriter PL
boolE2Prolog ETrue =
  return "1"
boolE2Prolog EFalse =
  return "0"
boolE2Prolog (ConstInt i) =
  return $ show i
boolE2Prolog (Op e1 op e2)
  | isJust boolFDOp = do
      p1 <- boolE2Prolog e1
      let pOp = fromJust boolFDOp
      p2 <- boolE2Prolog e2
      return $ p1 ++ pOp ++ p2
  where boolFDOp = lookup op boolFDOps
boolE2Prolog (Var n) =
  return $ "X" ++ show n
boolE2Prolog (Sig _ _) = return ""
boolE2Prolog (Hash _ _) = return ""
boolE2Prolog (MultiSig _ _) = return ""
boolE2Prolog e =
  throwError $ "e2Prolog not implemented for: " ++ show e

boolFDOps :: [(OpIdent,String)]
boolFDOps =
  [("\\/", " #\\/ "),
   ("/\\", " #/\\ ")]

relateTys :: AnnotTy -> OpIdent -> AnnotTy -> PrologWriter ()
relateTys t1 "==" t2 = do
  stateTy t1
  stateTy t2

  plFact $ tyBSPL t1 ++ " #= " ++ tyBSPL t2

tyBSPL :: AnnotTy -> PL
tyBSPL (i,_) = "T" ++ show i ++ "bs"
tyIPL :: AnnotTy -> PL
tyIPL (i,_) = "T" ++ show i ++ "ints"

stateTy :: AnnotTy -> PrologWriter ()
stateTy t@(i,ty) = do
  let bsFact = tyBSPL t ++ " in " ++ (intercalate " \\/ " $ map range2PL (bsRanges ty))
  plFact bsFact

  when (not $ null $ intRanges ty) $ do
    let intFact = "T" ++ show i ++ "ints in " ++ (intercalate " \\/ " $ map range2PL (intRanges ty))
    plFact intFact

range2PL :: R.Range Int -> PL
range2PL (R.SingletonRange i) =
  show i
range2PL (R.SpanRange i1 i2) =
  show i1 ++ ".." ++ show i2
{-
toProlog :: BConstraints -> String
toProlog c =
  let cPL = toProlog_df c
      fv  = map (\i -> "X" ++ show i)
          $ nub
          $ freeVars c
  in "r(" ++ intercalate "," fv ++ ") :- "
      ++ cPL ++ "."

toProlog_df :: BConstraints -> String
toProlog_df (ExprConstr e) =
  e2PL_df e
toProlog_df (AndConstr c1 c2) =
  let c1PL = toProlog_df c1
      c2PL = toProlog_df c2
  in "(" ++ c1PL ++ "),(" ++ c2PL ++ ")"
toProlog_df (OrConstr c1 c2) =
  let c1PL = toProlog_df c1
      c2PL = toProlog_df c2
  in "(" ++ c1PL ++ ");(" ++ c2PL ++ ")"
toProlog_df (NotConstr c) =
  let cPL = toProlog_df c
  in "\\+ (" ++ cPL ++ ")"
toProlog_df TrueConstr =
  "true"

e2PL_df :: Expr -> String
e2PL_df (Op e1 op e2) =
  let e1PL = e2PL_df e1
      e2PL = e2PL_df e2
  in "(" ++ e1PL ++ ") " ++ op ++ " (" ++ e2PL ++ ")"
e2PL_df (Var x) = "X" ++ show x
e2PL_df (Not e) = "\\+ (" ++ e2PL_df e ++ ")"
e2PL_df (ConstInt i) = show i
e2PL_df e = show e

freeVars :: BConstraints -> [Ident]
freeVars (ExprConstr e) = freeVarsE e
freeVars (AndConstr c1 c2) = freeVars c1 ++ freeVars c2
freeVars (OrConstr c1 c2) = freeVars c1 ++ freeVars c2
freeVars (NotConstr c) = freeVars c
freeVars TrueConstr = []

freeVarsE :: Expr -> [Ident]
freeVarsE (Var x) = [x]
freeVarsE (Abs e) = freeVarsE e
freeVarsE (Length e) = freeVarsE e
freeVarsE (Not e) = freeVarsE e
freeVarsE (Min e1 e2) = freeVarsE e1 ++ freeVarsE e2
freeVarsE (Max e1 e2) = freeVarsE e1 ++ freeVarsE e2
freeVarsE (Hash e) = freeVarsE e
freeVarsE (Sig e1 e2) = freeVarsE e1 ++ freeVarsE e2
freeVarsE (Op e1 _ e2) = freeVarsE e1 ++ freeVarsE e2
freeVarsE _ = []
-}
