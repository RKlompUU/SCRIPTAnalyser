module Constraints.ToProlog where

import Constraints.Types
import Data.List
import Data.Maybe
import Control.Monad.Except
import Control.Monad.Trans.Reader
import Control.Monad.Writer

type PL = String
type PrologWriter a = ReaderT BuildState (ExceptT String (Writer PL)) a

plFact :: PL -> PrologWriter PL
plFact pl =
  if null pl
    then return ""
    else return $ pl ++ ".\n"

branchToProlog :: BuildState -> Either String PL
branchToProlog b =
  case runWriter (runExceptT (runReaderT bToProlog b)) of
    (Left e,_)   -> Left e
    (Right _,pl) -> Right pl

bToProlog :: PrologWriter ()
bToProlog = do
  css <- val_cnstrs <$> ask
  pls <- mapM (\c -> cToProlog c >>= plFact) css
  mapM_ tell pls

cToProlog :: ValConstraint -> PrologWriter PL
cToProlog (C_IsTrue e) = do
  e2Prolog e
cToProlog (C_Not c) = do
  pl <- cToProlog c
  if null pl -- Can be null if c only contains expressions that are not verifiable (e.g. Sig x y)
    then return ""
    else return $ "#\\ " ++ pl
cToProlog c =
  throwError $ "cToProlog not implemented for: " ++ show c

e2Prolog :: Expr -> PrologWriter PL
e2Prolog ETrue =
  return "1"
e2Prolog EFalse =
  return "0"
e2Prolog (ConstInt i) =
  return $ show i
e2Prolog (Op e1 op e2)
  | isJust boolFDOp = do
    p1 <- e2Prolog e1
    let pOp = fromJust boolFDOp
    p2 <- e2Prolog e2
    return $ p1 ++ pOp ++ p2
  where boolFDOp = lookup op boolFDOps
e2Prolog (Var n) =
  return $ "X" ++ show n
e2Prolog (Sig _ _) = return ""
e2Prolog (Hash _) = return ""
e2Prolog (MultiSig _ _) = return ""
e2Prolog e =
  throwError $ "e2Prolog not implemented for: " ++ show e

boolFDOps :: [(OpIdent,String)]
boolFDOps =
  [("\\/", " #\\/ "),
   ("/\\", " #/\\ ")]

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
