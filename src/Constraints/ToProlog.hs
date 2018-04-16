module Constraints.ToProlog where

import Constraints.Types
import Data.List

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
