{-# LANGUAGE GADTs #-}
module Constraints.Types where

import qualified Data.ByteString as BS
import Data.List
import Data.Maybe

import qualified Debug.Trace as D

type Ident = Int
type OpTy = String
data Expr where
  ConstInt :: Int -> Expr
  ConstBS  :: BS.ByteString -> Expr

  Length :: Expr -> Expr
  Abs :: Expr -> Expr
  -- Not: \v -> if v == 0
  --              then Not(v) = 1
  --              else Not(v) = 0
  Not :: Expr -> Expr
  Min :: Expr -> Expr -> Expr
  Max :: Expr -> Expr -> Expr

  Hash :: Expr -> Expr
  Sig  :: Expr -> Expr -> Expr
  MultiSig :: [Expr] -> [Expr] -> Expr

  Var   :: Ident -> Expr
  Op    :: Expr -> OpTy -> Expr -> Expr
  deriving (Show,Eq)

flipOpSet =
  [
  ("==","=="),
  ("/=","/="),
  ("+","+"),
  ("<",">"),
  (">","<"),
  ("<=","=>"),
  (">=","<="),
  ("/\\","/\\"),
  ("\\/","\\/")
  ]

flipOpSet' =
  [
  ("==",(True,"/=")),
  ("/=",(True,"==")),
  (">",(True,"<")),
  ("<",(True,">")),
  (">=",(True,"<=")),
  ("<=",(True,">=")),
  ("/\\",(False,"\\/")),
  ("\\/",(False,"/\\"))
  ]


data BConstraints where
  ExprConstr :: Expr -> BConstraints
  AndConstr  :: BConstraints -> BConstraints -> BConstraints
  OrConstr   :: BConstraints -> BConstraints -> BConstraints
  NotConstr  :: BConstraints -> BConstraints
  TrueConstr :: BConstraints

falseConstr = NotConstr TrueConstr

normalize :: BConstraints -> Bool -> BConstraints
normalize c_@(NotConstr c) b = D.trace ("normalize c_: " ++ show c_ ++ ", |----> " ++ show (normalize c (not b))) normalize c (not b)
normalize (AndConstr c1 c2) b =
  let c1' = normalize c1 b
      c2' = normalize c2 b
  in (if b then AndConstr else OrConstr) c1' c2'
normalize (OrConstr c1 c2) b =
  let c1' = normalize c1 b
      c2' = normalize c2 b
  in (if b then OrConstr else AndConstr) c1' c2'
normalize TrueConstr True  = TrueConstr
normalize TrueConstr False = NotConstr TrueConstr
normalize (ExprConstr e) True = ExprConstr e
normalize (ExprConstr e) False = ExprConstr $ flipE e
  where flipE (Op e1 op e2)
          | isJust flipIndex && flippingDone = Op e1 op' e2
          | isJust flipIndex = Op (flipE e1) op' (flipE e2)
          where flipIndex = find ((==) op . fst) flipOpSet'
                flippingDone = (fst . snd) $ fromJust flipIndex
                op' = (snd . snd) $ fromJust flipIndex
        flipE e' = Not e'


instance Show BConstraints where
  show (ExprConstr e) = show e
  show (AndConstr b0 b1) = show b0 ++ " && " ++ show b1
  show (OrConstr  b0 b1) = "(" ++ show b0 ++ ") || (" ++ show b1 ++ ")"
  show (NotConstr c) = "Not (" ++ show c ++ ")"
  show TrueConstr = "True"

data BranchMutation =
    Popped Expr Stack
  | Pushed Expr Stack
  | Infered BConstraints

instance Show BranchMutation where
  show (Popped e s) = "Popped " ++ show e ++ "\n\t\t |-> " ++ show s
  show (Pushed e s) = "Pushed " ++ show e ++ "\n\t\t |-> " ++ show s
  show (Infered c)  = "Infering that: " ++ show c

type Stack = [Expr]
data BuildState =
  BuildState {
    cnstrs    :: BConstraints,
    stack     :: Stack,
    altStack  :: Stack,
    freshV    :: Ident,
    freshAltV :: Ident,
    muts      :: [BranchMutation]
  }
initBuildState =
  BuildState {
    cnstrs    = TrueConstr,
    stack     = [],
    altStack  = [],
    freshV    = 0,
    freshAltV = -1,
    muts      = []
  }

instance Show BuildState where
  show s = "BuildState {\n\tcnstrs: " ++ show (cnstrs s) ++
           ",\n\tstack: " ++ show (stack s) ++
           ",\n\taltStack: " ++ show (altStack s) ++
           ",\n\tbranch history:\n\t.. " ++
           intercalate "\n\t.. " (map show $ (reverse $ muts s)) ++
           "}\n"

