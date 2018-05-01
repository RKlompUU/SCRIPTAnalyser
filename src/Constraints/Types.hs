{-# LANGUAGE GADTs #-}
module Constraints.Types where

import Control.Monad.State.Lazy

import qualified Data.ByteString as BS
import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as M

import qualified Debug.Trace as D

type Ident = Int
type OpTy = String
data Expr where
  ConstInt :: Int -> Expr
  ConstBS  :: BS.ByteString -> Expr
  EFalse   :: Expr
  ETrue    :: Expr

  --Length :: Expr -> Expr
  --Abs :: Expr -> Expr
  -- Not: \v -> if v == 0
  --              then Not(v) = 1
  --              else Not(v) = 0
  --Not :: Expr -> Expr
  --Min :: Expr -> Expr -> Expr
  --Max :: Expr -> Expr -> Expr

  --Hash :: Expr -> Expr
  --Sig  :: Expr -> Expr -> Expr
  --MultiSig :: [Expr] -> [Expr] -> Expr

  Var   :: Ident -> Expr
  Op    :: Expr -> OpTy -> Expr -> Expr
  deriving (Show,Eq)


maxN = 0x7fffffff -- 32 bit signed int
maxBSL = 520 -- bytes
maxIntBSL = 4
hashOutBL = 32
sigBL = 71
pubBL = 65

data Ty =
    Ty {
      intRanges :: [R.Range Int], -- Integer bounds
      bsRanges  :: [R.Range Int]   -- ByteString representation length bounds
    }
  | NTy Ident -- Named type (instantiable in forall. closure)

int :: Ty
int =
  Ty { intRanges = [R.SpanRange (-maxN) maxN],
       bsRanges  = [R.SpanRange 0 maxIntBSL] }
bool :: Ty
bool =
  Ty { intRanges = [R.SpanRange 0 1],
       bsRanges  = [R.SpanRange 0 1] }
top :: Ty
top =
  Ty { intRanges = [R.SpanRange (-maxN) maxN],
       bsRanges  = [R.SpanRange 0 maxBSL] }

true :: Ty
true =
  Ty { intRanges = [R.SingletonRange 1],
       bsRanges  = [R.SingletonRange 1] }
false :: Ty
false =
  Ty { intRanges = [R.SingletonRange 0],
       bsRanges  = [R.SingletonRange 0] }

bsTy :: BS.ByteString -> Ty
bsTy bs
  | BS.length <= maxIntBSL
  = int { bsRanges = [R.SingletonRange (BS.length bs)] }
  | otherwise
  = Ty { intRanges = [],
         bsRanges = [R.SingletonRange (BS.length bs)] }


opTys :: Op -> BranchBuilder (Ty,Ty,Ty)
opTys "=="  = do
  nTy <- genNTy
  return $ (nTy,nTy,bool)
opTys "<"   = return $ (int,int,bool)
opTys ">"   = return $ (int,int,bool)
opTys "<="  = return $ (int,int,bool)
opTys ">="  = return $ (int,int,bool)
opTys "/\\" = return $ (int,int,bool)
opTys "\\/" = return $ (int,int,bool)
opTys "+"   = return $ (int,int,bool)
opTys "-"   = return $ (int,int,bool)

tySet :: Expr -> Ty -> BranchBuilder ()
tySet e t' =
  st <- get
  let t = (cnstrs st) M.?! e
  t_ <- case t of
          Just t_   -> tySubst t t'
          otherwise -> return t'
  put (st {cnstrs = M.insert e t_ (constrs st)})

tyGet :: Expr -> BranchBuilder Ty
tyGet e =
  st <- get
  case M.lookup e (cnstrs st) of
    Just t  -> return t
    Nothing -> throwError ("tyGet called for unmapped expression: " ++ show e)

type BranchBuilder a = State BuildState a

type Stack = [Expr]
data BuildState =
  BuildState {
    cnstrs    :: M.Map Expr Ty,
    stack     :: Stack,
    freshV    :: Ident,
    nTy       :: Ident,
    muts      :: [BranchMutation]
--    altStack  :: Stack,   Alststack ignored for now
--    freshAltV :: Ident,   Alststack ignored for now
  }
initBuildState =
  BuildState {
    cnstrs    = M.empty,
    stack     = [],
    freshV    = 0,
    nTy       = 0,
    muts      = []
  }
genNTy :: BranchBuilder Ident
genNTy = do
  st <- get
  put (st {nTy = nTy st + 1})
  return $ NTy (nTy st)

tySubst :: Ty -> Ty -> BranchBuilder Ty
tySubst (NTy n1) (NTy n2) =
  throwError "tySubst not (yet) implemented for 2 NTy args"
tySubst (NTy n) t =
  throwError "tySubst not (yet) implemented for 1 NTy arg"
tySubst t n@(NTy _) =
  tySubst n t
tySubst t1 t2 =
  let t' = Ty { intRanges = R.intersection (intRanges t1) (intRanges t2),
                bsRanges  = R.intersection (bsRanges t1) (bsRanges t2) }
  tyOK t'
  return t'

tyOK :: Ty -> BranchBuilder ()
tyOK t
  | (not . null) (bsRanges t) &&
    ((not . null) (intRanges t) ||
     (not . null) (R.intersection [R.SpanRange 5 maxBSL] (bsRanges c))) = return ()
  | otherwise = throwError ("ty NOT OK")


data BranchMutation =
    Popped Expr Stack
  | Pushed Expr Stack
  | Infered Expr Ty

instance Show BranchMutation where
  show (Popped e s) = "Popped " ++ show e ++ "\n\t\t |-> " ++ show s
  show (Pushed e s) = "Pushed " ++ show e ++ "\n\t\t |-> " ++ show s
  show (Infered e t)  = "Infering that: " ++ show e ++ " :: " ++ t



instance Show BuildState where
  show s = "BuildState {\n\tcnstrs: " ++ show (cnstrs s) ++
           ",\n\tstack: " ++ show (stack s) ++
           ",\n\taltStack: " ++ show (altStack s) ++
           ",\n\tbranch history:\n\t.. " ++
           intercalate "\n\t.. " (map show $ (reverse $ muts s)) ++
           "}\n"
