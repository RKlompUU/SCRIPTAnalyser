{-# LANGUAGE GADTs #-}
module Constraints.Types where

import Control.Monad.State.Lazy
import Control.Monad.Except

import qualified Data.ByteString as BS
import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as M
import qualified Data.Range.Range as R

import qualified Debug.Trace as D
import Bitcoin.Script.Integer

type Ident = Int
type OpIdent = String
data Expr where
  ConstInt :: Int -> Expr
  ConstBS  :: BS.ByteString -> Expr
  EFalse   :: Expr
  ETrue    :: Expr

  Length :: Expr -> Expr
  --Abs :: Expr -> Expr
  -- Not: \v -> if v == 0
  --              then Not(v) = 1
  --              else Not(v) = 0
  --Not :: Expr -> Expr
  --Min :: Expr -> Expr -> Expr
  --Max :: Expr -> Expr -> Expr

  Hash :: Expr -> Expr
  Sig  :: Expr -> Expr -> Expr
  --MultiSig :: [Expr] -> [Expr] -> Expr

  Var   :: Ident -> Expr
  Op    :: Expr -> OpIdent -> Expr -> Expr
  deriving (Show,Eq,Ord)


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
  deriving (Show)

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
hashOutTy :: Ty
hashOutTy =
  Ty { intRanges = [],
       bsRanges  = [R.SingletonRange hashOutBL] }
skTy :: Ty -- Secret key type
skTy =
  top { bsRanges = [R.SpanRange 0 100] }
pkTy :: Ty -- Public key type
pkTy =
 top { bsRanges = [R.SpanRange 0 100] }

toInt :: Ty -> Ty
toInt t =
  Ty { intRanges = R.intersection (intRanges t) [R.SpanRange (-maxN) maxN],
       bsRanges  = R.intersection (bsRanges t) [R.SpanRange 0 maxIntBSL] }
toBool :: Ty -> Ty
toBool = const bool -- Always castable. From any other type.

false :: Ty
false =
 Ty { intRanges = [R.SingletonRange 0],
      bsRanges  = [R.SingletonRange 0] }
true :: Ty
true =
  Ty { intRanges = R.difference (intRanges top) (intRanges false),
       bsRanges  = R.difference (bsRanges top) (bsRanges false) }

annotTy :: Expr -> (Expr,Ty)
annotTy e@(ConstBS bs)
  | BS.length bs <= maxIntBSL
  = (e, Ty { intRanges = [R.SingletonRange (fromIntegral $ asInteger bs)],
             bsRanges = [R.SingletonRange (BS.length bs)] } )
  | otherwise
  = (e, Ty { intRanges = [],
             bsRanges = [R.SingletonRange (BS.length bs)] } )
annotTy e@(ConstInt i) =
  (e, int { intRanges = [R.SingletonRange i] })
annotTy e@(Hash _) =
  (e, hashOutTy)
annotTy e@(Length _) =
  (e, top)

opTys :: OpIdent -> BranchBuilder ((Ty -> Ty),(Ty -> Ty),Ty)
opTys "<"   = return $ (toInt,toInt,bool)
opTys ">"   = return $ (toInt,toInt,bool)
opTys "<="  = return $ (toInt,toInt,bool)
opTys ">="  = return $ (toInt,toInt,bool)
opTys "/\\" = return $ (toInt,toInt,bool)
opTys "\\/" = return $ (toInt,toInt,bool)
opTys "+"   = return $ (toInt,toInt,int) -- Or maybe not.. because of overflow
opTys "-"   = return $ (toInt,toInt,int) -- Or maybe not.. because of overflow

tySet :: Expr -> Ty -> BranchBuilder ()
tySet e t' = do
  st <- get
  let maybeT = (ty_cnstrs st) M.!? e
  t_ <- case maybeT of
          Just t  -> tySubst t t'
          Nothing -> return t'
  put (st {ty_cnstrs = M.insert e t_ (ty_cnstrs st)})

tyCast :: Expr -> (Ty -> Ty) -> BranchBuilder ()
tyCast e c = do
  t <- tyGet e
  t' <- cast c t

  st <- get
  put (st {ty_cnstrs = M.insert e t' (ty_cnstrs st)})

tyGet :: Expr -> BranchBuilder Ty
tyGet e = do
  st <- get
  case M.lookup e (ty_cnstrs st) of
    Just t  -> return t
    Nothing -> throwError ("tyGet called for unmapped expression: " ++ show e)

data ValConstraint where
  C_IsTrue :: Expr -> ValConstraint
  C_Not    :: ValConstraint -> ValConstraint

type BranchBuilder a = ExceptT String (State BuildState) a

failBranch :: String -> BranchBuilder a
failBranch = throwError

unwrapBuildMonad :: BranchBuilder a -> Either (BuildState,String) BuildState
unwrapBuildMonad b =
  case flip runState (initBuildState) $ runExceptT b of
    (Left e,st)    -> Left (st,e)
    (Right _,st) -> Right st

type Stack = [Expr]
data BuildState =
  BuildState {
    ty_cnstrs  :: M.Map Expr Ty,
    val_cnstrs :: [ValConstraint], -- Will be verified after generation of constrains, using gnu prolog
    stack      :: Stack,
    freshV     :: Ident,
    nTy        :: Ident,
    muts       :: [BranchMutation]
--    altStack  :: Stack,   Alststack ignored for now
--    freshAltV :: Ident,   Alststack ignored for now
  }
initBuildState =
  BuildState {
    ty_cnstrs    = M.empty,
    val_cnstrs = [],
    stack     = [],
    freshV    = 0,
    nTy       = 0,
    muts      = []
  }
genNTy :: BranchBuilder Ty
genNTy = do
  st <- get
  put (st {nTy = nTy st + 1})
  return $ NTy (nTy st)

cast :: (Ty -> Ty) -> Ty -> BranchBuilder Ty
cast c t = do
  let t' = c t
  if tyOK t'
    then return t'
    else throwError ("ty NOT OK, before cast: " ++ show t)

tySubst :: Ty -> Ty -> BranchBuilder Ty
tySubst (NTy n1) (NTy n2) =
  throwError "tySubst not (yet) implemented for 2 NTy args"
tySubst (NTy n) t =
  throwError "tySubst not (yet) implemented for 1 NTy arg"
tySubst t n@(NTy _) =
  tySubst n t
tySubst t1 t2 = do
  let t' = Ty { intRanges = R.intersection (intRanges t1) (intRanges t2),
                bsRanges  = R.intersection (bsRanges t1) (bsRanges t2) }
  if tyOK t'
    then return t'
    else throwError ("ty NOT OK. Subst of: " ++ show t1 ++ ", and: " ++ show t2)

tyOK :: Ty -> Bool
tyOK t =
  (not . null) (bsRanges t) &&
  ((not . null) (intRanges t) ||
   (not . null) (R.intersection [R.SpanRange 5 maxBSL] (bsRanges t)))


data BranchMutation =
    Popped Expr Stack
  | Pushed Expr Stack
  | Infered Expr Ty

instance Show BranchMutation where
  show (Popped e s) = "Popped " ++ show e ++ "\n\t\t |-> " ++ show s
  show (Pushed e s) = "Pushed " ++ show e ++ "\n\t\t |-> " ++ show s
  show (Infered e t)  = "Infering that: " ++ show e ++ " :: " ++ show t



instance Show BuildState where
  show s = "BuildState {\n\tty_cnstrs:\n\t\t" ++ (intercalate "\n\t\t" (map show (M.toList $ ty_cnstrs s))) ++
            "\n\tval_cnstrs:\n\t\t" ++ (intercalate "\n\t\t" (map show (val_cnstrs s))) ++
           ",\n\tstack: " ++ show (stack s) ++
          -- ",\n\taltStack: " ++ show (altStack s) ++
           ",\n\tbranch history:\n\t.. " ++
           intercalate "\n\t.. " (map show $ (reverse $ muts s)) ++
           "}\n"
