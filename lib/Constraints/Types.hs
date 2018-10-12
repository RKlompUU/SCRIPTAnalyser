{-# LANGUAGE GADTs,DeriveDataTypeable #-}
module Constraints.Types where

import Control.Monad.State.Lazy
import Control.Monad.Except

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as M
import qualified Data.Range.Range as R

import qualified Debug.Trace as D
import Bitcoin.Script.Integer

import Parser.AST

import KlompStandard
import qualified Data.Typeable as T
import qualified Data.Data as TD

import qualified Data.Bitcoin.Script.Types as Bitcoin

import Control.Monad.Except
import Control.Monad.Writer

type Ident = Int
type OpIdent = String
data Expr where
  ConstInt :: Int -> Expr
  ConstBS  :: BS.ByteString -> Expr

  Hex :: String -> Expr

  EFalse   :: Expr
  ETrue    :: Expr
  Not :: Expr -> Expr

  Min :: Expr -> Expr -> Expr
  Max :: Expr -> Expr -> Expr

  Length :: Expr -> Expr

  Hash :: Expr -> Int -> Expr
  Sig  :: Expr -> Expr -> Expr
  MultiSig :: [Expr] -> [Expr] -> Expr

  BigInt :: Expr -> Expr

  Abs :: Expr -> Expr

  Var   :: Ident -> Expr
  AVar  :: Ident -> Expr
  AltVar   :: Ident -> Expr
  Op    :: Expr -> OpIdent -> Expr -> Expr
  deriving (Eq,Ord,T.Typeable,TD.Data)

instance Show Expr where
  show (ConstInt i) =
    "Int " ++ show i
  show (ConstBS bs) =
    "BS_" ++ show (BS.length bs) ++ " " ++ printBSInHex bs
  show (BigInt e) =
    "BigInt(" ++ show e ++ ")"
  show (Hex h) =
    "0x" ++ h
  show EFalse =
    "False"
  show ETrue =
    "True"
  show (Min e1 e2) =
    "Min (" ++ show e1 ++ ") (" ++ show e2 ++ ")"
  show (Max e1 e2) =
    "Max (" ++ show e1 ++ ") (" ++ show e2 ++ ")"
  show (Not e) =
    "Not (" ++ show e ++ ")"
  show (Length e) =
    "Length (" ++ show e ++ ")"
  show (Abs e) =
    "Abs (" ++ show e ++ ")"
  show (Hash e i) =
    "Hash_" ++ show i ++ " (" ++ show e ++ ")"
  show (Sig e1 e2) =
    "Sig (" ++ show e1 ++ ") (" ++ show e2 ++ ")"
  show (MultiSig e1s e2s) =
    "MultiSig " ++ show e1s ++ " " ++ show e2s
  show (Var 1) =
    "blockNum or timestamp"
  show (Var i) =
    "X_(" ++ show i ++ ")"
  show (AltVar i) =
    "Y_(" ++ show i ++ ")"
  show (AVar i) =
    "Z_(" ++ show i ++ ")"
  show (Op e1 op e2) =
    "(" ++ show e1 ++ ") " ++ op ++ " (" ++ show e2 ++ ")"

maxN  = 0x7fffffff -- 32 bit signed int
maxBN = 0x7fffffffff -- 40 bit signed int (big int, only used for locktime)
maxUN = 0xffffffff -- 32 bit unsigned int
maxBSL = 520 -- bytes
maxIntBSL = 4
maxBigIntBSL = 5
sigBL = 71
pubBL = 65

atomEs :: [TD.Constr]
atomEs =
  [TD.toConstr EFalse,
   TD.toConstr ETrue,
   TD.toConstr $ ConstInt undefined,
   TD.toConstr $ ConstBS undefined
   ]

isAtom :: Expr -> Bool
isAtom e =
  any (ccEq e) atomEs

atom2Bool :: Expr -> Bool
atom2Bool ETrue =
  True
atom2Bool EFalse =
  False
atom2Bool (ConstInt i) =
  i /= 0
atom2Bool (ConstBS bs) =
  case convert2Int (ConstBS bs) of
    Just (ConstInt 0) -> False
    _      -> True
atom2Bool (Abs e) =
  atom2Bool e

atom2BS :: Expr -> BS.ByteString
atom2BS ETrue = BS.pack [0x01]
atom2BS EFalse = BS.empty
atom2BS (ConstInt i) = asByteString (fromIntegral i)
atom2BS (ConstBS bs) = bs
{-
atom2BS (Abs e) =
  let i  = e2i e
      i' = if i < 0
            then i * (-1)
            else i
  in asByteString (fromIntegral i')
-}

lazy2StrictBS :: BSL.ByteString -> BS.ByteString
lazy2StrictBS =
  BS.concat . BSL.toChunks

convert2Int :: Expr -> Maybe Expr
convert2Int (ConstInt i) = Just $ ConstInt i
convert2Int (ConstBS bs)
  | BS.length bs <= 4 = ConstInt <$> return (fromIntegral $ asInteger bs)
{-
convert2Int (Abs e) =
  convert2Int e >>= \i -> case i of
                            (ConstInt i') -> Just $ ConstInt (if i' < 0 then i' * (-1) else i')
                            _             -> Nothing -}
convert2Int _ = Nothing

tryConvert2Int :: Expr -> Expr
tryConvert2Int e
  | isJust e' = fromJust e'
  | otherwise = e
  where e' = convert2Int e

e2i :: Expr -> Int
e2i (ConstInt i) = i
e2i e | isJust e' = e2i (fromJust e')
  where e' = convert2Int e
e2i e = error $ "Error: e2i for expr not implemented: " ++ show e

e2l :: Expr -> Int
e2l (ConstBS bs) = BS.length bs
e2l e = error $ "Error: e2l for expr not implemented: " ++ show e

data Ty =
    Ty {
      intRanges :: [R.Range Int], -- Integer bounds
      bsRanges  :: [R.Range Int]   -- ByteString representation length bounds
    }
  | NTy Ident -- Named type (instantiable in forall. closure)
  deriving (Show)

type AnnotTy = (Ident,(Bool -> Ty))

int :: Ty
int =
  Ty { intRanges = [R.SpanRange (-maxN) maxN],
       bsRanges  = [R.SpanRange 0 maxIntBSL] }
bint :: Ty
bint =
  Ty { intRanges = [R.SpanRange (-maxBN) maxBN],
       bsRanges  = [R.SpanRange 0 maxBigIntBSL] }
uint32 :: Ty
uint32 =
 Ty { intRanges = [R.SpanRange 0 maxUN],
      bsRanges  = [R.SpanRange 0 maxIntBSL] }
bool :: Ty
bool =
  Ty { intRanges = [R.SpanRange 0 1],
       bsRanges  = [R.SpanRange 0 1] }
top :: Ty
top =
 Ty { intRanges = [R.SpanRange (-maxBN) maxBN],
      bsRanges  = [R.SpanRange 0 maxBSL] }
skTy :: Ty -- Secret key type
skTy =
  top { bsRanges = [R.SpanRange 0 100] }
pkTy :: Ty -- Public key type
pkTy =
 top { bsRanges = [R.SpanRange 0 100] }

bot :: Ty
bot =
  top { intRanges = [], bsRanges = [] }

unsigned :: Ty -> Ty
unsigned t =
  t { intRanges = R.intersection (intRanges t) [R.LowerBoundRange 0] }

toInt :: Ty -> Ty
toInt t =
  Ty { intRanges = R.intersection (intRanges t) [R.SpanRange (-maxN) maxN],
       bsRanges  = R.intersection (bsRanges t) [R.SpanRange 0 maxIntBSL] }
toBool :: Ty -> Ty
toBool = id -- Always castable. From any other type.

flipTy :: Ty -> Ty
flipTy t =
  Ty { intRanges = R.difference (intRanges top) (intRanges t),
       bsRanges = R.difference (bsRanges top) (bsRanges t) }

false :: Ty
false =
 Ty { intRanges = [R.SingletonRange 0],
      bsRanges  = [R.SingletonRange 0] }
true :: Ty
true =
  Ty { intRanges = R.difference (intRanges top) (intRanges false),
       bsRanges  = R.difference (bsRanges top) (bsRanges false) }

genTrue :: Ty
genTrue =
   Ty { intRanges = [R.SingletonRange 1],
        bsRanges  = [R.SingletonRange 1] }

annotTy :: Expr -> (Expr,Ty)
annotTy e@(ConstBS bs)
  | BS.length bs <= maxIntBSL
  = (e, Ty { intRanges = [R.SingletonRange (fromIntegral $ asInteger bs)],
             bsRanges = [R.SingletonRange (BS.length bs)] } )
  | otherwise
  = (e, Ty { intRanges = [],
             bsRanges = [R.SingletonRange (BS.length bs)] } )
annotTy e@(ConstInt i) =
  (e, int { intRanges = [R.SingletonRange i],
            bsRanges = [R.SingletonRange (BS.length (asByteString (fromIntegral i)))] })
annotTy e@(BigInt e_)
  | isAtom e_ = let bs = atom2BS e_
                in (e, if BS.length bs <= 5
                          then bint { intRanges = [R.SingletonRange (fromIntegral $ asInteger bs)],
                                      bsRanges = [R.SingletonRange (BS.length bs)] }
                          else bot)
  | otherwise = (e, bint)
annotTy e@(Hash _ l) =
  (e, Ty { intRanges = [], bsRanges = [R.SingletonRange l] } )
annotTy e@(Length _) =
  (e, int { intRanges = [R.SpanRange 0 520] } )
annotTy ETrue =
  (ETrue, true)
annotTy EFalse =
  (EFalse, false)
annotTy e@(Abs _) =
  (e, int { intRanges = [R.SpanRange 0 maxN] })
annotTy e@(Not _) =
  (e, bool)
annotTy e@(Op _ op _)
  | any (==op) (cmpOps ++ boolOps) =
    (e, bool)
annotTy e@(Op _ op _)
  | any (==op) (numOps) =
    (e, bint)
annotTy e =
  error $ "annotTy not implemented (yet) for " ++ show e

cmpOps =
  ["==","/=","<=","<",">=",">"]
boolOps =
  ["/\\","\\/"]
numOps =
  ["+","-","*"]

opTys :: OpIdent -> BranchBuilder ((Ty -> Ty),(Ty -> Ty),Ty)
opTys "<"   = return $ (toInt,toInt,bool)
opTys ">"   = return $ (toInt,toInt,bool)
opTys "<="  = return $ (toInt,toInt,bool)
opTys ">="  = return $ (toInt,toInt,bool)
opTys "/\\" = return $ (toInt,toInt,bool)
opTys "\\/" = return $ (toInt,toInt,bool)
opTys "+"   = return $ (toInt,toInt,bint) -- bint because of possible over/underflow
opTys "-"   = return $ (toInt,toInt,bint) -- bint because of possible over/underflow
opTys "*"   = return $ (toInt,toInt,bint) -- bint because of possible over/underflow

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
  C_Spec   :: Expr -> ValConstraint
  deriving (Eq)

isSpecCnstr :: ValConstraint -> Bool
isSpecCnstr (C_Spec _) = True
isSpecCnstr _ = False

instance Show ValConstraint where
  show (C_IsTrue e) =
    "Constraint: " ++ show e
  show (C_Spec e) =
    "Specified value: " ++ show e

addCnstr :: ValConstraint -> BranchBuilder ()
addCnstr c = do
  st <- get
  put $ st {val_cnstrs = c : val_cnstrs st}

type BranchBuilder a = ExceptT String (StateT BuildState IO) a

failBranch :: String -> BranchBuilder a
failBranch = throwError

unwrapBuildMonad :: BranchBuilder () -> IO BranchReport
unwrapBuildMonad b = do
  r <- flip runStateT (initBuildState) $ runExceptT b
  case r of
    (Left e,st)    -> return $ branchReport { symbolicEval = st {val_cnstrs = postCnstrs ++ val_cnstrs st}, symbolicErrs = Just e, branchBuilder = b }
    (Right _,st) -> return $ branchReport { symbolicEval = st {val_cnstrs = postCnstrs ++ val_cnstrs st}, symbolicErrs = Nothing, branchBuilder = b }

type Stack = [Expr]
data BuildState =
  BuildState {
    ty_cnstrs  :: M.Map Expr Ty,
    val_cnstrs :: [ValConstraint], -- Will be verified after generation of constrains, using gnu prolog
    stack      :: Stack,
    freshV     :: Ident,
    nTy        :: Ident,
    branchInfo :: [(Label,Bool)],
    muts       :: [BranchMutation],
    altStack  :: Stack,
    freshAltV :: Ident,
    freshAV   :: Ident,
    successMsigConts :: [(Ident,[(Int,Int)])]
  }
initBuildState =
  BuildState {
    ty_cnstrs  = initialTypes,
    val_cnstrs = [],
    stack      = [],
    branchInfo = [],
    freshV     = 0,
    nTy        = 0,
    muts       = [],
    altStack   = [],
    freshAltV  = 0,
    freshAV    = 0,
    successMsigConts = []
  }

rerunFromContinuation :: BuildState -> BuildState
rerunFromContinuation bs =
  let msigConts = (\((ident,conts):xs) -> ((ident,tail conts):xs))
                $ successMsigConts bs
  in initBuildState {
      successMsigConts = msigConts
     }


logSuccessMsigContinuation :: Ident -> [(Int,Int)] -> BranchBuilder ()
logSuccessMsigContinuation ident conts = do
  st <- get
  let msigConts = successMsigConts st
      msigConts' =
        case findIndex ((== ident) . fst) msigConts of
          Just i -> replaceIndex msigConts i (ident,conts)
          Nothing -> (ident,conts) : msigConts
  put st { successMsigConts = msigConts' }

continueFromMsigContinuation :: Ident -> [(Int,Int)] -> BranchBuilder [(Int,Int)]
continueFromMsigContinuation ident conts = do
  st <- get
  case lookup ident (successMsigConts st) of
    Just conts' -> return conts'
    Nothing   -> return conts


rerunableBranch :: BranchReport -> Bool
rerunableBranch r =
  let msigConts = successMsigConts
                $ symbolicEval r
  in any (not . null) msigConts

data BranchReport =
  BranchReport {
    branchID     :: Int,
    symbolicEval :: BuildState,
    branchBuilder :: BranchBuilder (),
    symbolicErrs :: Maybe String,
    prologValid  :: Bool,
    prologReport :: String
  }
branchReport =
  BranchReport {
    branchID     = 0,
    symbolicEval = undefined,
    branchBuilder = undefined,
    symbolicErrs = Just "Not applicable",
    prologValid  = False,
    prologReport = "Not applicable"
  }

type IOReport a = ExceptT String (WriterT String IO) a

txSeqNum = 4194549

-- Var 1: represents the corresponding transaction's locktime value
-- Var 2: delta time between current transaction and the output's original transaction
initialTypes = M.fromList [(EFalse,false),
                           (ETrue,true),
                           (Var 1,uint32),
                           (Var 2,int {intRanges = [R.SingletonRange txSeqNum]}),
                           --(Var 2,int),
                           (Op (Var 2) "&" (Hex "0000FFFF"),int),
                           (Op (Var 2) "&" (Hex "00400000"),int),
                           (Op (Op (Var 2) "&" (Hex "00400000")) ">>" (ConstInt 22),bool),
                           (Hex "00400000",int),
                           (Hex "0000FFFF",int),
                           annotTy (ConstInt 22),
                           annotTy (ConstInt 31),
                           annotTy (ConstInt 1),
                           annotTy (ConstInt (-1)),
                           (Not (Op (Op (Var 2) "&" (Hex "00400000")) ">>" (ConstInt 22)),bool)]--(Var 2,int)]
postCnstrs = [C_Spec (Op (Op (Var 2) "&" (Hex "00400000")) ">>" (ConstInt 22)),
              C_Spec (Op (Var 2) "&" (Hex "0000FFFF"))]

knowledgeBased :: Expr -> Bool
knowledgeBased e =
  not $ null (varsInE False False e)

knowledgeCnstrsWithVar :: BuildState -> [[Expr]]
knowledgeCnstrsWithVar b =
  map nub
  $ map varsInC (val_cnstrs b)

varsInC :: ValConstraint -> [Expr]
varsInC (C_IsTrue e) =
  varsInE False False e

varsInE :: Bool -> Bool -> Expr -> [Expr]
varsInE inOp knowledgeReq (Hash e _) =
  let b' = inOp || knowledgeReq
  in varsInE b' b' e
varsInE inOp _ (Sig e1 e2) =
  varsInE inOp True e1 ++ varsInE inOp True e2
varsInE inOp _ (MultiSig es1 es2) =
  concat $ map (varsInE inOp True) es1 ++ map (varsInE inOp True) es2
varsInE inOp knowledgeReq (Length e) =
  varsInE inOp knowledgeReq e
varsInE _ knowledgeReq (Op e1 _ e2) =
  varsInE True knowledgeReq  e1 ++ varsInE True knowledgeReq e2
varsInE inOp knowledgeReq e@(Var _)
  | knowledgeReq = [e]
varsInE _ _ _ = []

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
  (not . null) (bsRanges t) -- &&
--  ((not . null) (intRanges t) ||
--   (not . null) (R.intersection [R.SpanRange 5 maxBSL] (bsRanges t)))


data BranchMutation =
    Popped Expr Stack
  | Pushed Expr Stack
  | Infered Expr Ty
  | Executing Label Bitcoin.ScriptOp
  | Log String

instance Show BranchMutation where
  show (Popped e s) = "Popped " ++ show e ++ "\n\t\t |-> " ++ show s
  show (Pushed e s) = "Pushed " ++ show e ++ "\n\t\t |-> " ++ show s
  show (Infered e t)  = "Infering that: " ++ show e ++ " :: " ++ show t
  show (Executing lbl op) = "------------------\n\tProgram pointer at " ++ show lbl ++ ": " ++ show op ++ "\n\t"
  show (Log str) = "*** " ++ str



instance Show BuildState where
  show s = "BuildState {\n\tty_cnstrs:\n\t\t" ++ (intercalate "\n\t\t" (map show (M.toList $ ty_cnstrs s))) ++
            "\n\tval_cnstrs:\n\t\t" ++ (intercalate "\n\t\t" (map show (val_cnstrs s))) ++
           ",\n\tstack:{\n" ++ printStack (stack s) ++ "}" ++
           ",\n\taltStack:{\n" ++ show (altStack s) ++ "}" ++
           ",\n\tbranch history:\n\t.. " ++
           intercalate "\n\t.. " (map show $ (reverse $ muts s))


printStack :: Show a => [a] -> String
printStack [] = "head -> |------------------|\n"
printStack (x:xs) =
  let sep = "\t|------------------|\n"
      xs' = concatMap (\e -> "\t| " ++ show e ++ "\n") xs
      x'  = "head -> | " ++ show x ++ "\n"
  in sep ++ x' ++ xs' ++ sep
