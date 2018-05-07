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
import qualified Data.ByteString as BS
import Bitcoin.Script.Integer

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

tellTy :: Expr -> PrologWriter ()
tellTy e =
  askTy e >>= stateTy

addStmt :: Int -> ValConstraint -> PrologWriter ()
addStmt i c = do
  tell $ "s" ++ show i ++ " :-\n"

  let es = esInC c
  mapM_ tellTy es

  cToProlog c
  tell "true.\n"

cBool :: ValConstraint -> Bool
cBool (C_IsTrue _) = True
cBool (C_Not c) = not $ cBool c

cToProlog :: ValConstraint -> PrologWriter ()
cToProlog (C_IsTrue e) = do
--  mapM_ (\e_ -> op2Prolog e_) (opsInE e)
  boolE2Prolog (Op e "/=" EFalse)
cToProlog (C_Not c) = do
  cToProlog c
  --tell "#\\ "
cToProlog c =
  throwError $ "cToProlog not implemented for: " ++ show c

esInC :: ValConstraint -> [Expr]
esInC (C_IsTrue e) =
  EFalse : esInE e
esInC (C_Not c) =
  esInC c

opsInE :: Expr -> [Expr]
opsInE e =
  filter (eqExprKind (Op undefined undefined undefined))
  $ esInE e

eqExprKind :: Expr -> Expr -> Bool
eqExprKind (Op _ _ _) (Op _ _ _) = True
eqExprKind _ _ = False -- Either not implemented or actually false

esInE :: Expr -> [Expr]
esInE e@(Op e1 _ e2) = e : esInE e1 ++ esInE e2
esInE e@(Sig e1 e2) = e : esInE e1 ++ esInE e2
esInE e@(MultiSig es1 es2) = e : (concat $ map esInE es1 ++ map esInE es2)
esInE e@(Hash e1 _) = e : esInE e1
esInE e@(Length e1) = e : esInE e1
esInE e = [e]

boolE2Prolog :: Expr -> PrologWriter ()
boolE2Prolog ETrue =
  tell "1"
boolE2Prolog EFalse =
  tell "0"
boolE2Prolog (ConstBS bs)
  | BS.length bs <= 4 =
      if asInteger bs == 0
        then tell "0"
        else tell "1"
  | otherwise = tell "1"
boolE2Prolog (ConstInt i) =
  tell $ show i
boolE2Prolog (Op e1 "/\\" e2) = do
  boolE2Prolog e1
  boolE2Prolog e2
boolE2Prolog (Op e1 "\\/" e2) = do
  tell "("
  boolE2Prolog e1
  tell ") #\\/ ("
  boolE2Prolog e2
  tell "),\n"
boolE2Prolog e@(Op e1 op e2)
  | any (==op) ["==","/="] =
    t2Prolog e
boolE2Prolog (Var n) =
  tell $ "X" ++ show n
boolE2Prolog (Sig _ _) = return ()
boolE2Prolog (Hash _ _) = return ()
boolE2Prolog (MultiSig _ _) = return ()
boolE2Prolog e =
  throwError $ "e2Prolog not implemented for: " ++ show e

t2Prolog :: Expr -> PrologWriter ()
t2Prolog (Op e1 op e2)
  | any (==op) ["==","/="] = do
      t1 <- askTy e1
      t2 <- askTy e2
      relateTys t1 op t2

boolFDOps :: [(OpIdent,String)]
boolFDOps =
  [("\\/", " #\\/ "),
   ("/\\", " #/\\ ")]

relateTys :: AnnotTy -> OpIdent -> AnnotTy -> PrologWriter ()
relateTys t1 "==" t2 = do
  plFact $ tyBSPL t1 ++ " #= " ++ tyBSPL t2
relateTys t1 "/=" t2 = do
  plFact $ tyBSPL t1 ++ " #\\= " ++ tyBSPL t2

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
  "(" ++ show i1 ++ ")..(" ++ show i2 ++ ")"
