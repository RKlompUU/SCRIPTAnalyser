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

data ScriptKnowledge =
  ScriptKnowledge {
    buildState :: BuildState,
    eMapping :: Expr -> Ident,
    factSep :: String
  }
initScriptKnowledge bs eIdents =
  ScriptKnowledge {
    buildState = bs,
    eMapping = eIdents,
    factSep = ",\n"
  }

type PL = String
type PrologWriter a = ReaderT ScriptKnowledge (ExceptT String (Writer PL)) a

withSep :: String -> String -> PrologWriter a -> PrologWriter a
withSep sep endStr writer = do
  oldSep <- factSep <$> ask
  tell "("
  r <- local (\sk -> sk {factSep = sep}) writer
  tell $ endStr ++ ")" ++ oldSep
  return r

askTy :: Expr -> PrologWriter (Ident,Ty)
askTy e = do
  i <- (\f -> f e) <$> eMapping <$> ask
  tys <- ty_cnstrs <$> buildState <$> ask
  if M.member e tys
    then return $ (i,tys M.! e)
    else throwError $ "Type unknown of expr: " ++ show e

plFact :: PL -> PrologWriter ()
plFact pl =
  if null pl
    then return ()
    else do
      sep <- factSep <$> ask
      tell $ "(" ++ pl ++ ")" ++ sep

branchToProlog :: BuildState -> Either String PL
branchToProlog b =
  let eIdents = \e -> M.findIndex e $ ty_cnstrs b
  in case runWriter (runExceptT (runReaderT bToProlog (initScriptKnowledge b eIdents))) of
    (Left e,_)   -> Left e
    (Right _,pl) -> Right pl

bToProlog :: PrologWriter ()
bToProlog = do
  tell $ ":- use_module(library(clpfd)).\n\n"
  css <- val_cnstrs <$> buildState <$> ask
  mapM_ (\(i,c) -> addStmt i c) (zip [0..] css)

tellTy :: Expr -> PrologWriter ()
tellTy e =
  askTy e >>= stateTy

addStmt :: Int -> ValConstraint -> PrologWriter ()
addStmt i c = do
  tell $ "s" ++ show i ++ " :-\n"

  let es = esInC c
  mapM_ tellTy es

  tell "("
  local (\sk -> sk {factSep = " #/\\\n"}) (cToProlog c)
  --tell "true.\n"
  tell "(#\\ 0)).\n"

cBool :: ValConstraint -> Bool
cBool (C_IsTrue _) = True
cBool (C_Not c) = not $ cBool c

cToProlog :: ValConstraint -> PrologWriter ()
cToProlog (C_IsTrue e) = do
--  mapM_ (\e_ -> op2Prolog e_) (opsInE e)
  e2Prolog e
cToProlog (C_Not c) = do
  --tell "#\\ ("
  cToProlog c
  --tell "#\\ 0) #/\\ "
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

e2Prolog :: Expr -> PrologWriter ()
e2Prolog e@(Op e1 op e2)
  | any (==op) cmpOps = do
    relateTys e1 op e2
e2Prolog (Op e1 "/\\" e2) = do
  e2Prolog e1
  e2Prolog e2
e2Prolog (Op e1 "\\/" e2) = do
  withSep " #\\/ " "0" $ do
    e2Prolog e1
    e2Prolog e2
e2Prolog (Sig _ _) = plFact "1 #\\/ 0"
e2Prolog (MultiSig _ _) = plFact "1"
e2Prolog (Hash _ _) = return ()
e2Prolog e =
  throwError $ "e2Prolog not (yet) implemented for " ++ show e

boolFDOps :: [(OpIdent,String)]
boolFDOps =
  [("\\/", " #\\/ "),
   ("/\\", " #/\\ ")]

relateTys :: Expr -> OpIdent -> Expr -> PrologWriter ()
relateTys e1 op e2 = do
  t1 <- askTy e1
  t2 <- askTy e2
  relateTys' t1 op t2

relateTys' :: (Ident,Ty) -> OpIdent -> (Ident,Ty) -> PrologWriter ()
relateTys' t1 "==" t2 = do
  plFact $ tyBSPL t1 ++ " #= " ++ tyBSPL t2
  when ((not $ null $ intRanges $ snd t1) && (not $ null $ intRanges $ snd t2)) $ do
    plFact $ tyIPL t1 ++ " #= " ++ tyIPL t2
relateTys' t1 "/=" t2 = do
  plFact $ tyBSPL t1 ++ " #\\= " ++ tyBSPL t2
relateTys' t1 "<=" t2= do
  plFact $ tyIPL t1 ++ " #=< " ++ tyIPL t2
relateTys' t1 "<" t2 = do
  plFact $ tyIPL t1 ++ " #< " ++ tyIPL t2

tyBSPL :: (Ident,Ty) -> PL
tyBSPL (i,_) = "T" ++ show i ++ "bs"
tyIPL :: (Ident,Ty) -> PL
tyIPL (i,_) = "T" ++ show i ++ "ints"

stateTy ::  (Ident,Ty) -> PrologWriter ()
stateTy t@(i,ty) = do
  let bsFact = tyBSPL t ++ " in " ++ (intercalate " \\/ " $ map range2PL (bsRanges ty))
  plFact bsFact

  when (not $ null $ intRanges ty) $ do
    let intFact = tyIPL t ++ " in " ++ (intercalate " \\/ " $ map range2PL (intRanges ty))
    plFact intFact

range2PL :: R.Range Int -> PL
range2PL (R.SingletonRange i) =
  show i
range2PL (R.SpanRange i1 i2) =
  "(" ++ show i1 ++ ")..(" ++ show i2 ++ ")"
