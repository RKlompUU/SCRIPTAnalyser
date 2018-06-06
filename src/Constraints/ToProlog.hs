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

import KlompStandard

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


  tell $ "s :-\n"

  let es = nub $ concatMap esInC css
  mapM_ tellTy es

  mapM_ cToProlog css
--  mapM_ (\(i,c) -> addStmt i c) (zip [0..] css)
  tell "(#\\ 0).\n"

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

cToProlog :: ValConstraint -> PrologWriter ()
cToProlog (C_IsTrue e) =
  e2Prolog e

esInC :: ValConstraint -> [Expr]
esInC (C_IsTrue e) =
  EFalse : esInE e

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
esInE e@(Not e1) = e : esInE e1
esInE e = [e]


contradiction :: PrologWriter ()
contradiction = plFact "false"

e2Prolog :: Expr -> PrologWriter ()
e2Prolog e
  | any (ccEq e) atomEs =
    if atom2Bool e
      then return ()
      else contradiction
e2Prolog e@(Var x) = do
  t <- askTy e
  plFact $ tyBSPL t ++ " #\\= 0"
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
e2Prolog (Sig _ _) =
  return () -- plFact "1 #\\/ 0"
e2Prolog (MultiSig _ _) =
  return () -- plFact "1"
e2Prolog (Hash _ _) =
  return ()

e2Prolog (Not (Not e)) =
  e2Prolog e
e2Prolog (Not e)
  | any (ccEq e) atomEs =
    if not $ atom2Bool e
      then return ()
      else contradiction
e2Prolog (Not e@(Var x)) = do
  t <- askTy e
  plFact $ tyBSPL t ++ " #= 0"
e2Prolog (Not (Op e1 op e2))
  | isJust flippedOp =
    relateTys e1 (fromJust flippedOp) e2
  where flippedOp = lookup op flipOps
e2Prolog (Not (Op e1 "/\\" e2)) =
  e2Prolog (Op (Not e1) "\\/" (Not e2))
e2Prolog (Op e1 "\\/" e2) =
  e2Prolog (Op (Not e1) "/\\" (Not e2))
e2Prolog e =
  throwError $ "e2Prolog not (yet) implemented for " ++ show e

flipOps :: [(OpIdent,OpIdent)]
flipOps =
  [("<", ">="),
   (">=", "<"),
   (">", "<="),
   ("<=", ">"),
   ("==", "/="),
   ("/=", "==")]
boolFDOps :: [(OpIdent,String)]
boolFDOps =
  [("\\/", " #\\/ "),
   ("/\\", " #/\\ ")]

relateTys :: Expr -> OpIdent -> Expr -> PrologWriter ()
relateTys e1 op e2 = do
  t1 <- askTy e1
  t2 <- askTy e2
  relateTys' t1 op t2

hasInts :: Ty -> Bool
hasInts t =
  not $ null $ intRanges $ t

relateTys' :: (Ident,Ty) -> OpIdent -> (Ident,Ty) -> PrologWriter ()
relateTys' t1 "==" t2 = do
  plFact $ tyBSPL t1 ++ " #= " ++ tyBSPL t2
  when (hasInts (snd t1) && hasInts (snd t2)) $
    plFact $ tyIPL t1 ++ " #= " ++ tyIPL t2
relateTys' t1 "/=" t2 = do
  when (hasInts (snd t1) && hasInts (snd t2)) $
    plFact $ tyIPL t1 ++ " #\\= " ++ tyIPL t2
relateTys' t1 "<=" t2= do
  plFact $ tyIPL t1 ++ " #=< " ++ tyIPL t2
relateTys' t1 "<" t2 = do
  plFact $ tyIPL t1 ++ " #< " ++ tyIPL t2
relateTys' t1 ">=" t2= do
  plFact $ tyIPL t1 ++ " #>= " ++ tyIPL t2
relateTys' t1 ">" t2 = do
  plFact $ tyIPL t1 ++ " #> " ++ tyIPL t2
relateTys' _ op _ =
  throwError $ "relateTys' not (yet) implemented for op relation: " ++ show op

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
