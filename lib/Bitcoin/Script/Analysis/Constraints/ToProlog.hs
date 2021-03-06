module Bitcoin.Script.Analysis.Constraints.ToProlog where

import Bitcoin.Script.Analysis.Constraints.Types
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

import Bitcoin.Script.Analysis.Standard

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
  in case runWriter (runExceptT (runReaderT (bToProlog [] Nothing "") (initScriptKnowledge b eIdents))) of
    (Left e,_)   -> Left e
    (Right _,pl) -> Right pl


solveForArgs :: BuildState -> [String] -> (String,[Expr]) -> String -> Either String PL
solveForArgs b args namedList customLogic =
  let eIdents = \e -> M.findIndex e $ ty_cnstrs b
  in case runWriter (runExceptT (runReaderT (bToProlog args (Just namedList) customLogic) (initScriptKnowledge b eIdents))) of
    (Left e,_)   -> Left e
    (Right _,pl) -> Right pl

bToProlog :: [String] -> Maybe (String,[Expr]) -> String -> PrologWriter ()
bToProlog args namedList customLogic = do
  tell $ ":- use_module(library(clpfd)).\n\n"
  css <- val_cnstrs <$> buildState <$> ask

  if not $ null args
    then tell $ "t(" ++ intercalate "," args ++ ") :-\n\
                 \s(" ++ intercalate "," args ++ "),\n\
                 \indomain(N),\n\
                 \indomain(M).\n"
    else return ()

  tell $ "s(" ++ intercalate "," args ++ ") :-\n"

  if isJust namedList
    then do
      listTys <- mapM askTy (snd $ fromJust namedList)
      tell $ (fst $ fromJust namedList) ++ "bs = [" ++ (intercalate "," $ map tyBSPL listTys) ++ "],\n"
      tell $ (fst $ fromJust namedList) ++ "ints = [" ++ (intercalate "," $ map tyIPL listTys) ++ "],\n"
    else return ()

  tell customLogic

  let es = if isJust namedList
            then nub $ (concatMap esInC css ++ concatMap esInE (snd $ fromJust namedList))
            else nub $ concatMap esInC css
  mapM_ tellTy es

  mapM_ cToProlog css
  tell "(#\\ 0).\n"

tellTy :: Expr -> PrologWriter ()
tellTy e =
  askTy e >>= stateTy

cToProlog :: ValConstraint -> PrologWriter ()
cToProlog (C_IsTrue e) = do
  e2Prolog e

  t <- askTy e
  plFact $ tyIPL t ++ " #\\= 0"
cToProlog (C_Spec e) =
  spec2Prolog e

esInC :: ValConstraint -> [Expr]
esInC (C_IsTrue e) =
  EFalse : esInE e
esInC (C_Spec e) =
  esInE e

esInE :: Expr -> [Expr]
esInE e@(Op e1 _ e2) = e : esInE e1 ++ esInE e2
esInE e@(Sig e1 e2) = e : esInE e1 ++ esInE e2
esInE e@(MultiSig es1 es2) = e : (concat $ map esInE es1 ++ map esInE es2)
esInE e@(Hash e1 _) = e : esInE e1
esInE e@(Length e1) = e : esInE e1
esInE e@(Not e1) = e : esInE e1
esInE e@(BigInt e') = e : esInE e'
esInE e@(Abs e') = e : esInE e'
esInE e@(Min e1 e2) = e : esInE e1 ++ esInE e2
esInE e@(Max e1 e2) = e : esInE e1 ++ esInE e2
esInE e = [e]

contradiction :: PrologWriter ()
contradiction = plFact "false"

spec2Prolog :: Expr -> PrologWriter ()
spec2Prolog e@(Op (Op e1 "&" e2) ">>" (ConstInt shift)) = do
  t1 <- askTy e1
  t  <- askTy e
  plFact $ tyIPL t ++ " #= (" ++ tyIPL t1 ++ " /\\ " ++ show e2 ++ ") >> " ++ show shift
spec2Prolog e@(Op e1 "&" e2) = do
  t1 <- askTy e1
  t  <- askTy e
  plFact $ tyIPL t ++ " #= (" ++ tyIPL t1 ++ " /\\ " ++ show e2 ++ ")"
spec2Prolog (Op (Length e1) "==" (Length e2)) = do
  t1 <- askTy e1
  t2 <- askTy e2
  plFact $ tyBSPL t1 ++ " #= " ++ tyBSPL t2
spec2Prolog (Op e1@(Length _) "==" e2) = do
  t1 <- askTy e1
  t2 <- askTy e2
  plFact $ tyIPL t1 ++ " #= " ++ tyBSPL t2

e2Prolog e@(Op e1 op e2)
  | any (==op) numOps = do
      t  <- askTy e
      t1 <- askTy e1
      t2 <- askTy e2

      e2Prolog e1
      e2Prolog e2

      plFact $ tyIPL t ++ " #= (" ++ tyIPL t1 ++ " " ++ op ++ " " ++ tyIPL t2 ++ ")"
e2Prolog e@(Op e1 op e2)
  | isJust op' = do
      t  <- askTy e
      t1 <- askTy e1
      t2 <- askTy e2

      e2Prolog e1
      e2Prolog e2

      plFact $ tyIPL t1 ++ " " ++ fromJust op' ++ " " ++ tyIPL t2 ++ " #==> " ++ tyIPL t ++ " #= 1"
      plFact $ "#\\ (" ++ tyIPL t1 ++ " " ++ fromJust op' ++ " " ++ tyIPL t2 ++ ") #==> " ++ tyIPL t ++ " #= 0"
  where op' = lookup op [("/\\","#/\\"),
                         ("\\/","#\\/"),
                         (">","#>"),
                         ("<","#<"),
                         (">=","#>="),
                         ("<=","#=<")]
e2Prolog e@(Op e1 "==" e2) = do
  t <- askTy e
  t1 <- askTy e1
  t2 <- askTy e2

  plFact $ tyBSPL t1 ++ " #= " ++ tyBSPL t2 ++ " #==> " ++ tyIPL t ++ " in (0..1)"
  plFact $ tyBSPL t1 ++ " #\\= " ++ tyBSPL t2 ++ " #==> " ++ tyIPL t ++ " #= 0"

  when (hasInts (snd t1) && hasInts (snd t2)) $ do
    plFact $ tyIPL t1 ++ " #= " ++ tyIPL t2 ++ " #==> " ++ tyIPL t ++ " #= 1"
    plFact $ tyIPL t1 ++ " #\\= " ++ tyIPL t2 ++ " #==> " ++ tyIPL t ++ " #= 0"
e2Prolog e@(Abs e') = do
  t <- askTy e
  t' <- askTy e'

  e2Prolog e'

  plFact $ tyIPL t ++ " #=< " ++ tyIPL t'

  {- Don't have to enforce that tyIPL t is >= 0, this is already enforced by its
     initially assigned type (in annotTy) -}
e2Prolog e@(Min e1 e2) = do
  t1 <- askTy e1
  t2 <- askTy e2
  t <- askTy e

  e2Prolog e1
  e2Prolog e2

  plFact $ tyIPL t ++ " #= min(" ++ tyIPL t1 ++ "," ++ tyIPL t2 ++ ")"
e2Prolog e@(Max e1 e2) = do
  t1 <- askTy e1
  t2 <- askTy e2
  t <- askTy e

  e2Prolog e1
  e2Prolog e2

  plFact $ tyIPL t ++ " #= max(" ++ tyIPL t1 ++ "," ++ tyIPL t2 ++ ")"
e2Prolog e@(Not e') = do
  t <- askTy e
  t' <- askTy e'

  e2Prolog e'

  plFact $ tyIPL t' ++ " #\\= 0 #==> " ++ tyIPL t ++ " #= 0"
  plFact $ tyIPL t' ++ " #= 0 #==> " ++ tyIPL t ++ " #= 1"
e2Prolog _ = return ()

hasInts :: Ty -> Bool
hasInts t =
  not $ null $ intRanges $ t

tyBSPL :: (Ident,Ty) -> PL
tyBSPL (i,_) = "T" ++ show i ++ "bs"
tyIPL :: (Ident,Ty) -> PL
tyIPL (i,_) = "T" ++ show i ++ "ints"

stateTy ::  (Ident,Ty) -> PrologWriter ()
stateTy t@(i,ty) = do
  plFact $ tyBSPL t ++ " in " ++ ranges2PL (bsRanges ty)

  when (not $ null $ intRanges ty) $ do
    plFact $ tyIPL t ++ " in " ++ ranges2PL (intRanges ty)

range2PL :: R.Range Int -> PL
range2PL (R.SingletonRange i) =
  show i
range2PL (R.SpanRange i1 i2) =
  "(" ++ show i1 ++ ")..(" ++ show i2 ++ ")"

ranges2PL :: [R.Range Int] -> PL
ranges2PL rs =
  intercalate " \\/ " $ map range2PL rs
