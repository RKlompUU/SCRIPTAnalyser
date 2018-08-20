{-# LANGUAGE GADTs #-}
module Constraints.Gen where

import KlompStandard

import Control.Monad.Trans.Reader
import Control.Monad.State.Lazy
import Control.Applicative

import Script.AST
import Data.Bitcoin.Script.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.Maybe

import Bitcoin.Script.Integer
import Constraints.Types

import qualified Data.Map as M


genBuildStates :: ScriptAST -> [BranchReport]
genBuildStates script
  = map (\(i,r) -> r { branchID = i })
  $ zip [0..]
  $ map (\s -> unwrapBuildMonad (s >> finalizeBranch)) (runReader (genCnstrs script) (return ()))

finalizeBranch :: BranchBuilder ()
finalizeBranch = do
  e <- popStack
  --tySet e bool
  addCnstr (C_IsTrue e)

type ConstraintBuilder a = Reader (BranchBuilder ()) a


--
-- Main stack operations
--

genV :: BranchBuilder Expr
genV = do
  st <- get
  let v = Var (freshV st)
  put $ st {freshV = freshV st - 1, muts = Popped v []: muts st}
  tySet v top
  return $ v

popStack :: BranchBuilder Expr
popStack = do
  st <- get
  if null (stack st)
    then genV
    else do
      st <- get
      let e      = head (stack st)
          stack' = tail $ stack st
      put $ st {stack = stack', muts = Popped e stack' : muts st}
      return $ e

popsStack :: Int -> BranchBuilder [Expr]
popsStack n =
  reverse <$> replicateM n popStack

pushStack :: Expr -> Ty -> BranchBuilder ()
pushStack e t = do
  tySet e t
  st <- get
  let stack' = e : stack st
  put $ st {stack = stack', muts = Pushed e stack' : muts st}

pushStack_ :: Expr -> BranchBuilder ()
pushStack_ e = do
  t <- tyGet e
  pushStack e t

pushsStack_ :: [Expr] -> BranchBuilder ()
pushsStack_ es = mapM_ pushStack_ es

peakStack :: BranchBuilder Expr
peakStack = do
  v <- popStack
  pushStack_ v
  return v

opStack :: OpIdent -> BranchBuilder ()
opStack op = do
  v_2 <- popStack
  v_1 <- popStack

  (t_1,t_2,t_r) <- opTys op
  tyCast v_1 t_1
  tyCast v_2 t_2

  let e = Op v_2 op v_1
  pushStack e t_r


--
-- Alternative stack operations
--
{- Alststack ignored for now
genAltV :: BranchBuilder Expr
genAltV = do
  st <- get
  put $ st {freshAltV = freshAltV st - 1}
  return $ Var (freshAltV st)

popAltStack :: BranchBuilder Expr
popAltStack = do
  st <- get
  if null (altStack st)
    then genAltV
    else do
      st <- get
      put $ st {altStack = tail $ altStack st}
      return $ head (altStack st)

pushAltStack :: Expr -> BranchBuilder ()
pushAltStack e = do
  st <- get
  put $ st {altStack = e : altStack st}

pushsAltStack :: [Expr] -> BranchBuilder ()
pushsAltStack es = mapM_ pushAltStack es
-}

--
-- Main constraint generation functions
--

noteBranchJump :: Label -> Bool -> BranchBuilder ()
noteBranchJump lbl b = do
  st <- get
  put $ st { branchInfo = (lbl,b) : branchInfo st }

branched :: (Bool -> BranchBuilder ()) ->
            Bool ->
            ScriptAST ->
            ConstraintBuilder [BranchBuilder ()]
branched f b cont = withReader (>> f b) (genCnstrs cont)

genCnstrs :: ScriptAST -> ConstraintBuilder [BranchBuilder ()]
genCnstrs (ScriptITE ifLbl b0 elseLbl b1 fiLbl cont) = do
  let stModITE = (\b -> do
                      v <- popStack
                      t <- tyGet v
                      noteBranchJump ifLbl b
                      if b
                        then addCnstr (C_IsTrue v) >>
                             tySet v true
                        else addCnstr (C_IsTrue $ Not v) >>
                             tySet (Not v) true
                 )
  ss0 <- branched stModITE True b0
  ss1 <- branched stModITE False b1
  concat <$> mapM (\s -> local (const s) (genCnstrs cont)) (ss0 ++ ss1)

genCnstrs stmnt@(ScriptOp lbl op cont) | isJust op' =
  genCnstrs (ScriptOp lbl (fromJust op') (ScriptOp lbl OP_VERIFY cont))
  where op' = lookup op verifyAfterOps
genCnstrs (ScriptOp lbl op cont) = do
  withReader (>> stModOp op) $ genCnstrs cont
genCnstrs ScriptTail = do
  s <- ask
  return [s]

verifyAfterOps =
  [
  (OP_EQUALVERIFY,OP_EQUAL),
  (OP_NUMEQUALVERIFY,OP_NUMEQUAL),
  (OP_CHECKSIGVERIFY,OP_CHECKSIG),
  (OP_CHECKMULTISIGVERIFY,OP_CHECKMULTISIG)
  ]



stModOp :: ScriptOp -> BranchBuilder ()
stModOp (OP_PUSHDATA bs _) = (uncurry pushStack) (annotTy (ConstBS bs))

stModOp OP_0 = (uncurry pushStack) (annotTy (ConstInt 0))
stModOp OP_1 = (uncurry pushStack) (annotTy (ConstInt 1))
stModOp OP_1NEGATE = (uncurry pushStack) (annotTy (ConstInt (-1)))
stModOp OP_2 = (uncurry pushStack) (annotTy (ConstInt 2))
stModOp OP_3 = (uncurry pushStack) (annotTy (ConstInt 3))
stModOp OP_4 = (uncurry pushStack) (annotTy (ConstInt 4))
stModOp OP_5 = (uncurry pushStack) (annotTy (ConstInt 5))
stModOp OP_6 = (uncurry pushStack) (annotTy (ConstInt 6))
stModOp OP_7 = (uncurry pushStack) (annotTy (ConstInt 7))
stModOp OP_8 = (uncurry pushStack) (annotTy (ConstInt 8))
stModOp OP_9 = (uncurry pushStack) (annotTy (ConstInt 9))
stModOp OP_10 = (uncurry pushStack) (annotTy (ConstInt 10))
stModOp OP_11 = (uncurry pushStack) (annotTy (ConstInt 11))
stModOp OP_12 = (uncurry pushStack) (annotTy (ConstInt 12))
stModOp OP_13 = (uncurry pushStack) (annotTy (ConstInt 13))
stModOp OP_14 = (uncurry pushStack) (annotTy (ConstInt 14))
stModOp OP_15 = (uncurry pushStack) (annotTy (ConstInt 15))
stModOp OP_16 = (uncurry pushStack) (annotTy (ConstInt 16))

stModOp OP_NOP = return ()

stModOp OP_VERIFY = do
  v <- popStack
  tySet v true
  addCnstr (C_IsTrue v)
stModOp OP_RETURN = tySubst true false >> return () -- i.e. make current branch invalid

{-
stModOp OP_TOALTSTACK = do
  v <- popStack
  pushAltStack v
stModOp OP_FROMALTSTACK = do
  v <- popAltStack
  pushStack v
-}
stModOp OP_DEPTH = do
  depth <- length . stack <$> get
  pushStack (ConstInt depth) int
stModOp OP_DROP = popStack >> return ()
stModOp OP_DUP = popStack >>= \v -> pushsStack_ [v,v]
stModOp OP_NIP = do
  v <- popStack
  popStack
  pushStack_ v
stModOp OP_OVER = do
  v2 <- popStack
  v1 <- popStack
  pushsStack_ [v1,v2,v1]
stModOp OP_PICK = do
  n <- e2i <$> popStack
  es <- popsStack (n-1)
  e_n <- popStack
  pushsStack_ (e_n : es)
  pushStack_ e_n
stModOp OP_ROLL = do
  n <- e2i <$> popStack
  es <- popsStack (n-1)
  e_n <- popStack
  pushsStack_ es
  pushStack_ e_n
stModOp OP_ROT = do
  v_3 <- popStack
  v_2 <- popStack
  v_1 <- popStack
  pushsStack_ [v_2, v_3, v_1]
stModOp OP_SWAP = popsStack 2 >>= \vs -> pushsStack_ (reverse vs)
stModOp OP_TUCK = do
  v_2 <- popStack
  v_1 <- popStack
  pushsStack_ [v_2, v_1, v_2]
stModOp OP_2DROP = popsStack 2 >> return ()
stModOp OP_2DUP = popsStack 2 >>= \vs -> pushsStack_ (vs ++ vs)
stModOp OP_3DUP = popsStack 3 >>= \vs -> pushsStack_ (vs ++ vs)
stModOp OP_2OVER = popsStack 4 >>= \vs -> pushsStack_ (vs ++ drop 2 vs)
stModOp OP_2ROT = popsStack 6 >>= \vs -> pushsStack_ (drop 2 vs ++ take 2 vs)
stModOp OP_2SWAP = popsStack 4 >>= \vs -> pushsStack_ (drop 2 vs ++ take 2 vs)


stModOp OP_SIZE = do
  v <- peakStack
  (uncurry pushStack) (annotTy (Length v))
  let link = Op (Length v) "==" v
  tySet link bool
  addCnstr (C_Spec link)
stModOp OP_NOT = popStack >>= \v -> (uncurry pushStack) (annotTy (Not v))
{-
stModOp OP_1ADD = popStack >>= \v -> pushStack (Op v "+" (ConstInt 1))
stModOp OP_1SUB = popStack >>= \v -> pushStack (Op v "-" (ConstInt 1))
stModOp OP_NEGATE = popStack >>= \v -> pushStack (Op v "*" (ConstInt (-1)))
stModOp OP_ABS = popStack >>= \v -> pushStack (Abs v)
-}

stModOp OP_ADD = opStack "+"
stModOp OP_SUB = opStack "-"
stModOp OP_BOOLAND = opStack "/\\"
stModOp OP_BOOLOR = opStack "\\/"
stModOp OP_NUMEQUAL = opStack "=="
stModOp OP_NUMNOTEQUAL = opStack "/="
stModOp OP_LESSTHAN = opStack ">"
stModOp OP_GREATERTHAN = opStack "<"
stModOp OP_LESSTHANOREQUAL = opStack ">="
stModOp OP_GREATERTHANOREQUAL = opStack "<="
stModOp OP_WITHIN = do
  v_3 <- popStack -- max    |
  v_2 <- popStack -- min    | min <= x < max
  v_1 <- popStack -- x      |

  (uncurry pushStack) (annotTy (Op v_2 "<=" v_1))
  (uncurry pushStack) (annotTy (Op v_1 "<" v_3))
  opStack "/\\"

{-
stModOp OP_MIN = do
  v_2 <- popStack
  v_1 <- popStack
  pushStack (Min v_1 v_2)
stModOp OP_MAX = do
  v_2 <- popStack
  v_1 <- popStack
  pushStack (Max v_1 v_2)
-}


stModOp OP_HASH160 = popStack >>= \v -> (uncurry pushStack) (annotTy (Hash v 20))
stModOp OP_HASH256 = popStack >>= \v -> (uncurry pushStack) (annotTy (Hash v 32))
stModOp OP_SHA256 = popStack >>= \v -> (uncurry pushStack) (annotTy (Hash v 32))

stModOp OP_EQUAL = do
  v_2 <- popStack
  v_1 <- popStack
  pushStack (Op v_1 "==" v_2) bool
stModOp OP_CHECKSIG = do
  v_2 <- popStack
  v_1 <- popStack
  -- TODO; is this enforced or not? The input types of Sig
  -- tySet v_1 skTy
  -- tySet v_2 pkTy
  pushStack (Sig v_1 v_2) bool
stModOp OP_CHECKMULTISIG = do
  n_p  <- e2i <$> popStack
  ks_p <- popsStack n_p
  n_s  <- e2i <$> popStack
  ks_s <- popsStack n_s
  popStack -- Due to a bug in the Bitcoin implementation :)
  pushStack (MultiSig ks_s ks_p) bool

stModOp OP_NOP1 = return ()
stModOp OP_NOP4 = return ()
stModOp OP_NOP5 = return ()
stModOp OP_NOP6 = return ()
stModOp OP_NOP7 = return ()
stModOp OP_NOP8 = return ()
stModOp OP_NOP9 = return ()
stModOp OP_NOP10 = return ()

stModOp OP_CHECKLOCKTIMEVERIFY = do
  l <- popStack
  let timeCnstrA = Op l "<" (Var 1)
      timeCnstrB = Op l ">=" (ConstInt 500000000)
      timeCnstrC = Op (Var 1) ">=" (ConstInt 500000000)
      timeCnstrD = Op l "<" (ConstInt 500000000)
      timeCnstrE = Op (Var 1) "<" (ConstInt 500000000)

      timeCnstrF = Op timeCnstrB "/\\" timeCnstrC
      timeCnstrG = Op timeCnstrD "/\\" timeCnstrE
      timeCnstrH = Op timeCnstrF "\\/" timeCnstrG

      timeCnstr  = Op timeCnstrA "/\\" timeCnstrH

  (uncurry tySet) (annotTy (ConstInt 500000000))
  tySet timeCnstr bool
  tySet timeCnstrA bool
  tySet timeCnstrB bool
  tySet timeCnstrC bool
  tySet timeCnstrD bool
  tySet timeCnstrE bool
  tySet timeCnstrF bool
  tySet timeCnstrG bool
  tySet timeCnstrH bool

  tySet l (unsigned bint)
  addCnstr (C_IsTrue timeCnstr)
stModOp OP_CHECKSEQUENCEVERIFY = do
  l <- popStack
  tySet l bint

  let l' = BigInt l
  (uncurry tySet) (annotTy l')

  let linkA = Length l
      linkB = Length l'
  let link = Op linkA "==" linkB
  tySet linkA int
  tySet linkB int
  tySet link bool
  addCnstr (C_Spec link)

  let cnstrMin0 = Op l' ">=" (ConstInt 0)
  tySet cnstrMin0 bool
  tySet (ConstInt 0) bint
  addCnstr (C_IsTrue cnstrMin0)

  let lBit31 = Op l' "&" (Hex "80000000")
      lBit22 = Op l' "&" (Hex "00400000")
      lMasked = Op l' "&" (Hex "0000FFFF")

      lVar3Bit22 = Op (Var 2) "&" (Hex "00400000")
      lVar3Masked = Op (Var 2) "&" (Hex "0000FFFF")

      timeCnstrA = Op lVar3Masked ">=" lMasked

      timeCnstrB = Op lVar3Bit22 "/\\" lBit22
      timeCnstrC = Op (Not $ lVar3Bit22) "/\\" (Not lBit22)
      timeCnstrD = Op timeCnstrB "\\/" timeCnstrC

      timeCnstrE = Op timeCnstrD "/\\" timeCnstrA

      timeCnstr = Op lBit31 "\\/" timeCnstrE

  tySet timeCnstrA bool
  tySet timeCnstrB bool
  tySet timeCnstrC bool
  tySet timeCnstrD bool
  tySet timeCnstrE bool
  tySet timeCnstr bool

  tySet (Not $ lBit22) bool

  tySet lMasked int
  tySet lBit31 bint
  tySet lBit22 int
  tySet (Hex "0000FFFF") int
  tySet (Hex "80000000") int
  tySet (Hex "00400000") int

  addCnstr (C_IsTrue timeCnstr)
  addCnstr (C_Spec lMasked)
  addCnstr (C_Spec lBit31)
  addCnstr (C_Spec lBit22)

-- DISABLED OP_CODES
stModOp op | any (== op) disabledOps = failBranch $ "Error, disabled OP used: " ++ show op

stModOp op =
  failBranch $ "Error, no stModOp implementation for operator: " ++ show op


disabledOps =
  [
  OP_CAT,
  OP_SUBSTR,
  OP_LEFT,
  OP_RIGHT,

  OP_INVERT,
  OP_AND,
  OP_OR,
  OP_XOR,

  OP_2MUL,
  OP_2DIV,
  OP_MUL,
  OP_DIV,
  OP_MOD,
  OP_LSHIFT,
  OP_RSHIFT,

  OP_VERIF
  ]
