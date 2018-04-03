{-# LANGUAGE GADTs #-}
module Constraints.Gen where

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

genConstraints :: ScriptAST -> BConstraints
genConstraints script
  = foldl1 OrConstr
  $ map cnstrs
  $ genBuildStates script

genConstraints' :: ScriptAST -> [(BConstraints, Stack)]
genConstraints' script
  = map (\buildState -> (cnstrs buildState, take 1 $ stack buildState))
  $ genBuildStates script

genBuildStates :: ScriptAST -> [BuildState]
genBuildStates script
  = map (\s -> execState (s >> finalizeBranch) initBuildState)
  $ runReader (genCnstrs script) (return ())


lazy2StrictBS :: BSL.ByteString -> BS.ByteString
lazy2StrictBS =
  BS.concat . BSL.toChunks

convert2Int :: Expr -> Maybe Expr
convert2Int (ConstInt i) = Just $ ConstInt i
convert2Int (ConstBS bs)
  | BS.length bs <= 4 = ConstInt <$> return (fromIntegral $ asInteger bs)
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


type BranchBuilder a = State BuildState a
type ConstraintBuilder a = Reader (BranchBuilder ()) a

cnstrsMod :: (BConstraints -> BConstraints) -> BranchBuilder ()
cnstrsMod f = do
  st <- get
  put $ st {cnstrs = f $ cnstrs st}

newConstr :: BConstraints -> BranchBuilder BConstraints
newConstr c = do
  st <- get
  put $ st {muts = Infered c : muts st}
  return c

--
-- Main stack operations
--

genV :: BranchBuilder Expr
genV = do
  st <- get
  let v = Var (freshV st)
  put $ st {freshV = freshV st + 1, muts = Popped v []: muts st}
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

pushStack :: Expr -> BranchBuilder ()
pushStack e = do
  st <- get
  let stack' = e : stack st
  put $ st {stack = stack', muts = Pushed e stack' : muts st}

pushsStack :: [Expr] -> BranchBuilder ()
pushsStack es = mapM_ pushStack es

peakStack :: BranchBuilder Expr
peakStack = do
  v <- popStack
  pushStack v
  return v

opStack :: OpTy -> BranchBuilder ()
opStack op = do
  v_2 <- popStack
  v_1 <- popStack
  pushStack (Op v_2 op v_1)


--
-- Alternative stack operations
--

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


--
-- Main constraint generation functions
--


finalizeBranch :: BranchBuilder ()
finalizeBranch = do
  e <- popStack
  nc <- newConstr (ExprConstr e) -- redundant -> $ Op e "/=" (ConstInt 0)
  cnstrsMod (AndConstr nc)

branched :: (Bool -> BranchBuilder ()) ->
            Bool ->
            ScriptAST ->
            ConstraintBuilder [BranchBuilder ()]
branched f b cont = withReader (>> f b) (genCnstrs cont)

genCnstrs :: ScriptAST -> ConstraintBuilder [BranchBuilder ()]
genCnstrs (ScriptITE b0 b1 cont) = do
  let stModITE = (\b -> do
                        v <- popStack
                        nc <- newConstr $ if b
                                            then ExprConstr v
                                            else NotConstr (ExprConstr v)
                        cnstrsMod (AndConstr nc))
  ss0 <- branched stModITE True b0
  ss1 <- branched stModITE False b1
  concat <$> mapM (\s -> local (const s) (genCnstrs cont)) (ss0 ++ ss1)
genCnstrs (ScriptOp OP_EQUAL cont) = do
  let stModEq = (\b -> do
                      v_2 <- popStack
                      v_1 <- popStack
                      nc <- newConstr $ if b
                                            then ExprConstr $ Op v_1 "==" v_2
                                            else ExprConstr $ Op v_1 "/=" v_2
                      cnstrsMod (AndConstr nc)
                      if b
                        then pushStack (ConstInt 1)
                        else pushStack (ConstInt 0))
  ss0 <- branched stModEq True  cont
  ss1 <- branched stModEq False cont
  return $ ss0 ++ ss1
genCnstrs (ScriptOp OP_0NOTEQUAL cont) = do
  let stMod0NotEq = (\b -> do
                      v_1 <- popStack
                      nc <- newConstr $ if b
                                            then ExprConstr v_1
                                            else NotConstr $ ExprConstr v_1
                      cnstrsMod (AndConstr nc)
                      if b
                        then pushStack (ConstInt 1)
                        else pushStack (ConstInt 0))
  ss0 <- branched stMod0NotEq True  cont
  ss1 <- branched stMod0NotEq False cont
  return $ ss0 ++ ss1
genCnstrs (ScriptOp OP_CHECKSIG cont) = do
  let stModSig = (\b -> do
                        v_2 <- popStack
                        v_1 <- popStack
                        if b
                          then do
                            nc <- newConstr $ ExprConstr $ Sig v_1 v_2
                            cnstrsMod (AndConstr nc)
                            pushStack (ConstInt 1)
                          else
                            pushStack (ConstInt 0))
  ss0 <- branched stModSig True  cont
  ss1 <- branched stModSig False cont
  return $ ss0 ++ ss1
genCnstrs (ScriptOp OP_CHECKMULTISIG cont) = do
  let stModMultiSig = (\b -> do
          n_p  <- e2i <$> popStack
          ks_p <- popsStack n_p
          n_s  <- e2i <$> popStack
          ks_s <- popsStack n_s
          popStack -- Due to a bug in the Bitcoin implementation :)
          if b
            then do
              nc <- newConstr $ ExprConstr $ MultiSig ks_s ks_p
              cnstrsMod (AndConstr nc)
              pushStack (ConstInt 1)
            else
              pushStack (ConstInt 0))
  ss0 <- branched stModMultiSig True  cont
  ss1 <- branched stModMultiSig False cont
  return $ ss0 ++ ss1
genCnstrs (ScriptOp op cont) | isJust op' =
  genCnstrs (ScriptOp (fromJust op') (ScriptOp OP_VERIFY cont))
  where op' = lookup op verifyAfterOps
genCnstrs (ScriptOp op cont) = do
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
stModOp (OP_PUSHDATA bs _) = pushStack (ConstBS bs)

stModOp OP_0 = pushStack (ConstBS BS.empty)
stModOp OP_1NEGATE = pushStack (ConstInt (-1))
stModOp OP_1 = pushStack (ConstInt 1)
stModOp OP_2 = pushStack (ConstInt 2)
stModOp OP_3 = pushStack (ConstInt 3)
stModOp OP_4 = pushStack (ConstInt 4)
stModOp OP_5 = pushStack (ConstInt 5)
stModOp OP_6 = pushStack (ConstInt 6)
stModOp OP_7 = pushStack (ConstInt 7)
stModOp OP_8 = pushStack (ConstInt 8)
stModOp OP_9 = pushStack (ConstInt 9)
stModOp OP_10 = pushStack (ConstInt 10)
stModOp OP_11 = pushStack (ConstInt 11)
stModOp OP_12 = pushStack (ConstInt 12)
stModOp OP_13 = pushStack (ConstInt 13)
stModOp OP_14 = pushStack (ConstInt 14)
stModOp OP_15 = pushStack (ConstInt 15)
stModOp OP_16 = pushStack (ConstInt 16)

stModOp OP_NOP = return ()

stModOp OP_VERIFY = do
  v <- popStack
  nc <- newConstr $ ExprConstr v -- redundant -> $ Op v "/=" (ConstInt 0)
  cnstrsMod (AndConstr nc)
stModOp OP_RETURN = cnstrsMod (AndConstr falseConstr)

stModOp OP_TOALTSTACK = do
  v <- popStack
  pushAltStack v
stModOp OP_FROMALTSTACK = do
  v <- popAltStack
  pushStack v
stModOp OP_DEPTH = do
  depth <- length . stack <$> get
  pushStack (ConstInt depth)
stModOp OP_DROP = popStack >> return ()
stModOp OP_DUP = popStack >>= \v -> pushsStack [v,v]
stModOp OP_NIP = do
  v <- popStack
  popStack
  pushStack v
stModOp OP_OVER = do
  v2 <- popStack
  v1 <- popStack
  pushsStack [v1,v2,v1]
stModOp OP_PICK = do
  n <- e2i <$> popStack
  es <- popsStack (n-1)
  e_n <- popStack
  pushsStack (e_n : es)
  pushStack e_n
stModOp OP_ROLL = do
  n <- e2i <$> popStack
  es <- popsStack (n-1)
  e_n <- popStack
  pushsStack es
  pushStack e_n
stModOp OP_ROT = do
  v_3 <- popStack
  v_2 <- popStack
  v_1 <- popStack
  pushsStack [v_2, v_3, v_1]
stModOp OP_SWAP = popsStack 2 >>= \vs -> pushsStack (reverse vs)
stModOp OP_TUCK = do
  v_2 <- popStack
  v_1 <- popStack
  pushsStack [v_2, v_1, v_2]
stModOp OP_2DROP = popsStack 2 >> return ()
stModOp OP_2DUP = popsStack 2 >>= \vs -> pushsStack (vs ++ vs)
stModOp OP_3DUP = popsStack 3 >>= \vs -> pushsStack (vs ++ vs)
stModOp OP_2OVER = popsStack 4 >>= \vs -> pushsStack (vs ++ drop 2 vs)
stModOp OP_2ROT = popsStack 6 >>= \vs -> pushsStack (drop 2 vs ++ take 2 vs)
stModOp OP_2SWAP = popsStack 4 >>= \vs -> pushsStack (drop 2 vs ++ take 2 vs)

stModOp OP_SIZE = peakStack >>= \v -> pushStack (Length v)
stModOp OP_1ADD = popStack >>= \v -> pushStack (Op v "+" (ConstInt 1))
stModOp OP_1SUB = popStack >>= \v -> pushStack (Op v "-" (ConstInt 1))
stModOp OP_NEGATE = popStack >>= \v -> pushStack (Op v "*" (ConstInt (-1)))
stModOp OP_ABS = popStack >>= \v -> pushStack (Abs v)
stModOp OP_NOT = popStack >>= \v -> pushStack (Not v)
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
stModOp OP_MIN = do
  v_2 <- popStack
  v_1 <- popStack
  pushStack (Min v_1 v_2)
stModOp OP_MAX = do
  v_2 <- popStack
  v_1 <- popStack
  pushStack (Max v_1 v_2)
stModOp OP_WITHIN = do
  v_3 <- popStack -- max    |
  v_2 <- popStack -- min    | min <= x < max
  v_1 <- popStack -- x      |
  pushStack (Op (Op v_2 "<=" v_1) "/\\" (Op v_1 "<" v_3))

stModOp op | any (== op) hashOps = popStack >>= \v -> pushStack (Hash v)

-- DISABLED OP_CODES
stModOp op | any (== op) disabledOps = cnstrsMod (AndConstr falseConstr)

stModOp op =
  error $ "Error, no stModOp implementation for operator: " ++ show op


hashOps =
  [
  OP_RIPEMD160,
  OP_SHA1,
  OP_SHA256,
  OP_HASH160,
  OP_HASH256
  ]

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