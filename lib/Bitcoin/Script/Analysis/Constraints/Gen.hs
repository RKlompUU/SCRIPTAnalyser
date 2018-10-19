{-# LANGUAGE GADTs #-}
module Bitcoin.Script.Analysis.Constraints.Gen (
  genBuildStates,
  rerunBranch,
  astOK
) where

import Bitcoin.Script.Analysis.Standard

import Control.Monad.Trans.Reader
import Control.Monad.State.Lazy
import Control.Applicative

import Bitcoin.Script.Parser.API
import Data.Bitcoin.Script.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.Maybe

import Bitcoin.Script.Integer
import Bitcoin.Script.Analysis.Constraints.Types
import Bitcoin.Script.Analysis.Constraints.ToProlog
import Bitcoin.Script.Analysis.Constraints.RunProlog

import qualified Data.Map as M
import qualified Control.Monad.Except as E

-- |'genBuildStates' generates, for each execution branch in the given script (of type
-- 'ScriptAST'), an analysis report.
genBuildStates :: ScriptAST -> IO [BranchReport]
genBuildStates script
  = map (\(i,r) -> r { branchID = i })
  <$> zip [0..]
  <$> mapM (\s -> unwrapBuildMonad (s >> finalizeBranch)) (map (\a -> a (return ())) (runReader (genCnstrs script) id))

-- |Returns 'Just' 'ScriptOp' if the script contains an instruction that renders the
-- entire script illegal (nonredeemable).
astOK :: ScriptAST -> Maybe ScriptOp
astOK (ScriptOp _ op cont)
  | isJust op' = op'
  | otherwise = astOK cont
  where op' = find (== op) bannedOPs
astOK (ScriptITE _ b1 _ b2 _ cont) =
  let b1' = astOK b1
      b2' = astOK b2
      cont' = astOK cont
  in if isJust b1'
      then b1'
      else if isJust b2'
            then b2'
            else cont'
astOK ScriptTail =
  Nothing

-- |'rerunBranch' reruns the analysis of a branch, and continues with the
-- next viable instantiation of one of the variables that require an exhaustive
-- search. This function is called when a branch execution was found that turned
-- out to contain contradictions in the imposed constraints, and still has untried
-- possibly viable instantiations of 1 or more variables.
rerunBranch :: BranchReport -> IO BranchReport
rerunBranch bReport = do
  (\r -> r { branchID = branchID bReport, branchBuilder = branchBuilder bReport })
  <$> unwrapBuildMonad (put (rerunFromContinuation (symbolicEval bReport)) >> branchBuilder bReport)

finalizeBranch :: BranchBuilder ()
finalizeBranch = do
  e <- popStack
  addCnstr (C_IsTrue e)

-- |'BranchCont' takes the continuation 'BranchBuilder ()'. At each inspected instruction,
-- when closed off by passing the continuation 'BranchBuilder ()', it is mutated into
-- a new continuation. This setup allows to catch at each inspected instruction
-- any analysis errors thrown during the analysis of subsequent instructions. This
-- enables the backtracking feature used during analysis of OP_CHECKMULTISIG, OP_ROLL
-- and OP_PICK.
type BranchCont = BranchBuilder () -> BranchBuilder ()
-- |'ConstraintBuilder' constructs the symbolic execution monad of each
-- execution branch. The Reader contains the BranchCont built up to a
-- currently inspected instruction, and eventually returns a list of
-- BranchCont (one entry per execution branch). The resulting BranchConts
-- then only need to be closed with an initial BranchBuilder () instance, finally
-- resulting in a constructed BranchBuilder () monad for each execution branch.
-- These monads can then be ran to obtain the actual analysis results.
type ConstraintBuilder = Reader BranchCont [BranchCont]


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

genArbitraryV :: BranchBuilder Expr
genArbitraryV = do
  st <- get
  let v = AVar (freshAV st)
  put $ st {freshAV = freshAV st - 1}
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

-- |'peakStack' returns the 'Expr' that is at the top of the stack. Note that this
-- function does have a side effect. If the symbolic stack is empty, a new 'Var'
-- is instantiated, as a consequence this will add a constraint that the input script
-- must instantiate this variable. Thus, only call 'peakStack' at places in the analysis
-- where the actual Bitcoin Core client would require a variable to be present!
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

genAltV :: BranchBuilder Expr
genAltV = do
  st <- get
  let v = AltVar (freshAltV st)
  put $ st {freshAltV = freshAltV st - 1}

  tySet v top
  return v

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

--
-- Main constraint generation functions
--

-- |'withReaderCont' is the default continuation builder.
withReaderCont :: BranchBuilder () -> ConstraintBuilder -> ConstraintBuilder
withReaderCont b cCont = do
  withReader r $ cCont
  where r :: BranchCont -> BranchCont
        r bPriorCont = \after -> bPriorCont (b >> after)

-- |'tryContinuations' is a more advanced continuation builder, that attempts a list
-- of instantiations associated to a variable at the instruction of unique label 'Ident'.
-- Each time an error is thrown during an attempted instantiation, it catches the error
-- and retries another continuation using the next viable instantiation.
--
-- OP_CHECKMULTISIG, OP_PICK and OP_ROLL use this continuation builder to backtrack
-- every time a possible solution turns out to contain type errors.
tryContinuations :: (Show a, Eq a) => Ident -> [a] -> (a -> BranchBuilder ()) -> BranchBuilder () -> BranchBuilder [a]
tryContinuations ident [] _ _ = failBranch $ "No possible continuation left for " ++ show ident ++ "!"
tryContinuations ident (x:xs) b bCont = do
  stateSave <- get
  E.catchError (verboseLog True ("For label " ++ show ident ++ ", trying continuation " ++ show x) >>
                b x >>
                bCont >>
                return (x:xs))
               (\e -> if null xs
                        then failBranch e
                        else put stateSave >> tryContinuations ident xs b bCont)

-- |'noteBranchJump' logs an execution branch decision point 'Bool', made at the OP_IF
-- that has unique label 'Label'.
noteBranchJump :: Label -> Bool -> BranchBuilder ()
noteBranchJump lbl b = do
  st <- get
  put $ st { branchInfo = (lbl,b) : branchInfo st }


-- |'genCnstrs' is the core branch analysis builder.
genCnstrs :: ScriptAST -> ConstraintBuilder
genCnstrs (ScriptITE ifLbl b0 elseLbl b1 fiLbl cont) = do
  let stModITE = (\b -> do
                      v <- popStack
                      t <- tyGet v
                      noteBranchJump ifLbl b
                      if b
                        then addCnstr (C_IsTrue v) >>
                             tySet v genTrue
                        else addCnstr (C_IsTrue $ Not v) >>
                             tySet (Not v) genTrue
                 )
  ss0 <- withReaderCont (stModITE True) $ genCnstrs b0
  ss1 <- withReaderCont (stModITE False) $ genCnstrs b1
  concat <$> mapM (\s -> local (const s) (genCnstrs cont)) (ss0 ++ ss1)

genCnstrs (ScriptOp lbl OP_ROLL cont) = do
  withReader r $ genCnstrs cont
  where r :: BranchCont -> BranchCont
        r bPriorCont = \bAfterCont ->
          bPriorCont $ do
            e_n <- peakStack
            case convert2Int e_n of
              Just (ConstInt i) -> do
                stModOpRoll i
                bAfterCont
              Nothing -> do
                let pairs = [1..]
                pairs' <- continueFromIntContinuation lbl pairs

                conts <- tryContinuations lbl pairs' tryRoll bAfterCont
                logSuccessIntContinuation lbl conts
                where tryRoll n = entering lbl OP_ROLL
                                >> stModOpRoll n

genCnstrs (ScriptOp lbl OP_PICK cont) = do
  withReader r $ genCnstrs cont
  where r :: BranchCont -> BranchCont
        r bPriorCont = \bAfterCont ->
          bPriorCont $ do
            e_n <- peakStack
            case convert2Int e_n of
              Just (ConstInt i) -> do
                stModOpPick i
                bAfterCont
              Nothing -> do
                let pairs = [1..]
                pairs' <- continueFromIntContinuation lbl pairs

                conts <- tryContinuations lbl pairs' tryPick bAfterCont
                logSuccessIntContinuation lbl conts
                where tryPick n = entering lbl OP_PICK
                                >> stModOpPick n

genCnstrs (ScriptOp lbl OP_CHECKMULTISIG cont) = do
  withReader r $ genCnstrs cont
  where r :: BranchCont -> BranchCont
        r bPriorCont = \bAfterCont ->
          bPriorCont $ do
            stateSave <- get

            -- Using prolog here to figure out the possible instances for
            -- nPub and nSig that might be viable (we can only use prolog here
            -- to verify if an nPub, nSig pair is valid up to this point of
            -- symbolic execution! Picking any of the solutions prolog finds
            -- can still result in a symbolic execution error at later instructions.
            --
            -- This is why we backtrack on a thrown error in successCont and retry
            -- a next viable solution. Until all viable solutions are exhausted,
            -- in which case the current execution branch is provably unexecutable

            let customLogic = "\
            \(MAX_PUBS = 20),\n\
            \(N #=< MAX_PUBS),\n\
            \(N #> 0),\n\
            \(M #=< N),\n\
            \(M #> 0),\n\

            \(element(1, Xsints, N)),\n\

            \(PosPUBS #= N + 1),\n\
            \(PosNSIG #= N + 2),\n\
            \(PosSIGS #= N + 2 + M),\n\

            \(element(PosPUBS, Xsbs, PUB)),\n\
            \(PUB in " ++ ranges2PL (bsRanges pkTy) ++ "),\n\

            \(element(PosNSIG, Xsbs, NSIG)),\n\
            \(NSIG in " ++ ranges2PL (bsRanges int) ++ "),\n\
            \(element(PosNSIG, Xsints, M)),\n\

            \(element(PosSIGS, Xsbs, SIG)),\n\
            \(SIG in " ++ ranges2PL (bsRanges sigTy) ++ "),\n"

            st <- get
            let maxPubs = 20
                maxStackPop = maxPubs*2+2
                extras = map Var $ take (maxStackPop - length (stack st)) [freshV st,((freshV st)-1)..]
            let stack' = if null extras
                          then take maxStackPop (stack st)
                          else stack st ++ extras

            mapM_ (flip tySet top) extras
            st' <- get

            let plGen = solveForArgs (st') ["N","M"] ("Xs",stack') customLogic
            (pl,pairs) <- case plGen of
                            Left err -> failBranch err
                            Right pl -> do
                              r <- liftIO $ prologSolve ["N","M"] pl
                              case r of
                                Left err -> failBranch err
                                Right solutions -> return (pl,solutions)

            put stateSave
            pairs' <- continueFromMsigContinuation lbl pairs

            verboseLog False $ "MSIG at label " ++ show lbl ++ ", generated prolog: " ++ pl
            conts <- tryContinuations lbl pairs' tryMSIG bAfterCont
            logSuccessMsigContinuation lbl conts
            where tryMSIG (nPub,nSig) = entering lbl OP_CHECKMULTISIG
                                      >> stModOpMSIG nPub nSig

genCnstrs stmnt@(ScriptOp lbl op cont) | isJust op' =
  genCnstrs (ScriptOp lbl (fromJust op') (ScriptOp lbl OP_VERIFY cont))
  where op' = lookup op verifyAfterOps
genCnstrs (ScriptOp lbl op cont) = do
  withReaderCont (entering lbl op >> stModOp op) $ genCnstrs cont
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

entering :: Label -> ScriptOp -> BranchBuilder ()
entering lbl op = do
  st <- get
  put $ st { muts = Executing lbl op : muts st }

verboseLog :: Bool -> String -> BranchBuilder ()
verboseLog ioPrint str = do
  st <- get
  put $ st { muts = Log str : muts st }
  if ioPrint
    then liftIO $ putStrLn str
    else return ()


-- |'stModOp' defines for the basic operations the symbolic execution.
-- The basic operations are those that that do not impose multiple possible execution
-- branches, nor pose multiple possible instantiations for some variable that need
-- to be tried exhaustively until 1 instantiation is found that creates a type correct
-- and constraint consistent symbolic execution.
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

stModOp OP_TOALTSTACK = popStack >>= pushAltStack
stModOp OP_FROMALTSTACK = popAltStack >>= pushStack_

stModOp OP_DEPTH = do
  depth <- length . stack <$> get
  (uncurry tySet) (annotTy (ConstInt depth))
  v <- genArbitraryV
  tySet v int
  let depthCnstr = Op v ">=" (ConstInt depth)
  tySet depthCnstr bool
  addCnstr (C_IsTrue depthCnstr)
stModOp OP_DROP = popStack >> return ()
stModOp OP_DUP = popStack >>= \v -> pushsStack_ [v,v]
-- OP_IFDUP supported via the parser (see lib/Parser/Parser.y)
stModOp OP_NIP = do
  v <- popStack
  popStack
  pushStack_ v
stModOp OP_OVER = do
  v2 <- popStack
  v1 <- popStack
  pushsStack_ [v1,v2,v1]
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

stModOp OP_0NOTEQUAL = do
  v <- popStack
  tySet v int
  pushStack (Op v "/=" (ConstInt 0)) bool

stModOp OP_1ADD = do
  v <- popStack
  tySet v int
  pushStack (Op v "+" (ConstInt 1)) int
stModOp OP_1SUB = do
  v <- popStack
  tySet v int
  pushStack (Op v "-" (ConstInt 1)) int
stModOp OP_NEGATE = do
  v <- popStack
  tySet v int
  pushStack (Op v "*" (ConstInt (-1))) int
stModOp OP_ABS = do
  v <- popStack
  tySet v int
  pushStack (Abs v) int

stModOp OP_ADD = opStack "+"
stModOp OP_SUB = opStack "-"
stModOp OP_BOOLAND = opStack "/\\"
stModOp OP_BOOLOR = opStack "\\/"
stModOp OP_NUMEQUAL = do
  v_2 <- popStack
  v_1 <- popStack
  tySet v_1 int
  tySet v_2 int
  pushStack (Op v_1 "==" v_2) bool
stModOp OP_NUMNOTEQUAL = do
    v_2 <- popStack
    v_1 <- popStack
    tySet v_1 int
    tySet v_2 int
    pushStack (Op v_1 "/=" v_2) bool
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

stModOp OP_MIN = do
  v_2 <- popStack
  v_1 <- popStack
  tySet v_2 int
  tySet v_1 int
  pushStack (Min v_1 v_2) int
stModOp OP_MAX = do
  v_2 <- popStack
  v_1 <- popStack
  tySet v_2 int
  tySet v_1 int
  pushStack (Max v_1 v_2) int

stModOp OP_RIPEMD160 = popStack >>= \v -> (uncurry pushStack) (annotTy (Hash v 20))
stModOp OP_SHA1 = popStack >>= \v -> (uncurry pushStack) (annotTy (Hash v 20))
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
  tySet v_1 sigTy
  tySet v_2 pkTy
  pushStack (Sig v_1 v_2) bool

stModOp OP_CODESEPARATOR = return ()
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
  (uncurry tySet) $ annotTy (ConstInt 0)
  addCnstr (C_IsTrue cnstrMin0)

  let lBit31 = Op l' "&" (Hex "80000000")
      lBit31_ = Op lBit31 ">>" (ConstInt 31)
      lBit22 = Op l' "&" (Hex "00400000")
      lBit22_ = Op lBit22 ">>" (ConstInt 22)
      lMasked = Op l' "&" (Hex "0000FFFF")

      lVar3Bit22 = Op (Var 2) "&" (Hex "00400000")
      lVar3Bit22_ = Op lVar3Bit22 ">>" (ConstInt 22)

      lVar3Masked = Op (Var 2) "&" (Hex "0000FFFF")

      timeCnstrA = Op lVar3Masked ">=" lMasked

      timeCnstrB = Op lVar3Bit22_ "/\\" lBit22_
      timeCnstrC = Op (Not $ lVar3Bit22_) "/\\" (Not lBit22_)
      timeCnstrD = Op timeCnstrB "\\/" timeCnstrC

      timeCnstrE = Op timeCnstrD "/\\" timeCnstrA

      timeCnstr = Op lBit31_ "\\/" timeCnstrE

  tySet timeCnstrA bool
  tySet timeCnstrB bool
  tySet timeCnstrC bool
  tySet timeCnstrD bool
  tySet timeCnstrE bool
  tySet timeCnstr bool

  tySet (Not $ lBit22_) bool

  tySet lMasked int
  tySet lBit31 bint
  tySet lBit31_ bool
  tySet lBit22 int
  tySet lBit22_ bool
  tySet (Hex "0000FFFF") int
  tySet (Hex "80000000") int
  tySet (Hex "00400000") int

  addCnstr (C_IsTrue timeCnstr)
  addCnstr (C_Spec lMasked)
  addCnstr (C_Spec lBit31_)
  addCnstr (C_Spec lBit22_)

-- DISABLED OP_CODES
stModOp op | any (== op) disabledOPs = failBranch $ "Error, disabled OP used: " ++ show op

stModOp op =
  failBranch $ "Error, no stModOp implementation for operator: " ++ show op

stModOpRoll n = do
  e <- popStack
  let constN = ConstInt n
      nCnstr = Op e "==" constN

  tySet nCnstr bool
  (uncurry tySet) (annotTy constN)
  addCnstr (C_IsTrue nCnstr)

  es <- popsStack (n-1)
  e_n <- popStack
  pushsStack_ es
  pushStack_ e_n

stModOpPick n = do
  e <- popStack
  let constN = ConstInt n
      nCnstr = Op e "==" constN

  tySet nCnstr bool
  (uncurry tySet) (annotTy constN)
  addCnstr (C_IsTrue nCnstr)

  es <- popsStack (n-1)
  e_n <- popStack
  pushsStack_ (e_n : es)
  pushStack_ e_n

stModOpMSIG nPub nSig = do
  e_npub <- popStack -- the n_pubs
  tySet e_npub int
  let n_pubsExpr = ConstInt nPub
      npubCnstr = Op e_npub "==" n_pubsExpr

  tySet npubCnstr bool
  (uncurry tySet) (annotTy n_pubsExpr)
  addCnstr (C_IsTrue npubCnstr)

  pubs <- popsStack nPub
  mapM (flip tySet pkTy) pubs

  e_nsig <- popStack
  tySet e_nsig int
  let nsigConstant = ConstInt nSig
      nsigCnstr = Op e_nsig "==" nsigConstant
  addCnstr (C_IsTrue nsigCnstr)
  tySet nsigCnstr bool
  (uncurry tySet) (annotTy nsigConstant)

  sigs <- popsStack nSig
  mapM (flip tySet sigTy) sigs

  bug <- popStack -- Due to a bug in the Bitcoin core implementation
  let const0 = ConstInt 0
      bugCnstr = Op bug "==" const0
  (uncurry tySet) (annotTy const0)
  tySet bugCnstr bool
  addCnstr (C_IsTrue bugCnstr)

  pushStack (MultiSig sigs pubs) bool

-- |The following instructions (defined by the 'disabledOPs' list) have been disabled
-- by the bitcoin core client. Upon encountering any one of these in an execution,
-- an error is thrown and the execution is deemed unsuccesful. This means that
-- they can be present in correct scripts, as long as the specific execution branch
-- taken during evaluation does not contain these.
disabledOPs =
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

  OP_VER,
  OP_RESERVED
  ]

-- |The following instructions (defined by the 'bannedOPs' list) have been banned
-- by the bitcoin core client. Any presence of any of these in a script invalidates
-- the entire script. This means that, even if they reside in an execution branch
-- that is not traversed in a hypothetical execution, the corresponding transaction
-- is still regarded as invalid.
bannedOPs = [
  OP_VERNOTIF,
  OP_VERIF
  ]
