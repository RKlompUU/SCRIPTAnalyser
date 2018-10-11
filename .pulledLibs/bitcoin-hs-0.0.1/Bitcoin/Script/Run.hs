
-- | Bitcoin script interpreter

{-# LANGUAGE PatternGuards, ScopedTypeVariables, PackageImports #-}
module Bitcoin.Script.Run
  ( 
  -- * types
    Entry , InterpreterConfig(..) , InterpreterState(..) , ScriptMonad 
  -- * high level functions
  , checkTransaction 
  -- * medium level functions
  , isDisabledOpcode 
  , initialState
  , executeScript , runScriptPre, runScriptFinal
  , scriptStep , scriptStep' 
  , isFalse, isTrue
  -- * internal types
  , Stream(..) , Context(..) , Hole(..) 
  , IfBranch(..) , IfType(..) , IfBlock(..)
  -- * some internal functions
  , streamMoveRight
  , fetchOpcode , fetchOpcodeWithinContext
  , fetchIfBlock
  , reconstructIfBlock
  -- * script monad
  , invalid
  , getState
  , putState
  -- * push\/pop
  , pushData , popData
  , pushAltData , popAltData
  , pushInteger , popInteger
  , pushBool , popBool
  -- * parsing (should be elsewhere)
  , parseTxScripts      
  , parseTxInScripts      
  , parseTxOutScripts      
  , parseSingleTxOutScript
  )  
  where

--------------------------------------------------------------------------------

import Data.Int
import Data.Word
import Data.Bits
import Data.List ( unfoldr , splitAt , mapAccumL )
import Data.Maybe

import Control.Monad
import Control.Applicative

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Identity

import qualified Data.ByteString as B

import Bitcoin.Misc.Bifunctor
import Bitcoin.Misc.BigInt
import Bitcoin.Misc.OctetStream
import Bitcoin.Misc.Zipper as Zipper

import Bitcoin.Crypto.Hash.SHA1
import Bitcoin.Crypto.Hash.SHA256
import Bitcoin.Crypto.Hash.RipEmd160

import Bitcoin.Crypto.EC.Curve
import Bitcoin.Crypto.EC.Key
import Bitcoin.Crypto.EC.DSA

import Bitcoin.Protocol.Hash
import Bitcoin.Protocol.Key
import Bitcoin.Protocol.Signature

import Bitcoin.BlockChain.Base
import Bitcoin.BlockChain.Parser ( serializeTx )
import Bitcoin.BlockChain.Tx

import Bitcoin.Script.Base
import Bitcoin.Script.Integer
import Bitcoin.Script.Serialize

--------------------------------------------------------------------------------

isDisabledOpcode :: Opcode -> Bool
isDisabledOpcode op = case op of

  -- splice
  OP_CAT       -> True
  OP_SUBSTR    -> True
  OP_LEFT      -> True
  OP_RIGHT     -> True

  -- bitwise logic
  OP_INVERT    -> True
  OP_AND       -> True
  OP_OR        -> True
  OP_XOR       -> True

  -- arithmetic
  OP_2MUL      -> True
  OP_2DIV      -> True
  OP_MUL       -> True
  OP_DIV       -> True
  OP_MOD       -> True
  OP_LSHIFT    -> True
  OP_RSHIFT    -> True

  _ -> False

--------------------------------------------------------------------------------

-- | An opcode stream consist of a zipper of opcodes, and a "Context" which
-- describes (possibly recursively) what is on the left and right side of 
-- this zipper. This is used for executing (possibly nested) OP_IF blocks. 
--
-- This may be overly complicated :)
--
data Stream = Stream 
  { _streamContext :: Context
  , _streamZipper  :: Zipper Opcode
  }
  deriving Show

-- | A context of opcodes and (nested) if blocks
data Context 
  = CtxEmpty                                         -- ^ we are the full opcode stream
  | CtxHole Context [Opcode] IfType Hole [Opcode]    -- ^ a hole has an outer context, opcodes on the left and right, and an if block in the middle
  deriving Show

data Hole 
  = HoleThen  { _elsePart :: Maybe [Opcode] }    -- ^ we are in the \"then\" branch
  | HoleElse  { _thenPart   :: [Opcode] 
              , _elseExists :: Bool }            -- ^ we are in the \"else\" branch (which may not physically exists - this is important when reconstructing)
  deriving Show

toListIfPart :: [Opcode] -> [Opcode]
toListIfPart ops = OP_IF : ops

toListElsePart :: Maybe [Opcode] -> [Opcode]
toListElsePart mbops = case mbops of 
  Nothing  -> []
  Just ops -> OP_ELSE : ops

flattenStreamToList :: Stream -> [Opcode]
flattenStreamToList (Stream ctx zipper) = worker ctx (zipperToList zipper) where
  worker ctx list = case ctx of
    CtxEmpty                      -> list
    CtxHole outer left typ hole right -> worker outer $ case hole of 
      HoleThen elsePart        -> ifOpcode typ : list     ++  toListElsePart elsePart                ++ [OP_ENDIF]
      HoleElse thenPart exists -> ifOpcode typ : thenPart ++ (if exists then (OP_ELSE:list) else []) ++ [OP_ENDIF]
      -- note: if `exists' is False then `list' must be empty

flattenStreamToZipper :: Stream -> Zipper Opcode
flattenStreamToZipper (Stream ctx zipper) = worker ctx zipper where
  worker ctx zip@(Zipper ys xs) = case ctx of
    CtxEmpty                      -> zip
    CtxHole outer left typ hole right -> worker outer $ case hole of 
      HoleThen elsePart -> Zipper 
        (ys ++ ifOpcode typ : reverse left)
        (xs ++ toListElsePart elsePart ++ OP_ENDIF : right)
      HoleElse thenPart exists -> Zipper 
        (ys ++ (if exists then [OP_ELSE] else []) ++ reverse thenPart ++ ifOpcode typ : reverse left)   
        (xs ++ OP_ENDIF : right)
        -- note: if `exists' is False then `xs' and `ys' must be empty

-- | Even where there is nothing on the right, we can change the stream itself during the discovery of this fact!
streamMoveRight :: Stream -> Either Stream (Opcode,Stream)
streamMoveRight fullStream@(Stream ctx zipper) = 
  case Zipper.moveRight zipper of 
    Just (x,zipper') -> Right (x, Stream ctx zipper')
    Nothing -> case ctx of
      CtxEmpty -> Left fullStream
      CtxHole outer left typ hole right -> streamMoveRight $ Stream outer $ case hole of 
        HoleThen elsePart        -> mkZipper (left ++ ifOpcode typ : zipperToList zipper ++  toListElsePart elsePart                               ++ [OP_ENDIF]) right
        HoleElse thenPart exists -> mkZipper (left ++ ifOpcode typ : thenPart            ++ (if exists then OP_ELSE : zipperToList zipper else []) ++ [OP_ENDIF]) right
        -- note: if `exists' is False then `zipperToList zipper' must be empty

--------------------------------------------------------------------------------
-- * Script interpreter

-- | Stack entry
type Entry = B.ByteString

-- | Two stacks, an opcode stream (the latter necessary for the somewhat convoluted IF parsing, and also for OP_CHECKSIG)
data InterpreterState = St 
  { _mainStack    :: [Entry]                -- ^ the main stack
  , _altStack     :: [Entry]                -- ^ the alt stack
  , _opcodeStream :: Stream                 -- ^ the opcode stream
  }
  deriving Show

-- | Empty stacks, empty script
initialState :: InterpreterState
initialState = St [] [] (Stream CtxEmpty (Zipper [] []))

data InterpreterConfig = Cfg
  { _newTx      :: !(Tx RawScript RawScript)  -- ^ the /new/ tx we are running (containing the scriptSigs which are combined with the previous tx's pubKeyScripts)
  , _curTxInIdx :: !Int                       -- ^ the index of the current transaction input (within the /new/ tx) we are checking
  }
  deriving Show

-- | Interpreter monad
type ScriptMonad a = ExceptT String (StateT InterpreterState (ReaderT InterpreterConfig Identity)) a

--------------------------------------------------------------------------------

-- | Given a transaction together with its inputs, we check if it is valid or not.
-- This is done by combining the input scripts of this transaction with the output
-- scripts of the previous transactions, and running the resulting scripts
--
-- If any of the scripts fails, the cause of failure is returned; if the scripts runs correctly,
-- the result is returned (which should be 'True' for valid transactions)
--
checkTransaction :: forall a. Tx (Tx a RawScript, RawScript) RawScript -> Either String Bool
checkTransaction txExt = result where

  result = if fee >= 0 
    then (go 0 insExt)
    else Left "total transaction output is more than total input"

  newtx  = fmapFst snd txExt :: Tx RawScript RawScript
  insExt = _txInputs txExt :: [TxInput (Tx a RawScript, RawScript)]

  fee = txFee (fmapFst fst txExt)

  go :: Int -> [TxInput (Tx a RawScript, RawScript)] -> Either String Bool
  go _ []          = Right True
  go j (this:rest) = 
    case parseSingleTxOutScript previdx prevtx_raw of
      Left err       -> Left err
      Right prevtxei -> case parseScript (_txInScript thisin_raw) of 
        Nothing       -> Left $ "cannot parse input script #" ++ show j
        Just inscript -> case runScriptPre cfg initialState inscript of
          (Left  err, _    ) -> Left err
          (Right _  , St mainstack _ _) -> 
            let prevout_raw = (_txOutputs prevtx_raw) !! previdx
                ei_prevout  = (_txOutputs prevtxei  ) !! previdx :: TxOutput (Either RawScript Script)
                -- NOTE: only the main stack is shared between sig and pk scripts, the alt stack is not!
                state'      = St mainstack [] (error "checkTransaction: stream shouldn't be evaluated")
            in  case _txOutScript ei_prevout of
                  Left  _raw_     -> Left "this shouldn't happen: parsed something else we wanted to parse"
                  Right outscript -> if _txHash prevtx_raw /= prevhash
                    then Left $ "fatal error: hash of input tx #" ++ show j ++ " does not match"
                    else case fst $ runScriptFinal cfg state' (outscript) of
                      Left err -> Left err
                      Right b  -> case b of
                        False -> Left $ "tx input #" ++ show j ++ " failed to check"
                        True  -> go (j+1) rest
    where
      cfg = Cfg newtx j
      prevtx_raw = fst (_txInScript this) :: Tx a RawScript
      thisin_raw = fmap snd this          :: TxInput RawScript
      previdx  = fromIntegral (_txInPrevOutIdx  thisin_raw) :: Int
      prevhash =               _txInPrevOutHash thisin_raw  :: Hash256

--------------------------------------------------------------------------------
    
-- | Tries to parse all scripts, both input and output. Since the output scripts can fail to parse (see below),
-- this may also fail.
parseTxScripts :: Tx RawScript RawScript -> Either String (Tx Script Script)
parseTxScripts tx = 
  case mapAccumLBoth (parseHelperFun tx) (parseHelperFun tx) Nothing tx of
    (Nothing ,tx') -> Right tx'
    (Just err,_  ) -> Left  err

-- | Tries to parse all input scripts. This shouldn't fail for a valid transaction.
parseTxInScripts :: Tx RawScript a -> Either String (Tx Script a)
parseTxInScripts tx = 
  case mapAccumLFst (parseHelperFun tx) Nothing tx of
    (Nothing ,tx') -> Right tx'
    (Just err,_  ) -> Left  err

-- | Tries to parse all output scripts. This may fail because invalid output scripts are allowed, just unspendable... :(
parseTxOutScripts :: Tx a RawScript -> Either String (Tx a Script)
parseTxOutScripts tx = 
  case mapAccumLSnd (parseHelperFun tx) Nothing tx of
    (Nothing ,tx') -> Right tx'
    (Just err,_  ) -> Left  err

-- | Parses only a single output script, leaves the rest (reason: tx validation shouldn\'t fail just
-- because there are other invalid scripts in the prev tx unrelated to this one).
parseSingleTxOutScript :: Int -> Tx a RawScript -> Either String (Tx a (Either RawScript Script))
parseSingleTxOutScript j tx = 
  case mapAccumLSnd worker (Nothing,0) tx of
    ((Nothing ,_),tx') -> Right tx'
    ((Just err,_),_  ) -> Left  err
  where
    worker :: (Maybe String,Int) -> RawScript -> ((Maybe String,Int),Either RawScript Script)
    worker (mberr,k) raw = case mberr of
      Just err -> ((mberr,k),undef)
      Nothing  -> if (k/=j) 
        then ((mberr,k+1),Left raw)
        else case parseScript raw of 
          Just script -> ((mberr,k+1),Right script)
          Nothing     -> ((Just ("cannot parse script in output " ++ show k ++ " of tx # " ++ show (_txHash tx)),k), undef) 
    undef = error "parseSingleTxOutScript/worker: this shouldn't be evaluated"

-- | Helper function.
parseHelperFun :: Tx a b -> Maybe String -> RawScript -> (Maybe String, Script)
parseHelperFun tx mberr rawscript = 
  case mberr of
    Just err -> (mberr,undef)
    Nothing  -> case parseScript rawscript of
      Just script -> (mberr, script)
      Nothing     -> (Just ("cannot parse script in tx # " ++ show (_txHash tx)), undef) 
  where
    undef = error "parseHelperFun: this shouldn't be evaluated"
 
--------------------------------------------------------------------------------

-- | Runs the scriptSig 
runScriptPre :: InterpreterConfig -> InterpreterState -> Script -> (Either String (), InterpreterState)
runScriptPre cfg st (Script opcodes) = runIdentity $ runReaderT (runStateT (runExceptT (executeScript opcodes)) st) cfg

-- | Runs the scriptPubKey
runScriptFinal :: InterpreterConfig -> InterpreterState -> Script -> (Either String Bool, InterpreterState)
runScriptFinal cfg st (Script opcodes) = runIdentity $ runReaderT (runStateT (runExceptT action) st) cfg
  where
    action :: ScriptMonad Bool
    action = do
      executeScript opcodes 
      St main alt stream <- getState
      case stream of 
        Stream CtxEmpty (Zipper finalRevOpcodes []) -> 
          if reverse finalRevOpcodes /= opcodes
            then invalid "executeScript: shouldn't happen (script finished but the \"consumed opcodes\" are not the same as the original ones)"
            else case main of
              (top:_) -> return (asInteger top /= 0)                         -- if the top of the stack is TRUE, we are ok, otherwise we failed.
              []      -> invalid "script finished but the stack is empty"
        _ -> invalid $ "executeScript: shouldn't happen (script finished but there are remaining opcodes)"   -- (++ "\n" ++ show stream)

--------------------------------------------------------------------------------

-- | Executes a list of opcodes
executeScript :: [Opcode] -> ScriptMonad ()
executeScript opcodes = 
  do
    when (any isDisabledOpcode opcodes) $ invalid "disabled opcode appearing in the script"
    St main alt _ <- getState
    putState (St main alt $ Stream CtxEmpty (zipperFromList opcodes))
    worker
  where
    worker = do
      finished <- scriptStep
      unless finished worker 

-- | Execute a single (except in case of conditionals) opcode 
-- 
-- \"The stacks hold byte vectors. Byte vectors are interpreted as little-endian 
-- variable-length integers with the most significant bit determining the sign
-- of the integer. Thus 0x81 represents -1. 0x80 is another representation of 
-- zero (so called negative 0). Byte vectors are interpreted as Booleans where 
-- False is represented by any representation of zero, and True is represented 
-- by any representation of non-zero.\"
--
-- Returns 'True' if the script finished.
--
scriptStep :: ScriptMonad Bool
scriptStep = fetchOpcode >>= \mbop -> case mbop of
  Nothing -> return True
  Just op -> do
    -- when (isDisabledOpcode op) $ invalid $ show op ++ ": disabled opcode"
    scriptStep' op
    return False

scriptStep' :: Opcode -> ScriptMonad ()
scriptStep' op = case op of

  OP_SMALLNUM n -> case n of 
                       0 -> pushWords []
                       _ -> pushWords [fromIntegral n]

  OP_1NEGATE         -> pushWords [0x81]    -- "Thus 0x81 represents -1"
  OP_PUSHDATA w8 bs  -> if is_valid_pushdata w8 bs 
                          then pushData bs 
                          else invalid "fatal error: invalid PUSHDATA opcode"

  -- control flow
  OP_NOP w8   -> if is_nop w8 
                   then return () 
                   else invalid "fatal error: invalid NOP opcode"
  OP_IF       -> do 
                   b <- popBool
                   let branchTaken = if b then IfBranch else ElseBranch
                   ifblock@(IfBlock _ ifops mbelseops) <- fetchIfBlock If branchTaken
                   -- unless (checkForValidIfBlock ifblock) $ invalid "OP_VERIF or OP_VERNOTIF appearing in an if branch"                   
                   return ()
  OP_NOTIF    -> do 
                   b <- popBool
                   let branchTaken = if not b then IfBranch else ElseBranch
                   ifblock@(IfBlock _ ifops mbelseops) <- fetchIfBlock NotIf branchTaken
                   -- unless (checkForValidIfBlock ifblock) $ invalid "OP_VERIF or OP_VERNOTIF appearing in an if branch"
                   return ()

  OP_ELSE     -> invalid "naked OP_ELSE found; this shouldn't happen"
  OP_ENDIF    -> invalid "naked OP_ENDIF found; this shouldn't happen"
  OP_VERIFY   -> do { x <- popData ; if isTrue x then return () else (pushData x >> invalid "OP_VERIFY: false") }
  OP_RETURN   -> invalid "OP_RETURN executed"

  -- stack
  OP_TOALTSTACK    -> do { x <- popData    ; pushAltData x } 
  OP_FROMALTSTACK  -> do { x <- popAltData ; pushData    x } 
  OP_IFDUP     -> do { x <- popData ; pushData x ; when (isTrue x) (pushData x) }
  OP_DEPTH     -> do { xs <- getMainStack ; pushInteger (fromIntegral $ length xs) }
  OP_DROP      -> do { _ <- popData ; return () }
  OP_DUP       -> do { x <- popData ; pushData x ; pushData x }
  OP_NIP       -> do { x <- popData ; _ <- popData ; pushData x }
  OP_OVER      -> do { x <- popData ; y <- popData ; pushData y ; pushData x ; pushData y }  -- Copies the second-to-top stack item to the top.

  -- The item n back in the stack is copied to the top. 
  OP_PICK  -> do   
                n <- popInteger ; xs <- getMainStack 
                when (n < 0)                         $ invalid "OP_PICK: negativ index"
                when (fromIntegral (length xs) <= n) $ invalid "OP_PICK: stack is not deep enough"
                putMainStack ( xs!!(fromIntegral n) : xs)                        
              
  -- The item n back in the stack is moved to the top.
  OP_ROLL  -> do
                n <- popInteger ; xs <- getMainStack 
                when (n < 0)                         $ invalid "OP_ROLL: negativ index"
                when (fromIntegral (length xs) <= n) $ invalid "OP_ROLL: stack is not deep enough"
                let (hd,tl) = splitAt (fromIntegral n + 1) xs
                putMainStack (last hd : init hd ++ tl)                        

  OP_ROT       -> do { (a,b,c) <- popTriple ; pushTriple (c,a,b) }
  OP_SWAP      -> do { x <- popData ; y <- popData ; pushData x ; pushData y }
  OP_TUCK      -> do { x <- popData ; y <- popData ; pushData x ; pushData y ; pushData x }  -- The item at the top of the stack is copied and inserted before the second-to-top item

  OP_2DROP     -> do { _   <- popPair ; return () }
  OP_2DUP      -> do { xy  <- popPair ; pushPair xy ; pushPair xy }
  OP_3DUP      -> do { xyz <- popTriple ; pushTriple xyz ; pushTriple xyz }
  OP_2OVER     -> do { xy  <- popPair ; zw <- popPair ; pushPair zw ; pushPair xy ; pushPair zw }  -- Copies the pair of items two spaces back in the stack to the front.

  OP_2ROT -> do
               a <- popPair ; b <- popPair ; c <- popPair
               pushPair b   ; pushPair a   ; pushPair c

  OP_2SWAP     -> do { xy <- popPair ; zw <- popPair ; pushPair xy ; pushPair zw }

  -- splice
  OP_CAT       -> do { b <- popData ; a <- popData ; pushData (B.append a b) }
  OP_SIZE      -> do { s <- popData ; pushData s ; pushInteger (dataLen s) }

  OP_SUBSTR    -> do
                    siz <- popInteger 
                    bgn <- popInteger 
                    str <- popData 
                    let n = dataLen str
                    when (n < bgn+siz) $ invalid "OP_SUBSTR: string not long enough"
                    pushData (B.take (fromIntegral siz) $ B.drop (fromIntegral bgn) str)

  OP_LEFT      -> do
                    siz <- popInteger 
                    str <- popData 
                    let n = dataLen str
                    when (n < siz) $ invalid "OP_SUBSTR: string not long enough"
                    pushData (B.take (fromIntegral siz) str)

  OP_RIGHT     -> do
                    siz <- popInteger 
                    str <- popData 
                    let n = dataLen str
                    when (n < siz) $ invalid "OP_SUBSTR: string not long enough"
                    pushData (B.drop (fromIntegral (n-siz)) str)

  -- bitwise logic
  OP_INVERT      -> do { x <- popWords ; pushWords (map complement x) }
  OP_AND         -> do { y <- popWords ; x <- popWords ; pushWords (extendedZipWith (.&.) x y) }
  OP_OR          -> do { y <- popWords ; x <- popWords ; pushWords (extendedZipWith (.|.) x y) }
  OP_XOR         -> do { y <- popWords ; x <- popWords ; pushWords (extendedZipWith  xor  x y) }
  OP_EQUAL       -> do { y <- popData  ; x <- popData  ; pushBool (x==y) }
  OP_EQUALVERIFY -> scriptStep' OP_EQUAL >> scriptStep' OP_VERIFY

  -- arithmetic
  -- Note: Arithmetic inputs are limited to signed 32-bit integers, but may overflow their output. 
  -- If any input value for any of these commands is longer than 4 bytes, the script must abort and fail. 
  -- If any opcode marked as disabled is present in a script - it must also abort and fail.
  OP_1ADD           -> do { x <- popArith ; pushArith (x+1) }
  OP_1SUB           -> do { x <- popArith ; pushArith (x-1) }
  OP_2MUL           -> do { x <- popArith ; pushArith (x+x) }
  OP_2DIV           -> do { x <- popArith ; pushArith (div x 2) }
  OP_NEGATE         -> do { x <- popArith ; pushArith (negate x) }
  OP_ABS            -> do { x <- popArith ; pushArith (abs x) }
  OP_NOT            -> do { x <- popArith ; pushBool (x==0) }
  OP_0NOTEQUAL      -> do { x <- popArith ; pushBool (x/=0) }
  OP_ADD            -> do { y <- popArith ; x <- popArith ; pushArith (x+y) }
  OP_SUB            -> do { y <- popArith ; x <- popArith ; pushArith (x-y) }
  OP_MUL            -> do { y <- popArith ; x <- popArith ; pushArith (x*y) }
  OP_DIV            -> do { y <- popArith ; x <- popArith ; when (y==0) (invalid "OP_DIV: division by zero") ; pushArith (div x y) }
  OP_MOD            -> do { y <- popArith ; x <- popArith ; when (y==0) (invalid "OP_MOD: division by zero") ; pushArith (mod x y) }

  -- Shifts a left b bits, preserving sign
  OP_LSHIFT -> do  
                 -- k <- popArith 
                 -- (sgn,absn) <- (toSignAbs . fromIntegral) <$> popArith
                 -- pushArith $ fromSignAbs (sgn, shiftL absn (fromIntegral k)) 
                 k <- fromIntegral <$> popArith
                 n <- popArith
                 pushArith (shiftL n (mod k 32))

  -- Shifts a right b bits, preserving sign
  OP_RSHIFT -> do  
                 -- k <- popArith
                 -- (sgn,absn) <- (toSignAbs . fromIntegral) <$> popArith 
                 -- pushArith $ fromSignAbs (sgn, shiftR absn (fromIntegral k)) 
                 k <- fromIntegral <$> popArith
                 n <- popArith
                 pushArith (shiftR n (mod k 32))   -- Right shifts perform sign extension on /signed/ number types (here Integer)

  OP_BOOLAND         -> do { y <- popArith ; x <- popArith ; pushBool (x/=0 && y/=0) }
  OP_BOOLOR          -> do { y <- popArith ; x <- popArith ; pushBool (x/=0 || y/=0) }
  OP_NUMEQUAL        -> do { y <- popArith ; x <- popArith ; pushBool (x==y) }
  OP_NUMNOTEQUAL     -> do { y <- popArith ; x <- popArith ; pushBool (x/=y) }
  OP_NUMEQUALVERIFY  -> scriptStep' OP_NUMEQUAL >> scriptStep' OP_VERIFY
  OP_LESSTHAN            -> do { y <- popArith ; x <- popArith ; pushBool (x<y) }
  OP_GREATERTHAN         -> do { y <- popArith ; x <- popArith ; pushBool (x>y) }
  OP_LESSTHANOREQUAL     -> do { y <- popArith ; x <- popArith ; pushBool (x<=y) }
  OP_GREATERTHANOREQUAL  -> do { y <- popArith ; x <- popArith ; pushBool (x>=y) }
  OP_MIN                 -> do { y <- popArith ; x <- popArith ; pushArith (min x y) }
  OP_MAX                 -> do { y <- popArith ; x <- popArith ; pushArith (max x y) }
  OP_WITHIN              -> do { b <- popArith ; a <- popArith ; x <- popArith ; pushBool (x>=a && x<b) }  -- Returns 1 if x is within the specified range (left-inclusive), 0 otherwise

  -- crypto
  OP_RIPEMD160       -> do { x <- popData ; pushData (toByteString $ ripemd160 x) }
  OP_SHA1            -> do { x <- popData ; pushData (toByteString $ sha1      x) }
  OP_SHA256          -> do { x <- popData ; pushData (toByteString $ sha256    x) }
  OP_HASH160         -> do { x <- popData ; pushData (toByteString $ doHash160 x) }
  OP_HASH256         -> do { x <- popData ; pushData (toByteString $ doHash256 x) }
  OP_CODESEPARATOR   -> return ()   -- this one has only a meta meaning
  OP_CHECKSIG            -> execute_OP_CHECKSIG       -- invalid "OP_CHECKSIG: not implemented"
  OP_CHECKMULTISIG       -> execute_OP_CHECKMULTISIG  -- invalid "OP_CHECKMULTISIG: not implemented"
  OP_CHECKSIGVERIFY      -> scriptStep' OP_CHECKSIG      >> scriptStep' OP_VERIFY
  OP_CHECKMULTISIGVERIFY -> scriptStep' OP_CHECKMULTISIG >> scriptStep' OP_VERIFY
  
  -- reserved words
  OP_RESERVED           -> invalid "OP_RESERVED executed"
  OP_VER                -> invalid "OP_VER executed"
  OP_VERIF              -> invalid "OP_VERIF executed" 
  OP_VERNOTIF           -> invalid "OP_VERNOTIF executed"
  OP_RESERVED1          -> invalid "OP_RESERVED1 executed"
  OP_RESERVED2          -> invalid "OP_RESERVED2 executed"

  -- pseudo
  OP_INVALIDOPCODE      -> invalid "OP_INVALIDOPCODE executed"
  OP_UNKNOWN w          -> invalid ("OP_UNKNOWN (decimal " ++ show w ++ ") executed")

  _  -> invalid ("unhandled or invalid opcode " ++ show op)

--------------------------------------------------------------------------------
-- * stack entries

dataLen :: Entry -> Integer
dataLen = fromIntegral . B.length

-- | zero-extended zipWith (the result will as long as the longer input)
extendedZipWith :: (Word8 -> Word8 -> Word8) -> [Word8] -> [Word8] -> [Word8] 
extendedZipWith f = go where
  go (x:xs) (y:ys) = f x y : go xs ys
  go []     (y:ys) = f 0 y : go [] ys
  go (x:xs) []     = f x 0 : go xs []
  go []     []     = []

--------------------------------------------------------------------------------

isFalse :: Entry -> Bool
isFalse bs = (asInteger bs == 0)

isTrue :: Entry -> Bool
isTrue bs = (asInteger bs /= 0)

--------------------------------------------------------------------------------
-- * stack


pushData :: Entry -> ScriptMonad ()
pushData bs = do
  St main alt stream <- getState
  putState (St (bs:main) alt stream)

popData :: ScriptMonad Entry
popData = do
  St main alt stream <- getState
  case main of 
    (x:rest) -> do
      putState (St rest alt stream)
      return x
    [] -> invalid "cannot pop from main stack: it's empty"

pushAltData :: Entry -> ScriptMonad ()
pushAltData bs = do
  St main alt stream <- getState
  putState (St main (bs:alt) stream)

popAltData :: ScriptMonad Entry
popAltData = do
  St main alt stream <- getState
  case alt of 
    (x:rest) -> do
      putState (St main rest stream)
      return x
    [] -> invalid "cannot pop from alt stack: it's empty"

----------------------------------------
-- arithmetic stupidity

{-
ok this is the craziest stupid fucking thing i ever seen. Inputs are limited to 32 bit signed 
integers, but then they are treated as a bigint, and output can be more than 32 bit... 
(let me bet here that they were originally implemented using @uint64_t@...)
-}

-- | Note: Arithmetic inputs are limited to signed 32-bit integers, but may overflow their output 
-- (and by overflow they don't mean Int32 overflow, but that the result will not fit into an Int32...)
pushArith :: Integer -> ScriptMonad ()
pushArith = pushData . asByteString

-- | If any input value for any of these commands is longer than 4 bytes, the script must abort and fail. 
-- If any opcode marked as disabled is present in a script - it must also abort and fail.
--
popArith :: ScriptMonad Integer
popArith = do
  bs <- popData
  when (B.length bs > 4) $ invalid "arithmetic operator with argument longer than 4 bytes"
  return $ asInteger bs

----------------------------------------

pushBool :: Bool -> ScriptMonad ()
pushBool b = pushWords $ case b of { True -> [1] ; False -> [] } 

popBool :: ScriptMonad Bool 
popBool = do 
  n <- popInteger
  return (n/=0)

pushWords :: [Word8] -> ScriptMonad ()
pushWords ws = pushData (B.pack ws)

popWords :: ScriptMonad [Word8]
popWords = B.unpack <$> popData

pushInteger :: Integer -> ScriptMonad ()
pushInteger = pushData . asByteString

popInteger ::  ScriptMonad Integer
popInteger = asInteger <$> popData

--------------------------------------------------------------------------------

popPair :: ScriptMonad (Entry,Entry)
popPair = do
  x <- popData
  y <- popData
  return (x,y)

pushPair :: (Entry,Entry) -> ScriptMonad ()
pushPair (x,y) = do
  pushData y
  pushData x

popTriple :: ScriptMonad (Entry,Entry,Entry)
popTriple = do
  x <- popData
  y <- popData
  z <- popData
  return (x,y,z)

pushTriple :: (Entry,Entry,Entry) -> ScriptMonad ()
pushTriple (x,y,z) = do
  pushData z
  pushData y
  pushData x

--------------------------------------------------------------------------------
-- * lifted monad operations

invalid :: String -> ScriptMonad a
invalid msg = throwE msg

askCfg :: ScriptMonad InterpreterConfig
askCfg = lift (lift ask)

getState :: ScriptMonad InterpreterState
getState = lift get

putState :: InterpreterState -> ScriptMonad ()
putState what = lift (put what)

getMainStack :: ScriptMonad [Entry]
getMainStack = _mainStack <$> getState

putMainStack :: [Entry] -> ScriptMonad ()
putMainStack main = do
  St _ alt stream <- getState
  putState (St main alt stream)  

--------------------------------------------------------------------------------
-- * if blocks

data IfBranch 
  = IfBranch      -- ^ \"if-then\"
  | ElseBranch    -- ^ \"else\"
  deriving (Eq,Show)

data IfType
  = If          -- ^ OP_IF
  | NotIf       -- ^ OP_NOTIF
  deriving Show

ifOpcode :: IfType -> Opcode
ifOpcode t = case t of
  If    -> OP_IF
  NotIf -> OP_NOTIF

data IfBlock = IfBlock 
  { _ifType     :: IfType 
  , _ifBranch   :: [Opcode]
  , _elseBranch :: Maybe [Opcode]
  } 
  deriving Show

-- | Checks if OP_VER or OP_VERIF appears in any branch. Returns "True" if the block is valid.
checkForValidIfBlock :: IfBlock -> Bool
checkForValidIfBlock (IfBlock _ ifbr mbelsebr) = 
  not $ or
    [ elem OP_VERIF    ifbr
    , elem OP_VERNOTIF ifbr 
    , elem OP_VERIF    elsebr 
    , elem OP_VERNOTIF elsebr 
    ]
  where
    elsebr = maybe [] id mbelsebr

reconstructIfBlock :: IfBlock -> [Opcode]
reconstructIfBlock (IfBlock typ ifbranch mbelsebranch) = opcode : ifbranch ++ elsebranch ++ [OP_ENDIF] where
  opcode     = ifOpcode typ
  elsebranch = case mbelsebranch of 
    Nothing -> [] 
    Just es -> OP_ELSE : es

fetchIfBlock_ :: IfType -> IfBranch -> ScriptMonad ()
fetchIfBlock_ iftype branch = fetchIfBlock iftype branch >> return () 

-- | We fetch an if block *and* take the given branch (second argument)
--
-- Note: if blocks can be nested...
fetchIfBlock :: IfType -> IfBranch -> ScriptMonad IfBlock
fetchIfBlock topLevelIfType branchWeTook = 
  do
    St main alt (Stream ctx (Zipper yys _)) <- getState
    case yys of
      []     -> invalid "fetchIfBlock: fatal error, shouldn't happen /1"
      (y:ys) -> if (y /= ifOpcode topLevelIfType) 
        then invalid "fetchIfBlock: fatal error, shouldn't happen /2"
        else do
          ifblock <- fetch topLevelIfType
          let iftype = _ifType ifblock
          St _ _ (Stream _ctx (Zipper _ xs)) <- getState
          putState $ St main alt $ case branchWeTook of
            IfBranch   -> Stream (CtxHole ctx (reverse ys) iftype (HoleThen (_elseBranch ifblock)) xs)
                                 (Zipper [] (_ifBranch ifblock)) 
            ElseBranch -> Stream (CtxHole ctx (reverse ys) iftype (HoleElse (_ifBranch ifblock) (isJust $ _elseBranch ifblock)) xs)
                                 (Zipper [] (maybe [] id (_elseBranch ifblock)))
          
          unless (checkForValidIfBlock ifblock) $ invalid "OP_VERIF or OP_VERNOTIF appearing in an if branch"
          return ifblock
  where

    fetch :: IfType -> ScriptMonad IfBlock
    fetch iftype = go IfBranch [] [] where

      -- first argument is which branch we are in right now
      -- second and third arguments accumulates the opcodes on the two branches
      go :: IfBranch -> [Opcode] -> [Opcode] -> ScriptMonad IfBlock
      go branch ifops elseops = do
        mbop <- fetchOpcodeWithinContext
        when (mbop == Nothing) $ invalid "unfinished IF (or NOTIF) block" 
        let Just op = mbop
        case op of
          OP_IF    -> do
                        ifblock <- fetch If    
                        let ops = reconstructIfBlock ifblock 
                        continue (reverse ops) 
          OP_NOTIF -> do 
                        ifblock <- fetch NotIf 
                        let ops = reconstructIfBlock ifblock 
                        continue (reverse ops) 
          OP_ELSE  -> go ElseBranch ifops elseops
          OP_ENDIF -> return $ case branch of 
                        IfBranch   -> IfBlock iftype (reverse ifops)  Nothing                
                        ElseBranch -> IfBlock iftype (reverse ifops) (Just $ reverse elseops)
          _ -> case branch of
            IfBranch   -> go branch (op : ifops) elseops
            ElseBranch -> go branch ifops (op : elseops) 
        where 
          continue ops = case branch of
            IfBranch   -> go branch (ops ++ ifops) elseops
            ElseBranch -> go branch ifops (ops ++ elseops) 

--------------------------------------------------------------------------------
-- * opcode stream manipulation

-- | Fetches an opcode, possibly exiting the current context
fetchOpcode :: ScriptMonad (Maybe Opcode)
fetchOpcode = do
  St main alt stream <- getState  
  case streamMoveRight stream of 
    Left     stream'  -> putState (St main alt stream') >> return Nothing
    Right (x,stream') -> putState (St main alt stream') >> return (Just x)
           
-- | Fetches an opcode, but does not exit the current context
fetchOpcodeWithinContext :: ScriptMonad (Maybe Opcode)
fetchOpcodeWithinContext = do
  St main alt stream@(Stream ctx zipper) <- getState  
  case Zipper.moveRight zipper of 
    Nothing          -> return Nothing
    Just (x,zipper') -> putState (St main alt (Stream ctx zipper')) >> return (Just x)

{-
getOpcodes :: ScriptMonad [Opcode]
getOpcodes = do
  St main alt stream <- getState
  return $ snd $ zipperView stream

putOpcodes :: [Opcode] -> ScriptMonad ()
putOpcodes stream = do
  St main alt _ <- getState
  putState (St main alt stream)
-}

--------------------------------------------------------------------------------
-- * CHECKSIG

{-
Hashtype SIGHASH_ALL (default)
  No special further handling occurs in the default case. 
  Think of this as "sign all of the outputs." Which is already done by the default procedure. 

Procedure for Hashtype SIGHASH_NONE
  1. The output of txCopy is set to a vector of zero size. 
  2. All other inputs aside from the current input in txCopy have their nSequence index set to zero 
  Think of this as "sign none of the outputs -- I don't care where the bitcoins go."
 
Procedure for Hashtype SIGHASH_SINGLE
  1. The output of txCopy is resized to the size of the current input index+1. 
  2. All other txCopy outputs aside from the output that is the same as the current input index are set to a blank script and a value of (long) -1. 
  3. All other txCopy inputs aside from the current input are set to have an nSequence index of zero. 
  Think of this as "sign one of the outputs-- I don't care where the other outputs go". 

  Note: The transaction that uses SIGHASH_SINGLE type of signature should not have more outputs than inputs. 
  However if it does (because of the pre-existing implementation), it shall not be rejected, but instead 
  for every "illegal" input (meaning: an input that has an index bigger than the maximum output index) 
  the node should still verify it, though assuming the hash of 0000000000000000000000000000000000000000000000000000000000000001 
  <https://bitcointalk.org/index.php?topic=260595.0>

Procedure for Hashtype SIGHASH_ANYONECANPAY
  1. The txCopy input vector is resized to a length of one. 
  2. The subScript (lead in by its length as a var-integer encoded!) is set as the first and only member of this vector. 
  Think of this as "Let other people add inputs to this transaction, I don't care where the rest of the bitcoins come from."
-}

--------------------------------------------------------------------------------

codeSeparatorSubscript :: Stream -> RawScript
codeSeparatorSubscript fullStream = subscript where

  Zipper revbefore after = flattenStreamToZipper fullStream

  before' = reverse $ takeWhile (/= OP_CODESEPARATOR) revbefore
  after'  = filter (/= OP_CODESEPARATOR) after

  -- here, in theory, signatures should be deleted from subscript.
  -- however, in practice:
  --   1) signatures do not appear in pubKeyScripts (because self-signing is impossible?)
  --   2) it is theoretically impossible to distinguish a signature from some other data
  -- so we ignore this step

  subscript = serializeScript $ Script (before' ++ after') :: RawScript

--------------------------------------------------------------------------------

execute_OP_CHECKSIG :: ScriptMonad ()
execute_OP_CHECKSIG = do

  pubKeyStr <- popData  
  sigStr    <- popData

  St _ _ fullStream   <- getState
  Cfg tx inputidx <- askCfg

  let subscript = codeSeparatorSubscript fullStream

  case decodeSignatureDER' False sigStr of 

    Nothing -> pushBool False  -- invalid "OP_CHECKSIG: cannot decode DER signature"        
    Just (SignatureExt signature sighash) -> case decodePubKey pubKeyStr of

      Nothing -> pushBool False  -- invalid "OP_CHECKSIG: cannnot decode PubKey"     
      Just pubKey -> do

        let tx'    = mapAccumLFst_ (\i old -> (i+1 , if i==inputidx then subscript else RawScript B.empty)) 0 tx     
            ins'   = _txInputs  tx'
            outs'  = _txOutputs tx'
            nouts' = length outs'

        let (singleIssue,tx'') = case _sigHashType sighash of
                    SigHashAll     -> ( False, tx' )
                    SigHashAllZero -> ( False, tx' )
                    SigHashNone    -> ( False, setSeqNoToZeroTxExcept inputidx $ tx' { _txOutputs = [] } )
                    SigHashSingle  -> if nouts' > inputidx
                                        then (False, setSeqNoToZeroTxExcept inputidx $ tx' { _txOutputs = (replicate inputidx blankTxOutput) ++ [outs' !! inputidx] } )
                                        else (True , setSeqNoToZeroTxExcept inputidx $ tx' { _txOutputs = (replicate nouts'   blankTxOutput) } )

        let tx''' = case _anyOneCanPay sighash of
                      False -> tx''
                      True  -> tx'' { _txInputs = [ ins' !! inputidx ] }

        let RawTx rawtx = serializeTx tx'''
            hash = if singleIssue
              then fromIntegerLE 1       -- this works this way because of a stupid bug in the reference implementation...
              else doHash256 (rawtx `B.append` (fromWord8List [encodeSigHash sighash,0,0,0]))
        
        pushBool $ verifySignatureWithHash pubKey signature hash

--------------------------------------------------------------------------------

execute_OP_CHECKMULTISIG :: ScriptMonad ()
execute_OP_CHECKMULTISIG = do
                      
  n <- popInteger
  when (n > 20 || n < 1) $ invalid "OP_CHECKMULTISIG: n must be at least 1 and at most 20"
  pubKeyStrs <- reverse <$> replicateM (fromIntegral n) popData

  m <- popInteger
  when (m > n || m < 1) $ invalid "OP_CHECKMULTISIG: m must be at least 1 and at most n"
  sigStrs <- reverse <$> replicateM (fromIntegral m) popData

  _ <- popData     -- this is a bug in the original Satoshi client, which we must faithfully reproduce :(

  -- the first signature seems to have *another* extra byte at the end
  -- EXCEPT when it is missing, or EVEN the normal hashtype byte is missing!
  -- of course, there is precisely *ZERO* information about this on the net
  -- ok, fuck it, let's loose up the signature decoding...

  St _ _ fullStream   <- getState
  Cfg tx inputidx <- askCfg

  let subscript = codeSeparatorSubscript fullStream

  let mbSigs    = map (decodeSignatureDER' False) sigStrs     -- we allow non-standard encodings because they appear in the blockchain :(
      mbPubKeys = map  decodePubKey               pubKeyStrs

  case all isJust mbSigs of 

    False -> invalid $ "OP_CHECKMULTISIG: cannot decode DER signature" 
               ++ "\n  pubkeys    = " ++ (show $ map RawScript pubKeyStrs)
               ++ "\n  signatures = " ++ (show $ map RawScript sigStrs)
              
    -- unfortunately, the original bitcoin client checks the pubkeys in order, and decodes on-the-fly, which means that
    -- it allows for invalid pubkeys after there is enough to satisfy the transaction :((( all these stupid on-the-fly
    -- validation issues (allowing partially-unparsable transaction in the blockchain) makes my nice code much more ugly :(
    --
    -- see (the output script of) testnet3 transaction 2e131d48f58cbb358cc53967a2fb89a80a6da337cb430fd719f5888af7a48507
{-
    True  -> case all isJust mbPubKeys of

      False -> invalid "OP_CHECKMULTISIG: cannnot decode PubKey"
      True  -> do
-}
    True -> do

        let -- pubKeys = map fromJust mbPubKeys
            sigExts = map fromJust mbSigs
            sigs      = map _extSignature sigExts
            hashtypes = map _extSigHash   sigExts

        let tx'    = mapAccumLFst_ (\i old -> (i+1 , if i==inputidx then subscript else RawScript B.empty)) 0 tx     
            ins'   = _txInputs  tx'
            outs'  = _txOutputs tx'
            nouts' = length outs'

        -- there are more signatures -> more hashtypes 
        -- (but no documentation, fuck, who needs documentation for idiosyncratic code...)
        -- ok it seems that you have to recompute the hash for the different signatures because they can contain
        -- different hashtypes... (why isn't this documented anywhere?!)
        -- fuck all this overcomplicated shit
        
        -- this could be optimized since sighash is typically the same for all the signatures, but i don't care
        hashes <- forM hashtypes $ \sighash -> do

          let (singleIssue,tx'') = case _sigHashType sighash of
                     SigHashAll     -> ( False, tx' )
                     SigHashAllZero -> ( False, tx' )
                     SigHashNone    -> ( False, setSeqNoToZeroTxExcept inputidx $ tx' { _txOutputs = [] } )
                     SigHashSingle  -> if nouts' > inputidx
                                         then ( False, setSeqNoToZeroTxExcept inputidx $ tx' { _txOutputs = (replicate inputidx blankTxOutput) ++ [outs' !! inputidx] } )
                                         else ( True , setSeqNoToZeroTxExcept inputidx $ tx' { _txOutputs = (replicate nouts'   blankTxOutput) } )

          let tx''' = case _anyOneCanPay sighash of
                        False -> tx''
                        True  -> tx'' { _txInputs = [ ins' !! inputidx ] }

          let RawTx rawtx = serializeTx tx'''
              hash = if singleIssue
                then fromIntegerLE 1       -- this works this way because of a stupid bug in the reference implementation...
                else doHash256 (rawtx `B.append` (fromWord8List [encodeSigHash sighash,0,0,0]))

          return hash

        b <- worker mbPubKeys (zip hashes sigs)
        pushBool b

  where

    -- we have n pubkeys (some of them can be invalid...) and m signatures, n >= m
    worker :: [Maybe PubKey] -> [(Hash256,Signature)] -> ScriptMonad Bool
    worker = go where
      go _      []          = return True    -- there are no more signatures -> all of them matched
      go []     _           = return False   -- there are at least one signature but no more pubkey -> not all of them matched
      go (mbp:ps) (hs@(h,s):hss) = case mbp of
        Nothing -> go ps (hs:hss)
        Just p  -> case verifySignatureWithHash p s h of
                     True  -> go ps     hss
                     False -> go ps (hs:hss)

--------------------------------------------------------------------------------
-- helpers for CHECKSIG

blankTxOutput :: TxOutput RawScript
blankTxOutput = TxOutput (-1) (RawScript B.empty)

setSeqNoToZeroTxExcept :: Int -> Tx a b -> Tx a b
setSeqNoToZeroTxExcept idx tx = tx { _txInputs = setSeqNoToZeroListExcept idx (_txInputs tx) }

setSeqNoToZeroListExcept :: Int -> [TxInput a] -> [TxInput a]
setSeqNoToZeroListExcept idx = mapAccumL_ (\i txin -> (i+1, if i==idx then txin else txin { _txInSeqNo = 0 })) 0 

--------------------------------------------------------------------------------

mapAccumL_ :: (acc -> x -> (acc,y)) -> acc -> [x] -> [y]
mapAccumL_ f acc = snd . mapAccumL f acc

--------------------------------------------------------------------------------

