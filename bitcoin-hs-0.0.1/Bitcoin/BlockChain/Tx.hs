
-- | Transaction data structures

{-# LANGUAGE BangPatterns, DeriveFunctor, DeriveFoldable #-}
module Bitcoin.BlockChain.Tx where

--------------------------------------------------------------------------------

import Prelude

import Data.Int
import Data.Word
import Data.List ( mapAccumL , foldl' )
import Data.Maybe

import Control.Monad
import Control.Applicative
import Data.Foldable ( Foldable(..) )

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L

import Data.Binary
import Data.Binary.Get

import Bitcoin.Misc.Bifunctor
import Bitcoin.Misc.HexString
import Bitcoin.Misc.UnixTime

import Bitcoin.Protocol.Hash

--------------------------------------------------------------------------------

-- | \"Raw\" transaction (basically a ByteString)
newtype RawTx = RawTx { unRawTx :: B.ByteString } deriving Eq

instance Show RawTx where
  showsPrec d (RawTx rs) = showParen (d>10) $ showString "RawTx " . shows (toHexStringChars rs)

--------------------------------------------------------------------------------

-- | Lock time
data LockTime
  = LockImmed                                -- ^ immediate (0x00000000)
  | LockBlock {-# UNPACK #-} !Int            -- ^ not until block #
  | LockTime  {-# UNPACK #-} !UnixTimeStamp  -- ^ not before
  deriving (Eq,Show)

parseLockTime :: Word32 -> LockTime
parseLockTime !w
  | w == 0         = LockImmed
  | w < 500000000  = LockBlock (fromIntegral w)       -- note: 500000000 is on 1985/11/04
  | otherwise      = LockTime (UnixTimeStamp w)

marshalLockTime :: LockTime -> Word32
marshalLockTime lt = case lt of
  LockImmed                   -> 0
  LockBlock n                 -> fromIntegral n
  LockTime  (UnixTimeStamp w) -> w

--------------------------------------------------------------------------------

-- | Transactions, parametrized by the script types
data Tx inscript outscript = Tx
  { _txVersion  :: {-# UNPACK #-} !Word32
  , _txInputs   :: [TxInput  inscript ]
  , _txOutputs  :: [TxOutput outscript]
  , _txLockTime :: !LockTime
  , _txHash     :: Hash256      -- ^ note: the hash is not actually stored in the blockchain; but it is computed when the transaction is loaded
  }
  deriving (Eq,Show)

--------------------------------------------------------------------------------

instance BiFunctor Tx where
  fmapFst  f   (Tx ver ins outs lock hash) = Tx ver (map (fmap f) ins) outs lock hash
  fmapSnd    g (Tx ver ins outs lock hash) = Tx ver ins (map (fmap g) outs) lock hash
  fmapBoth f g (Tx ver ins outs lock hash) = Tx ver (map (fmap f) ins) (map (fmap g) outs) lock hash

instance BiFoldable Tx where
  bifoldl f g x0 (Tx ver ins outs lock hash)    = Prelude.foldl g (Prelude.foldl f x0 (map _txInScript  ins )) (map _txOutScript outs)
  bifoldr f g    (Tx ver ins outs lock hash) x0 = Prelude.foldr f (Prelude.foldr g x0 (map _txOutScript outs)) (map _txInScript  ins )

instance BiTraversable Tx where
  mapAccumLFst f acc (Tx ver ins outs lock hash) = (acc', Tx ver ins' outs lock hash) where
    (acc', ins' ) = mapAccumL h acc ins
    h a txin  = let (a',y) = f a (_txInScript  txin ) 
                in  (a', txin  { _txInScript  = y })

  mapAccumLSnd g acc (Tx ver ins outs lock hash) = (acc', Tx ver ins outs' lock hash) where
    (acc', outs') = mapAccumL h  acc outs
    h a txout = let (a',y) = g a (_txOutScript txout) 
                in  (a', txout { _txOutScript = y })

--------------------------------------------------------------------------------

data TxInput inscript = TxInput 
  { _txInPrevOutHash :: {-# UNPACK #-} !Hash256       -- ^ hash of the previous transaction
  , _txInPrevOutIdx  :: {-# UNPACK #-} !Word32        -- ^ which output in that transaction
  , _txInScript      :: !inscript                     -- ^ signature script
  , _txInSeqNo       :: {-# UNPACK #-} !Word32        -- ^ sequence number is normally 0xffffffff, but only matters if locktime is nonzero; vica versa if all TxIn have 0xffffffff, then locktime doesn't matter
  }
  deriving (Eq,Show,Functor)

data TxOutput outscript = TxOutput
  { _txOutValue   :: {-# UNPACK #-} !Int64            -- ^ output amount (in satoshis)
  , _txOutScript  :: !outscript                       -- ^ public key script
  } 
  deriving (Eq,Show,Functor)
    
--------------------------------------------------------------------------------

-- | Computes the transaction fee, in satoshis. 
--
-- We return an 'Integer' so that
-- this can be used to check the validity of a transaction. See CVE-2010-5139 vulnerability:
-- <https://en.bitcoin.it/wiki/Common_Vulnerabilities_and_Exposures#CVE-2010-5139>
--
txFee :: Tx (Tx a b) c -> Integer
txFee txExt = totalInput - totalOutput where

  totalOutput = sum' [ fromIntegral (_txOutValue txout) | txout <- _txOutputs txExt ]
  totalInput  = sum' [ inputValue txin | txin <- _txInputs txExt ]

  inputValue :: TxInput (Tx a b) -> Integer
  inputValue txin = fromIntegral
                  $ _txOutValue ( (_txOutputs $ _txInScript txin) !! (fromIntegral $ _txInPrevOutIdx txin) ) 

  sum' :: [Integer] -> Integer
  sum' = Data.List.foldl' (+) 0

--------------------------------------------------------------------------------

-- | We recognize coinbase transactions.
isCoinBaseTx :: Tx a b -> Bool
isCoinBaseTx = isJust . isCoinBaseTx'

-- | We recognize coinbase transactions, and return the coinbase data.
isCoinBaseTx' :: Tx a b -> Maybe a
isCoinBaseTx' tx = case _txInputs tx of 
  [inp] -> if _txInPrevOutHash inp == zeroHash256 
             then Just (_txInScript inp)
             else Nothing 
  _     -> Nothing
  
--------------------------------------------------------------------------------
