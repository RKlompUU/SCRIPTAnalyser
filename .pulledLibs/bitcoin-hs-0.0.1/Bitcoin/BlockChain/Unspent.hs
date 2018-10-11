
-- | Build a table of unspent transactions outputs (the transactions are assumed to be valid!)

{-# LANGUAGE CPP, BangPatterns #-}
module Bitcoin.BlockChain.Unspent where

--------------------------------------------------------------------------------

import Control.Monad

import Data.Array
import Data.Maybe
import Data.Word
import Data.Int

import Data.Set (Set) ; import qualified Data.Set as Set
import Data.Map (Map) ; import qualified Data.Map as Map

import Data.IORef

import Bitcoin.Protocol

import Bitcoin.BlockChain.Base
import Bitcoin.BlockChain.Load
import Bitcoin.BlockChain.Tx
import Bitcoin.BlockChain.Cache
import Bitcoin.BlockChain.Chain
import Bitcoin.BlockChain.TxLookup

import Bitcoin.Script.Base

--------------------------------------------------------------------------------

-- | An unspent transaction output
data UnspentOutput = UnspentOutput
  { _unspentTxId    :: !Hash256                 -- ^ the transaction id
  , _unspentTxOut   :: {-# UNPACK #-} !Word32   -- ^ which output of that transaction 
  }
  deriving (Eq,Ord,Show)

{-
data Unspent
  , _unspentAmount  :: !Amount               -- ^ the amount of bitcoins unspent
  , _unspentAddress :: !(Maybe Address)      -- ^ if it is on output to an address, we also store the address
  } 
-}

--------------------------------------------------------------------------------

-- | Builds a table of unspent transactions outputs. Note that all transactions are assumed to be valid!
-- So you have to check them before, to be safe.
--
buildUnspentTable :: ChainTable -> TxLookup -> IO (Map UnspentOutput Amount)
buildUnspentTable chTable txTable = do

  let txLkp :: Hash256 -> IO (Maybe (Tx RawScript RawScript))
      txLkp = txLookup_ chTable txTable 

  let (a,b) = bounds (_tableLongest chTable)

  unspent <- newIORef Map.empty
  forM_ [a..b] $ \(!blkIdx) -> do
    block <- loadBlockAt $! _chainLocation (_tableLongest chTable ! blkIdx)

    -- As far as I know, it is required that a transaction which spends another transaction
    -- in the same block comes later within the block, thus this is safe to do:

    forM_ (_blockTxs block) $ \(!tx) -> do
      let !txhash = _txHash tx      
      forM_ (_txInputs  tx) $ \(TxInput !prevhash !prevout _ _) -> 
        myModifyIORef' unspent $ Map.delete (UnspentOutput prevhash prevout)
      forM_ (zip [0..] (_txOutputs tx)) $ \(outidx, TxOutput !amount _) -> 
        myModifyIORef' unspent $ Map.insert (UnspentOutput txhash   outidx ) (Amount $ fromIntegral amount)

  readIORef unspent

  where
    -- it is only present in newer versions of the base library...
    myModifyIORef' :: IORef a -> (a -> a) -> IO ()
    myModifyIORef' ref f = do
      !old <- readIORef ref
      writeIORef ref $! (f old)

--------------------------------------------------------------------------------
