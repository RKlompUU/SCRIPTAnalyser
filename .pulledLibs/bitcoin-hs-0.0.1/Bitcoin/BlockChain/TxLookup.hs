
-- | An simple but relatively compact data structure for looking up transactions in the blockchain
-- (TODO: replace these data structures by a better one...)
--
-- Blockchain stats (as of 2013): 
--
-- * block heigh currently ~270,000, so 20 bits for that is enough for a while, 23 enough for ever basically.
--
-- * number of transaction is currently ~27,000,000 (2016 data: ~162 million)
--
-- * average number of transactions per block is currently ~300-350 (2016: ~2000)
--
-- Idea: index into a large dense array by the first few (say 24 or 26) bits of the hash, 
-- then store a list (vector) of the possible block indices there. With 24 bit index there will be in 
-- average 2 blocks per tx, of course sometimes more sometimes less.
--
-- An IOArray -> 16M pointers, on 32 bit that is 64M
-- 30 million entries tx-s => 30 million entries, for short lists approx 1 word/entry, so let's say 150-200M on 32 bit.
--
-- On 64 bit we have more memory so it's ok :)
--
-- 2016 update: you will need more memory to store even 32 bits per transaction...
-- The most compact estimation already gives 1+ gigs...
--

--

{-# LANGUAGE UnboxedTuples, MagicHash, BangPatterns #-}
module Bitcoin.BlockChain.TxLookup where

--------------------------------------------------------------------------------

import Control.Monad

import Data.Bits
import Data.Word
import Data.List ( find )
import Data.Maybe

import Foreign
import System.IO.Unsafe as Unsafe

import GHC.Prim
import GHC.Exts
import GHC.Int

import Data.Array
import Data.Array.IO
import Data.Array.MArray
import Data.Array.Unsafe

import Control.DeepSeq
-- import GHC.DataSize

import Bitcoin.Protocol.Hash

import Bitcoin.BlockChain.Base
import Bitcoin.BlockChain.Load
import Bitcoin.BlockChain.Chain
import Bitcoin.BlockChain.Tx

import Bitcoin.Script.Base
import Bitcoin.Script.Run ( checkTransaction )

--------------------------------------------------------------------------------
-- * interal stuff

{-

newtype Word24 = Word24 { fromWord24 :: Word32 } deriving (Eq,Ord,Show)

instance Storable Word24 where
  alignment _ = 1
  sizeOf _    = 3
  peek ptr = do
    lo <- peek (castPtr  ptr              :: Ptr Word16)
    hi <- peek (castPtr (ptr `plusPtr` 2) :: Ptr Word8 )              -- little endian
    return $ Word24 $ fromIntegral lo + shiftL (fromIntegral hi) 16
  poke ptr (Word24 w) = do
    let lo = fromIntegral (w .&. 0xffff) :: Word16
        hi = fromIntegral (shiftR w 16)  :: Word8
    poke (castPtr  ptr              :: Ptr Word16) lo
    poke (castPtr (ptr `plusPtr` 2) :: Ptr Word8 ) hi              -- little endian

highestBit :: Word24 -> Bool
highestBit (Word24 w) = (w .&. 0x800000) > 0

lowerBits :: Word24 -> Int
lowerBits (Word24 w) = fromIntegral (w .&. 0x7fffff)

minus1 :: Word24
minus1 = Word24 0xffffff

-}

--------------------------------------------------------------------------------

toWord# :: Int -> Word#
toWord# !(I# i) = int2Word# i

fromWord# :: Word# -> Int
fromWord# w = I# (word2Int# w)

-- | A list which is more compact for a small number of elements
data CompactList
  = NilList
  | OneList !Word#
  | TwoList !Word# !Word#
  | ThrList !Word# !Word# !Word#
  | FouList !Word# !Word# !Word# !Word#
  | GenList !IndexList

instance NFData CompactList where
  rnf cl = case cl of
    NilList  -> ()
    OneList _ -> ()
    TwoList _ _ -> ()
    ThrList _ _ _ -> ()
    FouList _ _ _ _ -> ()
    GenList il -> rnf il

toCompactList :: [Int] -> CompactList
toCompactList xs = case xs of
  []        -> NilList
  [x]       -> OneList (toWord# x)
  [x,y]     -> TwoList (toWord# x) (toWord# y)
  [x,y,z]   -> ThrList (toWord# x) (toWord# y) (toWord# z)
  [x,y,z,w] -> FouList (toWord# x) (toWord# y) (toWord# z) (toWord# w)
  _         -> GenList (toIndexList xs)

fromCompactList :: CompactList -> [Int]
fromCompactList cl = case cl of
  NilList         -> []
  OneList u       -> [ fromWord# u ]
  TwoList u v     -> [ fromWord# u , fromWord# v ]
  ThrList u v w   -> [ fromWord# u , fromWord# v , fromWord# w ]
  FouList u v w z -> [ fromWord# u , fromWord# v , fromWord# w , fromWord# z ]
  GenList il      -> fromIndexList il

consCompactList :: Int -> CompactList -> CompactList 
consCompactList x rest = case rest of
    NilList       -> OneList (toWord# x) 
    OneList y     -> TwoList (toWord# x) y  
    TwoList y z   -> ThrList (toWord# x) y z 
    ThrList y z w -> FouList (toWord# x) y z w
    FouList {}    -> GenList $ toIndexList $ (x:) $ fromCompactList rest
    GenList il    -> GenList $ consIndexList x il

--------------------------------------------------------------------------------

-- | Hack for a more compact list structure. Last element of the list has the highest bit set to 1.
-- Empty list has all bits set to 1. Space consumption is 3 words per list entry instead of 5 for normal lists.
-- 
-- Note: compiling with -O1 seeems to create larger memory consumption than -O0 and -O2 ?!?!
data IndexList = IndexList !Word# IndexList   

instance NFData IndexList where
  rnf (IndexList !w rest) = (if not (isNullIndexList rest) then rnf rest else ()) `seq` ()

intToBool# :: Int# -> Bool
intToBool# i = case i of 
  0# -> False
  _  -> True

nullIndexList :: IndexList
nullIndexList = IndexList (int2Word# 0xffffffff#) nullIndexList

isNullIndexList :: IndexList -> Bool
isNullIndexList (IndexList w _) = intToBool# (w `eqWord#` (int2Word# 0xffffffff#))

isSingletonIndexList :: IndexList -> Bool
isSingletonIndexList (IndexList w _) = not $ intToBool# ((w `and#` (int2Word# 0x80000000#)) `eqWord#` (int2Word# 0#))

--------------------------------------------------------------------------------

consIndexList :: Int -> IndexList -> IndexList
consIndexList x rest = 
  if isNullIndexList rest
    then IndexList (w0 `or#` (int2Word# 0x80000000#)) rest
    else IndexList  w0                                rest
  where
    !(W# w0) = fromIntegral x

toIndexList :: [Int] -> IndexList
toIndexList = go where
  go (x : xs) = IndexList w (go xs) where
                   !w = (w0 `or#` (if null xs then (int2Word# 0x80000000#) else (int2Word# 0#)))
                   !(W# w0) = fromIntegral x
  go []       = nullIndexList

fromIndexList :: IndexList -> [Int]
fromIndexList il@(IndexList !w rest) = 
  if isNullIndexList il 
    then []
    else go il 
  where 
   go (IndexList w rest) = if intToBool# ((w `and#` (int2Word# 0x80000000#)) `eqWord#` (int2Word# 0#))
     then (fromIntegral $ W# (w                               )) : go rest
     else (fromIntegral $ W# (w `and#` (int2Word# 0x7fffffff#))) : []     

--------------------------------------------------------------------------------

newtype TxLookup = TxLookup (IOArray Word32 CompactList)

newEmptyTxLookup :: IO TxLookup
newEmptyTxLookup = do
  arr <- Data.Array.MArray.newArray (0,0xffffff) NilList
  return $ TxLookup arr

insertIntoTxLookup' :: Word32 -> Int -> TxLookup -> IO ()
insertIntoTxLookup' !key_ !value (TxLookup !arr) = do
  let !key = key_ .&. 0x00ffffff    -- 24 bits
  !old <- readArray arr key
  unless (elem value $ fromCompactList old) $ writeArray arr key $! consCompactList value old
  return () 
  
txLookupList' :: Word32 -> TxLookup -> IO [Int]
txLookupList' key_ (TxLookup arr) = do
  let key = key_ .&. 0x00ffffff    -- 24 bits
  list <- readArray arr key
  return $ fromCompactList list

--------------------------------------------------------------------------------

{- ForeignPtr version
first24Bits :: Hash256 -> Word32
first24Bits hash = Unsafe.unsafePerformIO (first24BitsIO hash)

-- | Depends on the endianness, but it's used for a memory only structure anyway
first24BitsIO :: Hash256 -> IO Word32
first24BitsIO (Hash256 fptr) = do
  w32 <- (withForeignPtr fptr $ \ptr -> peek (castPtr ptr :: Ptr Word32))
  return (w32 .&. 0xffffff)
-}

first24Bits :: Hash256 -> Word32
first24Bits (Hash256 !w1 _ _ _) = fromIntegral (w1 .&. 0xffffff)

--------------------------------------------------------------------------------

insertIntoTxLookup :: Hash256 -> Int -> TxLookup -> IO ()
insertIntoTxLookup !hash !blockidx !table = insertIntoTxLookup' (first24Bits hash) blockidx table 

txLookupList :: Hash256 -> TxLookup -> IO [Int]
txLookupList !hash !table = txLookupList' (first24Bits hash) table

--------------------------------------------------------------------------------
-- * Build and use the 'TxLookup' table

-- | Builds a 'TxLookup' table
buildTxLookupTable :: ChainTable -> IO TxLookup
buildTxLookupTable chTable = do
  txlkptable <- newEmptyTxLookup

  let (a,b) = bounds (_tableLongest chTable)
  forM_ [a..b] $ \(!blkIdx) -> do
    block <- loadBlockAt $! _chainLocation (_tableLongest chTable ! blkIdx)

    -- when (mod blkIdx 500 == 0) $ print blkIdx   -- hPrint stderr blkIdx

    let txs = _blockTxs block
    forM_ txs $ \(!tx) -> insertIntoTxLookup (_txHash tx) blkIdx txlkptable

  return txlkptable

-- | Looks up a transaction by hash. First we check the 'TxLookup' table, then we load the possible blocks (if any),
-- parse them and check them for the given transaction. We also return the block height.
txLookup :: ChainTable -> TxLookup -> Hash256 -> IO (Maybe (Int, Tx RawScript RawScript))
txLookup chTable txTable hash = do
  possible <- txLookupList hash txTable 
  results <- forM possible $ \blkIdx -> do
    let loc = _chainLocation (_tableLongest chTable ! blkIdx)
    block <- loadBlockAt loc
    let txs = _blockTxs block
    return $ case find (\tx -> hash == _txHash tx) txs of
      Nothing -> Nothing
      Just tx -> Just (blkIdx,tx)
  case catMaybes results of
    []    -> return $ Nothing
    [!tx] -> return $ Just tx 
    _     -> error "txLookup: fatal error, multiple transactions found in with the same hash"

-- | does not return the block height
txLookup_ :: ChainTable -> TxLookup -> Hash256 -> IO (Maybe (Tx RawScript RawScript))
txLookup_ cht txt = liftM (liftM snd) . txLookup cht txt 

--------------------------------------------------------------------------------

-- | Given a transaction, we load all the previous transactions from the disk (using the cache),
-- and add them the Tx structure (abusing the parametrized script field). The result can
-- then be fed to 'checkTransaction'.
--
loadPrevTxs :: ChainTable -> TxLookup -> Tx a b -> IO (Tx (Tx RawScript RawScript, a) b)
loadPrevTxs chTable txTable oldTx = 
  do
    newInputs <- mapM worker (_txInputs oldTx)
    return $! oldTx { _txInputs = newInputs } 
  where  
    lkp = txLookup_ chTable txTable 
    worker inp@(TxInput prevhash previdx script seqno) = do
      mbprevtx <- lkp prevhash 
      let !prevtx = case mbprevtx of
            Just tx -> tx
            Nothing -> error $ "loadPrevTxs: fatal error: previous tx not found; hash = " ++ show prevhash
      return $! inp { _txInScript = (prevtx,script) }      

--------------------------------------------------------------------------------

-- | Checks a transaction. Note: this automatically accepts coinbase transactions 
-- (does not check that that sum of reward and fees is the amount, since it has no access to the fees)
--
checkTx :: ChainTable -> TxLookup -> Tx RawScript RawScript -> IO (Either String Bool)
checkTx chTable txTable tx =
  case (isCoinBaseTx tx) of
    True  -> return (Right True)
    False -> do    
      txExt <- loadPrevTxs chTable txTable tx 
      return $ checkTransaction txExt 

-- | Checks a transaction given by its hash
checkTxByHash :: ChainTable -> TxLookup -> Hash256 -> IO (Either String Bool)
checkTxByHash chTable txTable txid = do
  mbtx <- txLookup_ chTable txTable txid
  case mbtx of
    Nothing -> error $ "checkTxByHash: tx not found (hash = " ++ show txid ++ ")"
    Just tx -> checkTx chTable txTable tx

--------------------------------------------------------------------------------

{- 

testCompactList = and
  [ and [ (fromCompactList $                      toCompactList [1..n] ) ==     [1..n] | n<-[0..77] ]
  , and [ (fromCompactList $ consCompactList 666 (toCompactList [1..n])) == 666:[1..n] | n<-[0..77] ]
  ]

main = do
  forM_ [0..(10::Int)] $ \n -> do
    let xs = [(1::Int)..n] 
        il = toIndexList   xs :: IndexList
        cl = toCompactList xs :: CompactList
        b1 = xs == fromIndexList   il
        b2 = xs == fromCompactList cl
    k1 <- recursiveSize $!! xs
    k2 <- recursiveSize $!! il
    k3 <- recursiveSize $!! cl
    print ( (length xs , k1,k2,k3, b1 , b2) :: (Int,Int,Int,Int,Bool,Bool) )

-}  
