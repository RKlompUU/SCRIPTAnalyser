
-- | Parsing the blockchain (as stored by bitcoind in the @blkNNNNN.dat@ files)

{-# LANGUAGE CPP, BangPatterns #-}
module Bitcoin.BlockChain.Parser where

--------------------------------------------------------------------------------

import Data.Int
import Data.Word
import Data.Bits

import Control.Monad
import Control.Applicative

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L

import System.IO ( stderr , hPutStrLn )     -- for extreme warning messages
import System.IO.Unsafe as Unsafe

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Bitcoin.Misc.HexString
import Bitcoin.Misc.OctetStream
import Bitcoin.Misc.UnixTime

import Bitcoin.Protocol.Hash

import Bitcoin.BlockChain.Base
import Bitcoin.BlockChain.Tx
import Bitcoin.Script.Base 

--------------------------------------------------------------------------------
-- debugging

{- 

import Debug.Trace
import System.IO.Unsafe -- debugging

getTrace' :: (a -> String) -> Get a -> Get a 
getTrace' fmsg action = liftM f action where
  f x = x `seq` (trace (fmsg x) x)

getTrace :: String -> Get a -> Get a 
getTrace msg = getTrace' (const msg)

getDebug' :: Show b => (a -> b) -> Get a -> Get a
getDebug' f action = do
  pos <- bytesRead 
  let fmsg x = ("at position " ++ show pos ++ ": " ++ show (f x))
  getTrace' fmsg action

getDebug :: Show b => b -> Get a -> Get a
getDebug user = getDebug' (const user) 

debugPrefix :: Show a => String -> Get a -> Get a
debugPrefix prefix = getDebug' (\x -> prefix ++ " = " ++ show x)

-}

--------------------------------------------------------------------------------

-- | Computes the hash of a block
computeBlockHash :: BlockHeader -> Hash256
computeBlockHash hdr = doHash256 $ B.concat $ L.toChunks $ runPut (putBlockHeader hdr)

-- | Computes the hash of a transaction
computeTxHash :: Tx RawScript RawScript -> Hash256
computeTxHash tx = doHash256 $ B.concat $ L.toChunks $ runPut (putTx tx)

serializeTx :: Tx RawScript RawScript -> RawTx
serializeTx tx = RawTx $ B.concat $ L.toChunks $ runPut (putTx tx)

--------------------------------------------------------------------------------

-- | Returns @(Just x)@ if the input could be parsed in full. Input is a strict ByteString
runGetMaybeB :: Get a -> B.ByteString -> Maybe a
runGetMaybeB p b = runGetMaybeL p (L.fromChunks [b])

-- | Returns @(Just x)@ if the input could be parsed in full. Input is a lazy ByteString
runGetMaybeL :: Get a -> L.ByteString -> Maybe a
runGetMaybeL p b = case runGetOrFail p b of
  Right (remaining,ofs,y) | L.null remaining  -> Just y
  _ -> Nothing

--------------------------------------------------------------------------------

-- | Note: We copy the bytestring so that the stream can be garbage collected later
-- ("fromByteString" copies from the @ByteString@ to the @ForeignPtr@ - actually I think two copies...)
getHash160 :: Get Hash160
getHash160 = getByteString 20 >>= \bs -> return $ fromByteString bs

getHash256 :: Get Hash256
getHash256 = getByteString 32 >>= \bs -> return $ fromByteString bs

putHash160 :: Hash160 -> Put 
putHash160 h = putByteString $ toByteString h

putHash256 :: Hash256 -> Put 
putHash256 h = putByteString $ toByteString h

--------------------------------------------------------------------------------

getUnixTimeStamp :: Get UnixTimeStamp
getUnixTimeStamp = UnixTimeStamp <$> getWord32le

putUnixTimeStamp :: UnixTimeStamp -> Put
putUnixTimeStamp (UnixTimeStamp ts) = putWord32le ts

--------------------------------------------------------------------------------

getBlockHeader :: Get BlockHeader
getBlockHeader = do
  ver    <- getWord32le
  prev   <- getHash256
  merkle <- getHash256
  stamp  <- getUnixTimeStamp
  diff   <- getWord32le
  nonce  <- getWord32le
  let bhdr = BlockHeader ver prev merkle stamp diff nonce zeroHash256
  return $ bhdr { _blkBlockHash = computeBlockHash bhdr } 

putBlockHeader :: BlockHeader -> Put 
putBlockHeader (BlockHeader ver prev merkle stamp diff nonce _) = do
  putWord32le      ver
  putHash256       prev
  putHash256       merkle
  putUnixTimeStamp stamp
  putWord32le      diff
  putWord32le      nonce

--------------------------------------------------------------------------------

-- | The magic word in big-endian
theMagicWordBE :: Word32
#ifdef WITH_TESTNET
theMagicWordBE = 0x0B110907 
#else
theMagicWordBE = 0xf9beb4d9 
#endif

-- | The magic word in little-endian
theMagicWordLE :: Word32
#ifdef WITH_TESTNET
theMagicWordLE = 0x0709110b
#else
theMagicWordLE = 0xd9b4bef9
#endif

-- | Returns "Nothing" if there are not enough bytes left
getMaybeWord32be :: Get (Maybe Word32)
getMaybeWord32be = do
  isEmpty >>= \eof -> if eof then return Nothing else do
    a <- getWord8
    isEmpty >>= \eof -> if eof then return Nothing else do
      b <- getWord8
      isEmpty >>= \eof -> if eof then return Nothing else do
        c <- getWord8
        isEmpty >>= \eof -> if eof then return Nothing else do
          d <- getWord8
          return $ Just $ shiftL (fromIntegral a) 24
                        + shiftL (fromIntegral b) 16
                        + shiftL (fromIntegral c) 8
                        +        (fromIntegral d) 

--------------------------------------------------------------------------------

-- | returns the number of zero bytes which were skipped ('Left' if the input ends)
skipZeroBytes :: Get (Either Int Int)
skipZeroBytes = go 0 where
  go !n = do
    isEmpty >>= \e -> if e
      then return (Left n)
      else do
        w <- lookAhead getWord8
        case w of
          0 -> skip 1 >> go (n+1)
          _ -> return (Right n)

-- | returns the next found \"magic bytes\" (which may be invalid) and their position, unless the file ends.
nextMagicBytes :: Get (Maybe (Word32,Word64))
nextMagicBytes = do
  ei <- skipZeroBytes                   -- sometimes (often) bitcoind puts zeros between blocks
  case ei of
    Left  _ -> return Nothing           -- end of file
    Right _ -> do                       -- not end of file
      pos   <- fromIntegral <$> bytesRead
      magic <- getWord32be
      return $ Just (magic,pos)

isValidMagic :: Word32 -> Bool
isValidMagic magic = 
  case magic of
#ifdef WITH_TESTNET
    0x0B110907 -> True
#else
    0xf9beb4d9 -> True
#endif
    _          -> False

--------------------------------------------------------------------------------

unsafeGetChunk :: Get (Maybe (Word64, L.ByteString))
unsafeGetChunk = do
  ei <- saferGetChunk 
  case ei of
    Left  badmagic -> fail "BlockParser/unsafeGetChunk: invalid magic bytes"
    Right mb       -> return mb

-- | Unfortunately, it can happen in practice that the chunk length is completely wrong... 
-- (or maybe simply the blockchain data is corrupted?)
--
-- In that case we have to parse the block to find the correct size (because the next block will start within this block...)
--
-- But normally we don't want to always parse the block when it is unnecessary... 
saferGetChunk :: Get (Either Word32 (Maybe (Word64,L.ByteString)))
saferGetChunk = do
  mbmagic <- nextMagicBytes
  case mbmagic of
    Nothing            -> return (Right Nothing)
    Just (!magic,!pos) -> case isValidMagic magic of
      False -> return (Left magic)
      True  -> do
        len  <- getWord32le
        !lbs <- getLazyByteString (fromIntegral len)
        return $! Right $! Just $! (pos,lbs)

--------------------------------------------------------------------------------

getVarInt :: Get Word64
getVarInt = do
  h <- getWord8
  case h of
    0xfd -> fromIntegral <$> getWord16le 
    0xfe -> fromIntegral <$> getWord32le 
    0xff ->                  getWord64le 
    _    -> return (fromIntegral h)

putVarInt :: Word64 -> Put
putVarInt w
  | w <= 0xfc       =                  putWord8    (fromIntegral w)
  | w <= 0xffff     = putWord8 0xfd >> putWord16le (fromIntegral w)
  | w <= 0xffffffff = putWord8 0xfe >> putWord32le (fromIntegral w)
  | otherwise       = putWord8 0xff >> putWord64le (fromIntegral w)

-- | Note: we copy the bytestring so that the stream can be garbage collected later
getVarString :: Get B.ByteString
getVarString = do
  l  <- getVarInt  
  bs <- getByteString (fromIntegral l)
  return (B.copy bs)    

putVarString :: B.ByteString -> Put
putVarString bs = do
  putVarInt (fromIntegral $ B.length bs)
  putByteString bs

--------------------------------------------------------------------------------

-- | Parses a lot of something, until the input ends
getMany :: Get (Maybe a) -> Get [a]
getMany getOne = go where
  go = do
    empty <- isEmpty
    if empty 
      then return []
      else do
        mbx  <- getOne
        case mbx of
          Nothing -> return []
          Just x -> do 
            xs <- x `seq` go
            return (x:xs) 

forceList :: [a] -> [a]
forceList (x:xs) = x `seq` (x : forceList xs)
forceList [] = []

--------------------------------------------------------------------------------

getTx_ :: Get (Tx RawScript RawScript)
getTx_ = fst <$> getTx
    
getTx :: Get (Tx RawScript RawScript, RawTx)
getTx = do
  pos <- bytesRead 
  (siz,tx0)  <- lookAhead $ do
    ver  <- getWord32le
    nIn  <- getVarInt
    ins  <- replicateM (fromIntegral nIn) getTxInput
    nOut <- getVarInt
    outs <- replicateM (fromIntegral nOut) getTxOutput
    locktime <- getWord32le
    let tx0 = Tx ver (forceList ins) (forceList outs) (parseLockTime locktime) zeroHash256
    pos2 <- bytesRead 
    return (pos2-pos, tx0)
  rawtx <- getByteString (fromIntegral siz)
  let hash = doHash256 rawtx
  let tx   = tx0 { _txHash = hash }
  return (tx, RawTx rawtx)

getTxInput :: Get (TxInput RawScript)
getTxInput = do
  prevHash <- getHash256
  prevIdx  <- getWord32le
  script   <- getVarString
  seqno    <- getWord32le
  return (TxInput prevHash prevIdx (RawScript script) seqno)

getTxOutput :: Get (TxOutput RawScript)
getTxOutput = do
  value  <- fromIntegral <$> getWord64le
  script <- getVarString  
  return (TxOutput value (RawScript script))

--------------------------------------------------------------------------------

putTx :: Tx RawScript RawScript -> Put
putTx (Tx ver ins outs locktime _) = do
  putWord32le ver
  putVarInt (fromIntegral $ length ins )
  forM_ ins putTxInput
  putVarInt (fromIntegral $ length outs)
  forM_ outs putTxOutput
  putWord32le (marshalLockTime locktime)

putTxInput :: TxInput RawScript -> Put
putTxInput (TxInput prevHash prevIdx (RawScript script) seqno) = do
  putHash256   prevHash
  putWord32le  prevIdx 
  putVarString script
  putWord32le  seqno

putTxOutput :: TxOutput RawScript -> Put
putTxOutput (TxOutput value (RawScript script)) = do
  putWord64le  (fromIntegral value)
  putVarString script

--------------------------------------------------------------------------------

warn :: String -> a -> a
warn msg next = this `seq` next where
  this = Unsafe.unsafePerformIO $ hPutStrLn stderr ("warning: " ++ msg)

-- | Returns the position, the block size (which usually equals the chunk size, /but not always/ unfortunately, which
-- complicates the parsing considerably... though now it seems its simply corruption of the block data) and the block
-- itself
--
getBlock :: Get (Maybe (Word64, Int, Block (Tx RawScript RawScript)))
getBlock = do
  -- mbchunk <- getChunk      
  mbchunk <- lookAhead unsafeGetChunk       -- bitcoin blockchain can contain errorneous chunk lenghts :((((
  case mbchunk of
    Nothing          -> return Nothing 
    Just (pos,chunk) -> do
      let (size,pos,block) = flip runGet chunk $ do
            header <- getBlockHeader
            ntxs   <- getVarInt
            txs    <- header `seq` (replicateM (fromIntegral ntxs) getTx_)
            size   <- (fromIntegral <$> bytesRead) :: Get Int
            return (size,pos,Block header txs)
      skip (8+size)                                   -- sometimes the chunk size is wrong... 8 is magic + chunk length
      if (size == fromIntegral (L.length chunk)) 
        then return $ Just $ (pos,size,block)
        else warn "chunk size does not equals block size" $ 
               return $ Just $ (pos,size,block)

getBlocks :: Get [(Word64, Block (Tx RawScript RawScript))]
getBlocks = getMany $ do
  mb <- getBlock
  return $ case mb of
    Nothing              -> Nothing
    Just (pos,siz,block) -> Just (pos,block)

--------------------------------------------------------------------------------

parseBlockHeader :: L.ByteString -> BlockHeader
parseBlockHeader chunk = flip runGet chunk $ getBlockHeader

-- | This parses the next block header, and checks the magic bytes after the chunk.
--
-- In the case they are invalid, we also parse the full block, and consume the block,
-- /not the chunk/, since the chunk size can be invalid in same cases... 
-- (though not it seems that instead simply the blockchain data was really corrupted, but
-- how can bitcoind survive that?)
--
getBlockHeaderOnly :: Get (Maybe (Word64, BlockHeader))
getBlockHeaderOnly = do

  mbchunk <- lookAhead $ unsafeGetChunk    
  case mbchunk of
    Nothing -> return Nothing
    Just (!pos,!chunk) -> do
      let !header   = parseBlockHeader chunk
          !chunksiz = fromIntegral (L.length chunk) :: Int
          skipchunk = skip $! (8+chunksiz)
      mbmagic <- lookAhead (skipchunk >> nextMagicBytes)
      header `seq` pos `seq` case mbmagic of
        Nothing            -> skipchunk >> (return $! Just (pos,header))
        Just (nextmagic,_) -> case isValidMagic nextmagic of
          True  -> skipchunk >> (return $! Just (pos,header))
          False -> warn "bad chunk size" $ do
            -- invalid chunk length, parse the block
            mbblock <- getBlock
            case mbblock of
              Nothing                -> return Nothing
              Just (!pos,siz,!block) -> return $! Just $! (pos, _blockHeader block)

getBlockHeadersOnly :: Get [(Word64,BlockHeader)]
getBlockHeadersOnly = getMany getBlockHeaderOnly  
                
--------------------------------------------------------------------------------

{-
test :: IO ()
test = do
  raw <- L.readFile "C:/Users/bkomuves/Application Data/Bitcoin/blocks/blk00022.dat"

  let blocks = runGet getBlocks raw  
  print (length blocks)
  print $ last $ blocks
-}
