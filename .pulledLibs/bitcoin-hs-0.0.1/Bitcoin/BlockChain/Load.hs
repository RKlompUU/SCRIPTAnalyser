
-- | Load the blockchain from the files downloaded by the Satoshi client (bitcoin-qt)

{-# LANGUAGE CPP #-}
module Bitcoin.BlockChain.Load where

--------------------------------------------------------------------------------

import Data.Char ( isDigit )
import Data.List ( sort )

import Control.Monad
import Control.Applicative ( (<$>) )

import System.FilePath
import System.Directory
import System.IO

import Foreign
import Foreign.Marshal
import Foreign.Storable

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L

import Data.Binary.Get
import Data.Binary.Put

import Bitcoin.Misc.Endian

import Bitcoin.BlockChain.Base
import Bitcoin.BlockChain.Parser

import Bitcoin.Script.Base

import Bitcoin.Protocol.Hash

--------------------------------------------------------------------------------

-- | A file and a position within the file, pointing to the magic bytes of a block
data BlockLocation = BlockLocation
  { _blockFile    :: FilePath
  , _blockFilePos :: !Word64
  } 
  deriving (Eq,Ord,Show)           -- ord is needed for BiMap

--------------------------------------------------------------------------------
-- * where to find the blocks

-- | Guess where the blocks are on the harddisk (as downloaded by the Satoshi client)
blockDirectory :: IO FilePath
blockDirectory = do

#ifdef linux_HOST_OS
  let appname = "bitcoin"
#else
  let appname = "Bitcoin"
#endif

  appdir <- getAppUserDataDirectory appname
#ifdef WITH_TESTNET
  return (appdir </> "testnet3" </> "blocks")
#else
  return (appdir </> "blocks")
#endif

--------------------------------------------------------------------------------

-- | Given the directory containing the blocks (blk000xx.dat), we return the
-- list of the block files (full paths)
--
-- For example on windows the directory is
-- 
-- > C:/Users/<username>/Application Data/Bitcoin/blocks/
--
blockFiles :: FilePath -> IO [FilePath]
blockFiles dir = 
  do
    files <- getDirectoryContents dir
    return $ map prepend $ sort $ filter cond files
  where
    prepend fn = dir </> fn
    cond fn = and 
        [ take 3 base == "blk" 
        , ext == ".dat" 
        , length base == 8 
        , all isDigit (drop 3 base) 
        ]
      where
        (base,ext) = splitExtension fn

--------------------------------------------------------------------------------
-- * load block headers

-- | Loads all block headers lazily.
--
-- Note: this will also load the out-of-longest-chain blocks' headers
--
loadAllHeaders_ :: IO [BlockHeader]
loadAllHeaders_ = map snd <$> loadAllHeaders

-- | Loads all block headers lazily.
loadAllHeaders :: IO [(BlockLocation, BlockHeader)]
loadAllHeaders = blockDirectory >>= loadAllHeaders'

-- | The argument is the block directory
loadAllHeaders' :: FilePath -> IO [(BlockLocation, BlockHeader)]
loadAllHeaders' dir = do
  fnames <- blockFiles dir
  hdrs <- forM fnames $ \fn -> do
    raw <- L.readFile fn
    let blockheaders = runGet getBlockHeadersOnly raw  
        f posblh = (BlockLocation fn (fst posblh), snd posblh)
    return $ map f blockheaders
  return (concat hdrs)   

--------------------------------------------------------------------------------
-- * load blocks

-- | Lazily loads all blocks. Note 1: all blocks won't fit the memory, so you 
-- must process this lazy stream immediately and let the GC free the old blocks.
--
-- Note 2: this will also load the out-of-longest-chain blocks
--
loadAllBlocks_ :: IO [Block (Tx RawScript RawScript)]
loadAllBlocks_ = map snd <$> loadAllBlocks 

-- | This version also returns the block file and the position within the file
loadAllBlocks :: IO [(BlockLocation, Block (Tx RawScript RawScript))]
loadAllBlocks = blockDirectory >>= loadAllBlocks'

-- | The argument is the block directory
loadAllBlocks' :: FilePath -> IO [(BlockLocation, Block (Tx RawScript RawScript))]
loadAllBlocks' dir = do
  fnames <- blockFiles dir
  blks <- forM fnames $ \fn -> do
    raw <- L.readFile fn
    let blocks = runGet getBlocks raw  
        f posbl = (BlockLocation fn (fst posbl), snd posbl)
    return $ map f blocks
  return (concat blks)   

--------------------------------------------------------------------------------

-- | Tries to load a block from a file at the given position
loadBlockAt :: BlockLocation -> IO (Block (Tx RawScript RawScript))
loadBlockAt (BlockLocation fpath fpos) = do 
  withBinaryFile fpath ReadMode $ \h -> do
    hSeek h AbsoluteSeek (fromIntegral fpos)
    alloca $ \pmagic -> alloca $ \pblocklen -> do
      hGetBuf h pmagic    4
      hGetBuf h pblocklen 4
      magic    <- (swapByteOrderToLE <$> peek pmagic   ) :: IO Word32
      blocklen <- (swapByteOrderToLE <$> peek pblocklen) :: IO Word32
      if magic /= theMagicWordLE
        then error "loadBlockAt: magic word does not match"
        else do
          chunk <- L.hGet h (fromIntegral blocklen)
          return $ flip runGet chunk $ do
            header <- getBlockHeader
            ntxs   <- getVarInt
            txs    <- header `seq` (replicateM (fromIntegral ntxs) getTx_)
            return (Block header txs)

--------------------------------------------------------------------------------
-- * load transactions

-- | Lazily loads all transactions. Note 1: these won't fit the memory, so you 
-- must process this lazy stream immediately and let the GC free the old blocks.
--
-- Note 2: this will also load the out-of-longest-chain blocks
--
loadAllTxs_ :: IO [Tx RawScript RawScript]
loadAllTxs_ = concatMap snd <$> loadAllTxs

-- | With this function, the list transactions are partitioned by blocks,
-- and the file and file position containing the block is also returned
loadAllTxs :: IO [(BlockLocation, [Tx RawScript RawScript])]
loadAllTxs = blockDirectory >>= loadAllTxs'

-- | The argument is the block directory
loadAllTxs' :: FilePath -> IO [(BlockLocation, [Tx RawScript RawScript])]
loadAllTxs' dir = map f <$> loadAllBlocks' dir where
  f (blockloc, block) = (blockloc, _blockTxs block)

--------------------------------------------------------------------------------
