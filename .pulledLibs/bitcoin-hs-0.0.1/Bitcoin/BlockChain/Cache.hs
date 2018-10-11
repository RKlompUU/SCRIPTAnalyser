
-- | Caching recently used blocks

{-# LANGUAGE BangPatterns #-}
module Bitcoin.BlockChain.Cache where

--------------------------------------------------------------------------------

import Control.Concurrent
import System.IO.Unsafe as Unsafe

import Data.IntMap        (IntMap) ; import qualified Data.IntMap        as IntMap
import Bitcoin.Misc.BiMap (BiMap ) ; import qualified Bitcoin.Misc.BiMap as BiMap

import Bitcoin.BlockChain.Base
import Bitcoin.BlockChain.Tx
import Bitcoin.BlockChain.Load

import Bitcoin.Script.Base

--------------------------------------------------------------------------------

data BlockCache script = BlockCache
  { _blkMap :: !(IntMap (Block (Tx script script)))
  , _locMap :: !(BiMap BlockLocation Int)
  }

-- | How many blocks we cache (128 at the moment)
theBlockCacheSize :: Int
theBlockCacheSize = 128

-- | if the largest key (approx equals the number of lookups) reaches this limit, we compactify
theBlockCacheCompactLimit :: Int
theBlockCacheCompactLimit = 1024 -- 2^30-1

-- | The global block cache
theBlockCache :: MVar (BlockCache RawScript)
theBlockCache = Unsafe.unsafePerformIO $ newMVar (BlockCache IntMap.empty BiMap.empty)

--------------------------------------------------------------------------------

compactTheBlockCache :: IO ()
compactTheBlockCache = do
  BlockCache blks locs <- takeMVar theBlockCache
  let oldlocs    = BiMap.toList locs
  let locs'      = BiMap.fromList  $ zipWith (\(loc,j) i -> (loc,i)) oldlocs [1..]
      translate  = IntMap.fromList $ zipWith (\(loc,j) i -> (j  ,i)) oldlocs [1..]
      f (!j,blk) = case IntMap.lookup j translate of 
                     Just !i -> (i,blk)
                     Nothing -> error "compactTheBlockCache: fatal error, shouldn't happen"
      blks'     = IntMap.fromList $ map f $ IntMap.toList blks
  putMVar theBlockCache $! (BlockCache blks' locs')

--------------------------------------------------------------------------------

loadBlockCached :: BlockLocation -> IO (Block (Tx RawScript RawScript))
loadBlockCached location = do

  orig@(BlockCache blks locs) <- takeMVar theBlockCache
  let n    = IntMap.size blks                                -- size
      lmax = if n==0 then 0 else fst (IntMap.findMax blks)   -- largest key
      l'   = lmax+1

  if n >= theBlockCacheCompactLimit 

    -- we have to compactify...
    then do 
      putMVar theBlockCache $! orig
      compactTheBlockCache
      loadBlockCached location

    else case BiMap.lookup location locs of

      -- it was in the cache
      Just i -> case IntMap.lookup i blks of
        Just block -> do

          putStrLn $ "block at " ++ show location ++ " was in the cache at pos " ++ show i ++ " (max = " ++ show lmax ++ " ; size = " ++ show n ++ ")"

          let blks' = IntMap.insert l' block $ IntMap.delete i $ blks
              locs' = BiMap.insert location l' locs
          putMVar theBlockCache $! (BlockCache blks' locs')
          return block
        Nothing    -> do
          let locs' = BiMap.delete location locs
          putMVar theBlockCache $! (BlockCache blks locs')         -- delete the location from the map (it was invalid anyway)
          loadBlockCached location                                 -- and try again! now it will load and put it into the map

      -- it was not in the cache
      Nothing -> do

        putStrLn $ "block at " ++ show location ++ " was not in the cache"

        block <- loadBlockAt location
        if n < theBlockCacheSize
          then do
            let blks' = IntMap.insert l' block blks
                locs' = BiMap.insert location l' locs
            putMVar theBlockCache $! BlockCache blks' locs'
            return block
          else
            case IntMap.minViewWithKey blks of
              Nothing -> error "loadBlockCached: fatal error, shouldn't happen; #1"
              Just ((i,_),rest) -> do
                let blks' = IntMap.insert l' block rest
                    locs' = BiMap.insert location l' $ BiMap.deleteRev i $ locs
                putMVar theBlockCache $! (BlockCache blks' locs')         -- delete the location from the map (it was invalid anyway) 
                return block

--------------------------------------------------------------------------------

