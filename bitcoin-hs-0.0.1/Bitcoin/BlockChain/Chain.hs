
-- | In-memory cache for blockchain metadata

{-# LANGUAGE BangPatterns #-}
module Bitcoin.BlockChain.Chain where

--------------------------------------------------------------------------------

import Data.Array

import Data.Word
import Data.Ord
import Data.List ( sort , group , unfoldr , maximumBy , foldl' )
import Data.Maybe

import Data.Set (Set) ; import qualified Data.Set as Set
import Data.Map (Map) ; import qualified Data.Map as Map

import Control.Monad

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L

import Data.Binary.Get
import Data.Binary.Put

import System.Mem ( performGC )

import Bitcoin.BlockChain.Base
import Bitcoin.BlockChain.Parser
import Bitcoin.BlockChain.Load
import Bitcoin.BlockChain.Checkpoint

import Bitcoin.Script.Base

import Bitcoin.Protocol.Hash
import Bitcoin.Protocol.Difficulty 

--------------------------------------------------------------------------------
-- * blockchain

-- | The block-chain as an (inverted) tree.
--
-- Note: The Ord instance compares the block hash (it defined only for internal reasons)
data Chain = Chain
  { _chainPrev      :: Maybe Chain                 -- ^ the previous block
  , _chainHeader    :: !BlockHeader                -- ^ the block header
  , _chainLocation  :: !BlockLocation              -- ^ the physical location on the harddisk where this block can be found
  , _chainHeight    :: {-# UNPACK #-} !Int         -- ^ the height of the block (the genesis block has height 0)
  , _chainTotalDiff :: {-# UNPACK #-} !Double      -- ^ the total summed difficulty on this branch
  }
  deriving (Eq,Show)

instance Ord Chain where
  compare ch1 ch2 = compare (chainBlockHash ch1) (chainBlockHash ch2)

chainBlockHash :: Chain -> Hash256
chainBlockHash = _blkBlockHash . _chainHeader

chainPrevHash :: Chain -> Hash256
chainPrevHash = _blkPrevBlock . _chainHeader

--------------------------------------------------------------------------------
-- * building the chain and related metadata

-- | BlockChain metadata
data ChainTable = ChainTable 
  { _tablePrev    :: !(Map Hash256 Hash256)      -- ^ lookup table for the previous block hash (this is somewhat redundant)
  , _tableNext    :: !(Map Hash256 [Hash256])    -- ^ lookup table for the next block(s) hash(es)
  , _tableBlock   :: !(Map Hash256 Chain)        -- ^ lookup table for the blocks
  , _tableHeight  :: !(Map Hash256 Int)          -- ^ reverse lookup table for the longest chain
  , _tableLongest :: !(Array Int Chain)          -- ^ the longest chain itself
  }
  deriving Show

buildChainTable :: IO ChainTable 
buildChainTable = blockDirectory >>= buildChainTable'

-- | The argument is the block directory
buildChainTable' :: FilePath -> IO ChainTable 
buildChainTable' dir = do

  fnames <- blockFiles dir
  -- print fnames

  pre_chains <- liftM concat $ forM fnames $ \fn -> do
    -- print fn 
    raw <- L.readFile fn
    let blockheaders = runGet getBlockHeadersOnly raw      
    -- print $ length blockheaders 
    return [ Chain Nothing hdr (BlockLocation fn pos) 0 0 | (!pos,!hdr) <- blockheaders ]

  let pre_table = Map.fromList
        [ c `seq` this `seq` (this, c) 
        | c <- pre_chains 
        , let this = chainBlockHash c
        ]

  let prevtable :: Map Hash256 Hash256
      prevtable = Map.fromList 
        [ prev `seq` this `seq` (this,prev)
        | chain <- pre_chains
        , let this = chainBlockHash chain
        , let prev = chainPrevHash  chain 
        ]

  let nexttable :: Map Hash256 [Hash256]
      nexttable = buildMap (:[]) (:) 
        [ this `seq` prev `seq` (prev,this) 
        | chain <- pre_chains
        , let this = chainBlockHash chain
        , let prev = chainPrevHash  chain 
        ]

  -- builds up the chain  
  let chainWorker :: Int -> Map Hash256 Chain -> [Chain] -> Map Hash256 Chain
      chainWorker !height !table ![] = table
      chainWorker !height !table !active = forceList_ active `seq` chainWorker height' table' active' where
        height' = height + 1
        table'  = foldl' add table active 
        active' = {- setNub $ -} concatMap h active
        h !c = let almosth = Map.findWithDefault [] (chainBlockHash c) nexttable :: [Hash256]
                   almostc = map (\h -> fromJust $ Map.lookup h pre_table) almosth
               in  map ( \d -> d { _chainHeight = height' 
                                 , _chainPrev   = Just c 
                                 , _chainTotalDiff = _chainTotalDiff c + decimalDifficulty (_blkDifficulty (_chainHeader d))
                                 } ) almostc
        add !old !chain = Map.insert (chainBlockHash chain) chain old

  let genesis :: Chain
      genesis = case Map.lookup zeroHash256 nexttable of 
        Just [h] -> if h == theGenesisBlock
                      then let c0 = fromJust $ Map.lookup h pre_table
                           in  c0 { _chainTotalDiff = decimalDifficulty (_blkDifficulty (_chainHeader c0)) }
                      else error "buildChainTables': genesis block hash does not match"
        Just []  -> error "buildChainTables'/genesis: shouldn't happen"
        Nothing  -> error "buildChainTables'/genesis: no genesis block candidate found"
        Just _   -> error "buildChainTables'/genesis: more than one genesis block candidate found"

  let fulltable = chainWorker 0 (Map.singleton (chainBlockHash genesis) genesis) [genesis]

  -- input: set of active heads
  -- output: the head of the longest chain 
  let findLongestChain :: [Chain] -> Chain
      findLongestChain active = 

        -- it seems to be important to force the list here, 
        -- otherwise stack overflow will happen...
        forceList_ active `seq` if all isLeft active'                        
          then maximumBy (comparing _chainTotalDiff) (map fromLeft active')
          else findLongestChain (concatMap ei active')

        where
          active' :: [Either Chain [Chain]]
          active' = map worker active

          worker :: Chain -> Either Chain [Chain]
          worker !ch = case Map.lookup (chainBlockHash ch) nexttable of
            Nothing -> Left ch    -- this is a "head", but it may be the longest, so we keep it
            Just [] -> error "buildChainTables'/longestChain: shouldn't happen"
            Just hs -> Right $ map (\h -> fromJust (Map.lookup h fulltable)) hs

          ei (Left  !x ) = [x]
          ei (Right !ys) = ys

          isLeft (Left _) = True
          isLeft _        = False

          fromLeft (Left !x) = x
          fromLeft _ = error "buildChainTables'/longestChain/fromLeft: Right"

  let longest_head = findLongestChain [genesis]
      longest_list = {- reverse -} (longest_head : unfoldr' f longest_head) where

        f !c = case _chainPrev c of
          Just !c' -> Just (c',c') 
          Nothing  -> Nothing

      longest_lkp = Map.fromList 
        [ height `seq` this `seq` (this,height)
        | c <- longest_list 
        , let this = chainBlockHash c
        , let height = _chainHeight c
        ]

      longest_arr = array (0, _chainHeight longest_head)  
        [ c `seq` height `seq` (height, c) 
        | c <- longest_list 
        , let height = _chainHeight c
        ]

{-    
  print longest_head
  print (head longest_list)
  print (last longest_list)
-}

  let final = (Map.size prevtable) `seq` 
              (Map.size nexttable) `seq` 
              (Map.size fulltable) `seq` 
              (Map.size longest_lkp) `seq` 
              longest_arr `seq` 
              ChainTable prevtable nexttable fulltable longest_lkp longest_arr

  final `seq` System.Mem.performGC `seq` (return final)

--------------------------------------------------------------------------------
-- * conveniance functions 

-- | Calls a user action for all blocks in the longest chain. The 'Int' is the block height
-- (with the genesis block having height zero).
forAllBlocks_ :: ChainTable -> (Int -> Block (Tx RawScript RawScript) -> IO ()) -> IO ()
forAllBlocks_ chTable userAction = do
  let (a,b) = bounds (_tableLongest chTable)
  forM_ [a..b] $ \(!blkIdx) -> do
    block <- loadBlockAt $! _chainLocation (_tableLongest chTable ! blkIdx)
    block `seq` userAction blkIdx block

forAllBlocks :: ChainTable -> (Int -> Block (Tx RawScript RawScript) -> IO a) -> IO [a]
forAllBlocks chTable userAction = do
  let (a,b) = bounds (_tableLongest chTable)
  forM [a..b] $ \(!blkIdx) -> do
    block <- loadBlockAt $! _chainLocation (_tableLongest chTable ! blkIdx)
    block `seq` userAction blkIdx block

--------------------------------------------------------------------------------
-- * misc helper functions (to be moved to some Misc.XXX modules)

buildMap :: Ord k => (b -> a) -> (b -> a -> a) -> [(k,b)] -> Map k a
buildMap f g xs = foldl worker Map.empty xs where
  worker old (k,y) = Map.alter h k old where
    h mb = case mb of
      Nothing -> Just (f y)
      Just x  -> Just (g y x)    

sortNub :: Ord a => [a] -> [a]
sortNub = map head . group . sort

setNub :: Ord a => [a] -> [a]
setNub xs = Set.toList (go Set.empty xs) where
  go !old []     = old
  go !old (x:xs) = go (Set.insert x old) xs

forceList_ :: [a] -> ()
forceList_ (x:xs) = x `seq` forceList_ xs
forceList_ [] = ()

-- strict unfold?
unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' f = go where
  go !b = case f b of
    Just (!a,!b') -> a : go b'
    Nothing       -> []

--------------------------------------------------------------------------------

{-
main = do
  stuff <- (blockDirectory >>= buildChainFull')
  mapM_ print stuff
-}
