
-- | Merkle trees with double SHA-256
-- 
-- See <https://en.bitcoin.it/wiki/Protocol_specification#Merkle_Trees>
--

module Bitcoin.Protocol.MerkleTree where

--------------------------------------------------------------------------------

import Control.Monad

import qualified Data.ByteString as B

import Bitcoin.Misc.HexString
import Bitcoin.Misc.OctetStream
import Bitcoin.Misc.Endian

import Bitcoin.Protocol.Hash

-- import Bitcoin.Hash.SHA256

--------------------------------------------------------------------------------

data MerkleTree hash
  = MerkleNode   hash (MerkleTree hash) (MerkleTree hash)
  | MerkleSingle hash (MerkleTree hash)
  | MerkleLeaf   hash
  deriving Show

-- | The root of the tree
merkleRoot :: MerkleTree hash -> hash
merkleRoot t = case t of
  MerkleNode   hash _ _ -> hash
  MerkleSingle hash _   -> hash
  MerkleLeaf   hash     -> hash  

--------------------------------------------------------------------------------

-- | Creates a Merkle tree from data
createMerkleTree :: OctetStream a => [a] -> MerkleTree Hash256
createMerkleTree = buildMerkleTree . map (MerkleLeaf . doHash256) where

-- | Builds a Merkle tree from data from a list of smaller Merkle trees
buildMerkleTree :: [MerkleTree Hash256] -> MerkleTree Hash256
buildMerkleTree = go where

  go xs = case xs of        
    []     -> error "createMerkleTree: empty input, shouldn't happen"
    [root] -> root
    _      -> go (map worker $ merklePairs xs)

  worker ei = case ei of
    Right (left,right) -> MerkleNode   (dhash left   right ) left right 
    Left  single       -> MerkleSingle (dhash single single) single

  dhash t1 t2 = doHash256 (B.append (toByteString $ merkleRoot t1) (toByteString $ merkleRoot t2))

--------------------------------------------------------------------------------

merklePairs :: [a] -> [Either a (a,a)]
merklePairs (x:y:rest) = Right (x,y) : merklePairs rest
merklePairs [x]        = [Left x]
merklePairs []         = []

--------------------------------------------------------------------------------

-- | Converts the tree into a format easier to visualize for humans
merklePyramid :: MerkleTree hash -> [[hash]]
merklePyramid = go where
  go t = case t of
    MerkleLeaf   hash            -> [[hash]]
    MerkleSingle hash single     ->  [hash] : go single
    MerkleNode   hash left right ->  [hash] : (zipWith (++) (go left) (go right))

{-
-- | For debugging (similar output to blockexporer, at least when using @BigEndian@)
viewMerkleTree :: Endian -> MerkleTree Hash256 -> IO ()
viewMerkleTree endian tree = do
  forM_ (reverse $ zip [0..] pyramid) $ \(i,list) -> do
    putStrLn "" 
    let indent = "" -- replicate (2*i) ' '
    forM_ list $ \hash -> putStrLn (indent ++ (toHexStringChars $ f $ toWord8List hash))
  where
    f = case endian of { LittleEndian -> id ; BigEndian -> reverse }
    pyramid = merklePyramid tree
-}

-- | For debugging (similar output to blockexporer)
viewMerkleTree :: Show hash => MerkleTree hash -> IO ()
viewMerkleTree tree = do
  forM_ (reverse $ zip [0..] pyramid) $ \(i,list) -> do
    putStrLn "" 
    let indent = "" -- replicate (2*i) ' '
    forM_ list $ \hash -> putStrLn (indent ++ show hash)
  where
    pyramid = merklePyramid tree

--------------------------------------------------------------------------------
