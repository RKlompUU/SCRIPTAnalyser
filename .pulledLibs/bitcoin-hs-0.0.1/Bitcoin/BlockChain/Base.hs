
-- | BlockChain data structures

{-# LANGUAGE BangPatterns, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Bitcoin.BlockChain.Base 
  (
  -- * the block header 
    BlockHeader(..)
  -- * one-parameter blocks
  , Block(..)
  , BlockChain(..)
  -- * two-parameter blocks
  , Block2(..)
  , BlockChain2(..)
  , module Bitcoin.BlockChain.Tx
  ) where

--------------------------------------------------------------------------------

import Prelude

import Data.Int
import Data.Word
import Data.List ( mapAccumL )

import Control.Monad
import Control.Applicative

import Data.Foldable ( Foldable(..) )
import Data.Traversable ( Traversable(..) )

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L

import Data.Binary
import Data.Binary.Get

import Bitcoin.Misc.Bifunctor
import Bitcoin.Misc.HexString
import Bitcoin.Misc.UnixTime

import Bitcoin.Protocol.Hash

import Bitcoin.BlockChain.Tx

--------------------------------------------------------------------------------

newtype BlockChain tx = BlockChain [Block tx] deriving (Functor,Foldable,Traversable)

--------------------------------------------------------------------------------

data Block tx = Block
  { _blockHeader  :: !BlockHeader 
  , _blockTxs     :: [tx]  
  }
  deriving (Eq,Show,Functor,Foldable,Traversable)

--------------------------------------------------------------------------------

-- | The header of a block
data BlockHeader = BlockHeader
  { _blkBlockVersion :: {-# UNPACK #-} !Word32         -- ^ block version (currently 1 or 2)
  , _blkPrevBlock    :: {-# UNPACK #-} !Hash256        -- ^ hash of the previous block 
  , _blkMerkleRoot   :: {-# UNPACK #-} !Hash256        -- ^ merkle root of the transaction tree
  , _blkTimeStamp    :: {-# UNPACK #-} !UnixTimeStamp  -- ^ timestamp of the block
  , _blkDifficulty   :: {-# UNPACK #-} !Word32         -- ^ the difficulty (see "Bitcoin.Protocol.Difficulty")
  , _blkNonce        :: {-# UNPACK #-} !Word32         -- ^ the nonce
  , _blkBlockHash    :: Hash256                        -- ^ the hash of /this/ block. Note: the hash is not actually stored in the blockchain; but it is computed when the block is loaded
  }
  deriving (Eq,Show)

--------------------------------------------------------------------------------

-- | A two-parameter version of 'Block', with 'BiFunctor' etc instances. 
-- It is a newtype only so that we can provide these instances...
newtype Block2 inscript outscript = Block2 { unBlock2 :: Block (Tx inscript outscript) }

-- | A two-parameter version of 'BlockChain', with 'BiFunctor' etc instances.
newtype BlockChain2 inscript outscript = BlockChain2 { unBlockChain2 :: [Block2 inscript outscript] }

instance BiFunctor BlockChain2 where
  fmapFst  f   (BlockChain2 blocks) = BlockChain2 (map (fmapFst  f  ) blocks)
  fmapSnd    g (BlockChain2 blocks) = BlockChain2 (map (fmapSnd    g) blocks)
  fmapBoth f g (BlockChain2 blocks) = BlockChain2 (map (fmapBoth f g) blocks)

instance BiFoldable BlockChain2 where
  bifoldl f g x0 (BlockChain2 blocks)    = Prelude.foldl (bifoldl f g) x0 blocks
  bifoldr f g    (BlockChain2 blocks) x0 = Prelude.foldr (bifoldr f g) x0 blocks

instance BiFunctor Block2 where
  fmapFst  f   (Block2 blk) = Block2 $ fmap (fmapFst f   ) blk
  fmapSnd    g (Block2 blk) = Block2 $ fmap (fmapSnd    g) blk
  fmapBoth f g (Block2 blk) = Block2 $ fmap (fmapBoth f g) blk

instance BiFoldable Block2 where
  bifoldl f g x0 (Block2 blk)    = Data.Foldable.foldl (bifoldl f g) x0 blk
  bifoldr f g    (Block2 blk) x0 = Data.Foldable.foldr (bifoldr f g) x0 blk

--------------------------------------------------------------------------------
