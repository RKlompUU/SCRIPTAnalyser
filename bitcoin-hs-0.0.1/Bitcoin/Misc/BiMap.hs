
-- | Bidirectional maps (bijections)

{-# LANGUAGE BangPatterns #-}
module Bitcoin.Misc.BiMap where

--------------------------------------------------------------------------------

import Data.List ( foldl' )

import qualified Data.Map as Map
import Data.Map (Map) 

--------------------------------------------------------------------------------

data BiMap a b = BiMap
  { _forwardMap  :: !(Map a b)
  , _backwardMap :: !(Map b a)
  }

--------------------------------------------------------------------------------

empty :: (Ord a, Ord b) => BiMap a b
empty = BiMap Map.empty Map.empty

toList :: (Ord a, Ord b) => BiMap a b -> [(a,b)]
toList (BiMap fwd bwd) = Map.toList fwd

toListRev :: (Ord a, Ord b) => BiMap a b -> [(b,a)]
toListRev (BiMap fwd bwd) = Map.toList bwd

fromList :: (Ord a, Ord b) => [(a,b)] -> BiMap a b 
fromList xys = foldl' f empty xys where
  f !old (!x,!y) = insert x y old

--------------------------------------------------------------------------------

lookup :: (Ord a) => a -> BiMap a b -> Maybe b
lookup !x (BiMap !fwd !bwd) = Map.lookup x fwd

lookupRev :: (Ord b) => b -> BiMap a b -> Maybe a
lookupRev !y (BiMap !fwd !bwd) = Map.lookup y bwd

-- | In case there is a (partial) conflict with the existing 'BiMap', the conflicting
-- pair is removed and the new pair is inserted.
insert :: (Ord a, Ord b) => a -> b -> BiMap a b -> BiMap a b
insert !x !y (BiMap !fwd !bwd) = 
  case Map.lookup x fwd of
    Nothing -> case Map.lookup y bwd of 
      Nothing -> BiMap (Map.insert x y                 fwd) (Map.insert y x                 bwd)
      Just x' -> BiMap (Map.insert x y $ Map.delete x' fwd) (Map.insert y x                 bwd)
    Just y' -> case Map.lookup y bwd of 
      Nothing -> BiMap (Map.insert x y                 fwd) (Map.insert y x $ Map.delete y' bwd)
      Just x' -> BiMap (Map.insert x y $ Map.delete x' fwd) (Map.insert y x $ Map.delete y' bwd)

delete :: (Ord a, Ord b) => a -> BiMap a b -> BiMap a b
delete !x old@(BiMap !fwd !bwd) = 
  case Map.lookup x fwd of
    Nothing -> old
    Just y  -> BiMap (Map.delete x fwd) (Map.delete y bwd)

deleteRev :: (Ord a, Ord b) => b -> BiMap a b -> BiMap a b
deleteRev !y old@(BiMap !fwd !bwd) = 
  case Map.lookup y bwd of
    Nothing -> old
    Just x  -> BiMap (Map.delete x fwd) (Map.delete y bwd)

--------------------------------------------------------------------------------

