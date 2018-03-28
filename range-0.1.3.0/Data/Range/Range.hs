{-# LANGUAGE Safe #-}

-- | This entire library is concerned with ranges and this module implements the absolute
-- basic range functions.
module Data.Range.Range (
      Range(..),
      inRange,
      inRanges,
      rangesOverlap,
      mergeRanges,
      invert,
      union,
      intersection,
      difference,
      fromRanges
   ) where

import Data.Range.Data
import Data.Range.Util
import qualified Data.Range.Algebra as Alg

-- | Performs a set union between the two input ranges and returns the resultant set of
-- ranges.
union :: (Ord a, Enum a) => [Range a] -> [Range a] -> [Range a]
union a b = Alg.eval $ Alg.union (Alg.const a) (Alg.const b)
{-# INLINE union #-}

-- | Performs a set intersection between the two input ranges and returns the resultant set of
-- ranges.
intersection :: (Ord a, Enum a) => [Range a] -> [Range a] -> [Range a]
intersection a b = Alg.eval $ Alg.intersection (Alg.const a) (Alg.const b)
{-# INLINE intersection #-}

-- | Performs a set difference between the two input ranges and returns the resultant set of
-- ranges.
difference :: (Ord a, Enum a) => [Range a] -> [Range a] -> [Range a]
difference a b = Alg.eval $ Alg.difference (Alg.const a) (Alg.const b)
{-# INLINE difference #-}

-- | An inversion function, given a set of ranges it returns the inverse set of ranges.
invert :: (Ord a, Enum a) => [Range a] -> [Range a]
invert = Alg.eval . Alg.invert . Alg.const
{-# INLINE invert #-}

-- | A check to see if two ranges overlap. If they do then true is returned; false
-- otherwise.
rangesOverlap :: (Ord a) => Range a -> Range a -> Bool
rangesOverlap (SingletonRange a) (SingletonRange b) = a == b
rangesOverlap (SingletonRange a) (SpanRange x y) = isBetween a (x, y)
rangesOverlap (SingletonRange a) (LowerBoundRange lower) = lower <= a
rangesOverlap (SingletonRange a) (UpperBoundRange upper) = a <= upper
rangesOverlap (SpanRange x y) (SpanRange a b) = isBetween x (a, b) || isBetween a (x, y)
rangesOverlap (SpanRange _ y) (LowerBoundRange lower) = lower <= y
rangesOverlap (SpanRange x _) (UpperBoundRange upper) = x <= upper
rangesOverlap (LowerBoundRange _) (LowerBoundRange _) = True
rangesOverlap (LowerBoundRange x) (UpperBoundRange y) = x <= y
rangesOverlap (UpperBoundRange _) (UpperBoundRange _) = True
rangesOverlap InfiniteRange _ = True
rangesOverlap a b = rangesOverlap b a

-- | Given a range and a value it will tell you wether or not the value is in the range.
-- Remember that all ranges are inclusive.
inRange :: (Ord a) => Range a -> a -> Bool
inRange (SingletonRange a) value = value == a
inRange (SpanRange x y) value = isBetween value (x, y)
inRange (LowerBoundRange lower) value = lower <= value
inRange (UpperBoundRange upper) value = value <= upper
inRange InfiniteRange _ = True

-- | Given a list of ranges this function tells you if a value is in any of those ranges.
-- This is especially useful for more complex ranges.
inRanges :: (Ord a) => [Range a] -> a -> Bool
inRanges rs a = any (`inRange` a) rs

-- | When you create a range there may be overlaps in your ranges. However, for the sake
-- of efficiency you probably want the list of ranges with no overlaps. The mergeRanges
-- function takes a set of ranges and returns the same set specified by the minimum number
-- of Range objects. A useful function for cleaning up your ranges. Please note that, if
-- you use any of the other operations on sets of ranges like invert, union and
-- intersection then this is automatically done for you. Which means that a function like
-- this is redundant: mergeRanges . intersection
mergeRanges :: (Ord a, Enum a) => [Range a] -> [Range a]
mergeRanges = Alg.eval . Alg.const
{-# INLINE mergeRanges #-}

-- | A set of ranges represents a collection of real values without actually instantiating
-- those values. This allows you to have infinite ranges. However, sometimes you wish to
-- actually get the values that your range represents, or even get a sample set of the
-- values. This function generates as many of the values that belong to your range as you
-- like.
fromRanges :: (Ord a, Enum a) => [Range a] -> [a]
fromRanges = concatMap fromRange
   where
      fromRange range = case range of
         SingletonRange x -> [x]
         SpanRange a b -> [a..b]
         LowerBoundRange x -> iterate succ x
         UpperBoundRange x -> iterate pred x
         InfiniteRange -> zero : takeEvenly (tail $ iterate succ zero) (tail $ iterate pred zero)
            where
               zero = toEnum 0
