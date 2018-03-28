{-# LANGUAGE Safe #-}

-- | Nested Ranges are common in practical usage. They appear in such forms as library
-- version numbers ("Version 1.4.5.6" for example). And it is very useful to be able to
-- compare these ranges to one another. This module exists for the purpose of allowing
-- these comparisons between nested ranges. The module builds upon the basic range concept
-- from other parts of this library.
module Data.Range.NestedRange where

import Data.Range.Range

-- | The Nested Range is a structure that in a nested form of many ranges where there can
-- be multiple ranges at every level.
data NestedRange a = NestedRange [[Range a]]


-- I wanted to know if a nested number of elements are in a given range. That way I can
-- just immediately run a single function and tell things about ranges.

-- | Given a list of nested values and a nested range tell us wether the nested value
-- exists inside the nested range.
inNestedRange :: Ord a => [a] -> NestedRange a -> Bool
inNestedRange values (NestedRange ranges) = go values ranges
   where
      go :: Ord a => [a] -> [[Range a]] -> Bool
      go [] [] = True -- If there is nothing left then they are equal
      go _  [] = True -- If you have already found the values you have to be in range then they are
      go [] _  = False -- If you have not fully matched it yet then it is not in range.
      go (value : vs) (range : rs) = inRanges range value && go vs rs
