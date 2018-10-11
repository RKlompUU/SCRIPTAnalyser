{-# LANGUAGE Safe #-}

-- This module contains every function that purely performs operations on spans.
module Data.Range.Spans where

import Data.List (sortBy, insertBy)
import Data.Ord (comparing)

import Data.Range.Util
   
-- Assume that both inputs are sorted spans
insertionSortSpans :: (Ord a) => [(a, a)] -> [(a, a)] -> [(a, a)]
insertionSortSpans = insertionSort (comparing fst)

spanCmp :: Ord a => (a, a) -> (a, a) -> Ordering
spanCmp x@(xlow, xhigh) y@(ylow, _) = if isBetween xlow y || isBetween ylow x
   then EQ
   else if xhigh < ylow then LT else GT

intersectSpans :: (Ord a) => [(a, a)] -> [(a, a)] -> [(a, a)]
intersectSpans (x@(xlow, xup) : xs) (y@(ylow, yup) : ys) = 
   case spanCmp x y of
      EQ -> (max xlow ylow, min xup yup) : if xup < yup
         then intersectSpans xs (y : ys)
         else intersectSpans (x : xs) ys
      LT -> intersectSpans xs (y : ys)
      GT -> intersectSpans (x : xs) ys
intersectSpans _ _ = []

insertSpan :: Ord a => (a, b) -> [(a, b)] -> [(a, b)]
insertSpan = insertBy (comparing fst)

sortSpans :: (Ord a) => [(a, a)] -> [(a, a)]
sortSpans = sortBy (comparing fst)

-- Assume that you are given a sorted list of spans
joinSpans :: (Ord a, Enum a) => [(a, a)] -> [(a, a)]
joinSpans (f@(a, b) : s@(x, y) : xs) = 
   if succ b == x
      then joinSpans $ (a, y) : xs
      else f : joinSpans (s : xs)
joinSpans xs = xs

-- Assume that you are given a sorted list of spans
unionSpans :: Ord a => [(a, a)] -> [(a, a)]
unionSpans (f@(a, b) : s@(x, y) : xs) = if isBetween x f 
   then unionSpans ((a, max b y) : xs)
   else f : unionSpans (s : xs)
unionSpans xs = xs

-- Assume that you are given a sorted and joined list of spans
invertSpans :: (Ord a, Enum a) => [(a, a)] -> [(a, a)]
invertSpans ((_, x) : s@(y, _) : xs) = (succ x, pred y) : invertSpans (s : xs)
invertSpans _ = []

hasOverlaps :: (Ord a, Enum a) => [(a, a)] -> Bool
hasOverlaps xs = any isOverlapping (pairs xs)
   where
      isOverlapping ((x, y), (a, b)) = isBetween x (pred a, succ b) || isBetween a (pred x, succ y)
