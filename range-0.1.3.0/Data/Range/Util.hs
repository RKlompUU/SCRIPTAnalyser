{-# LANGUAGE Safe #-}

module Data.Range.Util where

-- This module is supposed to contain all of the functions that are required by the rest
-- of the code but could be easily pulled into separate and completely non-related
-- codebases or libraries.

insertionSort :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a] -> [a]
insertionSort comp xs ys = go xs ys
   where
      go (f : fs) (s : ss) = case comp f s of 
         LT -> f : go fs (s : ss)
         EQ -> f : s : go fs ss
         GT -> s : go (f : fs) ss
      go [] z = z
      go z [] = z

isBetween :: (Ord a) => a -> (a, a) -> Bool
isBetween a (x, y) = (x <= a) && (a <= y)

takeEvenly :: [a] -> [a] -> [a]
takeEvenly (a : as) (b : bs) = a : b : takeEvenly as bs
takeEvenly xs [] = xs
takeEvenly [] xs = xs
   
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)
