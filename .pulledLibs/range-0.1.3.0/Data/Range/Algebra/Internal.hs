{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Range.Algebra.Internal where

import Prelude hiding (const)

import Data.Range.Data
import Data.Range.RangeInternal

import Control.Monad.Free

data RangeExprF r
  = Invert r
  | Union r r
  | Intersection r r
  | Difference r r
  deriving (Num,Show,Eq,Ord,Functor)

newtype RangeExpr a = RangeExpr { getFree :: Free RangeExprF a }
  deriving (Num,Functor)


type Algebra f a = f a -> a

rangeMergeAlgebra :: (Ord a, Enum a) => Algebra RangeExprF (RangeMerge a)
rangeMergeAlgebra (Invert a) = invertRM a
rangeMergeAlgebra (Union a b) = a `unionRangeMerges` b
rangeMergeAlgebra (Intersection a b) = a `intersectionRangeMerges` b
rangeMergeAlgebra (Difference a b) = a `intersectionRangeMerges` invertRM b
