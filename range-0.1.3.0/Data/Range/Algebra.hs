{-# LANGUAGE Safe #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.Range.Algebra
  ( RangeExpr
    -- ** Operations
  , const, invert, union, intersection, difference
    -- ** Evaluation
  , Algebra, RangeAlgebra(..)
  ) where

import Prelude hiding (const)

import Data.Range.Data
import Data.Range.Algebra.Internal
import Data.Range.Algebra.Range
import Data.Range.Algebra.Predicate

import Control.Monad.Free

const :: a -> RangeExpr a
const = RangeExpr . Pure

invert :: RangeExpr a -> RangeExpr a
invert = RangeExpr . Free . Invert . getFree

union :: RangeExpr a -> RangeExpr a -> RangeExpr a
union a b = RangeExpr . Free $ Union (getFree a) (getFree b)

intersection :: RangeExpr a -> RangeExpr a -> RangeExpr a
intersection a b = RangeExpr . Free $ Intersection (getFree a) (getFree b)

difference :: RangeExpr a -> RangeExpr a -> RangeExpr a
difference a b = RangeExpr . Free $ Difference (getFree a) (getFree b)

class RangeAlgebra a where
  eval :: Algebra RangeExpr a

instance (Ord a, Enum a) => RangeAlgebra [Range a] where
  eval = iter rangeAlgebra . getFree

instance RangeAlgebra (a -> Bool) where
  eval = iter predicateAlgebra . getFree
