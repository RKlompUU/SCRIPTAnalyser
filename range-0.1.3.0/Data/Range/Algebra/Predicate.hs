module Data.Range.Algebra.Predicate where

import Data.Range.Algebra.Internal

predicateAlgebra :: Algebra RangeExprF (a -> Bool)
predicateAlgebra (Invert f) a = not (f a)
predicateAlgebra (Union f g) a = f a || g a
predicateAlgebra (Intersection f g) a = f a && g a
predicateAlgebra (Difference f g) a = f a && not (g a)
