
-- | Simple zipper data structure

module Bitcoin.Misc.Zipper where

--------------------------------------------------------------------------------

import Data.Maybe
import Control.Monad ( liftM )

--------------------------------------------------------------------------------

-- | A linear zipper, where the focus can be optionally after the last element (but not before the first element).
-- That is, the focus is the first element (if exists) of the second list.
data Zipper a = Zipper [a] [a] deriving Show

--------------------------------------------------------------------------------

focus :: Zipper a -> Maybe a
focus (Zipper ys xs) = case xs of
  (x:_) -> Just x
  _     -> Nothing

moveRight_ :: Zipper a -> Maybe (Zipper a)
moveRight_ = liftM snd . moveRight

-- | returns the focus /before/ moving right
moveRight :: Zipper a -> Maybe (a, Zipper a)
moveRight (Zipper ys xs) = case xs of
  (x:zs) -> Just (x , Zipper (x:ys) zs)
  _      -> Nothing

moveLeft_ :: Zipper a -> Maybe (Zipper a)
moveLeft_ = liftM snd . moveLeft

-- | returns the focus /after/ moving left
moveLeft :: Zipper a -> Maybe (a, Zipper a)
moveLeft (Zipper ys xs) = case ys of
  (y:zs) -> Just (y , Zipper xs (y:xs))
  _      -> Nothing

zipperFromList :: [a] -> Zipper a
zipperFromList xs = Zipper [] xs

zipperToList :: Zipper a -> [a]
zipperToList (Zipper ys xs) = reverse ys ++ xs

mkZipper :: [a] -> [a] -> Zipper a
mkZipper left right = Zipper (reverse left) right

-- | Head of the second list is the focus (if it is not empty)
zipperView :: Zipper a -> ([a],[a])
zipperView (Zipper ys xs) = (reverse ys, xs)

--------------------------------------------------------------------------------
