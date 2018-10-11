
-- | Monadic helper functions

module Bitcoin.Misc.Monad where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.State

--------------------------------------------------------------------------------

mapAccumM :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapAccumM act s0 xs 
  = liftM swap 
  $ runStateT (mapM (\x -> StateT (\s -> liftM swap $ act s x)) xs) s0 
  where
    swap (x,y) = (y,x)

mapAccumM_ :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m [c]
mapAccumM_ act s0 xs = liftM snd (mapAccumM act s0 xs)

flippedMapAccumM :: Monad m => a -> [b] -> (a -> b -> m (a, c)) -> m (a, [c])
flippedMapAccumM s0 xs act = mapAccumM act s0 xs

flippedMapAccumM_ :: Monad m => a -> [b] -> (a -> b -> m (a, c)) -> m [c]
flippedMapAccumM_ s0 xs act = mapAccumM_ act s0 xs

--------------------------------------------------------------------------------

