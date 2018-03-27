
-- | Bifunctors and more. 
--
-- Used primarily for transactions, which are parametrized
-- in both input scripts and output scripts.

module Bitcoin.Misc.Bifunctor where

--------------------------------------------------------------------------------

class BiFunctor f where
  fmapFst  :: (a -> b) -> f a c -> f b c
  fmapSnd  :: (b -> c) -> f a b -> f a c
  fmapBoth :: (a -> c) -> (b -> d) -> f a b -> f c d

  fmapFst f = fmapBoth f id
  fmapSnd g = fmapBoth id g
  fmapBoth f g = fmapSnd g . fmapFst f 

--------------------------------------------------------------------------------

class BiFoldable f where
  bifoldl :: (a -> b -> a) -> (a -> c -> a) -> a -> f b c -> a
  bifoldr :: (b -> a -> a) -> (c -> a -> a) -> f b c -> a -> a

toListFst :: BiFoldable f => f a b -> [a]
toListFst what = bifoldr (:) (const id) what []

toListSnd :: BiFoldable f => f a b -> [b]
toListSnd what = bifoldr (const id) (:) what []

--------------------------------------------------------------------------------

-- | This is a rather nonstandard version of traverseable, but this is what we need
class BiTraversable f where
  mapAccumLFst  :: (acc -> b -> (acc,c)) -> acc -> f b d -> (acc, f c d)
  mapAccumLSnd  :: (acc -> c -> (acc,d)) -> acc -> f b c -> (acc, f b d)
  mapAccumLBoth :: (acc -> b -> (acc,d)) -> (acc -> c -> (acc,e)) -> acc -> f b c -> (acc, f d e)
 
  mapAccumLFst f = mapAccumLBoth f (,)
  mapAccumLSnd g = mapAccumLBoth (,) g
  mapAccumLBoth f g acc x = let (acc',y) = mapAccumLFst f acc  x 
                            in             mapAccumLSnd g acc' y

--------------------------------------------------------------------------------
  
mapAccumLFst_ :: BiTraversable f => (acc -> b -> (acc,c)) -> acc -> f b d -> f c d
mapAccumLFst_ f acc = snd . mapAccumLFst f acc

mapAccumLSnd_ :: BiTraversable f => (acc -> c -> (acc,d)) -> acc -> f b c -> f b d
mapAccumLSnd_ g acc = snd . mapAccumLSnd g acc

mapAccumLBoth_ :: BiTraversable f => (acc -> b -> (acc,d)) -> (acc -> c -> (acc,e)) -> acc -> f b c -> f d e
mapAccumLBoth_ f g acc = snd . mapAccumLBoth f g acc

--------------------------------------------------------------------------------

-- | Note: this is unsafe (the list must be long enough)
zipWithFst :: BiTraversable f => (x -> a -> b) -> [x] -> f a c -> f b c
zipWithFst f zs = mapAccumLFst_ (\(x:xs) a -> (xs, f x a)) zs

zipWithSnd :: BiTraversable f => (y -> b -> c) -> [y] -> f a b -> f a c
zipWithSnd g zs = mapAccumLSnd_ (\(y:ys) b -> (ys, g y b)) zs

--------------------------------------------------------------------------------
