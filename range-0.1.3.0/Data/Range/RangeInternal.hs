{-# LANGUAGE Safe #-}

module Data.Range.RangeInternal where

import Data.Maybe (catMaybes)
--import Data.Ord (comparing)

import Data.Range.Data
import Data.Range.Spans
import Data.Range.Util

{-
 - The following assumptions must be maintained at the beginning of these internal
 - functions so that we can reason about what we are given.
 -
 - RangeMerge assumptions:
 - * The span ranges will never overlap the bounds. 
 - * The span ranges are always sorted in ascending order by the first element.
 - * The lower and upper bounds never overlap in such a way to make it an infinite range.
 -}
data RangeMerge a = RM
   { largestLowerBound :: Maybe a
   , largestUpperBound :: Maybe a
   , spanRanges :: [(a, a)]
   }
   | IRM
   deriving (Show, Eq)

emptyRangeMerge :: RangeMerge a
emptyRangeMerge = RM Nothing Nothing []

storeRange :: (Ord a) => Range a -> RangeMerge a
storeRange InfiniteRange = IRM
storeRange (LowerBoundRange lower) = emptyRangeMerge { largestLowerBound = Just lower }
storeRange (UpperBoundRange upper) = emptyRangeMerge { largestUpperBound = Just upper }
storeRange (SpanRange x y) = emptyRangeMerge { spanRanges = [(min x y, max x y)] }
storeRange (SingletonRange x) = emptyRangeMerge { spanRanges = [(x, x)] }

storeRanges :: (Ord a, Enum a) => RangeMerge a -> [Range a] -> RangeMerge a
storeRanges start ranges = foldr unionRangeMerges start (map storeRange ranges)

loadRanges :: (Ord a, Enum a) => [Range a] -> RangeMerge a
loadRanges = storeRanges emptyRangeMerge
{-# INLINE[0] loadRanges #-}

exportRangeMerge :: (Ord a, Enum a) => RangeMerge a -> [Range a]
exportRangeMerge IRM = [InfiniteRange]
exportRangeMerge rm = putAll rm
   where
      putAll IRM = [InfiniteRange]
      putAll (RM lb up spans) = 
         putLowerBound lb ++ putUpperBound up ++ putSpans spans

      putLowerBound = maybe [] (return . LowerBoundRange)
      putUpperBound = maybe [] (return . UpperBoundRange)
      putSpans = map simplifySpan

      simplifySpan (x, y) = if x == y
         then SingletonRange x
         else SpanRange x y

{-# RULES "load/export" [1] forall x. loadRanges (exportRangeMerge x) = x #-}

intersectSpansRM :: (Ord a) => RangeMerge a -> RangeMerge a -> RangeMerge a
intersectSpansRM one two = RM Nothing Nothing newSpans
   where
      newSpans = intersectSpans (spanRanges one) (spanRanges two) 

intersectWith :: (Ord a) => (a -> (a, a) -> Maybe (a, a)) -> Maybe a -> [(a, a)] -> [(a, a)]
intersectWith _ Nothing _ = []
intersectWith fix (Just lower) xs = catMaybes $ fmap (fix lower) xs

fixLower :: (Ord a) => a -> (a, a) -> Maybe (a, a)
fixLower lower (x, y) = if lower <= y
   then Just (max lower x, y)
   else Nothing

fixUpper :: (Ord a) => a -> (a, a) -> Maybe (a, a)
fixUpper upper (x, y) = if x <= upper
   then Just (x, min y upper)
   else Nothing

intersectionRangeMerges :: (Ord a, Enum a) => RangeMerge a -> RangeMerge a -> RangeMerge a
intersectionRangeMerges IRM two = two
intersectionRangeMerges one IRM = one
intersectionRangeMerges one two = RM
   { largestLowerBound = newLowerBound
   , largestUpperBound = newUpperBound
   , spanRanges = joinedSpans
   }
   where 
      lowerOneSpans = intersectWith fixLower (largestLowerBound one) (spanRanges two)
      lowerTwoSpans = intersectWith fixLower (largestLowerBound two) (spanRanges one)
      upperOneSpans = intersectWith fixUpper (largestUpperBound one) (spanRanges two)
      upperTwoSpans = intersectWith fixUpper (largestUpperBound two) (spanRanges one)
      intersectedSpans = intersectSpans (spanRanges one) (spanRanges two) 

      sortedResults = foldr1 insertionSortSpans 
         [ lowerOneSpans
         , lowerTwoSpans
         , upperOneSpans
         , upperTwoSpans
         , intersectedSpans
         , calculateBoundOverlap one two
         ]

      joinedSpans = joinSpans . unionSpans $ sortedResults

      newLowerBound = calculateNewBound largestLowerBound max one two
      newUpperBound = calculateNewBound largestUpperBound min one two

      calculateNewBound 
         :: (Ord a) 
         => (RangeMerge a -> Maybe a) 
         -> (a -> a -> a) 
         -> RangeMerge a -> RangeMerge a -> Maybe a
      calculateNewBound ext comp one two = case (ext one, ext two) of
         (Just x, Just y) -> Just $ comp x y
         (_, Nothing) -> Nothing
         (Nothing, _) -> Nothing

calculateBoundOverlap :: (Ord a, Enum a) => RangeMerge a -> RangeMerge a -> [(a, a)]
calculateBoundOverlap one two = catMaybes [oneWay, secondWay]
   where
      oneWay = case (largestLowerBound one, largestUpperBound two) of
         (Just x, Just y) -> if y >= x 
            then Just (x, y)
            else Nothing
         _ -> Nothing

      secondWay = case (largestLowerBound two, largestUpperBound one) of
         (Just x, Just y) -> if y >= x 
            then Just (x, y)
            else Nothing
         _ -> Nothing
      
unionRangeMerges :: (Ord a, Enum a) => RangeMerge a -> RangeMerge a -> RangeMerge a
unionRangeMerges IRM _ = IRM
unionRangeMerges _ IRM = IRM
unionRangeMerges one two = infiniteCheck filterTwo
   where
      filterOne = foldr filterLowerBound boundedRM joinedSpans
      filterTwo = foldr filterUpperBound (filterOne { spanRanges = [] }) (spanRanges filterOne)
      
      infiniteCheck :: (Ord a, Enum a) => RangeMerge a -> RangeMerge a
      infiniteCheck IRM = IRM
      infiniteCheck rm@(RM (Just x) (Just y) _) = if x <= succ y 
         then IRM
         else rm
      infiniteCheck rm = rm

      newLowerBound = calculateNewBound largestLowerBound min one two
      newUpperBound = calculateNewBound largestUpperBound max one two

      sortedSpans = insertionSortSpans (spanRanges one) (spanRanges two)
      joinedSpans = joinSpans . unionSpans $ sortedSpans

      boundedRM = RM
         { largestLowerBound = newLowerBound
         , largestUpperBound = newUpperBound
         , spanRanges = []
         }

      calculateNewBound 
         :: (Ord a) 
         => (RangeMerge a -> Maybe a) 
         -> (a -> a -> a) 
         -> RangeMerge a -> RangeMerge a -> Maybe a
      calculateNewBound ext comp one two = case (ext one, ext two) of
         (Just x, Just y) -> Just $ comp x y
         (z, Nothing) -> z
         (Nothing, z) -> z

filterLowerBound :: (Ord a, Enum a) => (a, a) -> RangeMerge a -> RangeMerge a
filterLowerBound _ IRM = IRM
filterLowerBound a rm@(RM Nothing _ _) = rm { spanRanges = a : spanRanges rm }
filterLowerBound s@(lower, _) rm@(RM (Just lowestBound) _ _) = 
   case boundCmp lowestBound s of
      GT -> rm { spanRanges = s : spanRanges rm }
      LT -> rm
      EQ -> rm { largestLowerBound = Just $ min lowestBound lower }

filterUpperBound :: (Ord a, Enum a) => (a, a) -> RangeMerge a -> RangeMerge a
filterUpperBound _ IRM = IRM
filterUpperBound a rm@(RM _ Nothing _) = rm { spanRanges = a : spanRanges rm }
filterUpperBound s@(_, upper) rm@(RM _ (Just upperBound) _) =
   case boundCmp upperBound s of
      LT -> rm { spanRanges = s : spanRanges rm }
      GT -> rm
      EQ -> rm { largestUpperBound = Just $ max upperBound upper }

boundCmp :: (Ord a, Enum a) => a -> (a, a) -> Ordering
boundCmp x (a, b) = if isBetween x (pred a, succ b)
   then EQ
   else if x < pred a then LT else GT

appendSpanRM :: (Ord a, Enum a) => (a, a) -> RangeMerge a -> RangeMerge a
appendSpanRM _ IRM = IRM
appendSpanRM sp@(lower, higher) rm = 
   if (newUpper, newLower) == (lub, llb) && isLower lower newLower && (Just higher) > newUpper
      then newRangesRM
         { spanRanges = sp : spanRanges rm
         }
      else newRangesRM
         { spanRanges = spanRanges rm
         }
   where
      newRangesRM = rm 
         { largestLowerBound = newLower
         , largestUpperBound = newUpper
         }

      isLower :: Ord a => a -> Maybe a -> Bool
      isLower _ Nothing = True
      isLower y (Just x) = y < x

      lub = largestUpperBound rm
      llb = largestLowerBound rm

      newLower = do
         bound <- llb
         return $ if bound <= higher
            then min bound lower
            else bound

      newUpper = do
         bound <- lub
         return $ if lower <= bound
            then max bound higher
            else bound

invertRM :: (Ord a, Enum a) => RangeMerge a -> RangeMerge a
invertRM IRM = emptyRangeMerge
invertRM (RM Nothing Nothing []) = IRM
invertRM (RM (Just lower) Nothing []) = RM Nothing (Just . pred $ lower) []
invertRM (RM Nothing (Just upper) []) = RM (Just . succ $ upper) Nothing []
invertRM (RM (Just lower) (Just upper) []) = RM Nothing Nothing [(succ upper, pred lower)]
invertRM rm = RM
   { largestUpperBound = newUpperBound
   , largestLowerBound = newLowerBound
   , spanRanges = upperSpan ++ betweenSpans ++ lowerSpan
   }
   where
      newLowerValue = succ . snd . last . spanRanges $ rm
      newUpperValue = pred . fst . head . spanRanges $ rm

      newUpperBound = case largestUpperBound rm of
         Just _ -> Nothing
         Nothing -> Just newUpperValue

      newLowerBound = case largestLowerBound rm of
         Just _ -> Nothing
         Nothing -> Just newLowerValue

      upperSpan = case largestUpperBound rm of
         Nothing -> []
         Just upper -> [(succ upper, newUpperValue)]
      lowerSpan = case largestLowerBound rm of
         Nothing -> []
         Just lower -> [(newLowerValue, pred lower)] 

      betweenSpans = invertSpans . spanRanges $ rm

{-
unionRange :: (Ord a) => Range a -> RangeMerge a -> RangeMerge a
unionRange InfiniteRange rm = IRM
unionRange (LowerBoundRange lower) rm = case largestLowerBound rm of
   Just currentLowest -> rm { largestLowerBound = Just $ min lower currentLowest }
   Nothing -> rm { largestLowerBound = Just lower }
-}

{-
intersectSpansRM :: (Ord a) => RangeMerge a -> (a, a) -> [(a, a)]
intersectSpansRM rm sp@(lower, upper) = intersectedSpans
   where 
      spans = spanRanges rm
      intersectedSpans = catMaybes $ map (intersectCompareSpan sp) spans

      largestSpan :: Ord a => [(a, a)] -> [(a, a)]
      largestSpan [] = []
      largestSpan xs = (foldr1 (\(l, m) (x, y) -> (min l x, max m y)) xs) : []

intersectCompareSpan :: Ord a => (a, a) -> (a, a) -> Maybe (a, a)
intersectCompareSpan f@(l, m) s@(x, y) = if isBetween l s || isBetween m s
   then Just (max l x, min m y)
   else Nothing
-}

-- If it was an infinite range then it should not be after an intersection unless it was
-- an intersection with another infinite range.
{-
intersectionRange :: (Ord a, Enum a) => Range a -> RangeMerge a -> RangeMerge a
intersectionRange InfiniteRange rm = rm -- Intersection with universe remains same
intersectionRange (LowerBoundRange lower) rm = rm
   { largestLowerBound = largestLowerBound rm >>= return . max lower
   , spanRanges = catMaybes . map (updateRange lower) . spanRanges $ rm
   }
   where
      updateRange :: (Ord a) => a -> (a, a) -> Maybe (a, a)
      updateRange lower (begin, end) = if lower <= end
         then Just (max lower begin, end)
         else Nothing
intersectionRange (UpperBoundRange upper) rm = rm
   { largestUpperBound = largestUpperBound rm >>= return . min upper
   , spanRanges = catMaybes . map (updateRange upper) . spanRanges $ rm
   }
   where
      updateRange :: (Ord a) => a -> (a, a) -> Maybe (a, a)
      updateRange upper (begin, end) = if begin <= upper
         then Just (begin, min upper end)
         else Nothing
intersectionRange (SpanRange lower upper) rm = rm
   -- update the bounds first and then update the spans, if the spans were sorted then
   { largestUpperBound = largestUpperBound rm >>= return . min upper
   , largestLowerBound = largestLowerBound rm >>= return . max lower
   -- they would be faster to update I suspect, lets start with not sorted
   , spanRanges = joinUnionSortSpans . ((lower, upper) :) . spanRanges $ rm
   }
   where
      joinUnionSortSpans :: (Ord a, Enum a) => [(a, a)] -> [(a, a)]
      joinUnionSortSpans = joinSpans . unionSpans . sortSpans

intersectionRange (SingletonRange value) rm = intersectionRange (SpanRange value value) rm
-}
