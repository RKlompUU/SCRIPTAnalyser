{-# LANGUAGE Safe #-}

-- | The Data module for common data types within the code.
module Data.Range.Data where

-- | The Range Data structure; it is capable of representing any type of range. This is
-- the primary data structure in this library. Everything should be possible to convert
-- back into this datatype. All ranges in this structure are inclusively bound.
data Range a
   = SingletonRange a      -- ^ Represents a single element as a range.
   | SpanRange a a         -- ^ Represents a bounded and inclusive range of elements.
   | LowerBoundRange a     -- ^ Represents a range with only an inclusive lower bound.
   | UpperBoundRange a     -- ^ Represents a range with only an inclusive upper bound.
   | InfiniteRange         -- ^ Represents an infinite range over all values.
   deriving(Eq, Show)

-- | These are the operations that can join two disjunct lists of ranges together.
data RangeOperation
   = RangeUnion         -- ^ Represents the set union operation.
   | RangeIntersection  -- ^ Represents the set intersection operation.
   | RangeDifference    -- ^ Represents the set difference operation.

-- | A Range Tree is a construct that can be built and then efficiently evaluated so that
-- you can compress an entire tree of operations on ranges into a single range quickly.
-- The only purpose of this tree is to allow efficient construction of range operations
-- that can be evaluated as is required.
data RangeTree a
   = RangeNode RangeOperation (RangeTree a) (RangeTree a) -- ^ Combine two range trees together with a single operation
   | RangeNodeInvert (RangeTree a) -- ^ Invert a range tree, this is a 'not' operation.
   | RangeLeaf [Range a] -- ^ A leaf with a set of ranges that are collected together.
