
-- | Common quickcheck stuff

{-# LANGUAGE CPP, BangPatterns #-}
module Bitcoin.Test.Misc.QuickCheck where

--------------------------------------------------------------------------------

import Test.QuickCheck

--------------------------------------------------------------------------------

-- | An integer in the interval @[0,2^1024]@
newtype BigInt = BigInt Integer deriving (Eq,Ord,Show)                                     

instance Arbitrary BigInt where
  arbitrary = do
    n <- choose (0,(2^(1024::Int)))
    return $ BigInt n

--------------------------------------------------------------------------------

newtype MediumInt  = MediumInt  Integer deriving (Eq,Ord,Show)

-- | An integer in the interval @[0,2^256-1]@
instance Arbitrary MediumInt where
  arbitrary = do
    n <- choose (0,(2^(256::Int)-1))
    return $ MediumInt n

--------------------------------------------------------------------------------

-- | An integer in the interval @[0,32)@ (closed-open, so 32 is excluded)
newtype SmallInt32 = SmallInt32 Int deriving (Eq,Ord,Show)

instance Arbitrary SmallInt32 where
  arbitrary = do
    k <- choose (0,31)
    return $ SmallInt32 k

--------------------------------------------------------------------------------

newtype SmallExpo = SmallExpo Int deriving (Eq,Show)

instance Arbitrary SmallExpo where
  arbitrary = do
    n <- choose (0,350)
    return $ SmallExpo n

--------------------------------------------------------------------------------
