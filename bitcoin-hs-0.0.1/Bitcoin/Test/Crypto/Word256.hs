
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Bitcoin.Test.Crypto.Word256 where

--------------------------------------------------------------------------------

import Data.Word
import Data.Bits

-- import Bitcoin.Misc.BigInt

import Test.Tasty
import Test.Tasty.QuickCheck 
import Test.QuickCheck ( Arbitrary(..) , choose , quickCheckWith , stdArgs , maxSuccess , Testable )

import Bitcoin.Test.Misc.QuickCheck
import Bitcoin.Crypto.Word256

--------------------------------------------------------------------------------

testgroup_Word256 :: TestTree
testgroup_Word256 = testGroup "Word256"
  [ testProperty "conversion /1a"         prop_convert1
  , testProperty "conversion /1b"         prop_convert1b
  , testProperty "conversion /2"          prop_convert2
  , testProperty "roll . unroll == id"    prop_unroll_roll
  , testProperty "addition"               prop_add 
  , testProperty "subtraction"            prop_sub
  , testProperty "negation is involution" prop_doubleneg
  , testProperty "multiplication"         prop_mul 
  , testProperty "scaling by 32 bit int"  prop_scale
  , testProperty "ordering"               prop_cmp
  , testProperty "is even"                prop_even 
  , testProperty "is odd"                 prop_odd
  , testProperty "shift left by 32 bits"  prop_shiftl256_fullword 
  , testProperty "shift left <= 31 bits"  prop_shiftl256_small 
  , testProperty "shift right >= 32 bits" prop_shiftr256_fullword 
  , testProperty "shift right by 32 bits" prop_shiftr256_small 
  ]

--------------------------------------------------------------------------------
-- * quickcheck

modN :: Integer -> Integer
modN k = mod k twoToThe256

instance Arbitrary Word256 where
  arbitrary = do
    n <- choose (0,twoToThe256-1)
    return $ toWord256 n

--------------------------------------------------------------------------------

{-
runAllTests_word256 :: IO ()
runAllTests_word256 = runAllTests_word256' 1000

runAllTests_word256' :: Int -> IO ()
runAllTests_word256' n = do
  let args = stdArgs { maxSuccess = n }
  let qc :: Testable prop => prop -> IO ()
      qc = quickCheckWith args
 
  putStrLn "running all tests in Bitcoin.Crypto.Word256"
  putStrLn "==========================================="
  qc prop_convert1
  qc prop_convert1b
  qc prop_convert2
  qc prop_unroll_roll
  qc prop_add 
  qc prop_sub
  qc prop_doubleneg
  qc prop_mul 
  qc prop_scale
  qc prop_cmp
  qc prop_even 
  qc prop_odd
  qc prop_shiftl256_fullword 
  qc prop_shiftl256_small 
  qc prop_shiftr256_fullword 
  qc prop_shiftr256_small 
-}

--------------------------------------------------------------------------------

prop_convert1 :: Integer -> Bool
prop_convert1 n  =  fromWord256 (toWord256 n) == modN n

prop_convert1b :: BigInt -> Bool
prop_convert1b (BigInt n)  =  fromWord256 (toWord256 n) == modN n

prop_convert2 :: Word256 -> Bool
prop_convert2 n  =  toWord256 (fromWord256 n) == n

prop_unroll_roll :: BigInt -> Bool
prop_unroll_roll (BigInt n)  = littleEndianRollInteger32 (littleEndianUnrollInteger32 n) == n

prop_add :: Word256 -> Word256 -> Bool
prop_add a b = fromWord256 (a+b) == modN (fromWord256 a + fromWord256 b)

prop_sub :: Word256 -> Word256 -> Bool
prop_sub a b = fromWord256 (a-b) == modN (fromWord256 a - fromWord256 b)

prop_doubleneg :: Word256 -> Bool
prop_doubleneg a = (-(-a)) == a

prop_mul :: Word256 -> Word256 -> Bool
prop_mul a b = fromWord256 (a*b) == modN (fromWord256 a * fromWord256 b)

prop_scale :: Word256 -> Word32 -> Bool
prop_scale n k = modN (fromWord256 n * fromIntegral k) == fromWord256 (scale256 n k)

prop_cmp :: Word256 -> Word256 -> Bool
prop_cmp a b = (a >= b) == (fromWord256 a >= fromWord256 b)

prop_even :: Word256 -> Bool
prop_even a = isEven256 a == even (fromWord256 a)

prop_odd :: Word256 -> Bool
prop_odd a = isOdd256 a == odd (fromWord256 a)

prop_shiftl256_fullword :: Word256 -> Bool
prop_shiftl256_fullword a = fromWord256 (shiftl256_fullword a) == modN (shiftL (fromWord256 a) 32)

prop_shiftl256_small :: Word256 -> SmallInt32 -> Bool
prop_shiftl256_small a (SmallInt32 k) = fromWord256 (shiftl256_small a (fromIntegral k)) == modN (shiftL (fromWord256 a) k)

prop_shiftr256_fullword :: Word256 -> Bool
prop_shiftr256_fullword a = fromWord256 (shiftr256_fullword a) == (shiftR (fromWord256 a) 32)

prop_shiftr256_small :: Word256 -> SmallInt32 -> Bool
prop_shiftr256_small a (SmallInt32 k) = fromWord256 (shiftr256_small a (fromIntegral k)) == (shiftR (fromWord256 a) k)

--------------------------------------------------------------------------------


