

{-# LANGUAGE CPP, BangPatterns #-}
module Bitcoin.Test.Crypto.FiniteField.NaiveFp where

--------------------------------------------------------------------------------

import Prelude hiding ( sqrt )

import Test.Tasty
import Test.Tasty.QuickCheck 
import Test.QuickCheck ( Arbitrary(..) , choose , quickCheckWith , stdArgs , maxSuccess , Testable )

import Bitcoin.Crypto.FiniteField.Naive.Fp 
import Bitcoin.Test.Misc.QuickCheck

--------------------------------------------------------------------------------

testgroup_NaiveFp :: TestTree
testgroup_NaiveFp = testGroup "Naive.Fp"
  [ testProperty "conversion /1a"         prop_convert1
  , testProperty "conversion /1b"         prop_convert1b
  , testProperty "conversion /2"          prop_convert2
  , testProperty "addition"               prop_add 
  , testProperty "subtraction"            prop_sub
  , testProperty "negation is involution" prop_doubleneg
  , testProperty "multiplication"         prop_mul 
  , testProperty "division"               prop_div
  , testProperty "reciprocal"             prop_recip
  , testProperty "mult. inverse /1"       prop_inv_algos
  , testProperty "mult. inverse /2"       prop_inv_algos2
  , testProperty "mult. inverse /3"       prop_inv_algos3
  , testProperty "square root"            prop_sqrt
  , testProperty "exponent is additive"   prop_pow_add
  , testProperty "0th power"              prop_pow_0
  , testProperty "1st power"              prop_pow_1
  , testProperty "2nd power"              prop_pow_2
  , testProperty "3rd power"              prop_pow_3
  , testProperty "p-th power"             prop_pow_p
  , testProperty "(p-1)-th power"         prop_pow_pminus1
  , testProperty "small powers"           prop_pow_small
  ]

--------------------------------------------------------------------------------
-- * quickcheck

modP :: Integer -> Integer
modP k = mod k secp256k1_p

newtype NonZeroFp = NonZeroFp Fp deriving (Eq,Show)  

instance Arbitrary Fp where
  arbitrary = do
    n <- choose (0,secp256k1_p-1)
    return $ toFp n

instance Arbitrary NonZeroFp where
  arbitrary = do
    n <- choose (1,secp256k1_p-1)
    return $ NonZeroFp $ toFp n

--------------------------------------------------------------------------------

{-
runAllTests_fp_naive :: IO ()
runAllTests_fp_naive = runAllTests_fp_naive' 1000

runAllTests_fp_naive' :: Int -> IO ()
runAllTests_fp_naive' n = do
  let args = stdArgs { maxSuccess = n }
  let qc :: Testable prop => prop -> IO ()
      qc = quickCheckWith args
 
  putStrLn "running all tests in Bitcoin.Crypto.FiniteField.Naive.Fp"
  putStrLn "========================================================"
  qc prop_convert1
  qc prop_convert1b
  qc prop_convert2
  qc prop_add 
  qc prop_sub
  qc prop_doubleneg
  qc prop_mul 
  qc prop_div
  qc prop_recip
  qc prop_inv_algos
  qc prop_inv_algos2
  qc prop_inv_algos3
  qc prop_sqrt
  qc prop_pow_add
  qc prop_pow_0
  qc prop_pow_1
  qc prop_pow_2
  qc prop_pow_3
  qc prop_pow_p
  qc prop_pow_pminus1
-}

--------------------------------------------------------------------------------

prop_convert1 :: Integer -> Bool
prop_convert1 n  =  fromFp (toFp n) == modP n

prop_convert1b :: BigInt -> Bool
prop_convert1b (BigInt n)  =  fromFp (toFp n) == modP n

prop_convert2 :: Fp -> Bool
prop_convert2 n  =  toFp (fromFp n) == n

prop_add :: Fp -> Fp -> Bool
prop_add a b = fromFp (a+b) == modP (fromFp a + fromFp b)

prop_sub :: Fp -> Fp -> Bool
prop_sub a b = fromFp (a-b) == modP (fromFp a - fromFp b)

prop_doubleneg :: Fp -> Bool
prop_doubleneg a = (-(-a)) == a

prop_mul :: Fp -> Fp -> Bool
prop_mul a b = fromFp (a*b) == modP (fromFp a * fromFp b)

prop_div :: Fp -> NonZeroFp -> Bool
prop_div a (NonZeroFp b) = (toFp $ fromFp (a/b)) * b == a

prop_recip :: NonZeroFp -> Bool
prop_recip (NonZeroFp b) = (toFp $ fromFp (1/b)) * b == 1

prop_inv_algos :: NonZeroFp -> Bool
prop_inv_algos (NonZeroFp (Fp a)) = (invFp_pow a == invFp_euclid a)

prop_inv_algos2 :: NonZeroFp -> Bool
prop_inv_algos2 (NonZeroFp (Fp a)) = (invFp_pow_spec (Fp a) == Fp (invFp_euclid a))

prop_inv_algos3 :: NonZeroFp -> Bool
prop_inv_algos3 (NonZeroFp (Fp a)) = (Fp (invFp_pow a) == invFp_pow_spec (Fp a))

-- prop_cmp :: Fp -> Fp -> Bool
-- prop_cmp a b = (a >= b) == (fromFp a >= fromFp b)

prop_sqrt :: Fp -> Bool
prop_sqrt a = case sqrtFp (unFp a) of
  Nothing -> True
  Just x  -> (Fp x)*(Fp x) == a

prop_pow_add :: Fp -> BigInt -> BigInt -> Bool
prop_pow_add a (BigInt k) (BigInt l) = fromFp (pow_p a k * pow_p a l) == fromFp (pow_p a (k+l)) 

prop_pow_0 :: Fp -> Bool
prop_pow_0 a = fromFp (pow_p a 0) == 1

prop_pow_1 :: Fp -> Bool
prop_pow_1 a = (pow_p a 1) == a

prop_pow_2 :: Fp -> Bool
prop_pow_2 a = (pow_p a 2) == a*a

prop_pow_3 :: Fp -> Bool
prop_pow_3 a = (pow_p a 3) == a*a*a

prop_pow_p :: Fp -> Bool
prop_pow_p a = pow_p a secp256k1_p == a

prop_pow_pminus1 :: Fp -> Bool
prop_pow_pminus1 a = fromFp (pow_p a (secp256k1_p-1)) == 1

prop_pow_small :: Fp -> SmallExpo -> Bool
prop_pow_small a (SmallExpo k) = pow_p a (fromIntegral k) == product (replicate k a)

--------------------------------------------------------------------------------
