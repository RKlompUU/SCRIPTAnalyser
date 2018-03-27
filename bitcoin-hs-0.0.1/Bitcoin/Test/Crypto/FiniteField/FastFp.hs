

{-# LANGUAGE CPP, ForeignFunctionInterface, BangPatterns #-}
module Bitcoin.Test.Crypto.FiniteField.FastFp where

--------------------------------------------------------------------------------

import Data.Word
import Data.Bits

import Test.Tasty
import Test.Tasty.QuickCheck 
import Test.QuickCheck ( Arbitrary(..) , choose , quickCheckWith , stdArgs , maxSuccess , Testable , Property , (==>) )

import Bitcoin.Crypto.Word256
import Bitcoin.Test.Crypto.Word256 ()
import Bitcoin.Crypto.FiniteField.Fast.Fp 
import qualified Bitcoin.Crypto.FiniteField.Naive.Fp as Naive
import Bitcoin.Test.Misc.QuickCheck

--------------------------------------------------------------------------------

testgroup_FastFp :: TestTree
testgroup_FastFp = testGroup "Fast.Fp"
  [ testProperty "conversion /1a"         prop_convert1
  , testProperty "conversion /1b"         prop_convert1b
  , testProperty "conversion /2"          prop_convert2
  , testProperty "addition"               prop_add 
  , testProperty "addition /2"            prop_add2
  , testProperty "subtraction"            prop_sub
  , testProperty "subtraction /2"         prop_sub2
  , testProperty "negation is involution" prop_doubleneg
  , testProperty "small multiplication"   prop_mul_small 
  , testProperty "multiplication"         prop_mul 
  , testProperty "multiplication /2"      prop_mul2 
  , testProperty "division"               prop_div
  , testProperty "division /2"            prop_div2
  , testProperty "reciprocal"             prop_recip
  , testProperty "mult. inverse /1"       prop_inv1
  , testProperty "mult. inverse /2"       prop_inv2
  , testProperty "mult. inverse /3"       prop_inv3
  , testProperty "square root"            prop_sqrt
  , testProperty "square root/2"          prop_sqrt2
  , testProperty "power"                  prop_pow
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

newtype NonZeroFp  = NonZeroFp Fp deriving (Eq,Show)  
newtype SmallFp    = SmallFp   Fp deriving (Eq,Show)  

instance Arbitrary Fp where
  arbitrary = do
    n <- choose (0,secp256k1_p-1)
    return $ toFp n

instance Arbitrary NonZeroFp where
  arbitrary = do
    n <- choose (1,secp256k1_p-1)
    return $ NonZeroFp $ toFp n

instance Arbitrary SmallFp where
  arbitrary = do
    n <- choose (0,2^129-1)
    return $ SmallFp $ toFp n

naive :: Fp -> Naive.Fp
naive = Naive.toFp . fromFp 

--------------------------------------------------------------------------------

{-
runAllTests_fp_fast :: IO ()
runAllTests_fp_fast = runAllTests_fp_fast' 1000

runAllTests_fp_fast' :: Int -> IO ()
runAllTests_fp_fast' n = do
  let args = stdArgs { maxSuccess = n }
  let qc :: Testable prop => prop -> IO ()
      qc = quickCheckWith args
 
  putStrLn "running all tests in Bitcoin.Crypto.FiniteField.Fast.Fp"
  putStrLn "======================================================="
  qc prop_convert1
  qc prop_convert1b
  qc prop_convert2
  qc prop_add 
  qc prop_add2
  qc prop_sub
  qc prop_sub2
  qc prop_doubleneg
  qc prop_mul_small
  qc prop_mul 
  qc prop_mul2
  qc prop_div
  qc prop_div2
  qc prop_recip
  qc prop_inv1
  qc prop_inv2
  qc prop_inv3
  qc prop_sqrt
  qc prop_sqrt2
  qc prop_pow
  qc prop_pow_add
  qc prop_pow_0
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

prop_add2 :: Fp -> Fp -> Bool
prop_add2 a b = fromFp (a+b) == Naive.fromFp (naive a + naive b)

prop_sub2 :: Fp -> Fp -> Bool
prop_sub2 a b = fromFp (a-b) == Naive.fromFp (naive a - naive b)

prop_doubleneg :: Fp -> Bool
prop_doubleneg a = (-(-a)) == a

prop_mul :: Fp -> Fp -> Bool
prop_mul a b = fromFp (a*b) == modP (fromFp a * fromFp b)

prop_mul_small :: SmallFp -> SmallFp -> Bool
prop_mul_small (SmallFp a) (SmallFp b) = (fromFp (a*b) == modP (fromFp a * fromFp b)) 
{-
  = debug "fast" (a*b)
  $ debug "gmp"  (toFp $ modP (fromFp a * fromFp b))
  $ (fromFp (a*b) == modP (fromFp a * fromFp b)) 
  where
    debug :: String -> Fp -> a -> a
    debug !s !x y = trace ("\n >>> " ++ s ++ " -> " ++ show x ++ "\n") y
-}

prop_div :: Fp -> NonZeroFp -> Bool
prop_div a (NonZeroFp b) = (toFp $ fromFp (a/b)) * b == a

prop_mul2 :: Fp -> Fp -> Bool
prop_mul2 a b = fromFp (a*b) == Naive.fromFp (naive a * naive b)

prop_div2 :: Fp -> NonZeroFp -> Bool
prop_div2 a (NonZeroFp b) = fromFp (a/b) == Naive.fromFp (naive a / naive b)

prop_recip :: NonZeroFp -> Bool
prop_recip (NonZeroFp b) = (toFp $ fromFp (1/b)) * b == 1

prop_inv1 :: NonZeroFp -> Bool
prop_inv1 (NonZeroFp b) = (inv_modp_power (unFp b) == inv_modp_euclid (unFp b))

prop_inv2 :: NonZeroFp -> Bool
prop_inv2 (NonZeroFp b) = (inv_modp_pow_spec (unFp b) == inv_modp_euclid (unFp b))

prop_inv3 :: NonZeroFp -> Bool
prop_inv3 (NonZeroFp b) = (inv_modp_power (unFp b) == inv_modp_pow_spec (unFp b))

prop_sqrt :: Fp -> Bool
prop_sqrt a = case sqrtFp (unFp a) of
  Nothing -> True
  Just x  -> (Fp x)*(Fp x) == a

prop_sqrt2 :: Fp -> Bool
prop_sqrt2 a = (fromFp <$> sqrt_p a) == (Naive.fromFp <$> Naive.sqrt_p (naive a))

prop_pow :: Fp -> Word256 -> Bool
prop_pow a k = fromFp (pow_p a k) == Naive.fromFp (Naive.pow_p (naive a) (fromWord256 k))

prop_pow_add :: Fp -> MediumInt -> MediumInt -> Property    -- Bool
prop_pow_add a (MediumInt k) (MediumInt l) = 
  (k+l < twoToThe256) ==> (fromFp (pow_p a (toWord256 k) * pow_p a (toWord256 l)) == fromFp (pow_p a (toWord256 (k+l))))

prop_pow_0 :: Fp -> Bool
prop_pow_0 a = fromFp (pow_p a 0) == 1

prop_pow_1 :: Fp -> Bool
prop_pow_1 a = (pow_p a 1) == a

prop_pow_2 :: Fp -> Bool
prop_pow_2 a = (pow_p a 2) == a*a

prop_pow_3 :: Fp -> Bool
prop_pow_3 a = (pow_p a 3) == a*a*a

prop_pow_p :: Fp -> Bool
prop_pow_p a = (pow_p a (toWord256 secp256k1_p)) == a

prop_pow_pminus1 :: Fp -> Bool
prop_pow_pminus1 a = fromFp (pow_p a (toWord256 (secp256k1_p-1))) == 1

prop_pow_small :: Fp -> SmallExpo -> Bool
prop_pow_small a (SmallExpo k) = pow_p a (fromIntegral k) == product (replicate k a)

--------------------------------------------------------------------------------


