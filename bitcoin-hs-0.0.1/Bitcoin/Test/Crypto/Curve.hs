
{-# LANGUAGE CPP, BangPatterns #-}
module Bitcoin.Test.Crypto.Curve where

--------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck 
import Test.QuickCheck ( Arbitrary(..) , choose , quickCheckWith , stdArgs , maxSuccess , Testable )

import Bitcoin.Crypto.FiniteField.Fast.Fp  hiding ( secp256k1_p )
import Bitcoin.Crypto.FiniteField.Naive.Fn hiding ( secp256k1_n )
import Bitcoin.Crypto.EC.Curve

import Bitcoin.Test.Misc.QuickCheck

--------------------------------------------------------------------------------

testgroup_Curve :: TestTree
testgroup_Curve = testGroup "EC.Curve"
  [ testProperty "left unit"               prop_left_unit 
  , testProperty "right unit"              prop_right_unit 
  , testProperty "0 + 0 = 0"               prop_add_dbl_unit 
  , testProperty "2*0 = 0"                 prop_dbl_unit 
  , testProperty "inverse of 0 = 0"        prop_inv_unit 
  , testProperty "(p+q)-q = p"             prop_add_sub
  , testProperty "(p-q)+q = p"             prop_add_sub1
  , testProperty "((p+q)-q)-p = 0"         prop_add_sub2
  , testProperty "(2*p) - p = p"           prop_dbl_sub
  , testProperty "(2*p) - p - p = 0"       prop_dbl_sub2
  , testProperty "addition is commutative" prop_add_commutative
  , testProperty "addition is associative" prop_add_associative
  , testProperty "(p-q)-r = p-(q+r)"       prop_add_sub_associative1
  , testProperty "(p-q)+r = p-(q-r)"       prop_add_sub_associative2
  , testProperty "a + (-a) = 0"            prop_add_inv
  , testProperty "k*p = (k mod n)*p"       prop_mul_big
  , testProperty "0*p = 0"                 prop_mul_0
  , testProperty "1*p = p"                 prop_mul_1
  , testProperty "2*p = p + p"             prop_mul_2
  , testProperty "n*p = 0"                 prop_mul_n
  , testProperty "(n-1)*p = -p"            prop_mul_nminus1
  ]

--------------------------------------------------------------------------------
-- * quickcheck

newtype NonZeroEC = NonZeroEC ECPoint deriving (Eq,Show)

instance Arbitrary ECPoint where
  arbitrary = do
    n <- choose (0,secp256k1_n-1)
    return $ mulEC (secp256k1_G) n

instance Arbitrary NonZeroEC where
  arbitrary = do
    n <- choose (1,secp256k1_n-1)
    return $ NonZeroEC $ mulEC (secp256k1_G) n

--------------------------------------------------------------------------------

{-
runAllTests_ec_curve :: IO ()
runAllTests_ec_curve = runAllTests_ec_curve' 1000

runAllTests_ec_curve' :: Int -> IO ()
runAllTests_ec_curve' n = do
  let args = stdArgs { maxSuccess = n }
  let qc :: Testable prop => prop -> IO ()
      qc = quickCheckWith args
 
  putStrLn "running all tests in Bitcoin.Crypto.EC.Curve"
  putStrLn "============================================"
  qc prop_left_unit 
  qc prop_right_unit 
  qc prop_add_dbl_unit 
  qc prop_dbl_unit 
  qc prop_inv_unit 
  qc prop_add_sub
  qc prop_add_sub2
  qc prop_dbl_sub
  qc prop_dbl_sub2
  qc prop_add_commutative
  qc prop_add_associative
  qc prop_add_sub_associative1
  qc prop_add_sub_associative2
  qc prop_add_inv
  qc prop_mul_big
  qc prop_mul_0
  qc prop_mul_1
  qc prop_mul_2
  qc prop_mul_n
  qc prop_mul_nminus1
-}

--------------------------------------------------------------------------------

prop_left_unit :: ECPoint  -> Bool
prop_left_unit p = addEC p ECInfinity == p

prop_right_unit :: ECPoint  -> Bool
prop_right_unit q = addEC ECInfinity q == q

prop_add_dbl_unit :: Bool
prop_add_dbl_unit = ECInfinity + ECInfinity == ECInfinity

prop_dbl_unit :: Bool
prop_dbl_unit = dblEC ECInfinity == ECInfinity

prop_inv_unit :: Bool
prop_inv_unit = invEC ECInfinity == ECInfinity

prop_add_sub :: ECPoint -> ECPoint -> Bool
prop_add_sub p q  = subEC (addEC p q) q == p

prop_add_sub1 :: ECPoint -> ECPoint -> Bool
prop_add_sub1 p q  = addEC (subEC p q) q == p

prop_add_sub2 :: ECPoint -> ECPoint -> Bool
prop_add_sub2 p q  = subEC (subEC (addEC p q) q) p == ECInfinity

prop_dbl_sub :: ECPoint -> Bool
prop_dbl_sub p = subEC (dblEC p) p == p

prop_dbl_sub2 :: ECPoint -> Bool
prop_dbl_sub2 p = subEC (subEC (dblEC p) p) p == ECInfinity

prop_add_commutative :: ECPoint -> ECPoint -> Bool
prop_add_commutative p q  = (addEC p q) == (addEC q p)

prop_add_associative :: ECPoint -> ECPoint -> ECPoint -> Bool
prop_add_associative p q r = addEC (addEC p q) r == addEC p (addEC q r)

prop_add_sub_associative1 :: ECPoint -> ECPoint -> ECPoint -> Bool
prop_add_sub_associative1 p q r = subEC (subEC p q) r == subEC p (addEC q r)

prop_add_sub_associative2 :: ECPoint -> ECPoint -> ECPoint -> Bool
prop_add_sub_associative2 p q r = addEC (subEC p q) r == subEC p (subEC q r)

prop_add_inv :: ECPoint -> Bool
prop_add_inv p = addEC p (invEC p) == ECInfinity

prop_mul_big :: ECPoint -> BigInt -> Bool
prop_mul_big p (BigInt k) = mulEC p k == mulEC p (mod k secp256k1_n)

prop_mul_0 :: ECPoint -> Bool
prop_mul_0 p = mulEC p 0 == ECInfinity

prop_mul_1 :: ECPoint -> Bool
prop_mul_1 p = mulEC p 1 == p

prop_mul_2 :: ECPoint -> Bool
prop_mul_2 p = mulEC p 2 == addEC p p

prop_mul_n :: ECPoint -> Bool
prop_mul_n p = mulEC p secp256k1_n == ECInfinity

prop_mul_nminus1 :: ECPoint -> Bool
prop_mul_nminus1 p = mulEC p (secp256k1_n - 1) == invEC p

--------------------------------------------------------------------------------
