

{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
module Bitcoin.Test.Crypto.Projective where

--------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck 
import Test.QuickCheck ( Arbitrary(..) , choose , quickCheckWith , stdArgs , maxSuccess , Testable )

import Bitcoin.Crypto.FiniteField.Fast.Fp  hiding ( secp256k1_p )
import Bitcoin.Crypto.FiniteField.Naive.Fn hiding ( secp256k1_n )
import Bitcoin.Crypto.EC.Curve
import Bitcoin.Crypto.EC.Projective

import Bitcoin.Test.Crypto.Curve ()
import Bitcoin.Test.Misc.QuickCheck

--------------------------------------------------------------------------------

testgroup_Projective :: TestTree
testgroup_Projective = testGroup "EC.Projective" [ testgroup_proj , testgroup_ecp ]

testgroup_proj :: TestTree
testgroup_proj = testGroup "proj (projective curve representation)"
  [ testProperty "left unit"               prop_proj_left_unit
  , testProperty "right unit"              prop_proj_right_unit
  , testProperty "0 + 0 = 0"               prop_proj_add_dbl_unit 
  , testProperty "2*0 = 0"                 prop_proj_dbl_unit 
  , testProperty "-0 = 0"                  prop_proj_inv_unit  -- 5
  , testProperty "(p+q)-q = p"             prop_proj_add_sub
  , testProperty "((p+q)-q)-p = 0"         prop_proj_add_sub2
  , testProperty "2*p - p = p"             prop_proj_dbl_sub
  , testProperty "2*p - p - p = 0"         prop_proj_dbl_sub2
  , testProperty "addition is commutative" prop_proj_add_commutative  -- 10
  , testProperty "addition is associative" prop_proj_add_associative
  , testProperty "(p-q)-r = p-(q+r)"       prop_proj_add_sub_associative1
  , testProperty "(p-q)+r = p-(q-r)"       prop_proj_add_sub_associative2
  , testProperty "p + (-p) = 0"            prop_proj_add_inv
  , testProperty "k*p = (k mod n)*p"       prop_proj_mul_big  -- 15
  , testProperty "0*p = 0"                 prop_proj_mul_0
  , testProperty "1*p = p"                 prop_proj_mul_1
  , testProperty "2*p = p + p"             prop_proj_mul_2
  , testProperty "n*p = 0"                 prop_proj_mul_n    -- 20
  , testProperty "(n-1)*p = -p"            prop_proj_mul_nminus1
  , testProperty "hs add = c add"          prop_proj_add_chs
  , testProperty "hs dbl = c dbl"          prop_proj_dbl_chs
  , testProperty "hs mul = c mul"          prop_proj_mul_chs
  ]

testgroup_ecp :: TestTree
testgroup_ecp = testGroup "ecp (projective vs. affine conversions)"
  [ testProperty "fromP . toP = id"        prop_ecp_from_to
  , testProperty "toP (fromP p) ~ p"       prop_ecp_to_from
  , testProperty "proj. addition"          prop_ecp_add
  , testProperty "proj. subtraction"       prop_ecp_sub
  , testProperty "proj. doubling /1"       prop_ecp_add_dbl1
  , testProperty "proj. doubling /2"       prop_ecp_add_dbl2
  , testProperty "proj. doubling /3"       prop_ecp_add_dbl3
  , testProperty "proj. doubling /4"       prop_ecp_add_dbl4
  , testProperty "proj. doubling /5"       prop_ecp_dbl
  , testProperty "proj. inverse /1"        prop_ecp_inv1
  , testProperty "proj. inverse /2"        prop_ecp_inv2
  , testProperty "proj. inverse /3"        prop_ecp_inv3
  , testProperty "proj. multiplication"    prop_ecp_mul
  ]

--------------------------------------------------------------------------------
-- * quickcheck

scaleECProj :: Integer -> ECProj -> ECProj 
scaleECProj n0 (ECProj x y z) = ECProj (x*n2) (y*n3) (z*n) where
  n  = toFp n0
  n2 = n*n
  n3 = n2*n

newtype NonZeroECP = NonZeroECP ECProj deriving (Eq,Show)

data Same = Same ECProj ECProj deriving (Eq,Show)

instance Arbitrary ECProj where
  arbitrary = do
    ec <- arbitrary 
    z  <- toFp <$> choose (1,secp256k1_p-1)
    let z2 = z*z
    let z3 = z2*z
    return $ case ec of
      ECPoint x y -> ECProj (x*z2) (y*z3*2) z 
      ECInfinity  -> ecpInfinity

instance Arbitrary NonZeroECP where
  arbitrary = do
    n <- choose (1,secp256k1_n-1)
    return $ NonZeroECP $ mulECP (secp256k1_G_proj) n

instance Arbitrary Same where
  arbitrary = do
    ep@(ECProj x y z) <- arbitrary
    m  <- toFp <$> choose (1,secp256k1_p-1)
    return $ Same ep (ECProj (x*m*m) (y*m*m*m) (z*m))

--------------------------------------------------------------------------------

{- 
runAllTests_ec_proj :: IO ()
runAllTests_ec_proj = runAllTests_ec_proj' 1000

runAllTests_ec_proj' :: Int -> IO ()
runAllTests_ec_proj' n = do
  let args = stdArgs { maxSuccess = n }
  let qc :: Testable prop => prop -> IO ()
      qc = quickCheckWith args
 
  putStrLn "running all tests in Bitcoin.Crypto.EC.Projective"
  putStrLn "================================================="
  qc prop_proj_left_unit
  qc prop_proj_right_unit
  qc prop_proj_add_dbl_unit 
  qc prop_proj_dbl_unit 
  qc prop_proj_inv_unit  -- 5
  qc prop_proj_add_sub
  qc prop_proj_add_sub2
  qc prop_proj_dbl_sub
  qc prop_proj_dbl_sub2
  qc prop_proj_add_commutative  -- 10
  qc prop_proj_add_associative
  qc prop_proj_add_sub_associative1
  qc prop_proj_add_sub_associative2
  qc prop_proj_add_inv
  qc prop_proj_mul_big  -- 15
  qc prop_proj_mul_0
  qc prop_proj_mul_1
  qc prop_proj_mul_2
  qc prop_proj_mul_n    -- 20
  qc prop_proj_mul_nminus1
  qc prop_proj_add_chs
  qc prop_proj_dbl_chs
  qc prop_proj_mul_chs
  putStrLn "-------------------------------------------------"
  qc prop_ecp_from_to
  qc prop_ecp_to_from
  qc prop_ecp_add
  qc prop_ecp_add_dbl1
  qc prop_ecp_add_dbl2
  qc prop_ecp_add_dbl3
  qc prop_ecp_add_dbl4
  qc prop_ecp_dbl
  qc prop_ecp_sub
  qc prop_ecp_inv1
  qc prop_ecp_inv2
  qc prop_ecp_inv3
  qc prop_ecp_mul
-}

--------------------------------------------------------------------------------

prop_proj_left_unit :: ECProj  -> Bool
prop_proj_left_unit p = addECP p ecpInfinity =~= p

prop_proj_right_unit :: ECProj  -> Bool
prop_proj_right_unit q = addECP ecpInfinity q =~= q

prop_proj_add_dbl_unit :: Bool
prop_proj_add_dbl_unit = ecpInfinity + ecpInfinity =~= ecpInfinity

prop_proj_dbl_unit :: Bool
prop_proj_dbl_unit = dblECP ecpInfinity =~= ecpInfinity

prop_proj_inv_unit :: Bool
prop_proj_inv_unit = invECP ecpInfinity =~= ecpInfinity

prop_proj_add_sub :: ECProj -> ECProj -> Bool
prop_proj_add_sub p q  = subECP (addECP p q) q =~= p

prop_proj_add_sub2 :: ECProj -> ECProj -> Bool
prop_proj_add_sub2 p q  = subECP (subECP (addECP p q) q) p =~= ecpInfinity

prop_proj_dbl_sub :: ECProj -> Bool
prop_proj_dbl_sub p = subECP (dblECP p) p =~= p

prop_proj_dbl_sub2 :: ECProj -> Bool
prop_proj_dbl_sub2 p = subECP (subECP (dblECP p) p) p =~= ecpInfinity

prop_proj_add_commutative :: ECProj -> ECProj -> Bool
prop_proj_add_commutative p q  = (addECP p q) =~= (addECP q p)

prop_proj_add_associative :: ECProj -> ECProj -> ECProj -> Bool
prop_proj_add_associative p q r = addECP (addECP p q) r =~= addECP p (addECP q r)

prop_proj_add_sub_associative1 :: ECProj -> ECProj -> ECProj -> Bool
prop_proj_add_sub_associative1 p q r = subECP (subECP p q) r =~= subECP p (addECP q r)

prop_proj_add_sub_associative2 :: ECProj -> ECProj -> ECProj -> Bool
prop_proj_add_sub_associative2 p q r = addECP (subECP p q) r =~= subECP p (subECP q r)

prop_proj_add_inv :: ECProj -> Bool
prop_proj_add_inv p = addECP p (invECP p) =~= ecpInfinity

prop_proj_mul_big :: ECProj -> BigInt -> Bool
prop_proj_mul_big p (BigInt k) = mulECP p k =~= mulECP p (mod k secp256k1_n)

prop_proj_mul_0 :: ECProj -> Bool
prop_proj_mul_0 p = mulECP p 0 =~= ecpInfinity

prop_proj_mul_1 :: ECProj -> Bool
prop_proj_mul_1 p = mulECP p 1 =~= p

prop_proj_mul_2 :: ECProj -> Bool
prop_proj_mul_2 p = mulECP p 2 =~= addECP p p

prop_proj_mul_n :: ECProj -> Bool
prop_proj_mul_n p = mulECP p secp256k1_n =~= ecpInfinity

prop_proj_mul_nminus1 :: ECProj -> Bool
prop_proj_mul_nminus1 p = mulECP p (secp256k1_n - 1) =~= invECP p

prop_proj_dbl_chs :: ECProj -> Bool
prop_proj_dbl_chs p = hs_dblECP p == c_dblECP p

prop_proj_add_chs :: ECProj -> ECProj -> Bool
prop_proj_add_chs p q = hs_addECP p q == c_addECP p q

prop_proj_mul_chs :: ECProj -> BigInt -> Bool
prop_proj_mul_chs p (BigInt n) = c_mulECP p n =~= hs_mulECP p n

--------------------------------------------------------------------------------

prop_ecp_to_from :: ECPoint -> Bool
prop_ecp_to_from p = fromECProj (toECProj p) == p

prop_ecp_from_to :: ECProj -> Bool
prop_ecp_from_to p = toECProj (fromECProj p) =~= p

prop_ecp_add :: ECPoint -> ECPoint -> Bool
prop_ecp_add p q = p + q == fromECProj (toECProj p + toECProj q) 

prop_ecp_add_dbl1 :: ECPoint -> Bool
prop_ecp_add_dbl1 p = p + p == fromECProj (toECProj p + toECProj p) 

prop_ecp_add_dbl2 :: ECPoint -> Bool
prop_ecp_add_dbl2 p = dblEC p == fromECProj (toECProj p + toECProj p) 

prop_ecp_add_dbl3 :: Same -> Bool
prop_ecp_add_dbl3 (Same p q) = fromECProj p + fromECProj q == fromECProj (p+q) 

prop_ecp_add_dbl4 :: Same -> Bool
prop_ecp_add_dbl4 (Same p q) = dblEC (fromECProj p) == fromECProj (p+q) 

prop_ecp_sub :: ECPoint -> ECPoint -> Bool
prop_ecp_sub p q = p - q == fromECProj (toECProj p - toECProj q) 

prop_ecp_dbl :: ECPoint -> Bool
prop_ecp_dbl p = dblEC p == fromECProj (dblECP $ toECProj p) 

prop_ecp_inv1 :: ECPoint -> Bool
prop_ecp_inv1 p = invEC p == fromECProj (invECP $ toECProj p) 

prop_ecp_inv2 :: ECPoint -> Bool
prop_ecp_inv2 p = ecpInfinity =~= (invECP $ toECProj p) + (toECProj p)

prop_ecp_inv3 :: Same -> Bool
prop_ecp_inv3 (Same p q) = (invECP p =~= invECP q) 

prop_ecp_mul :: ECPoint -> BigInt -> Bool
prop_ecp_mul p (BigInt n) = fromECProj (mulECP (toECProj p) n) == mulEC p n

--------------------------------------------------------------------------------
