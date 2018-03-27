
{-# LANGUAGE CPP, BangPatterns #-}
module Bitcoin.Test.Crypto.Key where

--------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck 
import Test.QuickCheck ( Arbitrary(..) , choose , quickCheckWith , stdArgs , maxSuccess , Testable )

import Bitcoin.Crypto.FiniteField.Fast.Fp  hiding ( secp256k1_p )
import Bitcoin.Crypto.FiniteField.Naive.Fn hiding ( secp256k1_n )

import Bitcoin.Crypto.EC.Curve
import Bitcoin.Crypto.EC.Key

import Bitcoin.Test.Crypto.Curve
import Bitcoin.Test.Misc.QuickCheck

--------------------------------------------------------------------------------

testgroup_Key :: TestTree
testgroup_Key = testGroup "EC.Key"
  [ testProperty "uncompress . compress == id"      prop_compr_uncompr       
  , testProperty "uncompressed pubkeys are valid"   prop_valid_pubkey    
  , testProperty "compressed pubkeys are valid"     prop_valid_compr_pubkey    
  , testProperty "calculated pubkeys are valid"     prop_valid_calc_pubkey 
  ]

--------------------------------------------------------------------------------
-- quickcheck

instance Arbitrary PrivKey where
  arbitrary = do
    n <- choose (1,secp256k1_n-1)
    return (PrivKey n)

instance Arbitrary PubKey where
  arbitrary = do
    NonZeroEC (ECPoint x y) <- arbitrary
    return (FullPubKey (fromFp x) (fromFp y))  

--------------------------------------------------------------------------------

{-
runAllTests_ec_key :: IO ()
runAllTests_ec_key = runAllTests_ec_key' 1000

runAllTests_ec_key' :: Int -> IO ()
runAllTests_ec_key' n = do
  let args = stdArgs { maxSuccess = n }
  let qc :: Testable prop => prop -> IO ()
      qc = quickCheckWith args
 
  putStrLn "running all tests in Bitcoin.Crypto.EC.Key"
  putStrLn "=========================================="
  qc prop_compr_uncompr
  qc prop_valid_pubkey
-}

--------------------------------------------------------------------------------

prop_compr_uncompr :: PubKey -> Bool
prop_compr_uncompr key = uncompressPubKey (compressPubKey key) == Just key

prop_valid_pubkey :: PubKey -> Bool
prop_valid_pubkey = isValidPubKey

prop_valid_compr_pubkey :: PubKey -> Bool
prop_valid_compr_pubkey = isValidPubKey . compressPubKey

prop_valid_calc_pubkey :: PrivKey -> Bool
prop_valid_calc_pubkey priv 
  =  isValidPubKey (computePubKey Compressed   priv) 
  && isValidPubKey (computePubKey Uncompressed priv) 

--------------------------------------------------------------------------------
