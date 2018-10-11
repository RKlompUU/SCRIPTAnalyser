
{-# LANGUAGE CPP #-}
module Bitcoin.Test.Protocol.Hash where

--------------------------------------------------------------------------------

import Data.Word
import qualified Data.ByteString as B

import Test.Tasty
import Test.Tasty.QuickCheck 
import Test.QuickCheck ( Arbitrary(..) , choose , quickCheckWith , stdArgs , maxSuccess , Testable , Gen )

import Bitcoin.Misc.BigInt
import Bitcoin.Misc.HexString
import Bitcoin.Misc.OctetStream
import Bitcoin.Misc.Endian

import Bitcoin.Protocol.Hash

--------------------------------------------------------------------------------

testgroup_ProtocolHash :: TestTree
testgroup_ProtocolHash = testGroup "Hash160 / Hash256"
  [ testGroup "conversions"
      [ testProperty ""            prop_Hash160_toFromBytestring 
      , testProperty ""            prop_Hash160_toFromWord8List 
      , testProperty ""            prop_Hash160_showRead 
      , testProperty ""            prop_Hash256_toFromBytestring
      , testProperty ""            prop_Hash256_toFromWord8List
      , testProperty ""            prop_Hash256_showRead 
      ]
  , testGroup "byte order"
      [ testProperty ""            prop_Hash160_show_byte_order 
      , testProperty ""            prop_Hash256_show_byte_order 
      ]
  , testGroup "comparisons"
      [ testProperty ""            prop_Hash160_cmp_Word8List
      , testProperty ""            prop_Hash160_cmp_ByteString
      , testProperty ""            prop_Hash256_cmp_Word8List 
      , testProperty ""            prop_Hash256_cmp_ByteString 
      ]
  ]

--------------------------------------------------------------------------------
-- quickcheck

-- 1/4 of the time it gives back the given constant, otherwise random
genMaybeKst :: Arbitrary a => a -> Gen a
genMaybeKst x = do
  j <- choose (1::Int,4)
  if j==4
    then return x
    else arbitrary

instance Arbitrary Hash160 where
  arbitrary = do
    w1 <- genMaybeKst (0x1234567890abcdef :: Word64)
    w2 <- genMaybeKst (0xfedcba0987654321 :: Word64)
    w3 <- genMaybeKst (0x345678cd         :: Word32)
    return $ Hash160 w1 w2 w3

instance Arbitrary Hash256 where
  arbitrary = do
    w1 <- genMaybeKst (0x1234567890abcdef :: Word64)
    w2 <- genMaybeKst (0xfedcba0987654321 :: Word64)
    w3 <- genMaybeKst (0xabcdef1234567890 :: Word64)
    w4 <- genMaybeKst (0x654321fedcba0987 :: Word64)
    return $ Hash256 w1 w2 w3 w4

--------------------------------------------------------------------------------

{-
runAllTests_hash :: IO ()
runAllTests_hash = runAllTests_hash' 1000

runAllTests_hash' :: Int -> IO ()
runAllTests_hash' n = do
  let args = stdArgs { maxSuccess = n }
  let qc :: Testable prop => prop -> IO ()
      qc = quickCheckWith args
 
  putStrLn "running all tests in Bitcoin.Protocol.Hash"
  putStrLn "=========================================="
--  putStrLn "testing conversions..."
  qc prop_Hash160_toFromBytestring 
  qc prop_Hash160_toFromWord8List 
  qc prop_Hash160_showRead 
  qc prop_Hash256_toFromBytestring
  qc prop_Hash256_toFromWord8List
  qc prop_Hash256_showRead 
--  putStrLn "testing byte order..."
  qc prop_Hash160_show_byte_order 
  qc prop_Hash256_show_byte_order 
--  putStrLn "testing comparisons..."
  qc prop_Hash160_cmp_Word8List
  qc prop_Hash160_cmp_ByteString
  qc prop_Hash256_cmp_Word8List 
  qc prop_Hash256_cmp_ByteString 
-}

--------------------------------------------------------------------------------
-- conversion

prop_Hash160_toFromBytestring :: Hash160 -> Bool
prop_Hash160_toFromBytestring h  =  h == fromByteString (toByteString h)

prop_Hash160_toFromWord8List :: Hash160 -> Bool
prop_Hash160_toFromWord8List h  =  h == fromWord8List (toWord8List h)

prop_Hash160_showRead :: Hash160 -> Bool
prop_Hash160_showRead h  =  h == read (show h)

prop_Hash256_toFromBytestring :: Hash256 -> Bool
prop_Hash256_toFromBytestring h  =  h == fromByteString (toByteString h)

prop_Hash256_toFromWord8List :: Hash256 -> Bool
prop_Hash256_toFromWord8List h  =  h == fromWord8List (toWord8List h)

prop_Hash256_showRead :: Hash256 -> Bool
prop_Hash256_showRead h  =  h == read (show h)

-- byte order

prop_Hash160_show_byte_order :: Bool
prop_Hash160_show_byte_order = show (fromWord8List [1..20] :: Hash160) == "hash160FromTextBE \"14131211100f0e0d0c0b0a090807060504030201\""

prop_Hash256_show_byte_order :: Bool
prop_Hash256_show_byte_order = show (fromWord8List [1..32] :: Hash256) == "hash256FromTextBE \"201f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201\""

-- ordering

prop_Hash160_cmp_Word8List :: Hash160 -> Hash160 -> Bool
prop_Hash160_cmp_Word8List h1 h2  =  compare h1 h2 == compare (reverse $ toWord8List h1) (reverse $ toWord8List h2)

prop_Hash160_cmp_ByteString :: Hash160 -> Hash160 -> Bool
prop_Hash160_cmp_ByteString h1 h2  =  compare h1 h2 == compare (B.reverse $ toByteString h1) (B.reverse $ toByteString h2)

prop_Hash256_cmp_Word8List :: Hash256 -> Hash256 -> Bool
prop_Hash256_cmp_Word8List h1 h2  =  compare h1 h2 == compare (reverse $ toWord8List h1) (reverse $ toWord8List h2)

prop_Hash256_cmp_ByteString :: Hash256 -> Hash256 -> Bool
prop_Hash256_cmp_ByteString h1 h2  =  compare h1 h2 == compare (B.reverse $ toByteString h1) (B.reverse $ toByteString h2)

--------------------------------------------------------------------------------
