
-- | Hash-based message authentication code (HMAC).
-- 
-- See <http://en.wikipedia.org/wiki/Hmac>

module Bitcoin.Crypto.Hash.HMAC where

--------------------------------------------------------------------------------

import Data.Word
import Data.Bits
import Data.Char  -- for testing only

import Bitcoin.Misc.BigInt
import Bitcoin.Misc.OctetStream

import Bitcoin.Crypto.Hash.SHA1
import Bitcoin.Crypto.Hash.SHA256
import Bitcoin.Crypto.Hash.SHA512
import Bitcoin.Crypto.Hash.MD5

--------------------------------------------------------------------------------

newtype HMAC a = HMAC { unHMAC :: a } deriving (Eq,Ord,Show)

-- | The HMAC key should be an integer between @1@ and @2^512-1@ 
-- (or @2^1024-1@ in case of SHA512)
newtype HMACKey = HMACKey { unHMACKey :: Integer } deriving (Eq,Ord,Show)

--------------------------------------------------------------------------------

-- | Mainly for testing. 64 byte blocksize version.
--
-- Note: blocksize is 64 bytes (512 bits) for both SHA1 and SHA256 and even MD5,
-- but 128 bytes (1024 bits) for SHA512
--
-- Also it seems that according the spec long keys should be hashed.
-- Even though the hash is half as long (but we use zero padding anyway)
--
hmacKeyFromString64 :: OctetStream a => a -> HMACKey
hmacKeyFromString64 chars 
  | n <= 64   = HMACKey $ (toIntegerLE          word8list) -- `mod` modulus
  | otherwise = HMACKey $ (toIntegerLE $ sha256 word8list) -- `mod` modulus
  where
    modulus = 2^(512::Int) :: Integer 
    word8list = toWord8List chars
    n = length word8list

-- | Mainly for testing. 128 byte blocksize version
--
-- Note: blocksize is 64 bytes (512 bits) for both SHA1 and SHA256 and even MD5,
-- but 128 bytes (1024 bits) for SHA512
--
-- Also it seems that according the spec long keys should be hashed.
-- Even though the hash is half as long (but we use zero padding anyway)
--
hmacKeyFromString128 :: OctetStream a => a -> HMACKey
hmacKeyFromString128 chars 
  | n <= 128   = HMACKey $ (toIntegerLE          word8list) -- `mod` modulus
  | otherwise  = HMACKey $ (toIntegerLE $ sha512 word8list) -- `mod` modulus
  where
    modulus = 2^(1024::Int) :: Integer 
    word8list = toWord8List chars
    n = length word8list

--------------------------------------------------------------------------------

hmacSha1 :: OctetStream a => HMACKey -> a -> HMAC SHA1
hmacSha1 (HMACKey keyInt) msg = HMAC $ sha1 ( outer ++ inner_hash ) where

  blocksize = 64 :: Int

  key  = take blocksize (littleEndianUnrollInteger keyInt ++ replicate blocksize 0)

  opad = replicate blocksize 0x5c :: [Word8]
  ipad = replicate blocksize 0x36 :: [Word8]

  outer = zipWith xor key opad
  inner = zipWith xor key ipad

  inner_hash = toWord8List $ sha1 $ inner ++ toWord8List msg

--------------------------------------------------------------------------------

hmacSha256 :: OctetStream a => HMACKey -> a -> HMAC SHA256
hmacSha256 (HMACKey keyInt) msg = HMAC $ sha256 ( outer ++ inner_hash ) where

  blocksize = 64 :: Int

  key  = take blocksize (littleEndianUnrollInteger keyInt ++ replicate blocksize 0)

  opad = replicate blocksize 0x5c :: [Word8]
  ipad = replicate blocksize 0x36 :: [Word8]

  outer = zipWith xor key opad
  inner = zipWith xor key ipad

  inner_hash = toWord8List $ sha256 $ inner ++ toWord8List msg      

--------------------------------------------------------------------------------

hmacSha512 :: OctetStream a => HMACKey -> a -> HMAC SHA512
hmacSha512 (HMACKey keyInt) msg = HMAC $ sha512 ( outer ++ inner_hash ) where

  blocksize = 128 :: Int

  key  = take blocksize (littleEndianUnrollInteger keyInt ++ replicate blocksize 0)

  opad = replicate blocksize 0x5c :: [Word8]
  ipad = replicate blocksize 0x36 :: [Word8]

  outer = zipWith xor key opad
  inner = zipWith xor key ipad

  inner_hash = toWord8List $ sha512 $ inner ++ toWord8List msg      

--------------------------------------------------------------------------------

hmacMD5 :: OctetStream a => HMACKey -> a -> HMAC MD5
hmacMD5 (HMACKey keyInt) msg = HMAC $ md5 ( outer ++ inner_hash ) where

  blocksize = 64 :: Int

  key  = take blocksize (littleEndianUnrollInteger keyInt ++ replicate blocksize 0)

  opad = replicate blocksize 0x5c :: [Word8]
  ipad = replicate blocksize 0x36 :: [Word8]

  outer = zipWith xor key opad
  inner = zipWith xor key ipad

  inner_hash = toWord8List $ md5 $ inner ++ toWord8List msg      

--------------------------------------------------------------------------------

{- 
From wikipedia, some test vectors:

HMAC_MD5("", "") = 0x74e6f7298a9c2d168935f58c001bad88
HMAC_SHA1("", "") = 0xfbdb1d1b18aa6c08324b7d64b71fb76370690e1d
HMAC_SHA256("", "") = 0xb613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad

Here are some non-empty HMAC values, assuming 8-bit ASCII or UTF-8 encoding:

HMAC_MD5("key", "The quick brown fox jumps over the lazy dog") = 0x80070713463e7749b90c2dc24911e275
HMAC_SHA1("key", "The quick brown fox jumps over the lazy dog") = 0xde7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9
HMAC_SHA256("key", "The quick brown fox jumps over the lazy dog") = 0xf7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8

-}

{-  
main = do
  let key0 = hmacKeyFromString64 ""
  let key1 = hmacKeyFromString64 "key"
  print key0
  print key1
  print $ hmacMD5    key0 ""
  print $ hmacSha1   key0 ""
  print $ hmacSha256 key0 ""
  print $ hmacMD5    key1 "The quick brown fox jumps over the lazy dog"
  print $ hmacSha1   key1 "The quick brown fox jumps over the lazy dog"
  print $ hmacSha256 key1 "The quick brown fox jumps over the lazy dog"
-}

--------------------------------------------------------------------------------

{-

hmacTestcase256 (keystring,msg,hmacstring256,hmacstring512) = myhmac where
  key    = hmacKeyFromString64 keystring
  myhmac = hmacSha256 key msg

hmacTestcase512 (keystring,msg,hmacstring256,hmacstring512) = myhmac where
  key    = hmacKeyFromString128 keystring
  myhmac = hmacSha512 key msg

checkTestcase256_512 inp@(keystring,msg,hmacstring256,hmacstring512) = ok where
  my256 = hmacTestcase256 inp
  my512 = hmacTestcase512 inp
  ok    =  ( ("HMAC SHA256<" ++ hmacstring256 ++ ">") == show my256 )
        && ( ("HMAC SHA512<" ++ hmacstring512 ++ ">") == show my512 )

-- | from <http://tools.ietf.org/html/rfc4231>, test vectors for hmac-sha256 and hmac-sha512 
testcases_hmac_sha256_sha512 =
  [ testcase1
  , testcase2
  , testcase3
  , testcase4
  , testcase5
  , testcase6
  , testcase7
  ]
  where

  testcase1 = ( replicate 20 '\x0b' , "Hi There" , hmac256 , hmac512 ) where
    hmac256 = "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"
    hmac512 = "87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec2787ad0b30545e17cdedaa833b7d6b8a702038b274eaea3f4e4be9d914eeb61f1702e696c203a126854"

  testcase2 = ( "Jefe" , "what do ya want for nothing?" , hmac256 , hmac512 ) where
    hmac256 = "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
    hmac512 = "164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737"

  testcase3 = ( replicate 20 '\xaa' , replicate 50 '\xdd' , hmac256 , hmac512 ) where 
    hmac256 = "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe"
    hmac512 = "fa73b0089d56a284efb0f0756c890be9b1b5dbdd8ee81a3655f83e33b2279d39bf3e848279a722c806b485a47e67c807b946a337bee8942674278859e13292fb"

  testcase4 = ( map chr [1..25::Int] , replicate 50 '\xcd' , hmac256 , hmac512 ) where
    hmac256 = "82558a389a443c0ea4cc819899f2083a85f0faa3e578f8077a2e3ff46729665b"
    hmac512 = "b0ba465637458c6990e5a8c5f61d4af7e576d97ff94b872de76f8050361ee3dba91ca5c11aa25eb4d679275cc5788063a5f19741120c4f2de2adebeb10a298dd"

  testcase5 = ( replicate 20 '\x0c' , "Test With Truncation" , hmac256 , hmac512 ) where
    hmac256_truncated
            = "a3b6167473100ee06e0c796c2955552b"
    hmac256 = "a3b6167473100ee06e0c796c2955552bfa6f7c0a6a8aef8b93f860aab0cd20c5"
    hmac512_truncated 
            = "415fad6271580a531d4179bc891d87a6"  -- this is a truncation test, wtf. well, i don't care about truncation :)
    hmac512 = "415fad6271580a531d4179bc891d87a650188707922a4fbb36663a1eb16da008711c5b50ddd0fc235084eb9d3364a1454fb2ef67cd1d29fe6773068ea266e96b"

  testcase6 = ( replicate 131 '\xaa' , msg , hmac256 , hmac512 ) where 
    msg = concat 
     [ ("Test Using Large")
     , ("r Than Block-Siz")
     , ("e Key - Hash Key")
     , (" First") 
     ]
    hmac256 = "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54"
    hmac512 = "80b24263c7c1a3ebb71493c1dd7be8b49b46d1f41b4aeec1121b013783f8f3526b56d037e05f2598bd0fd2215d6a1e5295e64f73f63f0aec8b915a985d786598"

  testcase7 = ( replicate 131 '\xaa' , msg , hmac256 , hmac512 ) where 
    msg = concat 
      [ ("This is a test u")
      , ("sing a larger th")
      , ("an block-size ke")
      , ("y and a larger t")
      , ("han block-size d")
      , ("ata. The key nee")
      , ("ds to be hashed ")
      , ("before being use")
      , ("d by the HMAC al")
      , ("gorithm.")
      ]
    hmac256 = "9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2"
    hmac512 = "e37b6a775dc87dbaa4dfa9f96e5e3ffddebd71f8867289865df5a32d20cdc944b6022cac3c4982b10d5eeb55c3e4de15134676fb6de0446065c97440fa8c6a58"

-}
