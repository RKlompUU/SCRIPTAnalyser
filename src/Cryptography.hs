module Cryptography where

import Crypto.Hash

import qualified Data.ByteString.Lazy as BS

import qualified Data.ByteString.Base16.Lazy as BS16L

hash256 :: BS.ByteString -> BS.ByteString
hash256 = sha256 . sha256

hash160 :: BS.ByteString -> BS.ByteString
hash160 = ripemd160 . sha256

sha256 :: BS.ByteString -> BS.ByteString
sha256 bs =
  BS.fromStrict
  $ digestToHexByteString
  $ (hashlazy (fst $ BS16L.decode bs) :: Digest SHA256)

ripemd160 :: BS.ByteString -> BS.ByteString
ripemd160 bs =
  BS.fromStrict
  $ digestToHexByteString
  $ (hashlazy (fst $ BS16L.decode bs) :: Digest RIPEMD160)
