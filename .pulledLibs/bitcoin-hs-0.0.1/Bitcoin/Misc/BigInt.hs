
-- | Encoding and decoding of big /natural/ numbers as ByteString or [Word8]

module Bitcoin.Misc.BigInt where

--------------------------------------------------------------------------------

import Data.Word
import Data.Bits
import Data.List ( unfoldr )

--------------------------------------------------------------------------------
-- * encoding/decoding nonnegative integers

bigEndianRollInteger :: [Word8] -> Integer
bigEndianRollInteger = littleEndianRollInteger . reverse

littleEndianRollInteger :: [Word8] -> Integer
littleEndianRollInteger = foldr unstep 0 where
  unstep b a = shiftL a 8 .|. fromIntegral b

bigEndianUnrollInteger :: Integer -> [Word8]
bigEndianUnrollInteger = reverse . littleEndianUnrollInteger

littleEndianUnrollInteger :: Integer -> [Word8]
littleEndianUnrollInteger = unfoldr step where
  step 0 = Nothing
  step i = Just (fromIntegral i, shiftR i 8)

--------------------------------------------------------------------------------

-- | Always 32 byte long (if the integer was less than @2^256@)
bigEndianInteger32 :: Integer -> [Word8]
bigEndianInteger32 = extend32 . bigEndianUnrollInteger where
  extend32 what = replicate (32-n) 0 ++ what where n = length what

littleEndianInteger32 :: Integer -> [Word8]
littleEndianInteger32 = extend32 . littleEndianUnrollInteger where
  extend32 what = what ++ replicate (32-n) 0 where n = length what

--------------------------------------------------------------------------------

-- | Always 20 byte long (if the integer was less than @2^160@)
bigEndianInteger20 :: Integer -> [Word8]
bigEndianInteger20 = extend20 . bigEndianUnrollInteger where
  extend20 what = replicate (20-n) 0 ++ what where n = length what

littleEndianInteger20 :: Integer -> [Word8]
littleEndianInteger20 = extend20 . littleEndianUnrollInteger where
  extend20 what = what ++ replicate (20-n) 0 where n = length what

--------------------------------------------------------------------------------

