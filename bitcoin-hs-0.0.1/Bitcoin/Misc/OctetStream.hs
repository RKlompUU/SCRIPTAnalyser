
-- | Conversion between different "octet stream" formats for convenience.
--
-- TODO: utf8 handling for text

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Bitcoin.Misc.OctetStream where

--------------------------------------------------------------------------------

import Control.Monad ( liftM )

import Data.Char ( ord , chr )
import Data.Word

import qualified Data.ByteString as B

import Bitcoin.Misc.BigInt

--------------------------------------------------------------------------------

class OctetStream a where

  toByteString     :: a -> B.ByteString
  toWord8List      :: a -> [Word8]
  unsafeToCharList :: a -> [Char]
  toIntegerLE      :: a -> Integer
  toIntegerBE      :: a -> Integer

  fromByteString :: B.ByteString -> a
  fromWord8List  :: [Word8] -> a
  fromCharList   :: [Char]  -> a      
  fromIntegerLE  :: Integer -> a
  fromIntegerBE  :: Integer -> a

  toByteString     = B.pack . toWord8List
  toWord8List      = B.unpack . toByteString
  unsafeToCharList = map word8_to_char . toWord8List 
  toIntegerLE      = littleEndianRollInteger . toWord8List
  toIntegerBE      = bigEndianRollInteger    . toWord8List   

  fromByteString = fromWord8List  . B.unpack
  fromWord8List  = fromByteString . B.pack
  fromCharList   = fromWord8List . map char_to_word8
  fromIntegerLE  = fromWord8List . littleEndianUnrollInteger
  fromIntegerBE  = fromWord8List . bigEndianUnrollInteger

--------------------------------------------------------------------------------

instance OctetStream B.ByteString where
  toByteString   = id
  fromByteString = id
  toWord8List    = B.unpack
  fromWord8List  = B.pack
  
instance OctetStream [Word8] where
  toWord8List   = id
  fromWord8List = id
  toByteString   = B.pack
  fromByteString = B.unpack

-- | Note: we treat String as an ASCII string here, no fancy utf8 here (it's in the TODO)
instance OctetStream String where
  unsafeToCharList = id
  fromCharList     = id
  toWord8List      = map char_to_word8
  fromWord8List    = map word8_to_char
  toByteString     = B.pack . map char_to_word8
  fromByteString   = map word8_to_char . B.unpack

--------------------------------------------------------------------------------

{-# INLINE word8_to_char #-}
word8_to_char :: Word8 -> Char
word8_to_char = chr . fromIntegral

{-# INLINE char_to_word8 #-}
char_to_word8 :: Char -> Word8
char_to_word8 = fromIntegral . ord

--------------------------------------------------------------------------------
