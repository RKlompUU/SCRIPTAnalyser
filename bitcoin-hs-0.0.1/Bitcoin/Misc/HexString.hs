
-- | Encoding and decoding hex strings

{-# LANGUAGE EmptyDataDecls #-}
module Bitcoin.Misc.HexString where 

--------------------------------------------------------------------------------

import Data.Array 

import Data.Char
import Data.Int
import Data.Word
import Data.Bits

-- import Control.Monad ( liftM )

import Bitcoin.Misc.OctetStream

--------------------------------------------------------------------------------

-- | The phantom type is used to encode endianness
newtype HexString = HexString { unHexString :: String } deriving (Eq,Show)

--------------------------------------------------------------------------------

toHexString :: OctetStream a => a -> HexString
toHexString = toHexString' False

toHexString' :: OctetStream a => Bool -> a -> HexString
toHexString' uppercase x = hexEncode' uppercase (toWord8List x)

toHexStringChars :: OctetStream a => a -> String
toHexStringChars = unHexString . toHexString

fromHexString :: OctetStream a => HexString -> a
fromHexString  = fromWord8List . hexDecode

--------------------------------------------------------------------------------

reverseHexString :: HexString -> HexString
reverseHexString = HexString . unsafeReverseHexString . unHexString

unsafeReverseHexString :: String -> String
unsafeReverseHexString = fromPairs . reverse . toPairs where
  toPairs :: [Char] -> [(Char,Char)]
  toPairs (x:y:rest) = (x,y) : toPairs rest
  toPairs [] = []  
  toPairs _  = error "unsafeReverseHexString/toPairs: odd number of characters"
  fromPairs :: [(Char,Char)] -> [Char]
  fromPairs = concatMap (\(x,y) -> [x,y])

--------------------------------------------------------------------------------
-- * encoding and decoding ByteStrings as (little-endian) hex strings

safeHexDecode :: String -> Maybe [Word8]
safeHexDecode s = if even (length s) && all isHexDigit s then Just (go s) else Nothing where 

  go (x:y:rest) = (shiftL (f x) 4 + f y) : go rest
  go [] = []
  go [x] = error "hexDecode: expecting even number of characters"

  f :: Char -> Word8
  f c | c >= '0' && c <= '9'  =  ordW c - 48
      | c >= 'A' && c <= 'F'  =  ordW c - 65 + 10
      | c >= 'a' && c <= 'f'  =  ordW c - 97 + 10
      | otherwise             =  error "hexDecode: unexpected character"

  ordW :: Char -> Word8
  ordW = fromIntegral . ord

{- 
-- already implemented in Data.Char
isHexDigit :: Char -> Bool
isHexDigit c =  (c >= '0' && c <= '9') 
             || (c >= 'a' && c <= 'f') 
             || (c >= 'A' && c <= 'F') 
-}
 
--------------------------------------------------------------------------------

-- | from "4142" to [0xAB]
hexDecode :: HexString -> [Word8]
hexDecode (HexString s) = case safeHexDecode s of
  Just ws -> ws
  Nothing -> error "hexDecode: input is not a hex string"

-- | from [0xAB] to "4142"
hexEncode :: [Word8] -> HexString 
hexEncode = hexEncode' False

hexEncode' :: Bool -> [Word8] -> HexString 
hexEncode' useCapitalLetters = HexString . concatMap worker where
  worker :: Word8 -> String
  worker w = [ table ! (fromIntegral $ shiftR w 4) , table ! (fromIntegral $ w .&. 15) ]
  table = if useCapitalLetters then capitalHexTable else smallHexTable

showHexWord8 :: Word8 -> String
showHexWord8 w = [ smallHexTable ! (fromIntegral $ shiftR w 4) , smallHexTable ! (fromIntegral $ w .&. 15) ]

capitalHexTable, smallHexTable :: Array Word8 Char
capitalHexTable = listArray (0,15) "0123456789ABCDEF"
smallHexTable   = listArray (0,15) "0123456789abcdef"

--------------------------------------------------------------------------------

