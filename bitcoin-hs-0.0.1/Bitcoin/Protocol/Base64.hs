
-- | Base64 encoding/decoding (used for signatures for example)

module Bitcoin.Protocol.Base64 
  ( Base64(..) 
  , base64Encode, base64Decode
  , testCases , testErrors
  ) where

--------------------------------------------------------------------------------

import Control.Monad ( liftM )

import Data.Char ( isSpace )
import Data.Word
import Data.Bits
import Data.List ( unfoldr )
import Data.Maybe

import Data.Array.IArray
import Data.Array.Unboxed

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.ByteString as B

import Bitcoin.Misc.OctetStream

--------------------------------------------------------------------------------

alphabet :: UArray Word8 Char
alphabet = array (0,63) $ zip [0..63] stringAlphabet 

reverseAlphabet :: Map Char Word8
reverseAlphabet = Map.fromList $ zip stringAlphabet [0..63]

stringAlphabet :: [Char]
stringAlphabet = ['A'..'Z'] ++ ['a'..'z'] ++ "0123456789+/"

--------------------------------------------------------------------------------

newtype Base64 = Base64 { unBase64 :: String } deriving (Eq,Show)

base64Encode :: OctetStream a => a -> Base64
base64Encode = Base64 . concatMap worker . partition . toWord8List where
  worker [a,b,c] = map lkp [ shiftR a 2 
                           , shiftL (a .&. 3)  4 + shiftR b 4
                           , shiftL (b .&. 15) 2 + shiftR c 6 
                           , c .&. 63 
                           ] 
  worker [a,b  ] = take 3 (worker [a,b,0]) ++ "="
  worker [a    ] = take 2 (worker [a,0,0]) ++ "=="

  lkp :: Word8 -> Char
  lkp j = alphabet ! j

  -- partition into three-byte units
  partition :: [Word8] -> [[Word8]]
  partition [] = []
  partition xs = take 3 xs : partition (drop 3 xs)

--------------------------------------------------------------------------------
  
base64Decode :: OctetStream a => Base64 -> Maybe a
base64Decode = liftM fromByteString . returnMaybe . map worker . partition . filter (not . isSpace) . unBase64 where

  worker :: [Char] -> Maybe [Word8]
  worker abcd = case abcd of
    [a,b,'=','='] -> liftMaybe (take 1) $ worker [a,b,'A','A']
    [a,b,c,'=']   -> liftMaybe (take 2) $ worker [a,b,c,'A']
    [a,b,c,d]     -> if all isJust mws then Just [p,q,r] else Nothing where
      mws = map lkp abcd
      [u,v,w,z] = map fromJust mws
      p = shiftL  u         2 + shiftR v 4
      q = shiftL (v .&. 15) 4 + shiftR w 2
      r = shiftL (w .&.  3) 6 + z
    _ -> Nothing
  
  returnMaybe :: [Maybe [Word8]] -> Maybe B.ByteString
  returnMaybe mws = if all isJust mws 
    then Just $ B.pack $ concatMap fromJust mws
    else Nothing
    
  lkp :: Char -> Maybe Word8 
  lkp c = Map.lookup c reverseAlphabet

  -- partition into four-byte units
  partition :: [Char] -> [[Char]]
  partition [] = []
  partition xs = take 4 xs : partition (drop 4 xs)

  liftMaybe :: ([Word8] -> [Word8]) -> (Maybe [Word8] -> Maybe [Word8])
  liftMaybe f mb = case mb of { Just xs -> Just (f xs) ; Nothing -> Nothing }
  
--------------------------------------------------------------------------------

testErrors :: [String]
testErrors = testErrorsEncode ++ testErrorsDecode

testErrorsEncode :: [String]
testErrorsEncode = concatMap (\(raw,base64) -> if (Base64 base64 == base64Encode raw) then [] else [raw]) testCases

testErrorsDecode :: [String]
testErrorsDecode = concatMap (\(raw,base64) -> if (Just raw == base64Decode (Base64 base64)) then [] else [raw]) testCases

testCases :: [(String,String)]
testCases =
  [ ("any carnal pleasure.", "YW55IGNhcm5hbCBwbGVhc3VyZS4=" )
  , ("any carnal pleasure" , "YW55IGNhcm5hbCBwbGVhc3VyZQ==" )
  , ("any carnal pleasur"  , "YW55IGNhcm5hbCBwbGVhc3Vy" )
  , ("any carnal pleasu"   , "YW55IGNhcm5hbCBwbGVhc3U=" )
  , ("any carnal pleas"    , "YW55IGNhcm5hbCBwbGVhcw==" )
  , ("pleasure."     , "cGxlYXN1cmUu" )
  , ("leasure."      , "bGVhc3VyZS4=" )
  , ("easure."       , "ZWFzdXJlLg==" )
  , ("asure."        , "YXN1cmUu" )
  , ("sure."         , "c3VyZS4=" )
  ]
  
--------------------------------------------------------------------------------
  