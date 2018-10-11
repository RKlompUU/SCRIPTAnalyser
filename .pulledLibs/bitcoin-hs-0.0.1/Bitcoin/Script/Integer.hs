
-- | Numbers in Bitcoin scripts

{-# LANGUAGE PatternGuards #-}
module Bitcoin.Script.Integer where

--------------------------------------------------------------------------------

import Data.Int
import Data.Word
import Data.Bits
import Data.List ( unfoldr , splitAt , mapAccumL )
import Data.Maybe

import Control.Monad
import Control.Applicative

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Bitcoin.Misc.BigInt
import Bitcoin.Misc.OctetStream

--------------------------------------------------------------------------------
-- * signs

-- | Positive actually means non-negative here, but it looks better (and is easier to read) this way
data Sign = Positive | Negative  

signOf :: Integer -> Sign
signOf n = if n<0 then Negative else Positive

toSignAbs :: Integer -> (Sign,Integer)
toSignAbs n = (signOf n, abs n)

fromSignAbs :: (Sign,Integer) -> Integer
fromSignAbs (sign,absn) = case sign of
  Positive -> absn
  Negative -> negate absn

--------------------------------------------------------------------------------
-- * Bitcoin's special ByteString <-> Integer conversion

-- | Byte vectors are interpreted as little-endian variable-length integers 
-- with the most significant bit determining the sign of the integer. Thus 
-- 0x81 represents -1. 0x80 is another representation of zero (so called 
-- negative 0). Byte vectors are interpreted as Booleans where False is 
-- represented by any representation of zero, and True is represented by 
-- any representation of non-zero.
asInteger :: B.ByteString -> Integer
asInteger bs = 
  case sign of
    Positive -> absn
    Negative -> negate absn
  where
    absn = littleEndianRollInteger ws
    (sign,ws) = decodeSign (B.unpack bs)

asByteString :: Integer -> B.ByteString
asByteString 0 = B.empty
asByteString n = B.pack $ encodeSign (signOf n) $ littleEndianUnrollInteger (abs n)

encodeSign :: Sign -> [Word8] -> [Word8]
encodeSign sign = go where
  go ws = case ws of
    (x:rest@(y:_)) -> x : go rest
    [last]         -> if last < 0x80 
                        then case sign of { Positive -> [last] ; Negative -> [last + 0x80] }
                        else last : go []
    []             -> [ case sign of { Positive -> 0 ; Negative -> 0x80 } ]

decodeSign :: [Word8] -> (Sign,[Word8])
decodeSign = go where
  go ws = case ws of
    (x:rest@(y:_)) -> let (sign,xs) = go rest in (sign,x:xs)
    [last]         -> if last < 0x80 
                        then (Positive, [last     ] )
                        else (Negative, [last-0x80] )
    []             -> ( Positive , [] )

--------------------------------------------------------------------------------
