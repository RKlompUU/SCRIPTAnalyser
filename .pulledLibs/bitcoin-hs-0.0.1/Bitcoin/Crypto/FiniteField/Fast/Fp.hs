
-- | Finite field of order p, where p is the prime parameter of the
-- secp256k1 elliptic curve. Relatively fast arithmetic (written in C) 
--
-- Should work on both little-endian and big-endian architectures,
-- but only tested on little-endian.

{-# LANGUAGE CPP, ForeignFunctionInterface, BangPatterns #-}
module Bitcoin.Crypto.FiniteField.Fast.Fp where

--------------------------------------------------------------------------------

import Data.Word
import Data.Bits

import Data.List ( unfoldr )

import Foreign
import Foreign.C

import qualified System.IO.Unsafe as Unsafe
import qualified Data.ByteString  as B

import Bitcoin.Crypto.Word256

--------------------------------------------------------------------------------

newtype Fp = Fp { unFp :: Word256 } deriving (Eq,Show)

fpDecimal :: Fp -> String
fpDecimal = word256Decimal . unFp

fpHex :: Fp -> String
fpHex = word256Hex . unFp

--------------------------------------------------------------------------------

toFp :: Integer -> Fp
toFp = Fp . toWord256 . modp

fromFp :: Fp -> Integer
fromFp = fromWord256 . unFp

--------------------------------------------------------------------------------

-- | Converts to a little-endian bytestring
fpToByteStringLE :: Fp -> B.ByteString
fpToByteStringLE = word256ToByteStringLE . unFp

-- | Converts to a big-endian bytestring
fpToByteStringBE :: Fp -> B.ByteString
fpToByteStringBE = word256ToByteStringBE . unFp

-- | Converts to a little-endian sequence of bytes
fpToWord8ListLE :: Fp -> [Word8]
fpToWord8ListLE = word256ToWord8ListLE . unFp

--------------------------------------------------------------------------------

foreign import ccall unsafe "c_modp.c neg_modp" c_neg_modp :: Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "c_modp.c inv_modp_power"    c_inv_modp_power    :: Ptr Word32 -> Ptr Word32 -> IO ()
foreign import ccall unsafe "c_modp.c inv_modp_pow_spec" c_inv_modp_pow_spec :: Ptr Word32 -> Ptr Word32 -> IO ()
foreign import ccall unsafe "c_modp.c inv_modp_euclid"   c_inv_modp_euclid   :: Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "c_modp.c add_modp"   c_add_modp   :: Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO ()
foreign import ccall unsafe "c_modp.c sub_modp"   c_sub_modp   :: Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO ()
foreign import ccall unsafe "c_modp.c mul_modp"   c_mul_modp   :: Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO ()
foreign import ccall unsafe "c_modp.c div_modp"   c_div_modp   :: Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO ()
foreign import ccall unsafe "c_modp.c pow_modp"   c_pow_modp   :: Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO ()

-- foreign import ccall unsafe "c_modp.c shiftl32_modp" c_shiftl32_modp :: Ptr Word32 -> Ptr Word32 -> IO ()
-- foreign import ccall unsafe "c_modp.c scale_modp"    c_scale_modp    :: Ptr Word32 ->     Word32 -> Ptr Word32 -> IO ()

--------------------------------------------------------------------------------

neg_modp :: Word256 -> Word256
neg_modp a = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 c $ \pc -> c_neg_modp pa pc
  return c

inv_modp_power :: Word256 -> Word256
inv_modp_power a = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 c $ \pc -> c_inv_modp_power pa pc
  return c

inv_modp_pow_spec :: Word256 -> Word256
inv_modp_pow_spec a = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 c $ \pc -> c_inv_modp_pow_spec pa pc
  return c

inv_modp_euclid :: Word256 -> Word256
inv_modp_euclid a = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 c $ \pc -> c_inv_modp_euclid pa pc
  return c

add_modp :: Word256 -> Word256 -> Word256
add_modp a b = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 b $ \pb -> withWord256 c $ \pc -> c_add_modp pa pb pc
  return c

sub_modp :: Word256 -> Word256 -> Word256
sub_modp a b = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 b $ \pb -> withWord256 c $ \pc -> c_sub_modp pa pb pc
  return c

mul_modp :: Word256 -> Word256 -> Word256
mul_modp a b = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 b $ \pb -> withWord256 c $ \pc -> c_mul_modp pa pb pc
  return c

div_modp :: Word256 -> Word256 -> Word256
div_modp a b = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 b $ \pb -> withWord256 c $ \pc -> c_div_modp pa pb pc
  return c

pow_modp :: Word256 -> Word256 -> Word256
pow_modp a b = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 b $ \pb -> withWord256 c $ \pc -> c_pow_modp pa pb pc
  return c

--------------------------------------------------------------------------------

{-
scale_modp :: Word256 -> Word32 -> Word256
scale_modp a b = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 c $ \pc -> c_scale_modp pa b pc
  return c
-}

--------------------------------------------------------------------------------

instance Num Fp where
  Fp a + Fp b = Fp (add_modp a b)
  Fp a - Fp b = Fp (sub_modp a b)
  Fp a * Fp b = Fp (mul_modp a b)
  negate (Fp a) = Fp (neg_modp a)
  fromInteger = toFp
  abs      = id
  signum _ = Fp (toWord256 1)

instance Fractional Fp where
  Fp a / Fp b  = Fp (div_modp a b)
  recip (Fp a) = Fp (inv_modp_euclid a)
  fromRational = error "Fp/fromRational: does not make much sense"

{-
-- | Fake instance for Integral
instance Real Fp where
  toRational = toRational . fromFp

-- | Fake instance for Integral
instance Enum Fp where
  toEnum   = error "Enum/Fp/toEnum: Enum range (Int) is too small for 256 bits..."
  fromEnum = error "Enum/Fp/fromEnum: Enum range (Int) is too small for 256 bits..."

-- | Well it's a field so division is always exact, but we need toInteger
instance Integral Fp where
  toInteger = fromFp
  div  a b = div_modp a b
  quot a b = div_modp a b
  rem a b = 0
  mod a b = 0
-}
  
pow_p :: Fp -> Word256 -> Fp
pow_p (Fp a) b = Fp (pow_modp a b) 

-- | Note that this gives only one of the possibly two square roots
sqrt_p :: Fp -> Maybe Fp
sqrt_p (Fp x2) = case sqrtFp x2 of
  Nothing -> Nothing
  Just y  -> Just (Fp y)

--------------------------------------------------------------------------------
-- * square root in Fp

secp256k1_ndiv4   = toWord256 $ 28948022309329048855892746252171976963317496166410141009864396001977208667915
secp256k1_ndiv4p1 = toWord256 $ 28948022309329048855892746252171976963317496166410141009864396001977208667916

-- | (One of the) square roots mod p (if any exists). Since p is a prime and @p = 4k+3@, 
-- we have a fortunately a very easy solution by some quadratic reciprocity stuff I don't 
-- remember how exactly works
-- (but it's elementary number theory)
--
-- <http://course1.winona.edu/eerrthum/13Spring/SquareRoots.pdf>
--
unsafeSqrtFp :: Word256 -> Word256
unsafeSqrtFp !a = pow_modp a secp256k1_ndiv4p1

-- | Note that square roots do not always exist in Fp: consider for example p=7,
-- then 3, 5 and 6 do not have square roots, while the rest has two (except 0).
-- 
-- In general, if x is a square root then so is (p-x), since 
--
-- > (p-x)*(p-x) = p*p - p*(2*x) + x*x = x*x (mod p)
--
-- And that should be all solutions, since it's a quadratic equation.
--
sqrtFp :: Word256 -> Maybe Word256
sqrtFp x2 = if mul_modp x x == x2 then Just x else Nothing 
  where x = unsafeSqrtFp x2

--------------------------------------------------------------------------------
-- * useful

secp256k1_p :: Integer
secp256k1_p = 115792089237316195423570985008687907853269984665640564039457584007908834671663

instance Bounded Fp where
  minBound = toFp 0
  maxBound = toFp (secp256k1_p - 1)

modp :: Integer -> Integer
modp n = mod n secp256k1_p 

--------------------------------------------------------------------------------
