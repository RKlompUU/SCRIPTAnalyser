
-- | The finite field Fn of n elements, where n is the order of the elliptic curve secp256k1 (which happens to be a prime).
-- 
-- Naive implementation.
-- 
{-# LANGUAGE BangPatterns #-}
module Bitcoin.Crypto.FiniteField.Naive.Fn where

--------------------------------------------------------------------------------

import Prelude hiding ( sqrt )

import Data.Char
import Data.Bits
import Data.Word
import Data.Maybe

--------------------------------------------------------------------------------

secp256k1_n :: Integer
secp256k1_n = 115792089237316195423570985008687907852837564279074904382605163141518161494337

--------------------------------------------------------------------------------
-- * for the signatures, we also need mod n arithmetic, not only mod p...

-- | The prime field Fn (n seems to be a prime, after all)
newtype Fn = Fn Integer deriving (Eq,Show)

toFn :: Integer -> Fn
toFn x = Fn (modn x)

fromFn :: Fn -> Integer
fromFn (Fn x) = x 

instance Num Fn where
  Fn x + Fn y   = Fn $ modn (x+y)
  Fn x - Fn y   = Fn $ modn (x-y)
  Fn x * Fn y   = Fn $ modn (x*y)
  negate (Fn x) = Fn $ modn (secp256k1_n - x)
  fromInteger   = Fn . modn 
  abs    = error "Fn/abs"
  signum = error "Fn/signum"

instance Fractional Fn where
  Fn x / Fn y  = Fn $ modn ( x * invFn y )
  recip (Fn x) = Fn $ invFn x
  fromRational = error "Fn/fromRational"

pow_n :: Fn -> Fn -> Fn
pow_n (Fn b) (Fn e) = Fn (powFn b e)

--------------------------------------------------------------------------------

-- | helper function: Modulo n
modn :: Integer -> Integer
modn !a = mod a secp256k1_n

-- | Multiplicative inverse in Fp
invFn :: Integer -> Integer 
invFn !a = powFn a (secp256k1_n - 2)

-- | (Fast) exponentiation in Fn
powFn :: Integer -> Integer -> Integer
powFn !base !exp = go 1 base exp where
  go !acc _  0  = acc
  go !acc !b !e = if (e .&. 1 > 0)
    then go (modn (acc*b)) (modn (b*b)) (shiftR e 1)
    else go        acc     (modn (b*b)) (shiftR e 1)

--------------------------------------------------------------------------------

