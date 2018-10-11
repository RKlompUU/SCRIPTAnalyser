
-- | The finite field Fp of p elements, where p is the prime parameter of the elliptic curve secp256k1.
-- 
-- Naive implementation.
-- 
{-# LANGUAGE CPP, BangPatterns #-}
module Bitcoin.Crypto.FiniteField.Naive.Fp where

--------------------------------------------------------------------------------

import Control.Monad

import Prelude hiding ( sqrt )

import Data.Char
import Data.Bits
import Data.Word
import Data.Maybe

--------------------------------------------------------------------------------

-- import Debug.Trace
-- debug x y = trace (">>> " ++ show x) y

--------------------------------------------------------------------------------

secp256k1_p :: Integer
secp256k1_p  = 115792089237316195423570985008687907853269984665640564039457584007908834671663

--------------------------------------------------------------------------------
-- * Operations in the finite field Fp (p being the secp256k1 curve's p)

-- | helper function: Modulo p 
modp :: Integer -> Integer
modp !a = mod a secp256k1_p

addFp :: Integer -> Integer -> Integer
addFp !a !b = modp (a+b)

subFp :: Integer -> Integer -> Integer
subFp !a !b = modp (a-b)

-- | Negation in Fp
negFp :: Integer -> Integer
negFp !a = modp (secp256k1_p - a)

-- | Multiplication in Fp
mulFp :: Integer -> Integer -> Integer
mulFp !a !b = modp (a*b)

-- | Square in Fp
sqrFp :: Integer -> Integer
sqrFp !a = modp (a*a)

-- | Division in Fp
divFp :: Integer -> Integer -> Integer
divFp !a !b = mulFp a (invFp b)

--------------------------------------------------------------------------------

-- | Multiplicative inverse in Fp
invFp :: Integer -> Integer 
invFp = invFp_euclid

-- | Inverse using the power function (slower)
invFp_pow :: Integer -> Integer 
invFp_pow !a = powFp a (secp256k1_p - 2)

-- | Inverse using a specialized power function (should about 2x faster than 'invFp_pow')
--
-- This uses the fact that
--
-- > p-2 == 45 + 1024 * (1023 + 1024 * (1023 + 1024 * (1019 + 1024 * ( ... * 63 ) ) )
--
-- Since on the exponential level, both doubling and multiplying has the same cost,
-- additions and doubling in this type of representation of @p-2@ has the same cost.
-- And because in @p-2@ almost all bits are set, the standard binary power function
-- is very far from being optimal.
-- 
-- Total number of operations is 
--
-- > 16 + (21+1+2+1)*(10+1) = 291
--
-- instead of almost @512@.
--
invFp_pow_spec :: Fp -> Fp
invFp_pow_spec a1 = inv where
 
  dbl :: Fp -> Fp
  dbl x = x*x

  pow1024 :: Fp -> Fp
  pow1024 = dbl . dbl . dbl . dbl . dbl 
          . dbl . dbl . dbl . dbl . dbl

  iter :: Int -> (Fp -> Fp) -> Fp -> Fp
  iter n f = go n where
    go !n !x = case n of
      0 -> x
      _ -> go (n-1) (f x)

  inv =          (\x -> a45   * pow1024 x)
      $! iter  2 (\x -> a1023 * pow1024 x)
      $!         (\x -> a1019 * pow1024 x)
      $! iter 21 (\x -> a1023 * pow1024 x)
      $! a63

  a2    = a1    * a1
  a3    = a2    * a1
  a4    = a2    * a2
  a5    = a4    * a1
  a10   = a5    * a5

  a11   = a10   * a1
  a21   = a11   * a10
  a42   = a21   * a21
  a45   = a42   * a3
  a63   = a42   * a21

  a126  = a63   * a63
  a252  = a126  * a126
  a504  = a252  * a252
  a1008 = a504  * a504
  a1019 = a1008 * a11
  a1023 = a1019 * a4

-- | Inverse using the binary Euclidean algorithm (faster)
invFp_euclid :: Integer -> Integer 
invFp_euclid a 
  | a == 0     = 0
  | otherwise  = go 1 0 a secp256k1_p
  where

    go :: Integer -> Integer -> Integer -> Integer -> Integer
    go !x1 !x2 !u !v 
      | u==1       = x1
      | v==1       = x2
      | otherwise  = stepU x1 x2 u v

    stepU :: Integer -> Integer -> Integer -> Integer -> Integer
    stepU !x1 !x2 !u !v = if even u 
      then let u'  = shiftR u 1
               x1' = shiftR (if even x1 then x1 else x1 + secp256k1_p) 1                
           in  stepU x1' x2 u' v
      else     stepV x1  x2 u  v

    stepV :: Integer -> Integer -> Integer -> Integer -> Integer
    stepV !x1 !x2 !u !v = if even v
      then let v'  = shiftR v 1
               x2' = shiftR (if even x2 then x2 else x2 + secp256k1_p) 1               
           in  stepV x1 x2' u v' 
      else     final x1 x2  u v

    final :: Integer -> Integer -> Integer -> Integer -> Integer
    final !x1 !x2 !u !v = if u>=v

      then let u'  = u-v
               x1' = modp (x1-x2)               
           in  go x1' x2  u' v 

      else let v'  = v-u
               x2' = modp (x2-x1)               
           in  go x1  x2' u  v'

--------------------------------------------------------------------------------

-- | (Fast) exponentiation in Fp
powFp :: Integer -> Integer -> Integer
powFp !base !exp = go 1 base exp where
  go !acc _  0  = acc
  go !acc !b !e = if (e .&. 1 > 0)
    then go (modp (acc*b)) (modp (b*b)) (shiftR e 1)
    else go        acc     (modp (b*b)) (shiftR e 1)

--------------------------------------------------------------------------------
-- * square root in Fp

secp256k1_ndiv4   = 28948022309329048855892746252171976963317496166410141009864396001977208667915
secp256k1_ndiv4p1 = 28948022309329048855892746252171976963317496166410141009864396001977208667916

-- | (One of the) square roots mod p (if any exists). Since p is a prime and @p = 4k+3@, 
-- we have a fortunately a very easy solution by some quadratic reciprocity stuff I don't 
-- remember how exactly works
-- (but it's elementary number theory)
--
-- <http://course1.winona.edu/eerrthum/13Spring/SquareRoots.pdf>
--
unsafeSqrtFp :: Integer -> Integer
unsafeSqrtFp !a = powFp (modp a) secp256k1_ndiv4p1

-- | Note that square roots do not always exist in Fp: consider for example p=7,
-- then 3, 5 and 6 do not have square roots, while the rest has two (except 0).
-- 
-- In general, if x is a square root then so is (p-x), since 
--
-- > (p-x)*(p-x) = p*p - p*(2*x) + x*x = x*x (mod p)
--
-- And that should be all solutions, since it's a quadratic equation.
--
sqrtFp :: Integer -> Maybe Integer
sqrtFp x2 = if mulFp x x == modp x2 then Just x else Nothing 
  where x = unsafeSqrtFp x2

--------------------------------------------------------------------------------

-- | For simpler (and safer) code, we introduce a newtype for elements of Fp
newtype Fp = Fp { unFp :: Integer } deriving (Eq,Show)

toFp :: Integer -> Fp
toFp x = Fp (modp x)

fromFp :: Fp -> Integer
fromFp (Fp x) = x 

instance Num Fp where
  Fp x + Fp y   = Fp (addFp x y)
  Fp x - Fp y   = Fp (subFp x y)
  Fp x * Fp y   = Fp (mulFp x y)
  negate (Fp x) = Fp (negFp x)
  fromInteger   = Fp . modp
  abs    = error "Fp/abs"
  signum = error "Fp/signum"

instance Fractional Fp where
  Fp x / Fp y  = Fp (divFp x y)
  recip (Fp x) = Fp (invFp x)
  fromRational = error "Fp/fromRational: does not make much sense"

-- | Note that this gives only one of the possibly two square roots
sqrt_p :: Fp -> Maybe Fp
sqrt_p (Fp x2) = case sqrtFp x2 of
  Nothing -> Nothing
  Just y  -> Just (Fp y)

-- pow_p :: Fp -> Fp -> Fp
-- pow_p (Fp b) (Fp e) = Fp (powFp b e)

pow_p :: Fp -> Integer -> Fp
pow_p (Fp b) e = Fp (powFp b e)

--------------------------------------------------------------------------------
