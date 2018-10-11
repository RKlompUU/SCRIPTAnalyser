
-- | Using (weighted) projective coordinates on the curve we can maybe avoid the division bottleneck.
--
-- Based on: Chae Hoon Lim, Hyo Sun Hwang: Fast implementation of Elliptic Curve arithmetic in GF(p\^n).
--
-- We will use (2,3,1) weighting, and a constant factor of 2 in Y:
--
-- > x = X/Z^2 
-- > y = Y/(2*Z^3)
-- > z = 1
--
-- Thus the curve equation @y^2 = x^3 + 7@ becomes
--
-- > Y^2/4 = X^3 + 7*Z^6
--
-- and then the infinity point on the curve is @(1,2,0)@.
--
--

{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
module Bitcoin.Crypto.EC.Projective where

--------------------------------------------------------------------------------

import Data.Bits

import Bitcoin.Crypto.FiniteField.Fast.Fp  hiding ( secp256k1_p )
import Bitcoin.Crypto.FiniteField.Naive.Fn hiding ( secp256k1_n )

import Bitcoin.Crypto.EC.Curve

-- C stuff
import Data.Word
import Foreign
import System.IO.Unsafe as Unsafe
import Bitcoin.Crypto.Word256

--------------------------------------------------------------------------------

-- | Note: the "Eq" instance is equality of all coordinates, not equality on the projective plane (for that, use "(=~=)" instead)
data ECProj = ECProj !Fp !Fp !Fp deriving (Eq,Show)

toECProj :: ECPoint -> ECProj
toECProj ep = case ep of
  ECPoint x y -> ECProj x (y+y) 1
  ECInfinity  -> ECProj 1 2 0       -- vertical infinity

fromECProj :: ECProj -> ECPoint
fromECProj (ECProj x@(Fp xrep) y@(Fp yrep) z@(Fp zrep)) = 
  if zrep /= 0
    then ECPoint (x/z2) (y/(z3+z3)) 
    else if 2*xrep == yrep      
      then ECInfinity
      else error "fromECProj: infinity not on the curve"
  where 
    z2 = z*z 
    z3 = z*z2

--------------------------------------------------------------------------------

foreign import ccall unsafe "c_ec.c c_addECP" c_addECP_ :: Ptr Word32 -> Ptr Word32 -> Ptr Word32 
                                                        -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 
                                                        -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "c_ec.c c_dblECP" c_dblECP_ :: Ptr Word32 -> Ptr Word32 -> Ptr Word32 
                                                        -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "c_ec.c c_mulECP" c_mulECP_ :: Ptr Word32 -> Ptr Word32 -> Ptr Word32 
                                                        -> Ptr Word32 
                                                        -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO ()

withECProj :: ECProj -> (Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO a) -> IO a
withECProj (ECProj (Fp x) (Fp y) (Fp z)) action = 
  withWord256 x $ \px -> withWord256 y $ \py -> withWord256 z $ \pz -> action px py pz

withNewECProj :: (Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO ()) -> IO ECProj
withNewECProj action = do
  x <- newWord256
  y <- newWord256
  z <- newWord256
  withWord256 x $ \px -> withWord256 y $ \py -> withWord256 z $ \pz -> action px py pz
  return (ECProj (Fp x) (Fp y) (Fp z))  

c_dblECP :: ECProj -> ECProj
c_dblECP ep = Unsafe.unsafePerformIO $ do
  withECProj ep $ \xp yp zp -> withNewECProj $ \xr yr zr -> c_dblECP_ xp yp zp xr yr zr

c_addECP :: ECProj -> ECProj -> ECProj
c_addECP ep eq = Unsafe.unsafePerformIO $ do
  withECProj ep $ \xp yp zp -> withECProj eq $ \xq yq zq -> withNewECProj $ \xr yr zr -> c_addECP_ xp yp zp xq yq zq xr yr zr

c_mulECP :: ECProj -> Integer -> ECProj
c_mulECP ep m = Unsafe.unsafePerformIO $ do
  w256 <- makeWord256 (mod m secp256k1_n)     -- mod !!
  withECProj ep $ \xp yp zp -> withWord256 w256 $ \pw256 -> withNewECProj $ \xr yr zr -> c_mulECP_ xp yp zp pw256 xr yr zr  

dblECP :: ECProj -> ECProj
dblECP = c_dblECP

addECP :: ECProj -> ECProj -> ECProj
addECP = c_addECP

mulECP :: ECProj -> Integer -> ECProj
mulECP = c_mulECP

--------------------------------------------------------------------------------
-- * Num/Eq instances

instance Num ECProj where
  (+) = addECP
  (-) = subECP
  negate = invECP
  (*)    = error "ECProj/Num: (*) doesn't makes sense"
  abs    = error "ECProj/Num: `abs' doesn't makes sense"
  signum = error "ECProj/Num: `signum' doesn't makes sense"
  fromInteger n = case n of
    0 -> ecpInfinity
    _ -> error "ECProj/Num: `fromInteger' doesn't makes sense, apart from 0"

--------------------------------------------------------------------------------

infix 4 =~=

(=~=) :: ECProj -> ECProj -> Bool
(=~=) (ECProj xp yp zp) (ECProj xq yq zq) = ( zp==0 && zq==0 && z_zero) || z_nonzero 
  where
    zp2 = zp*zp
    zq2 = zq*zq
    zp3 = zp*zp2
    zq3 = zq*zq2
    z_zero    =  ( yp == 0 && yq == 0 ) || ( yq2*xp3 == yp2*xq3 )
    z_nonzero = ( xp*zq2 == xq*zp2 ) && ( yp*zq3 == yq*zp3 ) 
    xp2 = xp*xp
    xq2 = xq*xq    
    xp3 = xp2*xp
    xq3 = xq2*xq    
    yp2 = yp*xp
    yq2 = yq*xq    

ecpInfinity :: ECProj
ecpInfinity = ECProj 1 2 0

isECPInfinity :: ECProj -> Bool
isECPInfinity (ECProj x@(Fp xrep) y@(Fp yrep) z@(Fp zrep)) = (zrep==0) && (xrep/=0) && (yrep/=0) && (y2==4*x3) where
  y2 = y*y
  x2 = x*x
  x3 = x2*x

isECPOnCurve :: ECProj -> Bool
isECPOnCurve (ECProj x y z) = (y2 == 4 * (x3 + 7*z6)) where
  y2 = y*y
  x2 = x*x
  x3 = x2*x
  z2 = z*z
  z3 = z2*z
  z6 = z3*z3

secp256k1_G_proj :: ECProj
secp256k1_G_proj = ECProj (fromInteger secp256k1_Gx) (fromInteger secp256k1_Gy * 2) 1

--------------------------------------------------------------------------------

-- | Addition in the elliptic curve (or multiplication if you prefer to think it as a multiplicative group)
hs_addECP :: ECProj -> ECProj -> ECProj
hs_addECP ep@(ECProj xp yp zp) eq@(ECProj xq yq zq) 
  | zp == 0           =  if isECPInfinity ep then eq else error "addECP: eq not on the curve"
  | zq == 0           =  if isECPInfinity eq then ep else error "addECP: ep not on the curve"
  | b == 0 && d == 0  =  dblECP ep
  | otherwise         =  ECProj xr yr zr
  where
    zp2 = zp*zp
    zq2 = zq*zq
    zp3 = zp*zp2
    zq3 = zq*zq2                                             
    xpzq2 = xp*zq2
    xqzp2 = xq*zp2
    ypzq3 = yp*zq3
    yqzp3 = yq*zp3
    a = xpzq2 + xqzp2
    b = xpzq2 - xqzp2
    c = ypzq3 + yqzp3
    d = ypzq3 - yqzp3
    e = b+b
    e2 = e*e
    ae2 = a*e2
    xr = d*d-ae2
    yr = d*(ae2-(xr+xr)) - e2*b*c
    zr = e*zp*zq
    
-- | Doubling a point in the elliptic curve (multiplication by the integer 2)
hs_dblECP :: ECProj -> ECProj
hs_dblECP (ECProj xp yp zp) = ECProj xr yr zr
  where
    yp2 = yp*yp
    xp2 = xp*xp
    a = xp2+xp2+xp2
    xpyp2 = xp*yp2
    b = xpyp2 + xpyp2
    c = yp2*yp2
    xr = a*a - b
    yr = a*(b-(xr+xr)) - c
    zr = yp*zp

--------------------------------------------------------------------------------

-- | Inverse (negation) in the elliptic curve
invECP :: ECProj -> ECProj
invECP (ECProj x y z) = ECProj x (negate y) z

subECP :: ECProj -> ECProj -> ECProj
subECP a b = addECP a (invECP b)

-- | Multiplication by a positive integer (or exponentiation, if you think multiplicatively)
hs_mulECP :: ECProj -> Integer -> ECProj
hs_mulECP !base !exp = go ecpInfinity base exp where
  go !acc _  0  = acc
  go !acc !b !e = if (e .&. 1 > 0)
    then go (hs_addECP acc b) (hs_dblECP b) (shiftR e 1)
    else go            acc    (hs_dblECP b) (shiftR e 1)

--------------------------------------------------------------------------------
