
-- | Elliptic Curve Digital Signatura Algorithm (DSA), with the secp256k1 curve.
--
-- References:
--
--  * <http://en.wikipedia.org/wiki/Elliptic_curve_cryptography>
--
--  * <http://en.wikipedia.org/wiki/ECDSA>
--
--  * <http://www.secg.org/collateral/sec2_final.pdf>
--
{-# LANGUAGE BangPatterns #-}
module Bitcoin.Crypto.EC.DSA where

--------------------------------------------------------------------------------

import Control.Monad

import Prelude hiding ( sqrt )

import Data.Char
import Data.Bits
import Data.Word
import Data.Maybe

import qualified Data.ByteString as B

import System.Random

import Bitcoin.Misc.HexString
import Bitcoin.Misc.BigInt
import Bitcoin.Misc.OctetStream

import Bitcoin.Protocol.Hash

import Bitcoin.Crypto.FiniteField.Fast.Fp  hiding ( secp256k1_p )
import Bitcoin.Crypto.FiniteField.Naive.Fn hiding ( secp256k1_n )

import Bitcoin.Crypto.Hash.HMAC ( HMAC(..) , HMACKey , hmacSha256 , hmacKeyFromString64 )

import Bitcoin.Crypto.EC.Curve
import Bitcoin.Crypto.EC.Projective
import Bitcoin.Crypto.EC.Key

--------------------------------------------------------------------------------

-- | An ECDSA signature
data Signature = Signature { _signatureR :: !Integer , _signatureS :: !Integer } deriving (Eq,Show)

-- | Two extra bits of information, used to recover the public key from the signatures.
newtype SignBits = SignBits Word8 deriving (Eq,Show)

--------------------------------------------------------------------------------
-- * signing, verifying, recovering

-- | The integer representation of a hash. We should take the left log2[secp256k1_n] bits, 
-- but both the hash and n has 256 bits, so we take the whole hash.
hashInteger :: Hash256 -> Integer
hashInteger = toIntegerBE

-- | Signs the /hash/ of a message (using the defaul random source - this probably shouldn't be used!)
signMessageHashIO :: PrivKey -> Hash256 -> IO (SignBits,Signature)
signMessageHashIO priv hash = getStdRandom (\gen -> signMessageHash priv hash gen)

-- | Signs the /hash/ of a message (given a random number source)
signMessageHash :: RandomGen gen => PrivKey -> Hash256 -> gen -> ((SignBits,Signature),gen)
signMessageHash (PrivKey !da) !hash oldgen = go oldgen where
  z    = hashInteger hash

  go gen = if (ep /= ECInfinity && r/=0 && s/=0) then ((SignBits w8, signature),gen') else go gen' where
    signature = Signature (fromFn r) (fromFn s)
    -- (k,gen') = randomR (1,secp256k1_n) gen
    (!k,gen') = hashedRandom gen
    epp = mulECP secp256k1_G_proj k      -- since k is in [1,n-1], this cannot be infinity
    ep  = fromECProj epp
    (!x,!y) = case ep of  
      ECPoint x y -> (x,y)
      ECInfinity  -> error "signMessageHash: shouldn't happen"
    r = fromInteger (fromFp x) :: Fn
    s = (fromInteger z + r * fromInteger da) / (fromInteger k)

    odd_y  = if even (fromFp y)      then 0 else 1
    add_n  = if fromFn r == fromFp x then 0 else 2
    w8 = odd_y + add_n

  -- ECDSA is vulnerable if the same K is reused, or if the randomness of K is weak. 
  -- Hashing together the message, the private key, and some random numbers helps a 
  -- lot in case of weak random generators (it should be more-or-less safe even with
  -- the classic "k=4", chosen with fair dice "random" generator :)
  hashedRandom gen = if k>1 && k<secp256k1_n then (k,gen'') else hashedRandom gen'' where
    (k0,gen' ) = randomR (1,secp256k1_n) gen
    (k1,gen'') = randomR (1,secp256k1_n) gen'
    k = toIntegerBE 
       $ doHash256 
       $ (fromIntegerLE k0 ++ fromIntegerLE k1 ++ toWord8List hash ++ fromIntegerLE da ++ [0x12,0x34,0x56,0x78])

--------------------------------------------------------------------------------

-- | Deterministic signature as specified by RFC 6979: 
-- <http://tools.ietf.org/html/rfc6979>, in particular section 3.2
--
signMessageHashRFC6979 :: PrivKey -> Hash256 -> (SignBits,Signature)
signMessageHashRFC6979 (PrivKey da) hash = result where

  hmac_k :: OctetStream a => a -> [Word8] -> [Word8]
  hmac_k key = toWord8List . unHMAC . hmacSha256 (hmacKeyFromString64 key)

  z    = mod (hashInteger hash) secp256k1_n           -- for z, it doesn't matter (it will be in Fn anyway), but for h1 it matters :(

  x1   = bigEndianInteger32 da    :: [Word8]          -- x = private key here
  h1   = bigEndianInteger32 z     :: [Word8]          -- step a

  v0   = replicate 32 0x01 :: [Word8]                 -- step b
  k0   = replicate 32 0x00 :: [Word8]                 -- step c
  k1   = hmac_k k0 $ v0 ++ [0x00] ++ x1 ++ h1         -- step d
  v1   = hmac_k k1 $ v0                               -- step e
  k2   = hmac_k k1 $ v1 ++ [0x01] ++ x1 ++ h1         -- step f
  v2   = hmac_k k2 $ v1                               -- step g
  
  result = step_h k2 v2

  halfn = div secp256k1_n 2

  step_h k v0 =                                       -- final step (step h)

    if dsa_k > 0 && dsa_k < secp256k1_n && ep /= ECInfinity && r/=0 && s/=0
      then (signbits,signature)
      else step_h k' v'

    where

      v = hmac_k k v0          -- step h/1 
      t = v                    -- step h/2 (always a single step in our case?)
      dsa_k = toIntegerBE t

      k' = hmac_k k  $ v ++ [0x00]
      v' = hmac_k k' $ v

      epp = mulECP secp256k1_G_proj dsa_k
      ep  = fromECProj epp
      (!x,!y) = case ep of  
        ECPoint x y -> (x,y)
        ECInfinity  -> error "signMessageHashRFC6979: shouldn't happen"    -- k is [1,n-1], this shouldn't ever happen

      r  = fromInteger (fromFp x)                                     :: Fn
      s0 = (fromInteger z + r * fromInteger da) / (fromInteger dsa_k) :: Fn

      -- hmm, it seems this extra rule is used for some reason ??
      -- quote: "The theory behind this is: if you negate K you get the same R and the negated S. 
      --         Hence you need to negate S as a post-processing step, i.e., S' = prime - S in both cases"
      s = if (fromFn s0) > halfn then Fn (secp256k1_n - fromFn s0) else s0   

      odd_y  = if even (fromFp y)      then 0 else 1
      add_n  = if fromFn r == fromFp x then 0 else 2
      w8 = odd_y + add_n 
  
      signbits  = SignBits w8
      signature = Signature (fromFn r) (fromFn s)
    
--------------------------------------------------------------------------------
  
verifySignatureWithHash :: PubKey -> Signature -> Hash256 -> Bool
verifySignatureWithHash pubkey0 signature hash = isJust mbpubkey && isValidPubKey pubkey && check where
  mbpubkey = uncompressPubKey pubkey0  
  z     = (fromInteger $ hashInteger hash) :: Fn
  check = (r0>0 && r0 < secp256k1_n) && (s0>0 && s0 < secp256k1_n) && valid
  Signature r0 s0 = signature
  pubkey@(FullPubKey qx qy) = fromJust mbpubkey 
  w  = recip (fromInteger s0 :: Fn)
  r  = fromInteger r0 :: Fn
  u1 = fromFn (z * w)
  u2 = fromFn (r * w)
  qp  = ECPoint (fromInteger qx :: Fp) (fromInteger qy :: Fp)
  qpp = toECProj qp
  ep = (mulECP secp256k1_G_proj u1) `addECP` (mulECP qpp u2) 
  valid = case fromECProj ep of
    ECPoint x1 y1 -> fromFp x1 == r0        -- ??? !!! 
    _             -> False   

--------------------------------------------------------------------------------

-- | Recovers the public key from the (extended) signature and the /hash/ of the message.
-- 
-- On the Word8 paramter: Bit 0 encodes whether the curve point R (which has x coordinate r from the signature) 
-- has even or odd y coordinate; and bit 1 encodes how to reconstruct the x coordinate from r.
--
recoverPubKeyFromHash :: (PubKeyFormat,SignBits,Signature) -> Hash256 -> Maybe PubKey
recoverPubKeyFromHash (fmt, SignBits parities, Signature r s) hash = if isJust mbxy then Just cpubkey else Nothing where 

  z = hashInteger hash

  cpubkey = formatPubKey fmt pubkey
  pubkey  = FullPubKey (fromFp qx) (fromFp qy)  

  x = if not add_n then r else modp (r + secp256k1_n) 
  mbxy = uncompressPubKey $ ComprPubKey (if odd_y then 3 else 2) x
  Just (FullPubKey _ y) = mbxy

  rr = ECPoint (toFp x) (toFp y) 
  rr_proj = toECProj rr  

  -- rr1 = invEC rr                  -- (-R) corresponds to negating y, which is already encoded in odd_y
  
  inv_r = fromFn (recip $ Fn $ modn r)
  qqp   = mulECP (sR_proj `subECP` zG_proj) inv_r
  (!qx,!qy) = case fromECProj qqp of 
    ECPoint qx qy -> (qx,qy)
    ECInfinity    -> error "recoverPubKeyFromHash: shouldn't happen (???)"

  sR_proj = mulECP rr_proj s
  zG_proj = mulECP secp256k1_G_proj z

  odd_y  = (parities .&. 1) > 0
  add_n  = (parities .&. 2) > 0

--------------------------------------------------------------------------------
-- * some tests

ectest1 = do
  let msg = B.pack $ map char_to_word8 $ "almafa kortefa"
  let priv = PrivKey 420324792348973434283974283942354354
      pub  = computeFullPubKey priv
  gen <- getStdGen
  let ((signbits,signat),gen') = signMessageHash priv (doHash256 msg) gen
  setStdGen gen'
  print signat
  let b = verifySignatureWithHash pub signat (doHash256 msg)
  print b
  when (not b) $ do
    error $ "ECDSA signature check test failed for " ++ show (signat,gen)

