
-- | Elliptic Curve cryptography keys
--
{-# LANGUAGE CPP, BangPatterns #-}
module Bitcoin.Crypto.EC.Key where

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

import Bitcoin.Misc.BigInt

import Bitcoin.Protocol.Hash               

import Bitcoin.Crypto.FiniteField.Fast.Fp  hiding ( secp256k1_p )
import Bitcoin.Crypto.FiniteField.Naive.Fn hiding ( secp256k1_n )

import Bitcoin.Crypto.EC.Curve
import Bitcoin.Crypto.EC.Projective

--------------------------------------------------------------------------------
-- * private and public keys.

-- | The private key is a random number in the interval [1,n-1] (n being secp256k1_n)
newtype PrivKey = PrivKey { fromPrivKey :: Integer } deriving (Eq,Show)

-- | The public key (which is the point @priv*G@ on the curve, @G@ being the generator), either in long format (both coordinates)
-- or short format (x coordinate plus parity of y) 
data PubKey 
  = FullPubKey  !Integer !Integer    -- ^ <x> <y>
  | ComprPubKey !Word8   !Integer    -- ^ only <x>; the single byte encodes the parity of @y@ (then we have the curve equation)
  deriving (Eq,Show)

-- | Unfortunately there is this mess with compressed/uncompressed formats :(
--
-- See <http://bitcoin.stackexchange.com/questions/7299/when-importing-private-keys-will-compressed-or-uncompressed-format-be-used>
data PubKeyFormat = Uncompressed | Compressed deriving (Eq,Show)

--------------------------------------------------------------------------------

-- | Generates a private key with the built-in random generator. 
--
-- WARNING: this probably doesn't have enough entropy, use only for testing!
generatePrivKeyIO :: IO PrivKey
generatePrivKeyIO = getStdRandom generatePrivKey

-- | Generates a private key using the supplied random generator. 
-- 
-- WARNING! You are responsible for the random generator having enough entropy!
-- (be careful not to have a constant seed, for example...)
generatePrivKey :: RandomGen gen => gen -> (PrivKey,gen)
generatePrivKey = go where
  -- this isn't much help in case of weak random generators, but maybe better than nothing
  -- (for example, even if the original generator is predictable, this won't be easy to predict)
  go gen = if ( priv > 0 && priv < secp256k1_n ) then (PrivKey priv, gen'') else go gen'' where
    (priv0,gen' ) = randomR (1, secp256k1_n - 1) gen
    (priv1,gen'') = randomR (1, secp256k1_n - 1) gen'
    priv = toIntegerBE 
         $ doHash256 
         $ (fromIntegerLE priv0 ++ fromIntegerLE priv1 ++ [0x12::Word8,0x34,0x56,0x78,0x9a,0xbc,0xde,0xf0])

{-
generatePrivKey :: RandomGen gen => gen -> (PrivKey,gen)
generatePrivKey gen = (PrivKey priv, gen') where
   (priv,gen') = randomR (1, secp256k1_n - 1) gen
-}

--------------------------------------------------------------------------------

pubKeyFormat :: PubKey -> PubKeyFormat
pubKeyFormat pk = case pk of 
  FullPubKey  {} -> Uncompressed
  ComprPubKey {} -> Compressed

-- | Computes the public key in the given format
computePubKey :: PubKeyFormat -> PrivKey -> PubKey
computePubKey fmt priv = 
  case fmt of
    Uncompressed -> full 
    Compressed   -> compressPubKey full 
  where
    full = computeFullPubKey priv

computeFullPubKey :: PrivKey -> PubKey
computeFullPubKey (PrivKey da) 
  | da < 1 && da >= secp256k1_n = error "computePubKey: invalid private key"
  | otherwise = case fromECProj (mulECP secp256k1_G_proj da) of
      ECPoint x y -> FullPubKey (fromFp x) (fromFp y) where
      ECInfinity  -> error "computePubKey: invalid private key"

-- I think the condition @(n * ep = ECInfinity)@ is not necessary (follows from being on the curve), but cannot hurt.
-- On the other hand, I'm nut sure if @y==0@ is invalid, but it probably should...
isValidPubKey :: PubKey -> Bool
isValidPubKey pub = case uncompressPubKey pub of
  Nothing -> False
  Just (ComprPubKey _  _ ) -> error "isValidPubKey: this shouldn't happen"
  Just (FullPubKey  x0 y0) -> (y /= 0) && (y*y == x*x*x + 7) && (mulECP (toECProj ep) secp256k1_n =~= ecpInfinity) where
    x = fromInteger x0 :: Fp
    y = fromInteger y0 :: Fp
    ep = ECPoint x y

-- | Changes a pubkey to the given format
formatPubKey :: PubKeyFormat -> PubKey -> PubKey
formatPubKey fmt pk = case fmt of
  Compressed   -> compressPubKey   pk
  Uncompressed -> case uncompressPubKey pk of
                    Just new -> new
                    Nothing  -> error "formatPubKey: cannot expand compressed pubkey"

-- | Uncompresses a public key. This may actually fail if there is no point (x,y) on the curve for any y.
uncompressPubKey :: PubKey -> Maybe PubKey
uncompressPubKey full@(FullPubKey _ _)   = Just full
uncompressPubKey (ComprPubKey evenOdd x) = 
  case my of 
    Just y  -> let yi = fromFp y
               in  if even (evenOdd) == even yi
                     then Just ( FullPubKey x                yi  ) 
                     else Just ( FullPubKey x (secp256k1_p - yi) )
    Nothing -> Nothing
  where
    x1 = fromInteger x :: Fp
    x3 = x1*x1*x1
    y2 = x3 + 7     -- NOTE: this should be @y2 = x3 + a*x + b@ but a=0 and b=7 for our curve
    my = sqrt_p y2

-- | Compresses a public key
compressPubKey :: PubKey -> PubKey
compressPubKey compr@(ComprPubKey _ _) = compr
compressPubKey (FullPubKey x y)        = ComprPubKey (if even y then 2 else 3) x

--------------------------------------------------------------------------------
