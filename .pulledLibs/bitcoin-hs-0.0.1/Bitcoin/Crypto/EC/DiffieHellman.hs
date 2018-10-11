
-- | Diffie-Hellman key exchange

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bitcoin.Crypto.EC.DiffieHellman where

--------------------------------------------------------------------------------

import Data.Word

import Bitcoin.Misc.OctetStream

import Bitcoin.Crypto.Word256
import Bitcoin.Crypto.FiniteField.Fast.Fp

import Bitcoin.Crypto.EC.Curve
import Bitcoin.Crypto.EC.Projective
import Bitcoin.Crypto.EC.Key

--------------------------------------------------------------------------------

newtype SharedSecret = SharedSecret [Word8] deriving (Eq,Ord,Show,OctetStream)

--------------------------------------------------------------------------------

-- | Given your private key and somebody else's public key, this compute
-- a (256 bit) shared secret. 
--
-- This is actually very simple: since @pubkey = G * privkey@, the shared
-- secret will be simply @G * privkey1 * privkey2@.
--
-- This can fail if for example an invalid pubkey is given, and
-- in some other special circumstances.
--
diffieHellmanPrimitive :: PrivKey -> PubKey -> Maybe SharedSecret
diffieHellmanPrimitive (PrivKey d) pk = 
  case uncompressPubKey pk of
    Nothing -> Nothing    
    Just full@(FullPubKey x y) -> case isValidPubKey full of 
      False -> Nothing
      True  -> case fromECProj (mulECP (toECProj $ mkECPoint x y) d) of
        ECInfinity    -> Nothing
        ECPoint zx zy -> Just (SharedSecret $ word256ToWord8ListLE $ unFp zx)   -- zx=0 doesn't seem to be on the curve, so this is always nonzero
    _ -> Nothing                                                                -- shouldn't happen, but totality is always good.

--------------------------------------------------------------------------------
  