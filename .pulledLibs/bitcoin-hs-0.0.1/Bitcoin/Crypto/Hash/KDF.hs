
-- | Key Derivation Functions (KDF).
--
-- These are used to derive larger symmetric keys from a small (say, 256 bit) shared secret
-- generated using eg. Diffie-Hellman key exchange.

module Bitcoin.Crypto.Hash.KDF 
  ( SharedSecret(..)
  , concatenatingKDF 
  , foldingKDF
  ) 
  where

--------------------------------------------------------------------------------

import Data.Word
import Data.Bits

import Bitcoin.Misc.OctetStream

import Bitcoin.Crypto.Hash.SHA256

import Bitcoin.Crypto.EC.DiffieHellman ( SharedSecret(..) )

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L

--------------------------------------------------------------------------------

-- | Concatenation-based Key Derivation Function.
--
-- Basically:
--
-- > output = Hash[1] || Hash[2] || Hash[3] || ...
-- >
-- > Hash[counter] = H ( counter || Z || publicInfo )
--
-- where H is the SHA256 hash function, Z is the shared secret, 
-- and the counter is a big-endian encoded 32 bit word.
--
-- This is more-or-less the NIST-800-56-Concatenation-KDF standard.
-- 
concatenatingKDF 
  :: OctetStream publicInfo 
  => SharedSecret             -- ^ shared secret (for example estabilished by Diffie-Hellman key exchange)
  -> publicInfo               -- ^ publicly avaliable information about the parties (for example, the IDs of the two parties)
  -> Int                      -- ^ desired output length
  -> L.ByteString
concatenatingKDF z publicInfo len = L.take (fromIntegral len) (L.fromChunks $ take n hashes) where
  n       = div (len+31) 32
  zpublic = B.append (toByteString z) (toByteString publicInfo)
  hashes  = [ toByteString (sha256 (B.append (word32BE counter) zpublic)) | counter <- [1..] ]

--------------------------------------------------------------------------------

-- | This is similar to the previous, however, we also use the previous hash when
-- computing the next hash:
--
-- > Hash[counter] = H ( counter || Hash[counter-1] || Z || publicInfo )
--
-- @Hash[0]@ is set to ad-hoc value, presently @[0x5c,0x5c,0x5c...]@
--
foldingKDF :: OctetStream publicInfo 
  => SharedSecret             -- ^ shared secret (for example estabilished by Diffie-Hellman key exchange)
  -> publicInfo               -- ^ publicly avaliable information about the parties (for example, the IDs of the two parties)
  -> Int                      -- ^ desired output length
  -> L.ByteString
foldingKDF z publicInfo len = L.take (fromIntegral len) (L.fromChunks $ take n hashes) where

  n       = div (len+31) 32
  zpublic = B.append (toByteString z) (toByteString publicInfo)
  hashes  = tail $ scanl worker initial [1..] 

  initial = toByteString $ replicate 32 (0x5c :: Word8)

  worker :: B.ByteString -> Word32 -> B.ByteString
  worker prevhash counter = toByteString $ sha256 $ B.concat [ word32BE counter , prevhash , zpublic ]

--------------------------------------------------------------------------------

-- | A 32 bit word as a big-endian bytestring
word32BE :: Word32 -> B.ByteString
word32BE w = B.pack
  [ fromIntegral (  shiftR w 24           )
  , fromIntegral ( (shiftR w 16) .&. 0xff )
  , fromIntegral ( (shiftR w  8) .&. 0xff )
  , fromIntegral (         w     .&. 0xff )
  ]

--------------------------------------------------------------------------------
