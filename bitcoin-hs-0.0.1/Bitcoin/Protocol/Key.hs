
-- | Encoding/decoding of public and private keys 
--

{-# LANGUAGE CPP #-}
module Bitcoin.Protocol.Key 
  ( 
  -- * private keys
    PrivKey(..) 
  , generatePrivKeyIO
  , generatePrivKey
  , encodePrivKey32 
  , decodePrivKey32
  -- * wallet import format for private keys
  , WIF(..)
  , privKeyWIFEncode
  , privKeyWIFDecode
  -- * public keys
  , PubKey(..) , PubKeyFormat(..) , pubKeyFormat 
  , PubKeyHash(..) , pubKeyHash
  , decodePubKey
  , encodePubKeyNative
  , encodePubKey' 
  , encodePubKeyLong 
  , encodePubKeyShort 
  -- * computations on keys
  , computePubKey 
  , computeFullPubKey
  , isValidPubKey 
  , formatPubKey 
  , uncompressPubKey 
  , compressPubKey
  )
  where

--------------------------------------------------------------------------------

import Control.Monad

import Data.Char
import Data.Bits
import Data.Word
import Data.Maybe

import qualified Data.ByteString as B

import Bitcoin.Misc.BigInt
import Bitcoin.Misc.OctetStream

import Bitcoin.Protocol.Base58
import Bitcoin.Protocol.Base64
import Bitcoin.Protocol.Hash

import Bitcoin.Crypto.EC.Key

--------------------------------------------------------------------------------
-- * trivial encoding of private keys 

-- | Simple 32 byte big-endian integer format
encodePrivKey32 :: PrivKey -> B.ByteString
encodePrivKey32 (PrivKey da) = B.pack $ bigEndianInteger32 da

-- | Simple 32 byte big-endian integer format
decodePrivKey32 :: B.ByteString -> PrivKey
decodePrivKey32 bs = if B.length bs == 32 
  then PrivKey $ bigEndianRollInteger $ B.unpack bs
  else error "decodePrivKey32: expected a 32 byte long bytestring"

--------------------------------------------------------------------------------
-- * wallet import format for private keys

-- | Wallet Import Format
newtype WIF = WIF { unWIF :: String } deriving (Eq,Show)

-- | Encode to Wallet Import Format (WIF)
privKeyWIFEncode :: PubKeyFormat -> PrivKey -> WIF
privKeyWIFEncode pkformat (PrivKey key) = WIF $ unBase58Check $
  case pkformat of
    Uncompressed -> base58CheckEncode vb $ B.pack $ bigEndianInteger32 key
    Compressed   -> base58CheckEncode vb $ B.pack $ bigEndianInteger32 key ++ [1]
  where
#ifndef WITH_TESTNET
    vb = VersionByte 128
#else
    vb = VersionByte 239
#endif
       
-- | Decode from Wallet Import Format (WIF)
privKeyWIFDecode :: WIF -> Maybe (PubKeyFormat,PrivKey)
privKeyWIFDecode (WIF str) = case base58CheckDecode (Base58Check str) of
  Right (vb, bs) | vb == vb0 -> 
    case B.length bs of
      32 -> Just ( Uncompressed , PrivKey $ bigEndianRollInteger $ ws )
      33 -> case last ws of
              1 -> Just ( Compressed , PrivKey $ bigEndianRollInteger $ take 32 ws )
              _ -> Nothing
    where 
      ws = B.unpack bs
  _ -> Nothing
  where
#ifndef WITH_TESTNET
    vb0 = VersionByte 128
#else
    vb0 = VersionByte 239
#endif

--------------------------------------------------------------------------------
-- * public keys
 
-- | Encodes the public key as it is represented (either compressed 33 bytes or uncompressed 65 bytes)
encodePubKeyNative :: PubKey -> B.ByteString
encodePubKeyNative pk = encodePubKey' (pubKeyFormat pk) pk

-- | Encodes a public key according to the specified format
encodePubKey' :: PubKeyFormat -> PubKey -> B.ByteString
encodePubKey' fmt pk = fromByteString $ case fmt of
  Uncompressed -> encodePubKeyLong  pk
  Compressed   -> encodePubKeyShort pk

-- | Encodes a public key as a 65 byte ByteString (uncompressed)
encodePubKeyLong :: PubKey -> B.ByteString
encodePubKeyLong       (FullPubKey  x y) = B.pack ( 4 : bigEndianInteger32 x ++ bigEndianInteger32 y )
encodePubKeyLong compr@(ComprPubKey _ _) = case uncompressPubKey compr of
  Just full -> encodePubKeyLong full
  Nothing   -> error "encodePubKeyLong: invalid compressed public key found"

-- | Encodes a public key as a 33 byte ByteString (compressed)
encodePubKeyShort :: PubKey -> B.ByteString
encodePubKeyShort      (ComprPubKey s x) = B.pack ( (if even s then 2 else 3) : bigEndianInteger32 x )
encodePubKeyShort full@(FullPubKey  _ _) = encodePubKeyShort (compressPubKey full)

-- | Decode public key from 33/65 bytes long (compressed/uncompressed) octet streams
decodePubKey :: OctetStream a => a -> Maybe PubKey
decodePubKey octets = 
  case len of
    33 -> case head ws of
            02 -> Just (ComprPubKey 02 x)
            03 -> Just (ComprPubKey 03 x)
            _  -> Nothing
    65 -> case head ws of
            04 -> Just (FullPubKey x y)
            06 -> Just (FullPubKey x y)   -- testnet3 transaction cb2710fca03b99502f81d50a40690f160079459f6b1d27aeb13bbaf70ba976d2 ...
            _  -> Nothing 
    _  -> Nothing  
  where
    len = length ws
    ws  = toWord8List octets
    x = bigEndianRollInteger $ take 32 $ drop 1  ws
    y = bigEndianRollInteger $ take 32 $ drop 33 ws  

--------------------------------------------------------------------------------
-- * public key hashes

-- | The 160-bit hash of a public key
newtype PubKeyHash = PubKeyHash { fromPubKeyHash :: Hash160 } deriving (Eq,Show)

pubKeyHash :: PubKey -> PubKeyHash
pubKeyHash pubkey = PubKeyHash $ doHash160 (encodePubKeyNative pubkey :: B.ByteString)

instance OctetStream PubKeyHash where
  toByteString   = toByteString . fromPubKeyHash
  fromByteString = PubKeyHash . fromByteString

--------------------------------------------------------------------------------

