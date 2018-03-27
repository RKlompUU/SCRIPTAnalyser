
-- | Bitcoin addresses

{-# LANGUAGE CPP #-}
module Bitcoin.Protocol.Address
  (
  -- * addresses
    Address(..)
  , PubKeyHash(..)
  , pubKeyAddress
  , pubKeyHash
  , bitcoinAddressDecode
  , bitcoinAddressEncode, bitcoinAddressEncodeHash

  -- * version bytes
  , VersionByte(..)
  , bitcoinPubkeyHashVB  
  , bitcoinScriptHashVB  
  , namecoinPubkeyHashVB 
  , privateKeyVB          
  , bitcoinTestNetPubkeyHashVB 
  , bitcoinTextNetScriptHashVB 

  )
  where

--------------------------------------------------------------------------------

import qualified Data.ByteString as B

import Bitcoin.Misc.OctetStream

-- import Bitcoin.Crypto.EC.Curve

import Bitcoin.Script.Base

import Bitcoin.Protocol.Hash
import Bitcoin.Protocol.Base58
import Bitcoin.Protocol.Key

--------------------------------------------------------------------------------

-- | A bitcoin address
newtype Address = Address { unAddress :: String } deriving (Eq,Show)

--------------------------------------------------------------------------------

-- | Computes the address of a public key. 
--
-- NOTE: compressed and uncompressed versions of the same public key result in
-- different addresses!
pubKeyAddress :: PubKey -> Address
pubKeyAddress pubkey = Address $ unBase58Check $ base58CheckEncode vb (doHash160 $ encodePubKeyNative pubkey) where
#ifdef WITH_TESTNET
  vb = (VersionByte 111) 
#else
  vb = (VersionByte 0) 
#endif

--------------------------------------------------------------------------------

bitcoinAddressDecode :: Address -> Maybe (VersionByte,PubKeyHash)
bitcoinAddressDecode (Address addr) = case base58CheckDecode (Base58Check addr) of
  Left  _            -> Nothing
  Right (vb,hash160) -> Just (vb, PubKeyHash hash160)

-- | The second argument can be either a script or an encoded public key
bitcoinAddressEncode :: OctetStream a => VersionByte -> a -> Address
bitcoinAddressEncode vb rawscript = Address $ unBase58Check $ base58CheckEncode vb (doHash160 $ rawscript)

bitcoinAddressEncodeHash :: VersionByte -> Hash160 -> Address
bitcoinAddressEncodeHash vb hash160 = Address $ unBase58Check $ base58CheckEncode vb hash160

--------------------------------------------------------------------------------

