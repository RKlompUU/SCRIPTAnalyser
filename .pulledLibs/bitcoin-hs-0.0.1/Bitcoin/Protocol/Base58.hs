
-- | Bitcoin-specific Base58 encoding, and related stuff 

module Bitcoin.Protocol.Base58 
  ( 
  -- * base58 encoding
    Base58(..)
  , base58Decode, base58Encode
  , base58EncodeInteger , base58DecodeInteger

  -- * base58-check encoding
  , Base58Check(..) 
  , base58CheckDecode, base58CheckEncode

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

import Data.Word
import Data.Bits
import Data.List ( unfoldr )

import Data.Array.IArray
import Data.Array.Unboxed

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.ByteString as B

import Bitcoin.Misc.BigInt
import Bitcoin.Misc.HexString ( toHexStringChars ) 
import Bitcoin.Misc.OctetStream
import Bitcoin.Protocol.Hash

import Bitcoin.Script.Base

--------------------------------------------------------------------------------

alphabet :: UArray Int Char
alphabet = array (0,57) $ zip [0..57] stringAlphabet 

reverseAlphabet :: Map Char Int
reverseAlphabet = Map.fromList $ zip stringAlphabet [0..57]

stringAlphabet :: [Char]
stringAlphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
--               "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

--------------------------------------------------------------------------------
-- * base-58 encoding

-- | A raw base58 encoded octet stream (not base58-check!)
newtype Base58 = Base58 { unBase58 :: String } deriving (Eq,Show)

-- | Simple Base-58 encoding, without the leading \'1\'-s
base58Encode :: OctetStream a => a -> Base58
base58Encode = Base58 . base58EncodeInteger . toIntegerBE

-- | WARNING: @(base58Decode . base58Encode)@ is /NOT/ identity, because of the leading zero bytes.
base58Decode :: OctetStream a => Base58 -> Maybe a
base58Decode (Base58 input) = case base58DecodeInteger input of 
  Just n  -> Just $ fromIntegerBE n   -- fromWord8List $ bigEndianUnrollInteger n
  Nothing -> Nothing 

-- | Without leading \'1\'-s
base58EncodeInteger :: Integer -> String
base58EncodeInteger bigint = dropWhile (=='1') $ reverse $ go bigint where
  go n = case k of { 0 -> [lkp r] ; _ -> lkp r : go k } where (k,r) = divMod n 58
  lkp :: Integer -> Char
  lkp k | k>=0 && k<58 = alphabet ! (fromIntegral k)
        | otherwise    = error "base58Encode: shouldn't happen"

base58DecodeInteger :: String -> Maybe Integer
base58DecodeInteger input = 
  if and (map checkChar input) 
    then Just (decode 0 input) 
    else Nothing
  where
    checkChar x = Map.member x reverseAlphabet
    decode acc [] = acc
    decode acc (x:xs) = case Map.lookup x reverseAlphabet of
      Just i  -> decode (acc*58 + fromIntegral i) xs
      Nothing -> error "base58DecodeInteger: shouldn't happen"

--------------------------------------------------------------------------------
-- * base-58-check encoding

-- | A base58-check encoded octet stream
newtype Base58Check = Base58Check { unBase58Check :: String } deriving (Eq,Show)

-- | Base58Check encoding
-- <https://en.bitcoin.it/wiki/Base58Check_encoding>
base58CheckEncode :: OctetStream a => VersionByte -> a -> Base58Check
base58CheckEncode vb payload = Base58Check step5 where
  step1  = B.cons (fromVersionByte vb) $ toByteString payload
  step2  = B.take 4 (toByteString $ doHash256 step1)
  step3  = B.append step1 step2
  step4  = base58Encode step3 
  nzeros = B.length $ B.takeWhile (==0) $ step3
  step5  = replicate nzeros '1' ++ unBase58 step4

base58CheckDecode :: OctetStream a => Base58Check -> Either String (VersionByte,a)
base58CheckDecode (Base58Check input) = 
  case base58Decode (Base58 input) of
    Nothing -> Left "input is not valid Base58-encoded data"
    Just bs -> eiresult where
      k = B.length bs
      (stuff0,checksum) = B.splitAt (k-4) bs
      stuff = B.append (B.pack $ replicate nzeros 0) stuff0
      fullhash = toByteString $ doHash256 $ stuff     
      inithash = B.take 4 fullhash
      eiresult = if inithash /= checksum 
        then let aa = toHexStringChars $ inithash
                 bb = toHexStringChars $ checksum
             in  Left ("checksum mismatch: " ++ aa ++ " vs. " ++ bb) 
        else Right (VersionByte vb , fromByteString orig)
      vb   = B.head stuff
      orig = B.tail stuff
  where
    nzeros = length (takeWhile (=='1') input)


--------------------------------------------------------------------------------
-- * version bytes

newtype VersionByte = VersionByte { fromVersionByte :: Word8 } deriving (Eq,Show)

bitcoinPubkeyHashVB  = VersionByte 0 
bitcoinScriptHashVB  = VersionByte 5
namecoinPubkeyHashVB = VersionByte 52 
privateKeyVB         = VersionByte 128
bitcoinTestNetPubkeyHashVB = VersionByte 111
bitcoinTextNetScriptHashVB = VersionByte 196

--------------------------------------------------------------------------------
