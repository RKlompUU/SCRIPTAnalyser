
-- | Signing, verifying, and encoding\/decoding of signatures. 

{-
-- Signatures are typically Base64 encoded (yet another encoding, because why not...)
-- plus some special 65 byte encoding which allows reconstructing the public key,
-- plus a (not completely standard...) DER encoding again extended with an extra 
-- special byte...
-}

{-# LANGUAGE PatternGuards #-}
module Bitcoin.Protocol.Signature
  ( 
  -- * types
    Signature(..) , SignatureExt(..)
  , SignBits(..)
  , SigHash(..) , SigHashType(..) , sigHashAll
  , normalizeSigHashType , normalizeSigHash 
  -- * SigHash encoding
  , encodeSigHash
  , decodeSigHash
  -- * DER signature encoding
  , encodeSignatureDER
  , decodeSignatureDER , decodeSignatureDER'
  -- * \"compact\" signature encoding
  , decodeCompactSigBase64
  , encodeCompactSigBase64
  , decodeCompactSig
  , encodeCompactSig
  -- * signing messages (user specified random generator)
  , signTextMessage 
  , signRawMessage 
  , signTextMessageAddr_
  , signTextMessageAddr
  -- * signing messages (default random generator in IO - primarily for testing)
  , signTextMessageIO
  , signRawMessageIO
  , signTextMessageAddrIO_
  , signTextMessageAddrIO
  -- * signing messages (RFC6979 deterministic signatures)
  , signTextMessageRFC6979
  , signRawMessageRFC6979
  , signTextMessageAddrRFC6979_
  , signTextMessageAddrRFC6979
  -- * verifying signatures
  , verifyTextSignatureAddr
  , verifyTextSignaturePK 
  , verifyRawSignaturePK 
  -- * public key recovery
  , recoverTextPubKey 
  , recoverRawPubKey 
  -- * text message signing (bitcoin-qt compatible) 
  , messageMagic
  , prepareMessageForSigning 
  , messageHash 
  )
  where

--------------------------------------------------------------------------------

import Control.Monad

import Data.Char
import Data.Bits
import Data.Word
import Data.Maybe

import qualified Data.ByteString as B

import System.Random

-- import Bitcoin.Misc.HexString
import Bitcoin.Misc.BigInt
import Bitcoin.Misc.OctetStream

import Bitcoin.Protocol.Address
import Bitcoin.Protocol.Base58
import Bitcoin.Protocol.Base64
import Bitcoin.Protocol.Key
import Bitcoin.Protocol.Hash

import Bitcoin.Crypto.EC.Curve
import Bitcoin.Crypto.EC.DSA

--------------------------------------------------------------------------------
-- * Signature Hashtype

data SigHashType
  = SigHashAll
  | SigHashNone
  | SigHashSingle
  | SigHashAllZero     -- ^ 0 appears in the blockchain, should be handled as SigHashAll, but we must also properly serialize it back to 0 :(
  deriving (Eq,Show)

-- | Converts 'SigHashAllZero' to 'SigHashAll'
normalizeSigHashType :: SigHashType -> SigHashType
normalizeSigHashType t = case t of
  SigHashAllZero -> SigHashAll
  _              -> t 

normalizeSigHash :: SigHash -> SigHash
normalizeSigHash (SigHash t a) = SigHash (normalizeSigHashType t) a

-- | SigHash specifies how to the OP_CHECKSIG opcode should work (?)
data SigHash = SigHash 
  { _sigHashType  :: !SigHashType
  , _anyOneCanPay :: !Bool
  } 
  deriving (Eq,Show)

sigHashAll :: SigHash
sigHashAll = SigHash SigHashAll False

-- | \"Extended signature\": an ECDSA signature together with the sighash type
data SignatureExt = SignatureExt 
  { _extSignature :: !Signature 
  , _extSigHash   :: !SigHash 
  } 
  deriving (Eq,Show)

encodeSigHash :: SigHash -> Word8
encodeSigHash (SigHash typ anyflag) = f+t where
  f = if anyflag then 0x80 else 0x00
  t = case typ of
    SigHashAll     -> 1
    SigHashNone    -> 2
    SigHashSingle  -> 3
    SigHashAllZero -> 0        -- must serialize back to the original zero byte for tx checking...

decodeSigHash :: Word8 -> Maybe SigHash
decodeSigHash w = 
  case (w .&. 31) of
    0 -> sighash SigHashAllZero      -- this is because this appears in the blockchain because of a bug in some earlier implementation...
    1 -> sighash SigHashAll  
    2 -> sighash SigHashNone 
    3 -> sighash SigHashSingle  
    _ -> Nothing
  where
    f = (w .&. 0x80) > 0
    sighash t = Just (SigHash t f)

--------------------------------------------------------------------------------
-- * DER encoding/decoding of signatures

-- | Signatures use DER encoding to pack the r and s components into a single byte stream (this is also what OpenSSL produces by default).
-- (it seem that this is true only in the blockchain, not for signatures of messages, which use CompactSig?)
--
-- Howeever, there is an extra last byte appended, which is \"SIGHASH\"
--
encodeSignatureDER :: OctetStream a => SignatureExt -> a
encodeSignatureDER (SignatureExt (Signature r s) sighash) = fromWord8List (0x30 : fromIntegral (length rs) : rs ++ [encodeSigHash sighash]) where
  rs = derEncodeInteger r ++ derEncodeInteger s
  derEncodeInteger :: Integer -> [Word8]
  derEncodeInteger int = case ws of
    []    -> [0x02,0]
    (h:_) -> if h<0x80 
               then [0x02 ,     fromIntegral (length ws)     ] ++ ws
               else [0x02 , 1 + fromIntegral (length ws) , 0 ] ++ ws
    where
      ws = bigEndianUnrollInteger int

decodeSignatureDER :: OctetStream a => a -> Maybe SignatureExt
decodeSignatureDER = decodeSignatureDER' True

-- | DER encoding looks like this:
-- 
-- > 0x30 len [ 0x02 lenR [ R ] 0x02 lenS [ S ] ] SIGHASH
--
-- so that's 7 extra bytes on top of R and S.
--
-- Except when it doesn't look it that... (mostly in MULTISIG transactions). 
-- Of course nothing is documented anywhere.
-- 
-- So the 'Bool' argument controls if we are playing strict ('True') or loose ('False')
--
decodeSignatureDER' :: OctetStream a => Bool -> a -> Maybe SignatureExt
decodeSignatureDER' strict bs 
  | lws < 7     || (strict && lws > 73      ) = Nothing
  | lws < len+2 || (strict && lws /= len + 3) = Nothing
  | head ws /= 0x30                           = Nothing
  | isJust r , isJust s 
    , len_r + len_s + 6 <= lws
    , not strict || (len_r + len_s + 7 == lws)
    , isJust mbsighash                        = Just $ SignatureExt (Signature (fromJust r) (fromJust s)) (fromJust mbsighash)
  | otherwise                                 = Nothing
  where
    mbsighash = decodeSigHash $ last ws -- (if strict then ws else ws++[0x01]) !! (len+2)
    len   = fromIntegral (ws!!1) :: Int
    xxx_r = drop 2 ws
    len_r = fromIntegral (xxx_r!!1) :: Int
    der_r = if head xxx_r == 0x02 then Just (take len_r $ drop 2 xxx_r) else Nothing
    xxx_s = drop (2 + len_r) xxx_r
    len_s = fromIntegral (xxx_s!!1) :: Int
    der_s = if head xxx_s == 0x02 then Just (take len_s $ drop 2 xxx_s) else Nothing
    r     = liftM bigEndianRollInteger der_r :: Maybe Integer
    s     = liftM bigEndianRollInteger der_s :: Maybe Integer
    ws    = toWord8List bs :: [Word8]
    lws   = length ws

--------------------------------------------------------------------------------
-- * \"compact\" encoding of signatures

-- | Decodes a base64-encoded \"compact\" signature
decodeCompactSigBase64 :: Base64 -> Maybe (PubKeyFormat,SignBits,Signature)
decodeCompactSigBase64 str = (base64Decode str :: Maybe [Word8]) >>= decodeCompactSig

encodeCompactSigBase64 :: (PubKeyFormat,SignBits,Signature) -> Base64
encodeCompactSigBase64 what = base64Encode (encodeCompactSig what :: [Word8])

-- | Decodes a 65 bytes long \"compact\" signature.
--
-- First byte is either one of 0x1b, 0x1c, 0x1d, 0x1e (uncompressed public key)
-- or 0x1f, 0x20, 0x21, 0x22 (compressed public key). This information is necessary
-- to recover the public key from the message hash and the signature. In the output 
-- only the relevant two bits of information is retained.
--
-- After that comes 32 bytes R and 32 bytes S.
--
decodeCompactSig :: OctetStream a => a -> Maybe (PubKeyFormat,SignBits,Signature)
decodeCompactSig octets = 

  if n /= 65 || h < 0x1b || h > 0x23 
    then Nothing 
    else Just (fmt,SignBits parities,signat)

  where

    signat = Signature r s

    fmt = if h < 0x1f then Uncompressed else Compressed
    parities = (h - 0x1b) .&. 3  
 
    h  = head ws
    r  = bigEndianRollInteger (take 32 $ drop 1  $ ws)    -- these are without question big-endians. The (bitcoin-qt) source uses BN_bn2bin,
    s  = bigEndianRollInteger (take 32 $ drop 33 $ ws)    -- but it is clear even without that, because of the offsetting there
    n  = length ws
    ws = toWord8List octets

-- | About the Word8: 
-- Bit 0 encodes whether the curve point R (which has x coordinate r from the signature) 
-- has even or odd y coordinate; and bit 1 encodes how to reconstruct the x coordinate from r. The rest of the bits must be zero
--
encodeCompactSig :: OctetStream a => (PubKeyFormat,SignBits,Signature) -> a
encodeCompactSig (pkfmt , SignBits parities , Signature r s) = fromWord8List (h : rr ++ ss) where
  rr = bigEndianInteger32 r
  ss = bigEndianInteger32 s
  h0 = case pkfmt of
    Uncompressed -> 0x1b
    Compressed   -> 0x1f
  h = h0 + (parities .&. 3)

--------------------------------------------------------------------------------
-- * signing message

-- | Signing a bitcoin-QT compatible text message (using the default random number generator in IO).
-- 
signTextMessageIO :: (OctetStream msg) => PrivKey -> msg -> IO (SignBits,Signature)
signTextMessageIO priv msg = getStdRandom $ \gen -> signTextMessage priv msg gen

signRawMessageIO :: (OctetStream msg) => PrivKey -> msg -> IO (SignBits,Signature)
signRawMessageIO priv msg = getStdRandom $ \gen -> signRawMessage priv msg gen

-- | Signing a bitcoin-QT compatible text message
signTextMessage :: (OctetStream msg, RandomGen gen) => PrivKey -> msg -> gen -> ((SignBits,Signature),gen)
signTextMessage priv msg gen = signMessageHash priv (doHash256 $ prepareMessageForSigning msg) gen

signRawMessage :: (OctetStream msg, RandomGen gen) => PrivKey -> msg -> gen -> ((SignBits,Signature),gen)
signRawMessage priv msg gen = signMessageHash priv (doHash256 msg) gen

signTextMessageAddrIO_ :: OctetStream msg => PubKeyFormat -> PrivKey -> msg -> IO Base64
signTextMessageAddrIO_ pkfmt privkey msg = getStdRandom $ \gen -> signTextMessageAddr_ pkfmt privkey msg gen

-- | Bitcoin-QT compatible message signing with the default random generator
-- (can be checked with the address instead of the public key)
-- 
signTextMessageAddrIO :: OctetStream msg => PubKeyFormat -> PrivKey -> msg -> IO (Address,Base64)
signTextMessageAddrIO pkfmt privkey msg = getStdRandom $ \gen -> signTextMessageAddr pkfmt privkey msg gen

signTextMessageAddr_ :: (OctetStream msg, RandomGen gen) => PubKeyFormat -> PrivKey -> msg -> gen -> (Base64,gen)
signTextMessageAddr_ pkfmt privkey msg gen = 
  case signTextMessageAddr pkfmt privkey msg gen of
    ((_,signature),gen') -> (signature,gen')

-- | Bitcoin-QT compatible message signing (can be checked with the address instead of the public key)
-- 
signTextMessageAddr :: (OctetStream msg, RandomGen gen) => PubKeyFormat -> PrivKey -> msg -> gen -> ((Address,Base64),gen)
signTextMessageAddr pkfmt privkey msg gen = ((addr,base64),gen') where
  pubkey = computePubKey Uncompressed privkey
  addr   = pubKeyAddress pubkey
  ((bits,signat),gen') = signTextMessage privkey msg gen
  base64 = encodeCompactSigBase64 (pubKeyFormat pubkey,bits,signat)

--------------------------------------------------------------------------------
-- * signing messages (RFC6979 deterministic signatures)

-- | Signing a bitcoin-QT compatible text message using the deterministic RFC6979 signatures.
signTextMessageRFC6979 :: (OctetStream msg) => PrivKey -> msg -> (SignBits,Signature)
signTextMessageRFC6979 priv msg = signMessageHashRFC6979 priv (doHash256 $ prepareMessageForSigning msg) 

-- | Signing a raw (octet stream) message using the deterministic RFC6979 signatures.
signRawMessageRFC6979 :: (OctetStream msg) => PrivKey -> msg -> (SignBits,Signature)
signRawMessageRFC6979 priv msg = signMessageHashRFC6979 priv (doHash256 msg) 

signTextMessageAddrRFC6979_ :: (OctetStream msg) => PubKeyFormat -> PrivKey -> msg -> Base64
signTextMessageAddrRFC6979_ pkfmt privkey msg = snd $ signTextMessageAddrRFC6979 pkfmt privkey msg

-- | Bitcoin-QT compatible message signing (can be checked with the address instead of the public key),
-- using the deterministic RFC6979 signatures.
--
signTextMessageAddrRFC6979 :: (OctetStream msg) => PubKeyFormat -> PrivKey -> msg -> (Address,Base64)
signTextMessageAddrRFC6979 pkfmt privkey msg = (addr,base64) where
  pubkey = computePubKey Uncompressed privkey
  addr   = pubKeyAddress pubkey
  (bits,signat) = signTextMessageRFC6979 privkey msg 
  base64 = encodeCompactSigBase64 (pubKeyFormat pubkey,bits,signat)

--------------------------------------------------------------------------------
-- * verifying signatures

-- | First argument is the address, second is the base64-encoded \"compact signature\", third is the message.
--
-- TODO: UTF8 encoding!
verifyTextSignatureAddr :: OctetStream msg => Address -> Base64 -> msg -> Bool 
verifyTextSignatureAddr address base64signat text = isJust mbexsignat && isJust mbpubkey && cond1 && cond2 where
  message = toByteString text 
  cond1 = (pubKeyAddress pubkey == address)
  cond2 = verifyTextSignaturePK pubkey signat message
  pubkey   = fromJust mbpubkey
  mbpubkey = recoverTextPubKey exsignat message
  exsignat@(_,_,signat) = fromJust mbexsignat
  mbexsignat            = decodeCompactSigBase64 base64signat

-- | Verifying a bitcoin-QT compatible text signature using the public key
verifyTextSignaturePK :: OctetStream msg => PubKey -> Signature -> msg -> Bool
verifyTextSignaturePK pk signat msg = verifySignatureWithHash pk signat (doHash256 $ prepareMessageForSigning msg)

-- | Verifying a signature for raw data (no bitcoin-QT magic wrapper around the message)
verifyRawSignaturePK :: OctetStream msg => PubKey -> Signature -> msg -> Bool
verifyRawSignaturePK pk signat msg = verifySignatureWithHash pk signat (doHash256 msg)

--------------------------------------------------------------------------------
-- * public key recovery

-- | Recovers the public key from the compact signature and the /text message/ (Bitcoin-QT compatible)
recoverTextPubKey :: OctetStream msg => (PubKeyFormat,SignBits,Signature) -> msg -> Maybe PubKey
recoverTextPubKey exsignat msg = recoverPubKeyFromHash exsignat (doHash256 $ prepareMessageForSigning msg)

-- | Recovers the public key from the compact signature and the raw message (no Bitcoin-QT magic)
recoverRawPubKey :: OctetStream msg => (PubKeyFormat,SignBits,Signature) -> msg -> Maybe PubKey
recoverRawPubKey exsignat msg = recoverPubKeyFromHash exsignat (doHash256 msg)

--------------------------------------------------------------------------------
-- * text message signing (bitcoin-qt compatible)

-- | This is prepended to the message. Only it is not simply prepended...
messageMagic :: B.ByteString
messageMagic = B.pack $ map (char_to_word8) $ "Bitcoin Signed Message:\n"

-- | Now, this is a seriously braindead and completely undocumented protocol
prepareMessageForSigning :: OctetStream a => a -> B.ByteString
prepareMessageForSigning origmsg = B.concat [sizMagic,messageMagic,siz,msg] where

  msg = toByteString origmsg

  sizMagic = B.pack $ encodeVarInt $ B.length messageMagic
  siz      = B.pack $ encodeVarInt $ B.length msg

  encodeVarInt :: Int -> [Word8]
  encodeVarInt n 
    | n <  0           =  error "prepareMessageForSigning/encodeVarInt: negative input, shouldn't happen"
    | n <= 0xfc        =  [fromIntegral n]
    | n <= 0xffff      =  0xfd : leInt 2 n
    | n <= 0xffffffff  =  0xfe : leInt 4 n
    | otherwise        =  0xff : leInt 8 n

  leInt :: Int -> Int -> [Word8] 
  leInt k n = take k $ littleEndianUnrollInteger (fromIntegral n) ++ replicate k 0

-- | The message hash function we use for signing message.
--
-- The bool parameter specifies whether to sign the raw message or the
-- really stupidly serialized and magic prefixed text version...

-- (fuck, I just spent a whole day trying to figure out why my 
-- code doesn't give the same result as the official client...)
messageHash :: OctetStream msg => Bool -> msg -> Hash256
messageHash textmagic message 
  = doHash256
  $ if textmagic then (prepareMessageForSigning message) else (toByteString message)

--------------------------------------------------------------------------------
