
-- | Standard scripts

{-# LANGUAGE PatternGuards #-}
module Bitcoin.Script.Standard where

---------------------------------------------------------------------------------

import Data.Int
import Data.Word
import Data.Maybe

import Control.Monad

import qualified Data.ByteString as B

import Bitcoin.Misc.OctetStream

import Bitcoin.Script.Base 
import Bitcoin.Script.Serialize
import Bitcoin.BlockChain.Base 

-- import Bitcoin.Crypto.ECDSA

import Bitcoin.Protocol.Hash
import Bitcoin.Protocol.Key
import Bitcoin.Protocol.Signature

--------------------------------------------------------------------------------

-- | Standard input scripts (also known as \"scriptSig\")
data InputScript
  = CoinGeneration !RawScript                     -- ^ coin generation (the script has no meaning)
  | RedeemAddress  !SignatureExt !PubKey          -- ^ redeem a standard (to-address) transaction: \<sig\> \<pubKey\>
  | RedeemPubKey   !SignatureExt                  -- ^ redeem a to-pubkey transaction (?)
  | RedeemMultiSig [SignatureExt]                 -- ^ redeem a multisig transaction
  | RedeemP2SH     [SignatureExt] !RawScript      -- ^ redeem a pay-to-script-hash transaction (???)
  | RedeemEmpty                                   -- ^ OP_TRUE - accepts an AnyCanSpend script
  | UnknownInput   (Either RawScript Script)      -- ^ something else (that is, non-standard)
  deriving (Eq,Show)

isStandardInputScript :: InputScript -> Bool
isStandardInputScript input = case input of
  UnknownInput {} -> False
  _               -> True

--------------------------------------------------------------------------------

-- | Standard output scripts (also known as \"scriptSig\")
data OutputScript
  = PayToAddress    !PubKeyHash                -- ^ OP_DUP OP_HASH160 \<pubKeyHash\> OP_EQUALVERIFY OP_CHECKSIG
  | PayToPubKey     !PubKey                    -- ^ \<pubKey\> OP_CHECKSIG (accept generation, for example, but also others?)
  | PayToMultiSig   !Int [PubKey]              -- ^ eg. OP_2 \<pubkey1\> \<pubkey2\> \<pubkey3\> OP_3 OP_CHECKMULTISIG
  | PayToScriptHash !ScriptHash                -- ^ OP_HASH160 \<script-hash\> OP_EQUAL
  | Unspendable                                -- ^ OP_RETURN {zero or more ops}
  | AnyCanSpend                                -- ^ (empty)
  | UnknownOutput   (Either RawScript Script)  -- ^ something else (that is, non-standard)
  deriving (Eq,Show)

isStandardOutputScript :: OutputScript -> Bool
isStandardOutputScript output = case output of
  UnknownOutput {} -> False
  _                -> True

--------------------------------------------------------------------------------

-- | Note: RedeemP2SH is a hack. I should think again how that should work (certainly not this way...)
createInputScript :: InputScript -> RawScript
createInputScript input = case input of
  CoinGeneration raw -> raw
  RedeemAddress  sigext pubkey -> serializeScript $ Script [ op_PUSHDATA (encodeSignatureDER sigext) , op_PUSHDATA (encodePubKeyNative pubkey) ]
  RedeemPubKey   sigext        -> serializeScript $ Script [ op_PUSHDATA (encodeSignatureDER sigext) ]
  RedeemMultiSig sigexts       -> serializeScript $ Script $ op_0 : [ op_PUSHDATA (encodeSignatureDER sigext) | sigext <- sigexts ] 
  RedeemP2SH     sigexts raw   -> case sigexts of
    [single] -> serializeScript $ Script          [ op_PUSHDATA (encodeSignatureDER single) , op_PUSHDATA (fromRawScript raw) ]
    many     -> serializeScript $ Script $ op_0 : [ op_PUSHDATA (encodeSignatureDER sigext) | sigext <- many ] ++ [ op_PUSHDATA (fromRawScript raw) ]
  RedeemEmpty                  -> serializeScript $ Script [ op_TRUE ]
  UnknownInput ei              -> case ei of { Left raw -> raw ; Right script -> serializeScript script }

createTxInput :: TxInput InputScript -> TxInput RawScript
createTxInput txin0 = 
  case (_txInScript txin0) of
    CoinGeneration raw -> txin1 { _txInPrevOutHash = zeroHash256 , _txInPrevOutIdx = 0xffffffff, _txInSeqNo = 0xffffffff }
    _                  -> txin1
  where 
    txin1 = fmap createInputScript txin0

--------------------------------------------------------------------------------

createOutputScript :: OutputScript -> RawScript
createOutputScript output = case output of
  PayToAddress (PubKeyHash hash)    -> serializeScript $ Script [ OP_DUP , OP_HASH160 , op_PUSHDATA (toByteString hash) , OP_EQUALVERIFY , OP_CHECKSIG ]
  PayToPubKey  pubkey               -> serializeScript $ Script [ op_PUSHDATA (encodePubKeyNative pubkey) , OP_CHECKSIG ]
  PayToMultiSig k pubkeys           -> serializeScript $ Script $ (OP_SMALLNUM k) : [ op_PUSHDATA (encodePubKeyNative pubkey) | pubkey <- pubkeys ] ++ [ OP_SMALLNUM (length pubkeys) , OP_CHECKMULTISIG ]
  PayToScriptHash (ScriptHash hash) -> serializeScript $ Script [ OP_HASH160 , op_PUSHDATA (toByteString hash) , OP_EQUAL ]
  Unspendable                       -> serializeScript $ Script [ OP_RETURN ]
  AnyCanSpend                       -> serializeScript $ Script [ ]
  UnknownOutput   ei                -> case ei of { Left raw -> raw ; Right script -> serializeScript script }

createTxOutput :: TxOutput OutputScript -> TxOutput RawScript
createTxOutput = fmap createOutputScript

--------------------------------------------------------------------------------

-- | Recognize standard input scripts (also known as \"scriptSig\"). 
--
-- We need the whole TxInput since coinbase transactions are not possible to recognize only from the script (which can be anything).
recognizeTxInput :: TxInput RawScript -> TxInput InputScript
recognizeTxInput tx@(TxInput prevouthash prevoutidx rawscript seqno) 
  | prevouthash == zeroHash256   =  final $ CoinGeneration rawscript
  | otherwise                    =  final $ recognizeInputScript rawscript
  where
    final new = tx { _txInScript = new }

-- | Note: this cannot recognize coinbase transactions.
recognizeInputScript :: RawScript -> InputScript
recognizeInputScript rawscript

  | Just [OP_PUSHDATA _ dersig, OP_PUSHDATA _ rawpubkey] <- mbscript
  , Just sig    <- decodeSignatureDER' False dersig
  , Just pubkey <- decodePubKey rawpubkey 
       = RedeemAddress sig pubkey

  | Just [OP_PUSHDATA _ dersig] <- mbscript
  , Just sig    <- decodeSignatureDER' False dersig
       = RedeemPubKey sig 

  | Just (OP_SMALLNUM 0 : dersignatures) <- mbscript   -- OP_SMALLNUM 0 is there because of a bug in OP_CHECKMULTISIG (?!)
  , mbders  <- map is_op_pushdata dersignatures 
  , mbsigs <- map (>>= decodeSignatureDER' False) mbders      
  , all isJust mbsigs
       = RedeemMultiSig (map fromJust mbsigs)

  -- p2sh, single signature
  | Just [OP_PUSHDATA _ dersig , OP_PUSHDATA _ rawscript] <- mbscript     
  , Just sig     <- decodeSignatureDER' False dersig
  , outputscript <- recognizeOutputScript (RawScript rawscript)
  , isStandardOutputScript outputscript
       = RedeemP2SH [sig] (RawScript rawscript)          

  -- p2sh, multiple signatures and an OP_0 in the front (because we expect CHECK_MULTISIG, which needs an extra stack entry, typically OP_0)
  | Just (OP_SMALLNUM 0 : dersigs_and_rawscript) <- mbscript     
  , OP_PUSHDATA _ rawscript <- last dersigs_and_rawscript
  , mbders  <- map is_op_pushdata (init dersigs_and_rawscript)
  , mbsigs <- map (>>= decodeSignatureDER' False) mbders      
  , all isJust mbsigs
  , outputscript <- recognizeOutputScript (RawScript rawscript)
  , isStandardOutputScript outputscript
       = RedeemP2SH (map fromJust mbsigs) (RawScript rawscript)          
        
  | Just [OP_SMALLNUM 1] <- mbscript      -- OP_TRUE == OP_SMALLNUM 1
       = RedeemEmpty

  | otherwise
       = UnknownInput 
       $ case mbscript of { Nothing -> Left rawscript ; Just ops -> Right (Script ops) } 

  where
    mbscript = liftM fromScript $ parseScript rawscript :: Maybe [Opcode]

--------------------------------------------------------------------------------

-- | Recognize standard output scripts (also known as \"scriptPubKey\")
recognizeTxOutput :: TxOutput RawScript -> TxOutput OutputScript
recognizeTxOutput tx@(TxOutput value rawscript) = tx { _txOutScript = recognizeOutputScript rawscript }

recognizeOutputScript :: RawScript -> OutputScript
recognizeOutputScript rawscript 

  | Just [OP_DUP, OP_HASH160, OP_PUSHDATA _ pubKeyHash, OP_EQUALVERIFY, OP_CHECKSIG] <- mbscript
  , B.length pubKeyHash == 20 
       = PayToAddress (PubKeyHash $ fromByteString pubKeyHash)

  | Just [OP_PUSHDATA _ rawPubKey, OP_CHECKSIG] <- mbscript
  , Just pubKey <- decodePubKey rawPubKey
       = PayToPubKey pubKey

  | Just [OP_HASH160, OP_PUSHDATA _ scriptHash, OP_EQUAL] <- mbscript
  , B.length scriptHash == 20 
       = PayToScriptHash (ScriptHash $ fromByteString scriptHash)

  | Just script <- mbscript
  , Just (m,pubkeys) <- recogPayToMultiSig script
       = PayToMultiSig m pubkeys

  | Just [] <- mbscript
       = AnyCanSpend

  | Just (OP_RETURN:_) <- mbscript
       = Unspendable

  | otherwise
       = UnknownOutput
       $ case mbscript of { Nothing -> Left rawscript ; Just ops -> Right (Script ops) } 

  where
    mbscript  = liftM fromScript $ parseScript rawscript :: Maybe [Opcode]

--------------------------------------------------------------------------------

recogPayToMultiSig :: [Opcode] -> Maybe (Int,[PubKey])
recogPayToMultiSig ops 

  | [ OP_SMALLNUM m ] <- take 1 ops
  , [ OP_CHECKMULTISIG , OP_SMALLNUM n ] <- take 2 (reverse ops)
  , length ops == n + 3
  , mbrawdats <- map is_op_pushdata $ take n $ drop 1 $ ops
  , all isJust mbrawdats
  , rawdats <- map fromJust mbrawdats
  , mbpubkey <- map decodePubKey rawdats
  , all isJust mbpubkey  
      = Just (m, map fromJust mbpubkey)

  | otherwise = Nothing

--------------------------------------------------------------------------------

recognizeTx :: Tx RawScript RawScript -> Tx InputScript OutputScript 
recognizeTx tx@(Tx ver ins outs lock hash) = tx { _txInputs = ins' , _txOutputs = outs' } where
  ins'  = map recognizeTxInput  ins
  outs' = map recognizeTxOutput outs

recognizeBlockTxs :: Block (Tx RawScript RawScript) -> Block (Tx InputScript OutputScript)
recognizeBlockTxs (Block header txs) = Block header (map recognizeTx txs)

--------------------------------------------------------------------------------

