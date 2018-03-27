
-- | Create/sign standard transactions
{-# LANGUAGE ScopedTypeVariables #-}
module Bitcoin.Protocol.Tx where

--------------------------------------------------------------------------------

import Data.Word

import qualified Data.ByteString as B

import System.Random

import Bitcoin.Protocol.Address
import Bitcoin.Protocol.Amount 
import Bitcoin.Protocol.Signature 
import Bitcoin.Protocol.Key

import Bitcoin.BlockChain.Tx
import Bitcoin.BlockChain.Parser ( serializeTx )

import Bitcoin.Script.Base
import Bitcoin.Script.Standard
import Bitcoin.Script.Run
import Bitcoin.Script.Serialize

import Bitcoin.Misc.Bifunctor
import Bitcoin.Misc.Tuple

--------------------------------------------------------------------------------

data StdTxInput a = StdTxInput
  { _prevTx         :: !(Tx a RawScript)
  , _prevOutIndex   :: !Int
  , _prevOutPrivKey :: !PrivKey
  }

data StdTxOutput = StdTxOutput
  { _outAddress     :: !Address
  , _outAmount      :: !Amount
  }

{-
-- | Creates a standard transaction which spends outputs of previous standard transactions.
spendStandardTx :: [StdInput] -> [StdOutput] -> Either String (Tx RawScript RawScript, Amount)
spendStandardTx inputs outputs  
-}

--------------------------------------------------------------------------------

{-
-- | Creates a standard transaction which spends output(s) of previous standard transaction(s). Returns also the amount of fee.
createStandardTx' :: Tx (Tx a RawScript, PrivKey) Address -> Either String (Tx RawScript RawScript, Amount)
createStandardTx' preparedTx = 
  case all isPayToAddress recInputs of
    False -> Left "inputs are not all PayToAddress scripts"
    True  -> preparedTx { _txInputs = newInputs , _txOutputs = newOutputs , _txHash = newHash }
  where
    inputs    = map (fmap fst         ) $ _txInputs preparedTx recognizeTxInput :: [TxInput (Tx a RawScript)]
    privkeys  = map (snd . _txInScript) $ _txInputs preparedTx recognizeTxInput :: [PrivKey]
    recInputs = map recognizeTxInput inputs :: [TxInput InputScript]
  
    flippedZipWith inputs privkeys $ \inp privkey -> 
-}

--------------------------------------------------------------------------------

-- signRawMessage :: (OctetStream msg, RandomGen gen) => PrivKey -> msg -> gen -> ((SignBits,Signature),gen)

{-
signInputs :: forall a gen. RandomGen gen => Tx (Tx a RawScript, PrivKey) RawScript -> gen -> (Either String (SignatureExt,PubKey),gen)
signInputs privkey txext gen = (result,gen') where

  signOneInput :: Int -> Either (SignatureExt,PubKey)
  signOneInput k = 
    case subscript pkscript of
      Left err -> Left err
    inpExt = ((_txInputs txExt) !! k) :: TxInput (Tx a RawScript, PrivKey)
    outidx = _txInPrevOutIdx inpExt
    (prevtx, privkey) = _txInScript inpExt
    pubkey = computePubKey Compressed privkey 
    pkscript = (_txOutScript prevtx) !! outidx
-}


-- | Signs a (standard, and all previous outputs are pay-to-address) transaction
signTransaction :: forall a gen. RandomGen gen => Tx (Tx a RawScript, PrivKey) RawScript -> gen -> (Either String (Tx RawScript RawScript) ,gen)
signTransaction newTxExt gen0 = result where

  result = case mapAccumLFst worker (Right 0, gen0) newTxExt of
    ((Left err , gen1) , _      ) -> (Left err, gen1)
    ((Right _  , gen1) , finalTx) -> 
      let prevs      = map fst $ toListFst newTxExt :: [Tx a RawScript]
          finalTxExt = zipWithFst (,) prevs finalTx
      in  case checkTransaction finalTxExt of
            Left err -> (Left err, gen1)
            Right b  -> if b
              then (Right finalTx, gen1)
              else (Left "cannot verify the signed transaction", gen1)

  undefRawScript :: RawScript
  undefRawScript = error "signTransaction/undefRawScript: shouldn't be evaluated"

  worker :: (Either String Int, gen) -> (Tx a RawScript, PrivKey) -> ((Either String Int, gen), RawScript)
  worker (Left err, gen) _  = ((Left err,gen) , undefRawScript)
  worker (Right k , gen) (prevtx,privkey) =
    case signSingleInput privkey sigHashAll k prevtx newTxExt gen of
      Left err -> ((Left err, gen), undefRawScript)
      Right ((sigext,pubkey),gen') -> ((Right (k+1), gen') , sigScript) where
        sigScript = createInputScript $ RedeemAddress sigext pubkey 

-- | Signs a single input of a transaction
signSingleInput :: forall a b gen. RandomGen gen => PrivKey -> SigHash -> Int -> Tx a RawScript -> Tx b RawScript -> gen -> Either String ((SignatureExt,PubKey),gen)
signSingleInput privkey sighash inpidx prevtx thistx gen = result where

  result = case safeLookup inpidx thisinps of
    Nothing -> Left "signSingleInput: input index out of range"
    Just inp -> 
      let outidx = _txInPrevOutIdx inp
      in  case safeLookup (fromIntegral outidx) prevouts of
        Nothing -> Left "signSingleOutput: prev output index out of range"
        Just prevout -> 
          let pkscript = _txOutScript prevout
          in  case getSubscript pkscript of
                Left  err    -> Left err
                Right subscript -> 
                  let txcopy    = replaceTxIns emptyRawScript (inpidx,subscript) thistx
                      RawTx raw = serializeTx txcopy
                      msg = B.append raw (B.pack [encodeSigHash sighash, 0,0,0::Word8])
                      ((signbits,signat),gen') = signRawMessage privkey msg gen
                      sigext = SignatureExt signat sighash
                  in  Right ((sigext,pubkey),gen')

  thisinps = _txInputs  thistx :: [TxInput b]
  prevouts = _txOutputs prevtx :: [TxOutput RawScript]
  pubkey = computePubKey Compressed privkey 

  safeLookup :: forall x. Int -> [x] -> Maybe x
  safeLookup n xs 
    | n<0   = Nothing
    | n==0  = case xs of { (x:_) -> Just x              ; [] -> Nothing }
    | True  = case xs of { (x:_) -> safeLookup (n-1) xs ; [] -> Nothing }      
 
  -- not fully correct, as the full verification is braindeadly complicated because of the CODESEPARATOR mess :(
  -- (which was never ever used as far as I know...)
  getSubscript :: RawScript -> Either String RawScript
  getSubscript full = case parseScript full of
    Nothing -> Left    "signInput/subscript: cannot parse pkScript"
    Just pk -> Right $ serializeScript $ Script $ reverse $ takeWhile (/=OP_CODESEPARATOR) $ reverse $ fromScript $ pk 
  
  -- | We replace all inputs with @def@ except the kth which we replace by @spec@
  replaceTxIns :: forall a b c. b -> (Int,b) -> Tx a c -> Tx b c
  replaceTxIns def (k,spec) tx = mapAccumLFst_ worker 0 tx where
    worker j _ = if j==k then (j+1,spec) else (j+1,def)

--------------------------------------------------------------------------------

isPayToAddress :: OutputScript -> Bool
isPayToAddress s = case s of
  PayToAddress {} -> True
  _               -> False

isPayToPubKey :: OutputScript -> Bool
isPayToPubKey s = case s of
  PayToPubKey {} -> True
  _              -> False

--------------------------------------------------------------------------------
  