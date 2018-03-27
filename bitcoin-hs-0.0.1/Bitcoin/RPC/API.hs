
-- | bitcoind API access (via JSON-RPC over HTTP calls)
--
-- See <https://en.bitcoin.it/wiki/Original_Bitcoin_client/API_calls_list>
--

{-# LANGUAGE PatternGuards #-}
module Bitcoin.RPC.API where

--------------------------------------------------------------------------------

import Data.Word
import Data.Bits
import Data.Maybe

import Control.Applicative

import Text.JSON
import Text.JSON.Types

import qualified Data.ByteString as B

import Bitcoin.Misc.HexString
import Bitcoin.Misc.OctetStream
import Bitcoin.Misc.UnixTime

import Bitcoin.Protocol.Address
import Bitcoin.Protocol.Amount
import Bitcoin.Protocol.Base64
import Bitcoin.Protocol.Hash
import Bitcoin.Protocol.Key
import Bitcoin.Protocol.Signature

import Bitcoin.Script.Base
import Bitcoin.BlockChain.Base

import Bitcoin.RPC.JSON 
import Bitcoin.RPC.Call

--------------------------------------------------------------------------------

-- | An account in the Satoshi client wallet  
type Account = String 

-- | A network node
type Node = String

-- | A transaction id
type TxId = Hash256

-- | A public key
type Key = Either PubKey Address 

-- | Minimum number of confirmations (default is usually 1)
type MinConf = Maybe Int 
type MaxConf = Maybe Int

-- | A redeem script for a multi-sig address
type RedeemScript = RawScript

type PassPhrase = String

------------------

data AddNodeCmd 
  = NodeAdd 
  | NodeRemove 
  | NodeOneTry 
  deriving (Eq,Show)

--------------------------------------------------------------------------------

{-

-- | Subset of satoshi client API calls. 
-- See <https://en.bitcoin.it/wiki/Original_Bitcoin_client/API_Calls_list>
--
data APICALL  
  --  client info
  = GetInfo                                            -- ^ Returns an object containing various state info.   
  | GetConnectionCount                                 -- ^ Returns the number of connections to other nodes.   
  | Stop                                               -- ^ Stop bitcoin server.   
  --  blockchain info
  | GetDifficulty                                      -- ^ Returns the proof-of-work difficulty as a multiple of the minimum difficulty.   
  | GetBlock               Hash256                     -- ^ Returns information about the given block hash.   
  | GetBlockCount                                      -- ^ Returns the number of blocks in the longest block chain.  
  | GetBlockHash           Int                         -- ^ Returns hash of block in best-block-chain at \<index\>    
  --  transaction info
  | GetTransaction         TxId                        -- ^ Returns an object about the given transaction containing: \"amount\", \"confirmations\", \"txid\", \"time\", \"details\" (details is an array of objects containing: \"account\", \"address\", \"category\", \"amount\", \"fee\")
  | DecodeRawTransaction   B.ByteString                -- ^ version 0.7 Produces a human-readable JSON object for a (hex string of a) raw transaction.   
  --  wallet info
  | ValidateAddress        Address                     -- ^ Return information about \<bitcoinaddress\>.   
  | DumpPrivKey            Address                     -- ^ Reveals the private key corresponding to \<bitcoinaddress\>. NOTE: Wallet needs to be unlocked! 
  | GetBalance             (Maybe Account) MinConf     -- ^ If [account] is not specified, returns the server's total available balance. Int is # of minimum confirmations
  | GetAccountAddress      Account                     -- ^ Returns the current bitcoin address for receiving payments to this account.   
  | GetAddressesByAccount  Account                     -- ^ Returns the list of addresses for the given account.   
  | GetAccount             Address                     -- ^ Returns the account associated with the given address.   
  | ListAccounts           MinConf                     -- ^ Returns Object that has account names as keys, account balances as values. Int is number of minimal confirmations
  | ListAddressGroupings                               -- ^ version 0.7 Returns all addresses in the wallet and info used for coincontrol.   
  | ListReceivedByAccount  MinConf Bool                -- ^ Int is minconf. Bool is \"includeempty\". Returns an array of objects containing: \"account\", \"amount\", \"confirmations\"
  | ListReceivedByAddress  MinConf Bool                -- ^ Returns an array of objects containing: \"address\", \"account\", \"amount\", \"confirmations\". To get a list of accounts on the system, execute bitcoind listreceivedbyaddress 0 true   
  --  network info
  | GetRawMemPool                                      -- ^ version 0.7 Returns all transaction ids in memory pool                                   
  --  multi-sig
  | AddMultiSigAddress     Int [Key] (Maybe Account)   -- ^ Add a n-required-to-sign multisignature address to the wallet. Each key is a bitcoin address or hex-encoded public key. If [account] is specified, assign address to [account].  
  | CreateMultiSig         Int [Key]                   -- ^ Creates a multi-signature address and returns a json object 
  --  sending coins
  | SendFrom               Account Address Amount MinConf (Maybe String) (Maybe String) -- ^ \<fromaccount\> \<tobitcoinaddress\> \<amount\> [minconf=1] [comment] [comment-to]. Will send the given amount to the given address, ensuring the account has a valid balance using [minconf] confirmations. Returns the transaction ID if successful (not in JSON object). NOTE: Wallet needs to be unlocked! 
  | SendMany               Account [(Address,Amount)] MinConf (Maybe String)            -- ^ Amounts are double-precision floating point numbers. NOTE: Wallet needs to be unlocked! 
  | SendRawTransaction     B.ByteString                                                 -- ^ version 0.7 Submits raw transaction (serialized, hex-encoded) to local node and network.   
  | SendToAddress          Address Amount (Maybe String) (Maybe String)                 -- ^ \<amount\> is a real and is rounded to 8 decimal places. Returns the transaction ID \<txid\> if successful. NOTE: Wallet needs to be unlocked! 
  | Move                   Account Account Amount MinConf (Maybe String)                -- ^ \<fromaccount\> \<toaccount\> \<amount\> [minconf=1] [comment]. Move from one account in your wallet to another   
  | SetTxFee               Amount                                                       -- ^ \<amount\> is a real and is rounded to the nearest 0.00000001   
  --  signing
  | SignMessage            Address String              -- ^ Sign a message with the private key of an address. NOTE: Wallet needs to be unlocked! 
  | VerifyMessage          Address Signature String    -- ^ Verify a signed message.   
  --  wallet operations
  | GetNewAddress          (Maybe Account)             -- ^ Returns a new bitcoin address for receiving payments. If [account] is specified (recommended), it is added to the address book so payments received with the address will be credited to [account].   
  | ImportPrivKey          PrivKey (Maybe String) Bool -- ^ Adds a private key (as returned by dumpprivkey) to your wallet. This may take a while, as a rescan is done, looking for existing transactions. Optional [rescan] parameter added in 0.8.0. NOTE: Wallet needs to be unlocked! 
  | BackupWallet           FilePath                    -- ^ Safely copies wallet.dat to destination, which can be a directory or a path with filename.  
  | EncryptWallet          String                      -- ^ Encrypts the wallet with \<passphrase\>.   
  | WalletLock                                         -- ^ Removes the wallet encryption key from memory, locking the wallet. After calling this method, you will need to call walletpassphrase again before being able to call any methods which require the wallet to be unlocked.   
  | WalletPassPhrase       String Int                  -- ^ Stores the wallet decryption key in memory for \<timeout\> seconds.   
  | WalletPassPhraseChange String String               -- ^ Changes the wallet passphrase from \<oldpassphrase\> to \<newpassphrase\>.  
  | KeyPoolRefill                                      -- ^ Fills the keypool, requires wallet passphrase to be set. NOTE: Wallet needs to be unlocked! 

-}

--------------------------------------------------------------------------------
-- * client info

data ClientInfo = ClientInfo
  { _cliClientVersion     :: (Int,Int,Int)
  , _cliProtocolVersion   :: (Int,Int,Int)
  , _cliWalletVersion     :: (Int,Int,Int)
  , _cliTotalBalance      :: Amount
  , _cliNumberOfBlocks    :: Int
  , _cliTimeOffset        :: Double    -- ^ in hours, relative to GMT, but negated (?)
  , _cliNoConnections     :: Int
  , _cliProxy             :: String
  , _cliCurrentDifficulty :: Double
  , _cliOnTestnet         :: Bool
  , _cliKeyPoolOldest     :: UnixTimeStamp
  , _cliKeyPoolSize       :: Int
  , _cliPayTxFee          :: Amount 
  } 
  deriving Show

-- | Returns an object containing various state info.
getClientInfo :: Call ClientInfo
getClientInfo = makeCall "getinfo" () $ \js -> case js of
  JSObject obj -> 
    case obj of
      _ | Just cver <- lkp "version"
        , Just pver <- lkp "protocolversion"
        , Just wver <- lkp "walletversion"   
        , Just bal  <- lkp "balance"
        , Just nblk <- lkp "blocks"
        , Just tofs <- lkp "timeoffset"         
        , Just ncon <- lkp "connections"
        , Just prxy <- lkp "proxy"
        , Just diff <- lkp "difficulty"
        , Just test <- lkp "testnet"
        , Just old  <- lkp "keypoololdest"
        , Just pool <- lkp "keypoolsize"
        , Just fee  <- lkp "paytxfee"
                                                -> Just $ ClientInfo 
                                                     (parseVer cver) 
                                                     (parseVer pver)
                                                     (parseVer wver)
                                                     bal
                                                     nblk
                                                     tofs
                                                     ncon
                                                     prxy
                                                     diff
                                                     test
                                                     old
                                                     pool
                                                     fee
      _ -> Nothing
    where

      lkp :: JSON a => String -> Maybe a
      lkp fld = get_field obj fld >>= myReadJSON
{-
      lkp fld = case get_field obj fld of
        Just js -> case readJSON js of 
          Ok x    -> Just x 
          Error _ -> Nothing 
        Nothing -> Nothing
-}

      parseVer :: Int -> (Int,Int,Int)
      parseVer n = (a,b,c) where 
        (a,tmp) = divMod n 10000
        (b,c)   = divMod tmp 100

  _ -> Nothing

-- | Returns the number of connections to other nodes
getConnectionCount :: Call Int
getConnectionCount = makeCall "getconnectioncount" () myReadJSON

-- | Stops the bitcoin client
stopClient :: Call ()  
stopClient = makeCall "stop" () $ \_ -> Just ()

--------------------------------------------------------------------------------
-- * blockchain info

data BlockInfo = BlockInfo
  { _bliHash           :: Hash256
  , _bliConfirmations  :: Int
  , _bliSize           :: Int 
  , _bliHeight         :: Int
  , _bliVersion        :: Int
  , _bliMerkleRoot     :: Hash256
  , _bliTxHashes       :: [Hash256]
  , _bliTime           :: UnixTimeStamp
  , _bliNonce          :: Word32
  , _bliDifficultyBits :: Word32 
  , _bliDifficulty     :: Double
  , _bliPrevHash       :: Maybe Hash256
  , _bliNextHash       :: Maybe Hash256
  }
  deriving Show

-- | Returns the proof-of-work difficulty as a multiple of the minimum difficulty.
getDifficulty :: Call Double
getDifficulty = makeCall "getdifficulty" () myReadJSON

-- | Returns the number of blocks in the longest block chain.
getBlockCount :: Call Int
getBlockCount = makeCall "getblockcount" () myReadJSON

-- | Returns hash of block in best-block-chain at \<index\> transaction info
getBlockHash :: Int -> Call Hash256
getBlockHash n = makeCall "getblockhash" [n] myReadJSON 

{-
 ("hash",JSString (JSONString {fromJSString = "00000000000000205868fe2a3147ac3ca6061e5d21a4cfdf345adc0282993c68"}))
,("confirmations",JSRational False (1 % 1))
,("size",JSRational False (138687 % 1))
,("height",JSRational False (249228 % 1))
,("version",JSRational False (2 % 1))
,("merkleroot",JSString (JSONString {fromJSString = "fe750a784090601e6bda1fbb9da8f5528706844877892545db2d78702d7172b5"}))
,("tx",JSArray [ ... ] )
,("time",JSRational False (1375184425 % 1))
,("nonce",JSRational False (3818843198 % 1))
,("bits",JSString (JSONString {fromJSString = "1a008968"}))
,("difficulty",JSRational False (3125696072776893 % 100000000))
,("previousblockhash",JSString (JSONString {fromJSString = "0000000000000077d150668c20f4ee2bfd19696b14a134cdaf7dc682e82bcbe5"}))]}))
-}

-- | Returns information about the given block hash.
getBlockInfo :: Hash256 -> Call BlockInfo
getBlockInfo blockhash = makeCall "getblock" [blockhash] $ \js -> case js of
  JSObject obj -> 
    case obj of
      _ | Just hash <- lkp "hash"
        , Just conf <- lkp "confirmations"
        , Just size <- lkp "size"
        , Just hght <- lkp "height"
        , Just ver  <- lkp "version"
        , Just root <- lkp "merkleroot"
        , Just txs  <- lkp "tx"
        , Just time <- lkp "time"
        , Just nonc <- lkp "nonce"
        , Just bstr <- lkp "bits" , Just bits <- parseBits bstr
        , Just diff <- lkp "difficulty"
        , mbprev    <- lkp "previousblockhash"
        , mbnext    <- lkp "nextblockhash"
                                                -> Just $ BlockInfo 
                                                     hash
                                                     conf
                                                     size
                                                     hght
                                                     ver
                                                     root
                                                     txs
                                                     time
                                                     nonc
                                                     bits
                                                     diff
                                                     mbprev
                                                     mbnext
      _ -> Nothing
    where

      lkp :: JSON a => String -> Maybe a
      lkp fld = get_field obj fld >>= myReadJSON

      parseBits bitstring = case safeHexDecode bitstring of
        Just [a,b,c,d] -> Just $ shiftL (fromIntegral a) 24 
                               + shiftL (fromIntegral b) 16
                               + shiftL (fromIntegral c)  8
                               +        (fromIntegral d) 
        Nothing -> Nothing

  _ -> Nothing

--------------------------------------------------------------------------------
-- * transaction info

data TxDetail = TxDetail 
  { _txdAccount  :: Account
  , _txdAddress  :: Address
  , _txdCategory :: String
  , _txdAmount   :: Amount
  , _txdFee      :: Amount
  } 
  deriving Show

data TxInfo = TxInfo
  { _txiAmount        :: Amount
  , _txiConfirmations :: Int                                      
  , _txiId            :: TxId
  , _txiTime          :: UnixTimeStamp
  , _txiDetails       :: [TxDetail]
  } 
  deriving Show

instance JSON TxDetail where
  readJSON jsv = case parseTxDetail jsv of
    Nothing -> Error "TxDetail/readJSON: cannot parse"
    Just x  -> Ok x  
  showJSON = error "TxDetail/showJSON: not implemented"

parseTxDetail :: JSValue -> Maybe TxDetail
parseTxDetail jsv = case jsv of
  JSObject obj -> 
    case obj of
      _ | Just acc  <- lkp "account"
        , Just addr <- lkp "address"
        , Just cat  <- lkp "category"
        , Just amt  <- lkp "amount"
        , Just fee  <- lkp "fee"
           -> Just $ TxDetail acc addr cat amt fee
      _ -> Nothing
    where
      lkp :: JSON a => String -> Maybe a
      lkp fld = get_field obj fld >>= myReadJSON
  _ -> Nothing

parseTxInfo :: JSValue -> Maybe TxInfo
parseTxInfo jsv = case jsv of
  JSObject obj -> 
    case obj of
      _ | Just amt  <- lkp "amount"
        , Just conf <- lkp "confirmations"
        , Just txid <- lkp "txid"
        , Just time <- lkp "time"
        , Just (JSArray dets) <- lkp "details"
        , mbdetails <- map parseTxDetail dets
        , all isJust mbdetails
           -> Just $ TxInfo amt conf txid time (catMaybes mbdetails)
      _ -> Nothing
    where
      lkp :: JSON a => String -> Maybe a
      lkp fld = get_field obj fld >>= myReadJSON
  _ -> Nothing

-- | Returns an object about the given transaction. Note: this only works for the transaction in the wallet.
-- For transactions out in the blockchain, use "getRawTransaction" or "getTransactionInfo"
getWalletTransaction :: TxId -> Call TxInfo
getWalletTransaction txid = makeCall "getrawtransaction" (txid, (0::Int)) parseTxInfo

--------------------------------------------------------------------------------

-- | A scriptSig as returned by \"getrawtransaction\" (verbose=1) API call
data ScriptSigVerbose = ScriptSigVerbose
  { _scriptSigAsm :: String      -- ^ human-readable script
  , _scriptSigHex :: !RawScript  -- ^ raw script
  }
  deriving (Eq,Show)

parseScriptSigVerbose :: JSValue -> Maybe ScriptSigVerbose
parseScriptSigVerbose jsv = case jsv of
  JSObject obj -> 
    case obj of
      _ | Just asm <- lkp "asm"
        , Just hex <- lkp "hex"
           -> Just $ ScriptSigVerbose asm hex
      _ -> Nothing
    where
      lkp :: JSON a => String -> Maybe a
      lkp fld = get_field obj fld >>= myReadJSON
  _ -> Nothing

-- | A transaction input as returned by \"getrawtransaction\" (verbose=1) API call
data TxVIn = TxVIn                    
  { _vinTxId         :: !TxId              -- ^ hash of the input transaction
  , _vinVOut         :: !Int               -- ^ which output of the input transaction we spend
  , _vinScriptSig    :: !ScriptSigVerbose  -- ^ script signature
  , _vinSequence     :: !Word32            -- ^ sequence no
  } 
  deriving (Eq,Show)

parseTxVIn :: JSValue -> Maybe TxVIn
parseTxVIn jsv = case jsv of
  JSObject obj -> 
    case obj of
      _ | Just txid <- lkp "txid"
        , Just vout <- lkp "vout"
        , Just ssig <- lkp "scriptSig" , Just scriptSig <- parseScriptSigVerbose ssig
        , Just seqn <- lkp "sequence"
           -> Just $ TxVIn txid vout scriptSig seqn
      _ -> Nothing
    where
      lkp :: JSON a => String -> Maybe a
      lkp fld = get_field obj fld >>= myReadJSON
  _ -> Nothing

-- | A scriptPubKey as returned by \"getrawtransaction\" (verbose=1) API call
data ScriptPubKeyVerbose = ScriptPubKeyVerbose
  { _scriptPubKeyAsm        :: String      -- ^ human-readable script
  , _scriptPubKeyHex        :: !RawScript  -- ^ raw script
  , _scriptPubKeyReqSigs    :: !Int        -- ^ required number of signatures
  , _scriptPubKeyType       :: String      -- ^ script type (eg. \"pubkeyhash\")
  , _scriptPubKeyAddresses  :: [Address]   -- ^ list of addresses
  }
  deriving (Eq,Show)

parseScriptPubKeyVerbose :: JSValue -> Maybe ScriptPubKeyVerbose
parseScriptPubKeyVerbose jsv = case jsv of
  JSObject obj -> 
    case obj of
      _ | Just asm <- lkp "asm"
        , Just hex <- lkp "hex"
        , Just req <- lkp "reqSigs"
        , Just typ <- lkp "type"
        , Just (JSArray adrs) <- lkp "addresses"
        , let mbaddresses = map myReadJSON adrs
        , all isJust mbaddresses
           -> Just $ ScriptPubKeyVerbose asm hex req typ (catMaybes mbaddresses)
      _ -> Nothing
    where
      lkp :: JSON a => String -> Maybe a
      lkp fld = get_field obj fld >>= myReadJSON
  _ -> Nothing

-- | A transaction output as returned by \"getrawtransaction\" (verbose=1) API call
data TxVOut = TxVOut
  { _voutValue        :: !Amount
  , _voutN            :: !Int
  , _voutScriptPubKey :: !ScriptPubKeyVerbose
  }
  deriving (Eq,Show)

parseTxVOut :: JSValue -> Maybe TxVOut
parseTxVOut jsv = case jsv of
  JSObject obj -> 
    case obj of
      _ | Just amt  <- lkp "value"
        , Just n    <- lkp "n"
        , Just spub <- lkp "scriptPubKey" , Just scriptPubKey <- parseScriptPubKeyVerbose spub
           -> Just $ TxVOut amt n scriptPubKey
      _ -> Nothing
    where
      lkp :: JSON a => String -> Maybe a
      lkp fld = get_field obj fld >>= myReadJSON
  _ -> Nothing

-- | A transaction as decoded by the \"decoderawtransaction\" API call
data TxVerbose = TxVerbose
  { _txvTxId          :: !TxId           -- ^ hash of the transaction
  , _txvVersion       :: !Int            -- ^ tx version
  , _txvLockTime      :: !LockTime       -- ^ lock time
  , _txvVIn           :: [TxVIn]         -- ^ transaction inputs
  , _txvVOut          :: [TxVOut]        -- ^ transaction outputs
  }
  deriving (Eq,Show)

parseTxVerbose :: JSValue -> Maybe TxVerbose
parseTxVerbose jsv = case jsv of
  JSObject obj -> 
    case obj of
      _ | Just txid <- lkp "txid"
        , Just ver  <- lkp "version" 
        , Just lock <- lkp "locktime"
        , Just (JSArray vins)  <- lkp "vin"
        , Just (JSArray vouts) <- lkp "vout"
        , let mbins = map parseTxVIn vins
        , all isJust mbins
        , let mbouts = map parseTxVOut vouts
        , all isJust mbouts
           -> Just $ TxVerbose txid ver (parseLockTime lock) (catMaybes mbins) (catMaybes mbouts) 
      _ -> Nothing
    where
      lkp :: JSON a => String -> Maybe a
      lkp fld = get_field obj fld >>= myReadJSON
  _ -> Nothing

-- | A transaction as reported by the \"getrawtransaction\" (verbose=1) API call
data TxVerboseEx = TxVerboseEx
  { _txeHex           :: !RawTx          -- ^ full raw transaction 
  , _txeTxVerbose     :: !TxVerbose      -- ^ common details of the transaction
  , _txeBlockHash     :: !Hash256        -- ^ hash of the block containing the transaction (?)
  , _txeConfirmations :: !Int            -- ^ number of confirmations
  , _txeTime          :: !UnixTimeStamp  -- ^ timestamp of the transaction
  , _txeBlockTime     :: !UnixTimeStamp  -- ^ timestamp of the block
  } 
  deriving (Eq,Show)

parseTxVerboseEx :: JSValue -> Maybe TxVerboseEx
parseTxVerboseEx jsv = case jsv of
  JSObject obj -> 
    case obj of
      _ | Just hex     <- lkp "hex"
        , Just txid <- lkp "txid"
        , Just ver  <- lkp "version" 
        , Just lock <- lkp "locktime"
        , Just (JSArray vins)  <- lkp "vin"
        , Just (JSArray vouts) <- lkp "vout"
        , let mbins = map parseTxVIn vins
        , all isJust mbins
        , let mbouts = map parseTxVOut vouts
        , all isJust mbouts
        , Just bhsh <- lkp "blockhash"
        , Just conf <- lkp "confirmations"
        , Just time <- lkp "time"
        , Just btim <- lkp "blocktime" 
           -> Just $ TxVerboseEx hex (TxVerbose txid ver (parseLockTime lock) (catMaybes mbins) (catMaybes mbouts)) bhsh conf time btim
      _ -> Nothing
    where
      lkp :: JSON a => String -> Maybe a
      lkp fld = get_field obj fld >>= myReadJSON
  _ -> Nothing

-- | \"getrawtransaction\", verbose=0. Version 0.7. Returns raw transaction representation for given transaction id.
--
-- WARNING! Important note from the version 0.8 readme:
--
-- \"This release no longer maintains a full index of historical transaction ids
-- by default, so looking up an arbitrary transaction using the getrawtransaction
-- RPC call will not work. If you need that functionality, you must run once
-- with -txindex=1 -reindex=1 to rebuild block-chain indices (see below for
-- more details).\"
--
getRawTransaction :: TxId -> Call RawTx
getRawTransaction txid = makeCall "getrawtransaction" (txid, (0::Int)) $ \js -> myReadJSON js

-- | \"getrawtransaction\", verbose=1. Version 0.7. Returns transaction representation for given transaction id, 
-- in human-understandable format.
getTransactionInfo :: TxId -> Call TxVerboseEx 
getTransactionInfo txid = makeCall "getrawtransaction" (txid, (1::Int)) parseTxVerboseEx 

decodeRawTransaction :: RawTx -> Call TxVerbose
decodeRawTransaction rawtx = makeCall "decoderawtransaction" [rawtx] parseTxVerbose

--------------------------------------------------------------------------------
-- * wallet info

validateAddress :: Address -> Call (JSObject JSValue)
validateAddress address = makeCall "validateaddress" [address] mbJSObject

-- | Reveals the private key corresponding to \<bitcoinaddress\>. NOTE: Wallet needs to be unlocked! 
dumpPrivKeyWIF :: Address -> Call WIF
dumpPrivKeyWIF address = makeCall "dumpprivkey" [address] $ myReadJSON 

-- | We decode the WIF and also compute the corresponding public key for convenience.
dumpPrivPubKey :: Address -> Call (PrivKey,PubKey)
dumpPrivPubKey address = makeCall "dumpprivkey" [address] $ \js -> myReadJSON js >>= \s -> (f <$> privKeyWIFDecode (WIF s)) where
  f (pfmt,priv) = (priv, computePubKey pfmt priv)

-- | If @[account]@ is not specified, returns the server\'s total available balance. @MinConf = Maybe Int@ is the number of minimum confirmations (default is 1)
getBalance :: Maybe Account -> MinConf -> Call Amount
getBalance mbacc minconf = makeCall "getbalance" (maybe "" id mbacc , maybe 1 id minconf) myReadJSON

-- | Returns the /current/ bitcoin address for receiving payments to this account.   
getAccountAddress :: Account -> Call Address
getAccountAddress account = makeCall "getaccountaddress" [account] myReadJSON

-- | Returns the list of addresses for the given account.   
getAddressesByAccount :: Account -> Call [Address]
getAddressesByAccount account = makeCall "getaddressesbyaccount" [account] myReadJSON

-- | Returns the account associated with the given address.   
getAccount :: Address -> Call Account
getAccount address = makeCall "getaccount" [address] myReadJSON

-- | Returns a list of pairs that has account names as keys, account balances as values. Maybe Int is number of minimal confirmations (default is 1)
listAccounts :: MinConf -> Call [(Account,Amount)]
listAccounts minconf = makeCall "listaccounts" [maybe 1 id minconf] $ \jsv -> (g . map f . fromJSObject) =<< mbJSObject jsv where
  f :: (String,JSValue) -> (String, Maybe Amount)
  f (s,x) = (s,amountFromDouble <$> myReadJSON x)
  g ambs = if all (isJust . snd) ambs 
    then Just $ map (\(s,mb) -> (s,fromJust mb)) ambs 
    else Nothing

-- | version 0.7 Returns all addresses in the wallet and info used for coincontrol.   
--
-- Lists groups of addresses which have had their common ownership made public by 
-- common use as inputs or as the resulting change in past transactions.
listAddressGroupings :: Call JSValue --  (JSObject JSValue) [[(Address,Amount,Account)]]
listAddressGroupings = makeCall "listaddressgroupings" () Just -- mbJSObject

--------------------------------------------------------------------------------

data Received = Received
  { _rcvAddress       :: Maybe Address
  , _rcvAccount       :: Account
  , _rcvAmount        :: Amount
  , _rcvConfirmations :: Int
  }
  deriving Show

parseReceived :: JSValue -> Maybe Received
parseReceived jsv = case jsv of
  JSObject obj -> 
    case obj of
      _ | mbaddr    <- lkp "address"
        , Just acc  <- lkp "account"
        , Just amt  <- lkp "amount"
        , Just conf <- lkp "confirmations"
           -> Just $ Received mbaddr acc amt conf
      _ -> Nothing
    where
      lkp :: JSON a => String -> Maybe a
      lkp fld = get_field obj fld >>= myReadJSON
  _ -> Nothing

-- | Bool is \"includeempty\". Returns an array of objects containing: \"account\", \"amount\", \"confirmations\"
listReceivedByAccount :: MinConf -> Bool -> Call [Received]   
listReceivedByAccount minconf includeempty = 
  makeCall "listreceivedbyaccount" (maybe 1 id minconf, includeempty) $ \jsv -> case jsv of
    JSArray arr -> 
      if all isJust mbs 
        then Just (catMaybes mbs) 
        else Nothing 
      where
        mbs = map parseReceived arr
    _ -> Nothing


-- | Returns an array of objects containing: \"address\", \"account\", \"amount\", \"confirmations\". 
--
-- To get a list of accounts on the system, execute bitcoind listreceivedbyaddress 0 true   
listReceivedByAddress :: MinConf -> Bool -> Call [Received]  
listReceivedByAddress minconf includeempty = 
  makeCall "listreceivedbyaddress" (maybe 1 id minconf, includeempty) $ \jsv -> case jsv of
    JSArray arr -> 
      if all isJust mbs 
        then Just (catMaybes mbs) 
        else Nothing 
      where
        mbs = map parseReceived arr
    _ -> Nothing

--------------------------------------------------------------------------------

-- | An unspent transactions as returned by the \"listunspent\" API call
data Unspent = Unspent
  { _unsTxId          :: !TxId       -- ^ txid is the hexadecimal transaction id
  , _unsOutput        :: !Int        -- ^ output is which output of that transaction 
  , _unsScriptPubKey  :: !RawScript  -- ^ scriptPubKey is the hexadecimal-encoded CScript for that output 
  , _unsAmount        :: !Amount     -- ^ amount is the value of that output 
  , _unsConfirmations :: !Int        -- ^ confirmations is the transaction's depth in the chain.
  } 
  deriving (Eq, Show)

-- | Returns an array of unspent transaction outputs in the wallet that have between minconf and maxconf (inclusive) confirmations. 
-- Each output is a 5-element object with keys: txid, output, scriptPubKey, amount, confirmations. 
-- txid is the hexadecimal transaction id, output is which output of that transaction, 
-- scriptPubKey is the hexadecimal-encoded CScript for that output, 
-- amount is the value of that output and confirmations is the transaction's depth in the chain.
--
-- Minimum confirmations default is 1 and maximum confirmation default is 999999.
--
listUnspent :: MinConf -> MaxConf -> Call [Unspent]
listUnspent minconf maxconf = 
  makeCall "listunspent" [maybe 1 id minconf, maybe 999999 id maxconf] $ \jsv -> case jsv of
    JSArray arr -> 
      if all isJust mbs
        then Just (catMaybes mbs) 
        else Nothing 
      where
        mbs = map parseUnspent arr
    _ -> Nothing

  where

    parseUnspent :: JSValue -> Maybe Unspent
    parseUnspent jsv = case jsv of
      JSObject obj -> 
        case obj of
          _ | Just txid   <- lkp "txid"
            , Just out    <- lkp "output"
            , Just script <- lkp "scriptPubKey"
            , Just amt    <- lkp "amount"
            , Just conf   <- lkp "confirmations"
               -> Just $ Unspent txid out script amt conf
          _ -> Nothing
        where
          lkp :: JSON a => String -> Maybe a
          lkp fld = get_field obj fld >>= myReadJSON
      _ -> Nothing

--------------------------------------------------------------------------------
-- * network info

-- | version 0.7: Returns all transaction ids in memory pool                                   
getRawMemPool :: Call [TxId]
getRawMemPool = makeCall "getrawmempool" () myReadJSON

--------------------------------------------------------------------------------
-- * multi-sig

-- | Add a n-required-to-sign multisignature address to the wallet. 
-- Each key is a bitcoin address or hex-encoded public key. 
-- If [account] is specified, assign address to [account].  
addMultiSigAddress :: Int -> [Key] -> Maybe Account -> Call Address
addMultiSigAddress n keys mbacc =
  if n > length keys || n < 1 
    then error "addMultiSigAddress: <nrequired> must be least 1 and at most the number of keys"
    else makeCall "addmultisigaddress" ( n , jskeys , maybe "" id mbacc ) myReadJSON
  where 
    jskeys = map eiShowJSON keys

-- | Creates a multi-signature address and returns a json object.
createMultiSig :: Int -> [Key] -> Call (Address,RedeemScript)
createMultiSig n keys = 
  if n > length keys || n < 1
    then error "createMultiSig: <nrequired> must be least 1 and at most the number of keys"
    else makeCall "createmultisig" ( n , jskeys ) $ \jsv -> case jsv of
      JSObject obj -> 
        case obj of
          _ | Just addr   <- lkp "address"
            , Just script <- lkp "redeemScript"
                -> Just (addr,script)         
          _ -> Nothing
        where
          lkp :: JSON a => String -> Maybe a
          lkp fld = get_field obj fld >>= myReadJSON
      _ -> Nothing  
  where 
    jskeys = map eiShowJSON keys

--------------------------------------------------------------------------------
-- * sending coins

-- | Send coins from an account to an address
--
-- > <fromaccount> <tobitcoinaddress> <amount> [minconf=1] [comment] [comment-to]. 
--
-- Will send the given amount to the given address, ensuring the account has a valid 
-- balance using [minconf] confirmations. Returns the transaction ID if successful 
-- (not in JSON object). NOTE: Wallet needs to be unlocked! 
--
-- \"comment\" is the transaction comment, and \"comment-to\" is a local comment: a reminder that
-- who did we sent the coins to? (?)
sendFrom :: Account -> Address -> Amount -> MinConf -> Maybe String -> Maybe String -> Call TxId
sendFrom account address amount minconf comment comment_to = 
  makeCall "sendfrom" (account, address, amount, maybe 1 id minconf, maybe "" id comment, maybe "" id comment_to) myReadJSON

-- | Send coins from an account to many addresses 
-- NOTE: Wallet needs to be unlocked! 
sendMany :: Account -> [(Address,Amount)] -> MinConf -> Maybe String -> Call TxId
sendMany account destinations minconf comment = 
  makeCall "sendmany" (account,destinations,maybe 1 id minconf, maybe "" id comment) myReadJSON

-- | version 0.7. Submits raw transaction (serialized, hex-encoded) to local node and network.   
sendRawTransaction :: RawTx -> Call ()
sendRawTransaction rawtx = makeCall "sendrawtransaction" [rawtx] $ \_ -> Just ()

-- | > <bitcoinaddress> <amount> [comment] [comment-to]
sendToAddress :: Address -> Amount -> Maybe String -> Maybe String -> Call TxId
sendToAddress address amount comment comment_to = 
  makeCall "sendtoaddress" (address,amount,maybe "" id comment, maybe "" id comment_to) myReadJSON

-- | Move from one account in your wallet to another   
--
-- > <fromaccount> <toaccount> <amount> [minconf=1] [comment]. 
--
moveCoins :: Account -> Account -> Amount -> MinConf -> Maybe String -> Call ()
moveCoins accfrom accto amount minconf comment = 
  makeCall "move" (accfrom, accto, amount, maybe 1 id minconf, maybe "" id comment) $ \_ -> Just ()

-- | Sets the transaction fee
setTxFee :: Amount -> Call ()
setTxFee amount = makeCall "settxfee" [amount] $ \_ -> Just ()

--------------------------------------------------------------------------------
-- * wallet operations

-- | Adds a private key (as returned by dumpprivkey) to your wallet. The second argument is an optional label (account???).
-- This may take a while, as a rescan is done, looking for existing transactions. Optional [rescan] parameter added in 0.8.0. 
-- NOTE: Wallet needs to be unlocked! 
importPrivKey :: (PubKeyFormat,PrivKey) -> Maybe String -> Bool -> Call ()
importPrivKey (pkfmt,privkey) mblabel rescan = 
  makeCall "importprivkey" ( privKeyWIFEncode pkfmt privkey , maybe "" id mblabel , rescan ) $ \_ -> Just ()

-- | Imports a private key given as WIF (Wallet Import Format).
importPrivKeyWIF :: WIF -> Maybe String -> Bool -> Call ()
importPrivKeyWIF wif mblabel rescan = 
  makeCall "importprivkey" ( wif , maybe "" id mblabel , rescan ) $ \_ -> Just ()

-- | Returns a new bitcoin address for receiving payments. 
-- If [account] is specified (recommended), it is added to the address book so payments 
-- received with the address will be credited to [account].   
getNewAddress :: Maybe Account -> Call Address
getNewAddress mbacc = 
  makeCall "getnewaddress" [ maybe "" id mbacc ] myReadJSON

-- | Sets the account associated with the given address. 
-- Assigning address that is already assigned to the same account will create a new address associated with that account.
setAccount :: Address -> Account -> Call ()
setAccount address account = makeCall "setaccount" (address,account) $ \_ -> Just ()

-- | Fills the keypool, requires wallet passphrase to be set. NOTE: Wallet needs to be unlocked! 
keyPoolRefill :: Call ()
keyPoolRefill = makeCall "keypoolrefill" () $ \_ -> Just ()

-- | Safely copies wallet.dat to destination, which can be a directory or a path with filename.  
backupWallet :: FilePath -> Call ()
backupWallet fpath = makeCall "backupwallet" [fpath] $ \_ -> Just ()

-- | Removes the wallet encryption key from memory, locking the wallet. 
-- After calling this method, you will need to call walletpassphrase again 
-- before being able to call any methods which require the wallet to be unlocked.   
walletLock :: Call ()
walletLock = makeCall "walletlock" () $ \_ -> Just ()

-- | Stores the wallet decryption key in memory for \<timeout\> seconds.   
walletPassPhrase :: PassPhrase -> Int -> Call ()
walletPassPhrase pw seconds = makeCall "walletpassphrase" (pw,seconds) $ \_ -> Just ()

-- | Changes the wallet passphrase from \<oldpassphrase\> to \<newpassphrase\>.  
walletPassPhraseChange :: PassPhrase -> PassPhrase -> Call ()
walletPassPhraseChange old new = makeCall "walletpassphrasechange" (old,new) $ \_ -> Just ()

-- | Encrypts the wallet with \<passphrase\>.   
encryptWallet :: PassPhrase -> Call ()
encryptWallet pw = makeCall "encryptwallet" [pw] $ \_ -> Just ()

--------------------------------------------------------------------------------

