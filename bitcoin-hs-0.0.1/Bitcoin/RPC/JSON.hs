
-- | Simple JSON-RPC stuff, and JSON helper functions

{-# LANGUAGE CPP #-}
module Bitcoin.RPC.JSON where

--------------------------------------------------------------------------------

import Data.List ( sort )

import Control.Applicative

import Text.JSON

import qualified Data.ByteString as B

import Bitcoin.Misc.OctetStream
import Bitcoin.Misc.HexString
import Bitcoin.Misc.UnixTime

import Bitcoin.Protocol.Hash
import Bitcoin.Protocol.Address
import Bitcoin.Protocol.Amount
import Bitcoin.Protocol.Base64
import Bitcoin.Protocol.Hash
import Bitcoin.Protocol.Key
import Bitcoin.Protocol.Signature

import Bitcoin.Script.Base
import Bitcoin.BlockChain.Base

--------------------------------------------------------------------------------
-- * JSON-RPC

type RequestId = String 

data Request a = Request
  { requestMethod :: String
  , requestParams :: [a]
  , requestId     :: RequestId
  } 
  deriving Show

data Response = Response
  { responseResult :: JSValue
  , responseError  :: Maybe JSValue
  , responseId     :: RequestId
  }
  deriving Show

data Notification a = Notification
  { notifMethod :: String
  , notifParams :: [a]
  } 
  deriving Show

--------------------------------------------------------------------------------

encodeRequest :: JSON a => Request a -> JSValue
encodeRequest (Request method params rqid) = JSObject $ toJSObject
  [ ( "method" , jsString method )
  , ( "params" , JSArray (map showJSON params) )
--  , ( "id"     , jsNumber rqid )
  , ( "id"     , jsString rqid )
  ]

encodeNotification :: JSON a => Notification a -> JSValue
encodeNotification (Notification method params) = JSObject $ toJSObject
  [ ( "method" , jsString method )
  , ( "params" , JSArray (map showJSON params) )
  ]

decodeResponse :: JSValue -> Maybe Response
decodeResponse jv = case jv of
  JSObject obj -> 
    case (sort keys) of
      [ "error" , "id" , "result" ] -> case lkp "id" of
        JSString rqid -> Just $ Response (lkp "result") mberr rqid where 
          rqid  = case lkp "id" of
--            JSRational _ r -> round r
            JSString s -> fromJSString s
            _          -> error "decodeResponse: request id is not a string"
          mberr = case lkp "error" of
            JSNull -> Nothing
            err    -> Just err
        _ -> Nothing
      _ -> Nothing
    where
      kvs   = fromJSObject obj
      keys  = map fst kvs      
      lkp k = case lookup k kvs of
        Just x  -> x
        Nothing -> error "decodeResponse: shouldn't happen"
  _ -> Nothing

--------------------------------------------------------------------------------
-- * misc helper functions

jsString :: String -> JSValue
jsString = JSString . toJSString

jsNumber :: Int -> JSValue
jsNumber i = JSRational False (fromIntegral i)

myReadJSON :: JSON a => JSValue -> Maybe a
myReadJSON js = case readJSON js of
  Ok y    -> Just y    
  Error _ -> Nothing 

mbJSObject :: JSValue -> Maybe (JSObject JSValue)
mbJSObject js = case js of 
  JSObject obj -> Just obj
  _            -> Nothing

eiShowJSON :: (JSON a, JSON b) => Either a b -> JSValue 
eiShowJSON (Left  x) = showJSON x
eiShowJSON (Right y) = showJSON y

--------------------------------------------------------------------------------
-- * JSON parsing for special types

{- 
-- already defined in Text.JSON
instance Applicative Result where
  pure x = Ok x
  rf <*> rx = case rf of
    Error e1 -> Error e1
    Ok f     -> case rx of
      Error e2 -> Error e2
      Ok x     -> Ok (f x)
-}

--------------------------------------------------------------------------------
-- * JSON instances

instance JSON Hash256 where
  showJSON hash = showJSON $ unsafeReverseHexString $ toHexStringChars hash
  readJSON jsv  = case (readJSON jsv :: Result String) of 
    Ok s | even (length s) -> case safeHexDecode (unsafeReverseHexString s) of
                                Just x  -> Ok (fromWord8List x)
                                Nothing -> Error "invalid characters in hex string"
         | otherwise       -> Error $ "hex string of odd length"
    Error s                -> Error s

instance JSON UnixTimeStamp where
  showJSON (UnixTimeStamp x) = showJSON x
  readJSON jsv = UnixTimeStamp <$> readJSON jsv

instance JSON Address where
  showJSON (Address x) = showJSON x
  readJSON jsv = Address <$> readJSON jsv

instance JSON Amount where
  showJSON x   = showJSON (doubleAmount x)
  readJSON jsv = amountFromDouble <$> readJSON jsv

-- | Unfortunately, Text.JSON already have a ByteString instance, which is different from what we need; hence this newtype
newtype BS = BS { unBS :: B.ByteString }

instance JSON BS where
  showJSON (BS bs)  = showJSON (toHexStringChars bs)
  readJSON hex = case readJSON hex of
    Ok s | even (length s) ->  case safeHexDecode s of
                                Just x  -> Ok (BS (fromWord8List x))
                                Nothing -> Error "invalid characters in hex string"
         | otherwise       -> Error $ "hex string of odd length"
    Error s                -> Error s

instance JSON PubKey where
  showJSON pubkey = showJSON $ BS (encodePubKeyNative pubkey :: B.ByteString)
  readJSON jsv    = case (readJSON jsv :: Result BS) of
                      Error err -> Error err
                      Ok bs -> case decodePubKey (unBS bs) of
                        Nothing -> Error "readJSON/PubKey: cannot decode pubkey"
                        Just pk -> Ok pk

instance JSON RawScript where
  showJSON (RawScript bs) = showJSON (BS bs)
  readJSON jsv = (RawScript . unBS) <$> readJSON jsv

instance JSON RawTx where
  showJSON (RawTx bs) = showJSON (BS bs)
  readJSON jsv = (RawTx . unBS) <$> readJSON jsv

instance JSON WIF where
  showJSON (WIF wif) = showJSON wif  
  readJSON jsv = WIF <$> readJSON jsv

--------------------------------------------------------------------------------
