
-- | JSON-RPC over HTTP. We only support Basic Authentication at the moment.

{-# LANGUAGE CPP #-}
module Bitcoin.RPC.HTTP where

--------------------------------------------------------------------------------

import Data.Maybe

-- import qualified Data.ByteString.Char8 as B

import qualified Network.URI       as HTTP
import qualified Network.Stream    as HTTP
import qualified Network.HTTP.Base as HTTP
import qualified Network.HTTP      as HTTP

import Text.JSON

import Bitcoin.Protocol.Base64
import Bitcoin.RPC.JSON

--------------------------------------------------------------------------------

-- | An URI plus username/password for basic auth
data BitcoinURI = BitcoinURI (String,String) HTTP.URI deriving Show

-- | Example:
--
-- > myuri = bitcoinURI username password host port
--
-- Default host is 127.0.0.1, default port is 8332
--
bitcoinURI :: String -> String -> Maybe String -> Maybe Int -> BitcoinURI
bitcoinURI username pw mbhost mbport = BitcoinURI (username,pw) (fromJust (HTTP.parseURI text)) where
  host = case mbhost of { Nothing -> "127.0.0.1" ; Just h -> h }
  port = case mbport of 
    Just p  -> p 
#ifdef WITH_TESTNET
    Nothing -> 18332   
#else
    Nothing -> 8332   
#endif
  text = "http://" ++ host ++ ":" ++ show port ++ "/"

--------------------------------------------------------------------------------

rpcCall :: JSON a => BitcoinURI -> Request a -> IO (Either String Response)
rpcCall (BitcoinURI (uname,pw) uri) request = do

  let text = (Text.JSON.encode $ encodeRequest request) 
  let Base64 unpw = base64Encode (uname ++ ":" ++ pw)
      auth = "Basic " ++ unpw

  -- print    auth                   -- debugging !!!!!!!!!!!!!!
  -- putStrLn text

  let headers = 
        [ HTTP.Header HTTP.HdrContentType   "application/json-rpc"
        , HTTP.Header HTTP.HdrContentLength (show $ length text)         -- now, seriously... why do i have to do this manually??
        , HTTP.Header HTTP.HdrAccept        "application/json-rpc"
        , HTTP.Header HTTP.HdrAuthorization auth
        ]

  let req = HTTP.Request uri HTTP.POST headers text
  result <- HTTP.simpleHTTP req :: IO (Either HTTP.ConnError (HTTP.Response String))

  case result of
    Left connerror -> return $ Left ("HTTP connection error - " ++ show connerror)
    Right rsp -> case HTTP.rspCode rsp of
      (2,0,0) -> return $ case (Text.JSON.decode $ HTTP.rspBody rsp) of
                   Text.JSON.Error msg  -> Left ("cannot parse JSON - " ++ show msg)
                   Text.JSON.Ok jsvalue -> case decodeResponse jsvalue of
                     Just x  -> Right x
                     Nothing -> Left "cannot decode JSON-RPC response"
      (a,b,c) -> return $ Left ("HTTP error code " ++ show a ++ show b ++ show c ++ " - "  ++ HTTP.rspReason rsp)

--------------------------------------------------------------------------------
