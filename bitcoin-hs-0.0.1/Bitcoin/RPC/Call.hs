
-- | wrappers around the RPC calls

{-# LANGUAGE PatternGuards #-}
module Bitcoin.RPC.Call where

--------------------------------------------------------------------------------

import Data.Word

import Text.JSON 
import Text.JSON.Types

import Control.Monad.Reader

import System.Random

import Bitcoin.Misc.Unique

import Bitcoin.RPC.JSON
import Bitcoin.RPC.HTTP

--------------------------------------------------------------------------------

-- | Type of an RPC call (without arguments)
type Call a = ReaderT BitcoinURI IO (Either String a)

-- | Executes the "call monad"
runCalls :: BitcoinURI -> ReaderT BitcoinURI IO a -> IO a
runCalls uri action = runReaderT action uri

--------------------------------------------------------------------------------

-- | Creates a unique request id, using a combination of a counter and a random number.
newRequestId :: IO String
newRequestId = do
  a <- randomRIO (1000000000,9999999999::Word64)
  b <- newUnique
  return (show a ++ ":" ++ show b)

--------------------------------------------------------------------------------

-- | Things which can be converted to an RPC call parameter list. 
class ParamList l where
  paramListJSON :: l -> [JSValue]

instance ParamList () where paramListJSON _ = []

instance JSON a => ParamList [a] where
  paramListJSON xs = map showJSON xs

instance (JSON a, JSON b) => ParamList (a,b) where
  paramListJSON (x,y) = [showJSON x, showJSON y]

instance (JSON a, JSON b, JSON c) => ParamList (a,b,c) where
  paramListJSON (x,y,z) = [showJSON x, showJSON y, showJSON z]

instance (JSON a, JSON b, JSON c, JSON d) => ParamList (a,b,c,d) where
  paramListJSON (x,y,z,w) = [showJSON x, showJSON y, showJSON z, showJSON w]

instance (JSON a, JSON b, JSON c, JSON d, JSON e) => ParamList (a,b,c,d,e) where
  paramListJSON (x,y,z,w,u) = [showJSON x, showJSON y, showJSON z, showJSON w, showJSON u]

instance (JSON a, JSON b, JSON c, JSON d, JSON e, JSON f) => ParamList (a,b,c,d,e,f) where
  paramListJSON (x,y,z,w,u,v) = [showJSON x, showJSON y, showJSON z, showJSON w, showJSON u, showJSON v]

--------------------------------------------------------------------------------

-- | Generic API call
makeCall :: ParamList l => String -> l -> (JSValue -> Maybe a) -> Call a
makeCall method params parseResponse = do
  uri <- ask
  lift $ do
    reqid <- newRequestId
    let pars = paramListJSON params
    let req  = Request method pars reqid :: Request JSValue
    -- print req 
    eiresp <- rpcCall uri req
    case eiresp of
      Left  err -> return (Left err)
      Right (Response result mberror respid) 
        | respid  /= reqid                  ->  return (Left "response id does not match request id")
        | Just err <- mberror               ->  return (Left "err")
        | Just x <- parseResponse result    ->  return (Right x)
        | otherwise                         ->  return (Left "cannot parse response")

--------------------------------------------------------------------------------
