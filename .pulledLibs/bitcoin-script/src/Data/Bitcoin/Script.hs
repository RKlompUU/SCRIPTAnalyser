module Data.Bitcoin.Script ( decode
                           , encode
                           , parseMemnomicCodes
                           , scriptClass
                           , ScriptClass (..)
                           , ScriptOp (..)
                           , Script (..)
                           , PushDataType (..) ) where

import qualified Data.Binary                 as B
import qualified Data.ByteString.Lazy.Char8  as BS8L
import qualified Data.ByteString.Base16.Lazy as BS16L
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.ByteString        as BS

import           Data.Bitcoin.Script.Types

import Data.Maybe
import Data.List

import Debug.Trace

-- | Decodes a hex representation of a script into a 'Script' object.
decode :: BSL.ByteString -> Script
decode =
  B.decode . fst . BS16L.decode

-- | Encodes a 'Script' object into a hex representation
encode :: Script -> BSL.ByteString
encode =
  BS16L.encode . B.encode

parseMemnomicCodes :: BSL.ByteString -> BSL.ByteString
parseMemnomicCodes scrpt =
  replaceXs memnomic2Hex scrpt

replaceXs :: [(BSL.ByteString, BSL.ByteString)] -> BSL.ByteString -> BSL.ByteString
replaceXs _ ys | BSL.null ys = BSL.empty
replaceXs xss ys =
  case find isJust (map (\(xs,xs') -> let ys_ = BSL.take (BSL.length xs) ys
                                      in if xs == ys_ then Just (xs,xs') else Nothing) xss) of
    Just (Just (xs,xs')) -> BSL.append xs' (replaceXs xss (BSL.drop (BSL.length xs) ys))
    otherwise            -> BSL.cons (BSL.head ys) (replaceXs xss (BSL.tail ys))

scriptClass :: BSL.ByteString -> ScriptClass
scriptClass bs =
  let scrpt = decode bs
  in case scriptOps scrpt of
    [OP_HASH160, OP_PUSHDATA h _, OP_EQUAL] | BS.length h == 20 -> Redeem h
    _  -> NonStandard
