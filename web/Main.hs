{-# LANGUAGE OverloadedStrings #-}

import System.IO

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Text.Blaze.Html.Renderer.Text
import qualified Views.Index

import Control.Monad.IO.Class
import Network.HTTP.Simple
import Network.HTTP.Conduit

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE

import Data.Maybe

import Data.Bitcoin.Script
import Types

import Views.Index
import Lib
import Cryptography
import KlompStandard

blaze = html . renderHtml

main = do
  hSetBuffering stdout NoBuffering
  putStrLn "*******\nServer booted"
  scotty 3000 $ do
    middleware logStdoutDev
    get "/" $ do
      blaze $ renderFrontPage
    get "/analyse" $ do
      oscrpt <- serializeScript <$> param "output_script"
      rscrpt <- serializeScript <$> param "redeem_script"
      verStr <- param "verbosity"
      let ver = if all (\c -> any (c==) ['0'..'9']) verStr
                  then read verStr :: Int
                  else 1
      result <- case scriptClass oscrpt of
                  Redeem rscrptHash ->
                    let rHash = hash160 rscrpt
                        rscrptHash' = BAE.convertToBase BAE.Base16 rscrptHash
                    in if BL.toStrict rHash == rscrptHash'
                        then liftIO $ analyseOpenScript rscrpt "/tmp/" "" ver
                        else return $ Left $ "Error: hash in output script does not equal hash of redeem script!\n\nHash inside output script: " ++ show rscrptHash' ++ "\nActual hash of redeem script: " ++ show rHash ++ ",  test: " ++ show (sha256 rscrpt)
                  _ -> liftIO $ analyseOpenScript oscrpt "/tmp/" "" ver
      blaze $ renderAnalysis result
      liftIO $ putStrLn "---"
    get "/submitInfo" $ do
      return ()
    get "/serialize" $ do
      scrpt <- serializeScript <$> param "script"
      blaze $ renderLargeString (B.unpack scrpt)
  putStrLn "Server shutdown\n*******"
