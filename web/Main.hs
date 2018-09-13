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

import Data.Maybe

import Data.Bitcoin.Script
import Types

import Views.Index
import Lib

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
                  Redeem rscrptHash -> liftIO $ analyseOpenScript rscrpt "/tmp/" "" ver
                  _ -> liftIO $ analyseOpenScript oscrpt "/tmp/" "" ver
      blaze $ renderAnalysis result
      liftIO $ putStrLn "---"
    get "/submitInfo" $ do
      return ()
    get "/serialize" $ do
      scrpt <- serializeScript <$> param "script"
      blaze $ renderLargeString (B.unpack scrpt)
  putStrLn "Server shutdown\n*******"
