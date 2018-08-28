{-# LANGUAGE OverloadedStrings #-}

import System.IO

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Text.Blaze.Html.Renderer.Text
import qualified Views.Index

import Control.Monad.IO.Class
import Network.HTTP.Simple
import Network.HTTP.Conduit

import Data.Maybe

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
      scrpt <- param "output_script"
      verStr <- param "verbosity"
      let ver = if all (\c -> any (c==) ['0'..'9']) verStr
                  then read verStr :: Int
                  else 1
      result <- liftIO $ analyseOpenScript scrpt "/tmp/" "" ver
      blaze $ renderAnalysis scrpt result
      liftIO $ putStrLn "---"
  putStrLn "Server shutdown\n*******"
