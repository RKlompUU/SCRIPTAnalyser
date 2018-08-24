{-# LANGUAGE OverloadedStrings #-}

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
  scotty 3000 $ do
    middleware logStdoutDev
    get "/" $ do
      blaze $ renderFrontPage
    get "/analyse" $ do
      scrpt <- param "output_script"
      result <- liftIO $ analyseOpenScript scrpt "/tmp/" "" 3
      blaze $ renderAnalysis scrpt result
