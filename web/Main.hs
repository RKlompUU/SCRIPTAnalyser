{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Text.Blaze.Html.Renderer.Text
import qualified Views.Index

import Control.Concurrent

import Control.Monad.IO.Class
import Network.HTTP.Simple
import Network.HTTP.Conduit

import Data.Maybe

import Types


blaze = html . renderHtml

main = do
  scotty 3000 $ do
    middleware logStdoutDev
    get "/" $ do
      json ("hello world" :: String)
