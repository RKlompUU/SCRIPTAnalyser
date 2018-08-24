{-# LANGUAGE OverloadedStrings #-}
module Views.Index where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

import Control.Monad.IO.Class

import Types

{-
renderFrontPage content = do
  html $ do
    body $ do
      h1 "Front Page"
      ul $ do
        mapM_ (li . renderPubEntry) content

renderPubEntry pub = do
  p $ do
    a "Publication" ! href (stringValue $ pub_link pub)
  p $ do
    toHtml $ "Vote tally: " ++ show (pub_vote pub)
  p $ do
    toHtml $ "Transaction: " ++ pub_origin pub
  p $ do
    toHtml $ "Block height: " ++ show (pub_block pub)
-}
