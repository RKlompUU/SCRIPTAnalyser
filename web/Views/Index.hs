{-# LANGUAGE OverloadedStrings #-}
module Views.Index where

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as Blaze
import qualified Text.Blaze.Html5.Attributes as Attr

import Control.Monad.IO.Class

import Types

import KlompStandard


renderFrontPage = do
  html $ do
    body $ do
      h1 "Front Page"
      form ! (Attr.action "/analyse") $ do
        textarea ! Attr.name "output_script" $ toHtml ("Output script" :: String)
        br
        input ! Attr.type_ "submit"
        -- Blaze.span $ toHtml ("test" :: String)
    --  ul $ do
  --      mapM_ (li . renderPubEntry) content

renderAnalysis scrpt result = do
  html $ do
    body $ do
      h1 (toHtml $ "Analysis of \n\n" ++ scrpt)
      let htmlResult = case result of
                        Left err -> toHtml err
                        Right str -> toHtml str
      textarea ! Attr.cols "80" ! Attr.rows "40" $ htmlResult

{-
renderPubEntry = do
  p $ do
    a "Publication" ! href (stringValue $ pub_link pub)
  p $ do
    toHtml $ "Vote tally: " ++ show (pub_vote pub)
  p $ do
    toHtml $ "Transaction: " ++ pub_origin pub
  p $ do
    toHtml $ "Block height: " ++ show (pub_block pub)
-}
