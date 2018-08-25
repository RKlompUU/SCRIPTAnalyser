{-# LANGUAGE OverloadedStrings #-}
module Views.Index where

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as Blaze
import qualified Text.Blaze.Html5.Attributes as Attr

import Control.Monad.IO.Class

import Types

import KlompStandard

defaultScript :: String
defaultScript =
  "# Use '#' to start a comment\n\
  \# Whitespaces are allowed anywhere\n\n\
  \01 01   # PUSH: 1\n\
  \01 02   # PUSH: 2"

renderFrontPage = do
  html $ do
    body $ do
      h1 "Front Page"
      form ! (Attr.action "/analyse") $ do
        textarea ! Attr.name "output_script" ! Attr.cols "80" ! Attr.rows "40" $ toHtml defaultScript
        br
        input ! Attr.type_ "submit"
        -- Blaze.span $ toHtml ("test" :: String)
    --  ul $ do
  --      mapM_ (li . renderPubEntry) content

renderAnalysis scrpt result = do
  html $ do
    body $ do
      h1 (toHtml $ ("Analysis results" :: String))
      let resultTxt = case result of
                        Left err -> err
                        Right str -> str
          lineSplitted = foldl grabLine [[]] resultTxt
          height = length lineSplitted + 1
          width = maximum
                $ Prelude.map length lineSplitted
      textarea ! Attr.readonly "true" ! Attr.cols (stringValue $ show width) ! Attr.rows (stringValue $ show height) $ toHtml resultTxt
  where grabLine :: [String] -> Char -> [String]
        grabLine strs '\n' = [] : strs
        grabLine (line:lines) c = (c : line) : lines

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
