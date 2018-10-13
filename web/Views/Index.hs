{-# LANGUAGE OverloadedStrings #-}
module Views.Index where

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as Blaze
import qualified Text.Blaze.Html5.Attributes as Attr

import Control.Monad.IO.Class

import Bitcoin.Script.Analysis.Standard

renderAnalysis result = do
  html $ do
    body $ do
      h1 (toHtml $ ("Analysis results" :: String))
      let resultTxt = case result of
                        Left err -> err
                        Right str -> str
      renderLargeString resultTxt True

renderLargeString str boundHeight = do
  let lineSplitted = foldl grabLine [[]] str
      height = if boundHeight
                then min 35 $ length lineSplitted + 1
                else length lineSplitted + 1
      width = min 100
            $ maximum
            $ Prelude.map length lineSplitted
  textarea ! Attr.readonly "true" ! Attr.cols (stringValue $ show width) ! Attr.rows (stringValue $ show height) $ toHtml str
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
