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

  \# Write bytecodes to specify instructions (e.g. 01 specifies a PUSH of 1 byte)\n\
  \01 01   # PUSH: 1\n\n\

  \# Alternatively, use the custom language features (e.g. the PUSH keyword, that\n\
  \# automatically determines the right OP_PUSHDATA Bitcoin instruction).\n\
  \PUSH 02   # PUSH: 2\n\n\

  \# Or, alternatively, use the i prefix to specify an integer format\n\
  \PUSH i30   # PUSH: 30\n\
  \PUSH i-5040 # PUSH: (-5040)"

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
