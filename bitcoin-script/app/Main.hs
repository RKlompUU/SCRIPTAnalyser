module Main where

import System.IO
import System.Environment
import Data.Bitcoin.Script
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
  file <- head <$> getArgs
  bs <- B.readFile file
  let bs' = B.tail $ B.tail $ bs
  let script = decode bs'
  putStrLn (show $ bs')
  putStrLn (show script)
