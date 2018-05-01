module Main where

import System.IO
import System.Environment
import Script.Parser
import Data.Bitcoin.Script
import qualified Data.ByteString.Lazy as B

import Data.List

import Constraints.Gen
import Constraints.Types
--import Constraints.ToProlog

main :: IO ()
main = do
  file <- head <$> getArgs
  bs <- B.readFile file
  let bs' = bs
  let script = decode bs'
  putStrLn (show $ bs')
  putStrLn (show script)
  let ast = buildAST (scriptOps script)
  putStrLn $ "-------------------------"
  putStrLn $ show ast
  putStrLn $ "-------------------------"

  let cnstrs = genConstraints ast
  putStrLn $ show cnstrs
