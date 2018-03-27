module Main where

import System.IO
import System.Environment
import Script.Parser
import Data.Bitcoin.Script
import qualified Data.ByteString.Lazy as B

import Data.List

import ConstraintsGen

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
  let v = 1
  if v == 0
    then do -- Less verbose
      let cnstrs = genConstraints ast
      putStrLn $ show cnstrs
    else if v == 1
      then do -- More verbose
        let cnstrs = genConstraints' ast
        putStrLn $ intercalate "\n\n||\n\n" (map show cnstrs)
      else do -- Most verbose
        let ss = genBuildStates ast
        putStrLn $ show ss
