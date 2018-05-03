module Main where

import System.IO
import System.Environment
import Script.Parser
import Data.Bitcoin.Script
import qualified Data.ByteString.Lazy as B

import Data.List
import Data.Maybe

import Constraints.Gen
import Constraints.Types
import Constraints.ToProlog

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

  let buildStates = genBuildStates ast
  putStrLn $ "-------------------------"
  putStrLn $ dumpBuildStates buildStates
  putStrLn $ "-------------------------"
  let successBuilds = mapMaybe (either (const Nothing) Just) buildStates

  let pls = map branchToProlog successBuilds
  putStrLn $ "-------------------------"
  putStrLn $ dumpList pls
  putStrLn $ "-------------------------"


dumpList :: Show a => [a] -> String
dumpList xs =
  intercalate "\n-----------\n"
  $ map show xs

dumpBuildStates :: [Either (BuildState,String) BuildState] -> String
dumpBuildStates xs =
  let xs' = zip xs [0..]
      f   = \(x,i) -> "-------\n" ++ show i ++ "\n" ++
                      case x of
                        Left (b,e) -> "!" ++ e ++ "!\n" ++ show b ++ "\n"
                        Right b -> dumpBuildState b ++ "\n"
  in intercalate "\n"
     $ map f xs'

dumpBuildState :: BuildState -> String
dumpBuildState b =
  show b
