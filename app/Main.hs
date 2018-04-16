module Main where

import System.IO
import System.Environment
import Script.Parser
import Data.Bitcoin.Script
import qualified Data.ByteString.Lazy as B

import Data.List

import Constraints.Gen
import Constraints.Solver
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
  let v = 0
  if v == 0
    then do -- Less verbose
      let cnstrs = genConstraints ast
      putStrLn $ show cnstrs
      let check = solveConstraints cnstrs
      putStrLn $ "--------------------"
      putStrLn $ show check
      putStrLn $ "--------------------"
      putStrLn $ "------------PROLOG--------"
      putStrLn $ toProlog cnstrs
    else if v == 1
      then do -- More verbose
        let cnstrs = genConstraints' ast
        putStrLn $ intercalate "\n\n||\n\n" (map show cnstrs)
        putStrLn $ "~~~~~~~~~~~~~~~~~~~And Normalized:~~~~~~~~~~~~"
        let cnstrs' = map (\(c,s) -> (normalize c True,s)) cnstrs
        putStrLn $ intercalate "\n\n||\n\n" (map show cnstrs')
        putStrLn $ "--------------------"
        putStrLn $ "--------------------"
        let checks = map (\c -> (c,solveConstraints (fst c))) cnstrs'
        putStrLn $ intercalate "\n\n||\n\n" (map showResult checks)
      else do -- Most verbose
        let ss = genBuildStates ast
        putStrLn $ show ss
  where showResult (c,r) = case r of
                            Left e -> "Solving for: " ++ show c ++ "\n::" ++ e
                            Right r' -> "Solving for: " ++ show c ++ "\n::" ++ show r'
