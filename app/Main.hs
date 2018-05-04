module Main where

import System.IO
import System.Exit
import System.Environment
import Script.Parser
import Data.Bitcoin.Script
import qualified Data.ByteString.Lazy.Char8 as B

import Data.List
import Data.Maybe

import Constraints.Gen
import Constraints.Types
import Constraints.ToProlog
import Control.Monad

-- If nonredeemable, exit code = failure
-- If redeemable or requires prolog to determine this (which is not yet implemented), exit code = success
main :: IO ()
main = do
  args <- getArgs
  -- Modes
  --    0: redeemable/prolog/nonredeemable
  --    1: verbose
  let m = fromMaybe "0" (args !? 0)

  bs <- B.pack <$> getLine
  let bs' = bs
  let script = decode bs'

  let ast = buildAST (scriptOps script)
      buildStates = genBuildStates ast
      successBuilds = mapMaybe (either (const Nothing) Just) buildStates
      pls = map (either id id)
          $ map branchToProlog successBuilds
  case m of
    "1" -> do -- Verbose section
          putStrLn (show $ bs')
          putStrLn (show script)

          putStrLn $ "-------------------------"
          putStrLn $ show ast
          putStrLn $ "-------------------------"

          putStrLn $ "-------------------------"
          putStrLn $ dumpBuildStates buildStates
          putStrLn $ "-------------------------"

          putStrLn $ "-------------------------"
          putStrLn $ dumpList pls
          putStrLn $ "-------------------------"
          putStrLn $ "------V-E-R-D-I-C-T------"
    otherwise -> return ()
  -- Non verbose section. Outputs one of these: redeemable/prolog/nonredeemable
  when (null successBuilds) $ putStrLn "nonredeemable" >> exitFailure
  if (all null pls)
    then putStrLn "redeemable" >> exitSuccess
    else putStrLn "prolog" >> exitSuccess


(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x:xs) !? i
  | i < 0  = Nothing
  | i == 0 = Just x
  | i > 0  = xs !? (i - 1)


dumpList :: [String] -> String
dumpList xs =
  intercalate "\n-----------\n" xs

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
