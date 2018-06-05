module Main where

import System.IO
import System.Exit
import System.Process
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
import qualified Control.Exception as E


replaceX :: Eq a => (a,a) -> [a] -> [a]
replaceX _ [] = []
replaceX (f,t) (x:xs)
  | f == x = t : replaceX (f,t) xs
  | True   = x : replaceX (f,t) xs

-- If nonredeemable, exit code = failure
-- If redeemable or requires prolog to determine this (which is not yet implemented), exit code = success
main :: IO ()
main = do
  args <- getArgs
  -- Modes
  --    0: i/prolog/nonredeemable,
  --        where i = the lowest number of variables that need to match sig or hash
  --    1: verbose
  let m = fromMaybe "0" (args !? 0)

  bs <- B.pack <$> getLine
  let bs' = bs
  let script = decode bs'

  ast <- E.catch (E.evaluate $ buildAST (scriptOps script))
                 (\e -> putStrLn ("parse errors: " ++ replaceX ('\n',' ') (show (e :: E.ErrorCall))) >> exitFailure)
  let buildStates = genBuildStates ast
      successBuilds = mapMaybe (either (const Nothing) Just) buildStates

  logicBuilds <- mapM prologVerify successBuilds
  let logicBuildsInfo = map (either id fst) logicBuilds
      logicOKBuilds = mapMaybe (either (const Nothing) Just) logicBuilds
      i = minimum
        $ map length
        $ map (knowledgeCnstrsWithVar . snd) logicOKBuilds

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
          putStrLn $ dumpList logicBuildsInfo
          putStrLn $ "-------------------------"

          putStrLn $ "------V-E-R-D-I-C-T------"
    otherwise -> return ()
  -- Non verbose section. Outputs one of these: redeemable/prolog/nonredeemable
  when (null successBuilds) $ putStrLn "type errors" >> exitFailure
  when (null logicOKBuilds) $ putStrLn "nonredeemable" >> exitFailure
  putStrLn "types correct" >> exitSuccess

prologVerify :: BuildState -> IO (Either String (String,BuildState))
prologVerify bs =
  case branchToProlog bs of
    Left e -> return $ Left e
    Right pl -> do
      let fn = "/tmp/BitcoinAnalysis-script.pl"
      writeFile fn pl
      results <- mapM (verifyC fn) (zip [0..] (val_cnstrs bs))
      let r = pl ++ "-----P-R-O-L-O-G-----\n" ++ concat (map fst results)
      if all snd results
        then return $ Right (r,bs)
        else return $ Left r

verifyC :: String -> (Int,ValConstraint) -> IO (String,Bool)
verifyC fn (i,c) = do
  let expected = "true."
  (c,r,e) <- readProcessWithExitCode "/usr/bin/swipl" [fn] ("s" ++ show i ++ ".")
  return $ ("***\n" ++ "Expecting: " ++ expected ++ "\n" ++ r ++ e ++ "***\n",isInfixOf expected r)

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
