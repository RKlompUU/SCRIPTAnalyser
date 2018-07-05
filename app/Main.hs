module Main where

import System.IO
import System.Exit
import System.Process
import System.Environment
import Script.Parser
import Script.AST
import Data.Bitcoin.Script
import qualified Data.ByteString.Lazy.Char8 as B

import Data.List
import Data.Maybe

import qualified Data.Map.Lazy as M

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

printHelp :: IO ()
printHelp = do
  putStrLn $ "Usage:\n" ++
             "\tArg 1 (optional): verbosity level\n" ++
             "\t\t0: minimal print, only prints verdicts\n" ++
             "\t\t1: Verbose prints, additionally prints inferred (default) constraints\n" ++
             "\t\t2: _More_ verbose prints, additionally prints inferred types of expressions, as well as a trace of stack mutations\n" ++
             "\t\t>=3: Verbose prints (debugging mode), additionally prints prolog related information\n" ++
             "\tArg 2 (optional): path for creating temporary prolog code file (default is /tmp/)\n" ++
             "\tArg 3 (optional): string to prepend the verdict line (useful to track metadata through large batch computations)"
  exitFailure

-- If nonredeemable, exit code = failure
-- If redeemable or requires prolog to determine this (which is not yet implemented), exit code = success
main :: IO ()
main = do
  args <- getArgs
  -- Modes
  --    0: i/prolog/nonredeemable,
  --        where i = the lowest number of variables that need to match sig or hash
  --    1: verbose
  --    2: verbose (debug level)
  case (args !? 0) of
    Just n -> if not (all (\c -> any (==c) "0123456789") n)
                then printHelp
                else return ()
    Nothing -> return ()

  let m = (read $ fromMaybe "1" (args !? 0)) :: Int
  let dir = fromMaybe "/tmp/" (args !? 1)
  let preVerdict = fromMaybe "" (args !? 2)

  bs <- B.pack <$> getLine
  let bs' = bs
  let script = decode bs'

  ast <- E.catch (E.evaluate $ runFillLabels $ buildAST (scriptOps script))
                 (\e -> putStrLn (preVerdict ++ "parse errors: " ++ replaceX ('\n',' ') (show (e :: E.ErrorCall))) >> exitFailure)
  let buildStates = genBuildStates ast
      successBuilds = mapMaybe (either (const Nothing) Just) buildStates

  logicBuilds <- mapM (prologVerify dir) successBuilds
  let logicBuildsInfo = map (either id fst) logicBuilds
      logicOKBuilds = mapMaybe (either (const Nothing) Just) logicBuilds
      i = minimum
        $ map length
        $ map (knowledgeCnstrsWithVar . snd) logicOKBuilds

  when (m >= 1) $ do -- Verbose section
      putStrLn (show $ bs')
      putStrLn (show script)

      putStrLn $ "-------------------------"
      putStrLn $ show ast
      putStrLn $ "-------------------------"

      putStrLn $ "-------------------------"
      putStrLn $ dumpBuildStates buildStates m
      putStrLn $ "-------------------------"

  when (m >= 3) $ do -- Verbose (debug) section
      putStrLn $ "-------------------------"
      putStrLn $ dumpList logicBuildsInfo
      putStrLn $ "-------------------------"

  putStrLn $ "------V-E-R-D-I-C-T------"

  -- Non verbose section. Outputs one of these: redeemable/prolog/nonredeemable
  when (null successBuilds) $ putStrLn (preVerdict ++ "type errors") >> exitFailure
  when (null logicOKBuilds) $ putStrLn (preVerdict ++ "nonredeemable") >> exitFailure
  putStrLn (preVerdict ++ "types correct, " ++ show (length logicOKBuilds)) >> exitSuccess

prologVerify :: String -> BuildState -> IO (Either String (String,BuildState))
prologVerify dir bs =
  case branchToProlog bs of
    Left e -> return $ Left e
    Right pl -> do
      let fn = dir ++ "BitcoinAnalysis-script.pl"
      writeFile fn pl
      --results <- mapM (verifyC fn) (zip [0..] (val_cnstrs bs))
      result <- verifyC fn (-1,undefined)
      let r = pl ++ "-----P-R-O-L-O-G-----\n" ++ fst result --concat (map fst results)
      if snd result
        then return $ Right (r,bs)
        else return $ Left r

verifyC :: String -> (Int,ValConstraint) -> IO (String,Bool)
verifyC fn (i,c) = do
  let expected = "true."
  let str = "s" ++ (if i == -1 then "" else show i) ++ "."
  (c,r,e) <- readProcessWithExitCode "/usr/bin/swipl" [fn] str
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

dumpBuildStates :: [Either (BuildState,String) BuildState] -> Int -> String
dumpBuildStates xs verbosity =
  let f x = "****** Gamma solution for branch ******\n" ++
            case x of
              Left (b,e) -> "This execution branch contains type errors: " ++ e ++ "!\n" ++ dumpBuildState b verbosity ++ "\n"
              Right b -> dumpBuildState b verbosity ++ "\n"
  in intercalate "\n"
     $ map f xs

dumpBuildState :: BuildState -> Int -> String
dumpBuildState b verbosity =
  let showJump = \(lbl,b) -> "Line " ++ show lbl ++ ": " ++ show b
      jumps = intercalate "\n\t-> "
            $ map showJump (reverse $ branchInfo b)
      trace = "Stack trace:\n\t" ++
              (intercalate "\n\t" $ map show (muts b))
      vconstrs = "Inferred constraints:\n\t" ++
                 (intercalate "\n\t" $ map show (val_cnstrs b))
      tconstrs = "Inferred types:\n\t" ++
                 (intercalate "\n\t" $ map show (M.toList $ ty_cnstrs b))
      st = "Resulting symbolic stack:\n\t[" ++
           (intercalate ",\n\t" $ map show (stack b)) ++ "]"
  in "Branch's decision points: \n" ++
     (if (not . null) jumps then "\t-> " ++ jumps else "") ++ "\n" ++
     vconstrs ++
     (if verbosity >= 2 then "\n" ++ tconstrs ++ "\n" ++ trace else "") ++ "\n" ++
     st
