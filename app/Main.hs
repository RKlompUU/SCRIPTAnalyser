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
  let branchReports = genBuildStates ast
  branchReports' <- mapM (prologVerify dir) branchReports

  when (m >= 2) $ do
      putStrLn "SCRIPT echo, followed by the lexed intermediate variant:"
      putStrLn (show $ bs')
      putStrLn (show script)
  when (m >= 1) $ do -- Verbose section
      putStrLn $ "-------------------------"
      putStrLn $ "---------- AST ----------"
      putStrLn $ show ast

      putStrLn $ "-------------------------"
      putStrLn $ "-------- Inferred -------"
      putStr $ dumpBranchReports branchReports' m

  putStrLn $ "------------------------"
  putStrLn $ "-------- Verdict -------"

  let successBuilds = filter (prologValid) branchReports'

  -- Non verbose section.
  when (null successBuilds) $ putStrLn (preVerdict ++ "nonredeemable") >> exitFailure
  putStrLn (preVerdict ++ "types correct, " ++ show (length successBuilds) ++ " branch(es) viable") >> exitSuccess

prologVerify :: String -> BranchReport -> IO BranchReport
prologVerify dir report@(BranchReport _ _ (Just err) _ _) =
  return report
prologVerify dir report =
  case branchToProlog (symbolicEval report) of
    Left e -> return $ report { prologReport = e }
    Right pl -> do
      let fn = dir ++ "BitcoinAnalysis-script.pl"
      writeFile fn pl
      --results <- mapM (verifyC fn) (zip [0..] (val_cnstrs bs))
      result <- verifyC fn (-1,undefined)
      let r = pl ++ "-----P-R-O-L-O-G-----\n" ++ fst result --concat (map fst results)
      if snd result
        then return $ report { prologValid = True, prologReport = r }
        else return $ report { prologReport = r }

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

dumpBranchReports :: [BranchReport] -> Int -> String
dumpBranchReports reports verbosity =
  intercalate "" $ map (flip dumpBranchReport verbosity) reports

dumpBranchReport :: BranchReport -> Int -> String
dumpBranchReport report verbosity =
  let bs = symbolicEval report
      showJump = \(lbl,b) -> "Line " ++ show lbl ++ ": " ++ show b
      jumps = intercalate "\n\t-> "
            $ map showJump (reverse $ branchInfo bs)
      trace = "Stack trace:\n\t" ++
              (intercalate "\n\t" $ map show (reverse $ muts bs))
      vconstrs = "Inferred constraints:\n\t" ++
                 (intercalate "\n\t" $ map show (val_cnstrs bs))
      tconstrs = "Inferred types:\n\t" ++
                 (intercalate "\n\t" $ map show (M.toList $ ty_cnstrs bs))
      st = "Resulting symbolic stack:\n\t[" ++
           (intercalate ",\n\t" $ map show (stack bs)) ++ "]"
      errMsg = if prologValid report
                then ""
                else "!!!!! This branch is unsolvable" ++
                     (if verbosity >= 3 && isJust (symbolicErrs report) then ": " ++ fromJust (symbolicErrs report) else "") ++
                     " !!!!!\n"
      prolog = if verbosity >= 3
                then "*** Generated prolog statements and evaluation:\n" ++ prologReport report
                else ""
  in "---\n" ++
     "--- Symbolic evaluation report of execution branch " ++ show (branchID report) ++ "\n" ++
     "---\n" ++
     errMsg ++
     "Branch's decision points:\n" ++
     (if (not . null) jumps then "\t-> " ++ jumps else "") ++ "\n" ++
     vconstrs ++
     (if verbosity >= 2 then "\n" ++ tconstrs ++ "\n" ++ trace else "") ++ "\n" ++
     st ++ "\n" ++
     prolog ++ "\n"
