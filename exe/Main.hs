module Main where

import System.IO
import System.Exit
import System.Environment

import KlompStandard
import Data.List
import Data.Maybe

import Lib
import Cryptography

printHelp :: IO ()
printHelp = do
  putStrLn $ "Usage:\n" ++
             "\tArg 1 (optional): verbosity level\n" ++
             "\t\t0: minimal print, only prints verdicts\n" ++
             "\t\t1: Verbose prints, additionally prints inferred (default) constraints\n" ++
             "\t\t2: _More_ verbose prints, additionally prints inferred types of expressions, as well as a trace of stack mutations\n" ++
             "\t\t>=3: Verbose prints (debugging mode), additionally prints prolog related information\n" ++
             "\tArg 2 (optional): path for creating temporary prolog code file (default is /tmp/)\n" ++
             "\tArg 3 (optional): string to prepend the verdict line (useful to track metadata through large batch computations)\n\n" ++ genLanguageDocs
  exitFailure

readStdin :: IO String
readStdin = do
  done <- isEOF
  if done
    then return ""
    else do
      line <- getLine
      lines <- readStdin
      return $ line ++ ('\n' : lines)

main :: IO ()
main = do
  args <- getArgs
  case (args !? 0) of
    Just n -> if not (all (\c -> any (==c) "0123456789") n)
                then printHelp
                else return ()
    Nothing -> return ()

  let m = (read $ fromMaybe "1" (args !? 0)) :: Int
  let dir = fromMaybe "/tmp/" (args !? 1)
  let preVerdict = fromMaybe "" (args !? 2)

  scrpt <- serializeScript <$> readStdin
  result <- case scrpt of
              Left err -> return $ Left err
              Right justScrpt -> analyseOpenScript justScrpt dir preVerdict m
  case result of
    Left err -> putStrLn err
    Right str -> putStrLn str
