module Constraints.RunProlog
  ( prologVerify, prologSolve ) where

import Constraints.Types
import Constraints.ToProlog
import Control.Monad
import System.Process
import KlompStandard
import System.IO.Temp
import qualified Control.Exception as E
import Control.Monad.State.Lazy
import Data.List

import Constraints.PrologResultParser

prologSolve :: String -> String -> IO (Either String Int)
prologSolve var pl = do
  dir' <- liftIO $ createTempDirectory "/tmp" "SCRIPTAnalyser"
  let fn = dir' ++ "/BitcoinAnalysis-script.pl"

  liftIO $ writeFile fn pl

  let str = "s(" ++ var ++ ")."
  (c,r,e) <- liftIO $ readProcessWithExitCode "/usr/bin/swipl" [fn] str
  let solution = parsePrologResult var r

  liftIO $ removeIfFileExists fn
  liftIO $ removeIfDirExists dir'
  return solution

prologVerify :: String -> BranchReport -> IOReport BranchReport
prologVerify dir report@(BranchReport _ _ (Just err) _ _) =
  return report
prologVerify dir report =
  case branchToProlog (symbolicEval report) of
    Left e -> return $ report { prologReport = e }
    Right pl -> do
      dir' <- liftIO $ createTempDirectory "/tmp" "SCRIPTAnalyser"
      let fn = dir' ++ "/BitcoinAnalysis-script.pl"

      liftIO $ writeFile fn pl
      --results <- mapM (verifyC fn) (zip [0..] (val_cnstrs bs))
      result <- verifyC fn (-1,undefined)
      liftIO $ removeIfFileExists fn
      liftIO $ removeIfDirExists dir'

      let r = pl ++ "-----P-R-O-L-O-G-----\n" ++ fst result --concat (map fst results)
      if snd result
        then return $ report { prologValid = True, prologReport = r }
        else return $ report { prologReport = r }

verifyC :: String -> (Int,ValConstraint) -> IOReport (String,Bool)
verifyC fn (i,c) = do
  let expected = "true."
  let str = "s" ++ (if i == -1 then "" else show i) ++ "."
  (c,r,e) <- liftIO $ readProcessWithExitCode "/usr/bin/swipl" [fn] str
  return $ ("***\n" ++ "Expecting: " ++ expected ++ "\n" ++ r ++ e ++ "***\n",isInfixOf expected r)
