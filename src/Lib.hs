module Lib
    ( analyseOpenScript
    , serializeScript
    ) where


import Control.Monad

import Script.Parser
import Script.AST
import Data.Bitcoin.Script
import Data.Word (Word8)
import Data.Binary (Binary)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Base16.Lazy as BS16L
import qualified Data.ByteString.Lazy as BSL

import Data.List
import Data.Maybe

import qualified Control.Exception as E
import Control.Monad.Writer
import Control.Monad.Except
import Debug.Trace

import qualified Data.Map.Lazy as M

import KlompStandard
import Constraints.Gen
import Constraints.Types
import Control.Monad
import Constraints.RunProlog

import Script.Sugar


serializeScript :: String -> Either String B.ByteString
serializeScript str =
  B.pack <$> unsugar str

analyseOpenScript :: B.ByteString -> String -> String -> Int -> IO (Either String String)
analyseOpenScript scrpt dir preVerdict verbosity = do
  result <- E.catch ((runWriterT (runExceptT (analyseOpenScript_ scrpt dir preVerdict verbosity))))
                    (\e -> return $ (Left (show (e :: E.ErrorCall)), ""))
  case result of
    (Left err,str) -> return $ Left ("Error: " ++ err)
    (Right _,str) -> return $ Right str

analyseOpenScript_ :: B.ByteString -> String -> String -> Int -> IOReport ()
analyseOpenScript_ bs dir preVerdict verbosity = do
  let script = decode bs

  let ast = runFillLabels $ buildAST (scriptOps script)
  branchReports <- lift $ lift $ genBuildStates ast
  branchReports' <- mapM (prologVerify dir) branchReports

  when (verbosity >= 2) $ do
      tell "SCRIPT echo, followed by the lexed intermediate variant:\n"
      tell $ (show $ bs) ++ "\n"
      tell $ (show script) ++ "\n"
  when (verbosity >= 1) $ do -- Verbose section
      tell $ "-------------------------\n"
      tell $ "---------- AST ----------\n"
      tell $ show ast

      tell $ "-------------------------\n"
      tell $ "-------- Inferred -------\n"
      tell $ dumpBranchReports branchReports' verbosity

  tell $ "------------------------\n"
  tell $ "-------- Verdict -------\n"

  let successBuilds = filter (prologValid) branchReports'

  -- Non verbose section.
  if (null successBuilds)
    then tell (preVerdict ++ "nonredeemable")
    else tell (preVerdict ++ "types correct, " ++ show (length successBuilds) ++ " branch(es) viable") -- >> exitSuccess

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
      initialStack = "Required initial main stack:\n" ++
                     printStack (map Var [0,(-1)..freshV bs+1])
      initialAltStack = "Required initial alternative stack:\n" ++
                        printStack (map AltVar [0,(-1)..freshAltV bs+1])
      vconstrs = "Inferred constraints:\n\t" ++
                 (intercalate "\n\t" $ map show (filter (not . isSpecCnstr) $ val_cnstrs bs))
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
     initialStack ++
     initialAltStack ++
     vconstrs ++
     (if verbosity >= 2 then "\n" ++ tconstrs ++ "\n" ++ trace else "") ++ "\n" ++
     st ++ "\n" ++
     prolog ++ "\n"
