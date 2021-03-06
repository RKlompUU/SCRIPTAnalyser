module Bitcoin.Script.Analysis.API
    ( analyseOpenScript
    , serializeScript
    , genLanguageDocs
    ) where


import Control.Monad

import Bitcoin.Script.Parser.API
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

import Bitcoin.Script.Analysis.Standard
import Bitcoin.Script.Analysis.Constraints.Gen
import Bitcoin.Script.Analysis.Constraints.Types
import Control.Monad
import Bitcoin.Script.Analysis.Constraints.RunProlog


-- |'genLanguageDocs' generates a description of the supported syntax in the custom
-- SCRIPT language.
genLanguageDocs :: String
genLanguageDocs =
  languageDescription

-- |Serialization of Bitcoin scripts. Call this function to translate a script written
-- in the custom syntax supported by this tool to the serialized format that the scripts
-- occur in on the Bitcoin blockchain.
-- 'serializeScript' parses the given script (of type 'String'), returning either:
--  'Left' errorMessage if the script cannot be parsed (due to a syntax error), or
--  'Right' scrptByteString if the script was parsed successfully.
serializeScript :: String -> Either String B.ByteString
serializeScript str =
  B.pack <$> eScriptToBytecode str

-- |'analyseOpenScript' performs a static analysis on a given open script. "open"
-- denotes a script that is not complete, and that can be "closed" by prepending an
-- arbitrary set of instructions. In Bitcoin's transactions the output scripts are
-- "open", and are closed by the input scripts. For each execution branch
-- in the open script, 'analyseOpenScript': verifies if it is type correct, derives
-- the constraints that an execution of this branch imposes on the prepended input
-- script, and, if these constraints contain contradictions it tries to prove that
-- the constraints cannot be solved (through application of swi-prolog).
--
-- If all execution branches of the open script contain either type errors or have
-- been proven to impose contradicting constraints, then the open script is unredeemable.
--
-- 'analyseOpenScript' takes 4 arguments: the open script (in extended script format),
--  a directory path (of type 'String') in which files can
--  be written to (this is used to communicate to swi-prolog), a message which is
--  prepended to the verdict message (of type 'String'), and a number which sets
-- the verbosity of logging (of type 'Int'). It returns either: 'Left' errorMessage
-- (if something went wrong, for example the given open script is not a valid Bitcoin
-- script), or 'Right' verdict.
analyseOpenScript :: String -> String -> String -> Int -> IO (Either String String)
analyseOpenScript eScrpt dir preVerdict verbosity = do
  result <- E.catch ((runWriterT (runExceptT (analyseOpenScript_ eScrpt dir preVerdict verbosity))))
                    (\e -> return $ (Left (show (e :: E.ErrorCall)), ""))
  case result of
    (Left err,str) -> return $ Left ("Error: " ++ err)
    (Right _,str) -> return $ Right str

analyseOpenScript_ :: String -> String -> String -> Int -> IOReport ()
analyseOpenScript_ eScrpt dir preVerdict verbosity = do
  case eScriptToAST eScrpt of
    Left err ->
      tell $ preVerdict ++ " parse error: " ++ err
    Right ast -> do
      let maybeNotOK = astOK ast :: Maybe ScriptOp
      if isJust maybeNotOK
        then tell (preVerdict ++ "Script ast is not OK! Nonredeemable due to a presence of: " ++ show (fromJust maybeNotOK))
        else do
          branchReports <- lift $ lift $ genBuildStates ast
          branchReports' <- mapM (prologVerify (\report -> rerunBranch report) dir) branchReports

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
                     printListAsStack (map Var [0,(-1)..freshV bs+1])
      initialAltStack = "Required initial alternative stack:\n" ++
                        printListAsStack (map AltVar [0,(-1)..freshAltV bs+1])
      vconstrs_ = (if prologValid report then filterAtomConstrs else id)
                $ filter (not . isSpecCnstr) $ val_cnstrs bs
      vconstrs = "Inferred constraints:\n\t" ++
                 (intercalate "\n\t" $ map show vconstrs_)
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
