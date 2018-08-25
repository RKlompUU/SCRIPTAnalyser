module Lib
    ( analyseOpenScript
    ) where


import Control.Monad.Writer
import Control.Monad

import System.Process

import Script.Parser
import Script.AST
import Data.Bitcoin.Script
import qualified Data.ByteString.Lazy.Char8 as B

import Data.List
import Data.Maybe

import qualified Data.Map.Lazy as M

import Control.Monad.Except
import KlompStandard
import Constraints.Gen
import Constraints.Types
import Constraints.ToProlog
import Control.Monad
import qualified Control.Exception as E

type IOReport a = ExceptT String (WriterT String IO) a

analyseOpenScript :: String -> String -> String -> Int -> IO (Either String String)
analyseOpenScript scrpt dir preVerdict verbosity = do
  result <- E.catch ((runWriterT (runExceptT (analyseOpenScript_ scrpt dir preVerdict verbosity))))
                    (\e -> return $ (Left (show (e :: E.ErrorCall)), ""))
  case result of
    (Left err,_) -> return $ Left err
    (Right _,str) -> return $ Right str
--(preVerdict ++ "parse errors: " ++ replaceX ('\n',' ') (show (e :: E.ErrorCall)))

replaceX :: Eq a => (a,a) -> [a] -> [a]
replaceX _ [] = []
replaceX (f,t) (x:xs)
  | f == x = t : replaceX (f,t) xs
  | True   = x : replaceX (f,t) xs

stripComments :: B.ByteString -> B.ByteString
stripComments bs = B.reverse . snd
                 $ B.foldl walker (False,B.empty) bs
  where walker :: (Bool, B.ByteString) -> Char -> (Bool, B.ByteString)
        walker (False, bs') '#' = (True, bs')
        walker (False, bs') c   = (False, B.cons c bs')
        walker (True, bs') '\n' = (False, bs')
        walker (True, bs') _    = (True, bs')

analyseOpenScript_ :: String -> String -> String -> Int -> IOReport ()
analyseOpenScript_ scrpt dir preVerdict verbosity = do
  let bs = B.pack scrpt
  when (verbosity >= 3) $ do
    tell $ show bs ++ "\n"
  let bs' = B.filter (\c -> not $ any (== c) [' ','\n','\t','\r'])
          $ stripComments bs
  let script = decode bs'

  let ast = runFillLabels $ buildAST (scriptOps script)
      branchReports = genBuildStates ast
  branchReports' <- mapM (prologVerify dir) branchReports

  when (verbosity >= 2) $ do
      tell "SCRIPT echo, followed by the lexed intermediate variant:\n"
      tell $ (show $ bs') ++ "\n"
      tell $ (show script) ++ "\n"
  when (verbosity >= 1) $ do -- Verbose section
      tell $ "-------------------------\n"
      tell $ "---------- AST ----------\n"
      tell $ show ast

      tell $ "-------------------------"
      tell $ "-------- Inferred -------"
      tell $ dumpBranchReports branchReports' verbosity

  tell $ "------------------------\n"
  tell $ "-------- Verdict -------\n"

  let successBuilds = filter (prologValid) branchReports'

  -- Non verbose section.
  when (null successBuilds) $ tell (preVerdict ++ "nonredeemable")-- >> exitFailure
  tell (preVerdict ++ "types correct, " ++ show (length successBuilds) ++ " branch(es) viable") -- >> exitSuccess

prologVerify :: String -> BranchReport -> IOReport BranchReport
prologVerify dir report@(BranchReport _ _ (Just err) _ _) =
  return report
prologVerify dir report =
  case branchToProlog (symbolicEval report) of
    Left e -> return $ report { prologReport = e }
    Right pl -> do
      let fn = dir ++ "BitcoinAnalysis-script.pl"
      liftIO $ writeFile fn pl
      --results <- mapM (verifyC fn) (zip [0..] (val_cnstrs bs))
      result <- verifyC fn (-1,undefined)
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
      initialStack = "Required initial stack:\n" ++
                     printStack (map Var [0,(-1)..freshV bs+1])
      vconstrs = "Inferred additional constraints:\n\t" ++
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
     vconstrs ++
     (if verbosity >= 2 then "\n" ++ tconstrs ++ "\n" ++ trace else "") ++ "\n" ++
     st ++ "\n" ++
     prolog ++ "\n"


printStack :: Show a => [a] -> String
printStack [] = "head -> |------------------|\n"
printStack (x:xs) =
  let sep = "\t|------------------|\n"
      xs' = concatMap (\e -> "\t| " ++ show e ++ "\n") xs
      x'  = "head -> | " ++ show x ++ "\n"
  in sep ++ x' ++ xs' ++ sep
