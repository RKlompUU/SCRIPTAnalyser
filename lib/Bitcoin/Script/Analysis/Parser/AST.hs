{-# LANGUAGE GADTs #-}
module Bitcoin.Script.Analysis.Parser.AST where

import Data.Bitcoin.Script.Types

import qualified Data.ByteString as BS
import Bitcoin.Script.Analysis.Standard

type Label = Int

data ScriptAST where
  ScriptITE  :: Label -> ScriptAST -> Label -> ScriptAST -> Label -> ScriptAST -> ScriptAST
  ScriptOp   :: Label -> ScriptOp  -> ScriptAST -> ScriptAST
  ScriptTail :: ScriptAST

instance Show ScriptAST where
  show (ScriptOp l (OP_PUSHDATA bs _) cont) = show l ++ ":\tBS_" ++ show (BS.length bs) ++ " " ++ printBSInHex bs ++ ";\n" ++ show cont
  show (ScriptOp l op cont) = show l ++ ":\t" ++ show op ++ ";\n" ++ show cont
  show (ScriptITE ifLbl b0 elseLbl b1 fiLbl cont) =
    show ifLbl ++ ":\tIF {\n" ++
    show b0 ++
    show elseLbl ++ ":\t} ELSE {\n" ++
    show b1 ++
    show fiLbl ++ ":\t}\n" ++
    show cont
  show ScriptTail = ""


runFillLabels :: ScriptAST -> ScriptAST
runFillLabels scrpt = evalCounter (fillLabels scrpt)

fillLabels :: ScriptAST -> CounterState ScriptAST
fillLabels (ScriptITE _ b0 _ b1 _ cont) = do
  ifLbl <- tickCounter
  b0' <- fillLabels b0
  elseLbl <- tickCounter
  b1' <- fillLabels b1
  fiLbl <- tickCounter
  cont' <- fillLabels cont
  return $ ScriptITE ifLbl b0' elseLbl b1' fiLbl cont'
fillLabels (ScriptOp _ op cont) = do
  lblOp <- tickCounter
  cont' <- fillLabels cont
  return $ ScriptOp lblOp op cont'
fillLabels ScriptTail = return ScriptTail
