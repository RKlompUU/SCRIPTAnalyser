{-# LANGUAGE GADTs #-}
module Script.AST where

import Data.Bitcoin.Script.Types

import KlompStandard

type Label = Int

data ScriptAST where
  ScriptITE  :: ScriptAST -> ScriptAST -> ScriptAST -> ScriptAST
  ScriptOp   :: Label -> ScriptOp  -> ScriptAST -> ScriptAST
  ScriptTail :: ScriptAST

instance Show ScriptAST where
  show (ScriptOp l op cont) = show l ++ ":\t" ++ show op ++ ";\n" ++ show cont
  show (ScriptITE b0 b1 cont) = "\tIF {\n" ++ show b0 ++ "\t} ELSE {\n" ++ show b1 ++ "\t}\n" ++ show cont
  show ScriptTail = ""


runFillLabels :: ScriptAST -> ScriptAST
runFillLabels scrpt = evalCounter (fillLabels scrpt)

fillLabels :: ScriptAST -> CounterState ScriptAST
fillLabels (ScriptITE b0 b1 cont) = do
  b0' <- fillLabels b0
  b1' <- fillLabels b1
  cont' <- fillLabels cont
  return $ ScriptITE b0' b1' cont'
fillLabels (ScriptOp _ op cont) = do
  lblOp <- tickCounter
  cont' <- fillLabels cont
  return $ ScriptOp lblOp op cont'
fillLabels ScriptTail = return ScriptTail
