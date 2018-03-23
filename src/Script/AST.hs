{-# LANGUAGE GADTs #-}
module Script.AST where

import Data.Bitcoin.Script.Types

type Label = Int

data ScriptAST where
  ScriptITE  :: Label -> ScriptAST -> ScriptAST -> ScriptAST -> ScriptAST
  ScriptOp   :: Label -> ScriptOp  -> ScriptAST -> ScriptAST
  ScriptTail :: ScriptAST

instance Show ScriptAST where
  show (ScriptOp l op cont) = show op ++ ";\n" ++ show cont
  show (ScriptITE l b0 b1 cont) = "IF {\n" ++ show b0 ++ "} ELSE {\n" ++ show b1 ++ "}\n" ++ show cont
  show ScriptTail = ""
