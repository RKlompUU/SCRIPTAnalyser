{-# LANGUAGE GADTs #-}
module Script.AST where

import Data.Bitcoin.Script.Types

type Label = Int

data ScriptAST where
  ScriptITE  :: Label -> ScriptAST -> ScriptAST -> ScriptAST -> ScriptAST
  ScriptOp   :: Label -> ScriptOp -> ScriptAST -> ScriptAST
  ScriptTail :: ScriptAST
  deriving (Show)
