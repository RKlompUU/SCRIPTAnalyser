{-# LANGUAGE GADTs #-}
module Script.AST where

import Data.Bitcoin.Script.Types

type Label = Int

data ScriptAST where
  ScriptBlock :: Label -> [ScriptAST] -> ScriptAST
  ScriptITE   :: Label -> ScriptAST -> ScriptAST -> ScriptAST
  ScriptOp    :: Label -> ScriptOp -> ScriptAST
  ScriptEnd   :: Label -> ScriptAST
  deriving (Show)
