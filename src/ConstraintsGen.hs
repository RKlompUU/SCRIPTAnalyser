{-# LANGUAGE GADTs #-}
module ConstraintsGen where

import Control.Monad.Trans.Reader
import Control.Monad.Writer.Lazy

import Script.AST

type Ident = Int
data Expr where
  Const :: Int   -> Expr
  Var   :: Ident -> Expr

data BConstraints where
  ExprConstr :: Expr -> BConstraints
  AndConstr  :: BConstraints -> BConstraints -> BConstraints
  OrConstr   :: BConstraints -> BConstraints -> BConstraints
  LeafConstr :: BConstraints

type Stack = [Expr]
data BuildState =
  BuildState {
    cnstrs     :: BConstraints,
    stackSt    :: Stack,
    altStackSt :: Stack,
    freshV     :: Ident
  }

type ConstraintBuilder a = ReaderT BuildState (Writer [BuildState]) a

stModITE :: Bool -> BuildState -> BuildState
stModITE b =
  \st -> undefined

genCnstrs :: ScriptAST -> ConstraintBuilder ()
genCnstrs (ScriptITE l b0 b1 cont) = do
  withReaderT (stModITE True)  (genCnstrs b0)
  withReaderT (stModITE False) (genCnstrs b1)
genCnstrs ScriptTail = do
  s <- ask
  tell [s]
