
-- | Strict version of common data types

{-# LANGUAGE BangPatterns #-}
module Bitcoin.Misc.Strict where

--------------------------------------------------------------------------------
-- * Maybe

data SMaybe a 
  = SJust !a 
  | SNothing
  deriving (Eq,Ord,Show)

catSMaybes :: [SMaybe a] -> [a]
catSMaybes = go where
  go mbs = case mbs of 
    (mb:rest) -> case mb of
      SJust x  -> x : go rest
      SNothing ->     go rest
    [] -> [] 

--------------------------------------------------------------------------------
-- * Either

data SEither a b
  = SLeft  !a
  | SRight !b
  deriving (Eq,Ord,Show)

--------------------------------------------------------------------------------
-- * Tuples

data SPair   a b     = SPair   !a !b        deriving (Eq,Ord,Show)
data STriple a b c   = STriple !a !b !c     deriving (Eq,Ord,Show)
data SQuad   a b c d = SQuad   !a !b !c !d  deriving (Eq,Ord,Show)

--------------------------------------------------------------------------------
-- * List

data SList a 
  = SCons !a !(SList a)
  | SNil
  deriving (Eq,Ord,Show)

--------------------------------------------------------------------------------
