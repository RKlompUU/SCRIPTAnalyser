module Constraints.PrologResultParser
  ( parsePrologResult ) where

import Prelude hiding ((<$>), (<*), (*>), (<*>))
import ParseLib.Simple

parsePrologResult :: String -> String -> Either String Int
parsePrologResult var r =
  let res = parse (prologResult var) r
  in if null res
      then Left ("Failed to parse prolog result: " ++ r)
      else Right $ fst $ head $ res

type PLRParser a = Parser Char a

prologResult :: String -> PLRParser Int
prologResult var =
  tryResult var <|> (anySymbol *> tryResult var)

tryResult :: String -> PLRParser Int
tryResult var =
  token var *> token " = " *> integer
