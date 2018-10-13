module Bitcoin.Script.Analysis.Constraints.PrologResultParser
  ( parsePrologPairResults ) where

import Prelude hiding ((<$>), (<*), (*>), (<*>))
import ParseLib.Simple

parsePrologPairResults :: String -> Either String [(Int,Int)]
parsePrologPairResults r =
  let res = parse (prologPairResult) r
  in if null res
      then Left ("Failed to parse prolog result: " ++ r)
      else Right $ fst $ head $ res

type PLRParser a = Parser Char a

prologPairResult :: PLRParser [(Int,Int)]
prologPairResult =
  many (stripwhite *> result)

findStart :: PLRParser String
findStart =
  token "For built-in help, use ?- help(Topic). or ?- apropos(Word)."
  <|> (anySymbol *> findStart)

stripwhite :: PLRParser ()
stripwhite =
  const () <$> greedy (choice (map symbol [' ', '\n', '\r', '\t']))

result :: PLRParser (Int, Int)
result =
  (,) <$> integer <*> (token "+" *> integer)
