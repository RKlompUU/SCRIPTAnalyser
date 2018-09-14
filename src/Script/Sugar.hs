module Script.Sugar (unsugar) where

import Prelude hiding ((<$>), (<*), (*>), (<*>))

import ParseLib.Simple
import qualified Data.ByteString.Base16.Lazy as BS16L
import qualified Data.ByteString.Lazy        as BSL
import Data.Bitcoin.Script hiding (decode)
import Data.Binary (decode)

import Numeric (showHex)

import KlompStandard

type SParser a = Parser Char a

unsugar :: String -> String
unsugar str =
  fst $ head $ parse sugarsParser str

sugarsParser :: SParser String
sugarsParser =
  concat <$> many (stripwhite *> atom) <* stripwhite <* eof

atom :: SParser String
atom =
  (const "" <$> comment)
  <|> push
  <|> opKeyword
  <|> byte

push :: SParser String
push =
  pushit . concat <$> (token "PUSH" *> stripwhite *> many byte)
  where pushit bytes
          | numBytes <= 75 = numOp ++ bytes
          | numBytes > 75 && numBytes <= 256 = "4c" ++ numOp ++ bytes
          | numBytes > 256 && numBytes <= 2^16 = "4d" ++ numOp ++ bytes
          | numBytes > 2^16 = "4e" ++ numOp ++ bytes
          where numBytes = div (length bytes) 2
                numOp = let str = showHex numBytes ""
                        in if odd (length str)
                            then "0" ++ str
                            else str

comment :: SParser String
comment =
  (:) <$> symbol '#' <*> greedy (satisfy (/= '\n'))

opKeyword :: SParser String
opKeyword =
  choice (map (\(mem,bs) -> const bs <$> token mem) memnomic2Hex)

byte :: SParser String
byte =
  (\a b -> [a,b]) <$> satisfy isHexChar <*> satisfy isHexChar

isHexChar :: Char -> Bool
isHexChar c
  | any (==c) ['0'..'9'] = True
  | any (==c) ['a'..'f'] = True
  | any (==c) ['A'..'F'] = True
  | otherwise = False

memnomic2Hex :: [(String, String)]
memnomic2Hex =
  let autoSet = map (\op -> (show $ (decode (BSL.singleton op) :: ScriptOp), hexBS2Str $ BSL.toStrict $ BSL.singleton op))
              $ [0x4f..0xb9]
      manualSet = [("OP_0", "00"),
                   ("OP_FALSE", "00"),
                   ("OP_PUSHDATA1", "4c"),
                   ("OP_PUSHDATA2", "4d"),
                   ("OP_PUSHDATA4", "4e")]
  in manualSet ++ autoSet

stripwhite :: SParser ()
stripwhite =
  const () <$> greedy (choice (map symbol [' ', '\n', '\r', '\t']))
