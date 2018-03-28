{-# LANGUAGE FlexibleContexts #-}

-- | It should not be unexpected that you will be given a string representation of some
-- ranges and you will need to parse them so that you can then do some further processing.
-- This parser exists in order to make the most common forms of range strings easy to
-- parse. It does not cover all cases however but you should not be too worried about
-- that because you should be able to write your own parser using parsec or Alex/Happy and
-- then you can convert everything that you parse into a RangeTree object for easier
-- processing.
module Data.Range.Parser 
   ( parseRanges
   , ranges
   , RangeParserArgs(..)
   , defaultArgs
   ) where

import Text.Parsec
import Text.Parsec.String

import Data.Range.Range

-- | The arguments that are used, and can be modified, while parsing a standard range
-- string.
data RangeParserArgs = Args 
   { unionSeparator :: String -- ^ A separator that represents a union.
   , rangeSeparator :: String -- ^ A separator that separates the two halves of a range.
   , wildcardSymbol :: String -- ^ A separator that implies an unbounded range.
   }
   deriving(Show)

-- | These are the default arguments that are used by the parser. Please feel free to use
-- the default arguments for you own parser and modify it from the defaults at will.
defaultArgs :: RangeParserArgs 
defaultArgs = Args
   { unionSeparator = ","
   , rangeSeparator = "-"
   , wildcardSymbol = "*"
   }

-- | Given a string this function will either return a parse error back to the user or the
-- list of ranges that are represented by the parsed string.
parseRanges :: (Read a) => String -> Either ParseError [Range a]
parseRanges = parse (ranges defaultArgs) "(range parser)"

string_ :: Stream s m Char => String -> ParsecT s u m ()
string_ x = string x >> return ()

-- | Given the parser arguments this returns a parser that is capable of parsing a list of
-- ranges.
ranges :: (Read a) => RangeParserArgs -> Parser [Range a]
ranges args = range `sepBy` (string $ unionSeparator args)
   where 
      range :: (Read a) => Parser (Range a)
      range = choice 
         [ infiniteRange
         , spanRange
         , singletonRange
         ]

      infiniteRange :: (Read a) => Parser (Range a)
      infiniteRange = do
         string_ $ wildcardSymbol args
         return InfiniteRange

      spanRange :: (Read a) => Parser (Range a)
      spanRange = try $ do
         first <- readSection
         string_ $ rangeSeparator args
         second <- readSection
         case (first, second) of
            (Just x, Just y)  -> return $ SpanRange x y
            (Just x, _)       -> return $ LowerBoundRange x
            (_, Just y)       -> return $ UpperBoundRange y
            _                 -> parserFail ("Range should have a number on one end: " ++ rangeSeparator args)

      singletonRange :: (Read a) => Parser (Range a)
      singletonRange = fmap (SingletonRange . read) $ many1 digit

readSection :: (Read a) => Parser (Maybe a)
readSection = fmap (fmap read) $ optionMaybe (many1 digit)
