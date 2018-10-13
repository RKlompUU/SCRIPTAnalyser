{-# LANGUAGE DeriveDataTypeable #-}
module Bitcoin.Script.Analysis.Standard where

import qualified Data.Typeable as T
import qualified Data.Data as TD

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as BS8L

import Control.Monad.State.Lazy

import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)

import Data.Maybe
import Data.List

type CounterState a = State Int a

groupList :: [a] -> Int -> [[a]]
groupList xs n =
  filter (not . null)
  $ map (\i -> take n $ drop (i*n) xs) [0..(length xs) `div` n]

replaceX :: Eq a => (a,a) -> [a] -> [a]
replaceX _ [] = []
replaceX (f,t) (x:xs)
  | f == x = t : replaceX (f,t) xs
  | True   = x : replaceX (f,t) xs

replaceXs :: Eq a => [([a], [a])] -> [a] -> [a]
replaceXs _ [] = []
replaceXs xss ys =
  case find isJust (map (\(xs,xs') -> if xs == ys then Just (xs,xs') else Nothing) xss) of
    Just (Just (xs,xs')) -> xs' ++ replaceXs xss (drop (length xs) ys)
    otherwise            -> head ys : replaceXs xss (tail ys)

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x:xs) !? i
  | i < 0  = Nothing
  | i == 0 = Just x
  | i > 0  = xs !? (i - 1)

removeIfFileExists :: FilePath -> IO ()
removeIfFileExists fileName =
  removeFile fileName `catch` handleExists
  where handleExists e | isDoesNotExistError e = return ()
                       | otherwise = throwIO e

removeIfDirExists :: FilePath -> IO ()
removeIfDirExists dir =
  removeDirectory dir `catch` handleExists
  where handleExists e | isDoesNotExistError e = return ()
                       | otherwise = throwIO e

replace :: Eq a => (a, [a]) -> [a] -> [a]
replace (x,x') xs
  = concatMap (\y -> if x == y then x' else [y]) xs

replaceIndex :: [a] -> Int -> a -> [a]
replaceIndex xs i x =
  take i xs ++ [x] ++ drop (i+1) xs

printBSInHex :: BS.ByteString -> String
printBSInHex = show . BSB.toLazyByteString . BSB.byteStringHex

hexBS2Str :: BS.ByteString -> String
hexBS2Str = BS8L.unpack . BSB.toLazyByteString . BSB.byteStringHex

tickCounter :: CounterState Int
tickCounter = do
  i <- get
  put (i+1)
  return i

evalCounter :: CounterState a -> a
evalCounter f = evalState f 0


ccEq :: (TD.Data a) => a -> TD.Constr -> Bool
ccEq e c = TD.toConstr e == c


{-
class CC a where
  cc :: a -> TD.Constr

instance TD.Data a => CC a where
  cc = TD.toConstr

instance CC a => CC (b -> a) where
  cc f = cc (f undefined)
-}
