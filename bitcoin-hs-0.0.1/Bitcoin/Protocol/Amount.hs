
-- | Amounts of BTC

module Bitcoin.Protocol.Amount 
  ( 
    Amount(..)
  -- * human-friendly definitions of amounts
  , btc , mbtc , ubtc , satoshi
  -- * constants  
  , amountMultiplier , amountExponent
  -- * conversion
  , integerAmount, doubleAmount
  , amountFromDouble
  , showAmount , parseAmount  
  ) where

--------------------------------------------------------------------------------

import Data.Word
import Data.List ( findIndex )
import Data.Maybe 

import Text.Show
import Text.Read


--------------------------------------------------------------------------------

-- | A (nonnegative) amount of bitcoins.
--
-- Note: the Show/Read instances look like 
-- 
-- > newtype Amount = BTC Double
--
-- eg. @BTC 1.3@
--
-- However, the internal representation is different.
newtype Amount = Amount { unAmount :: Word64 } deriving (Eq,Ord)

--------------------------------------------------------------------------------

instance Show Amount where
  showsPrec d amt = showParen (d>10) $ showString "BTC " . showString (showAmount amt)

instance Read Amount where
  readsPrec d r = readParen (d > 10) 
    (\r -> [ (fromJust mbAmt,t) 
           | ("BTC",s) <- lex r
           , (m,t) <- lex s -- readsPrec 11 s
           , let mbAmt = parseAmount m
           , isJust mbAmt
           ]) r
  
--------------------------------------------------------------------------------
-- * constants

-- | @10^amountExponent == 10^8@
amountMultiplier :: Num a => a
amountMultiplier = 100000000       

-- | The exponent is 8, as @10^(-8)@ is the smallest unit.
amountExponent :: Int
amountExponent = 8

--------------------------------------------------------------------------------
-- * easy amounts

-- | Whole BTCs
btc :: Double -> Amount
btc = amountFromDouble

-- | milli-BTCs
mbtc :: Double -> Amount
mbtc n = amountFromDouble (n/1000)

-- | micro-BTCs
ubtc :: Double -> Amount
ubtc n = amountFromDouble (n/1000000)

-- | \"satoshi\" is the smallest unit: 1 satoshi = 10\^(-8) BTC
satoshi :: Int -> Amount
satoshi n = Amount $ fromIntegral $ n

--------------------------------------------------------------------------------
-- * conversion

-- | The amount measured in smallest units (satoshis)
integerAmount :: Amount -> Integer
integerAmount (Amount n) = fromIntegral n

-- | The amount in BTCs as a "Double"
doubleAmount :: Amount -> Double
doubleAmount (Amount n) = fromIntegral n / amountMultiplier

-- | Convert whole BTCs to an Amount. Rounding to whole satoshis.
amountFromDouble :: Double -> Amount
amountFromDouble d = Amount $ round (d*amountMultiplier)

-- | Shows an amount in BTCs using the minimum necessary digits
showAmount :: Amount -> String
showAmount (Amount n) = show a ++ if (b==0) then "" else ("." ++ fr) where
  (a,b) = divMod n amountMultiplier
  fr = reverse $ dropWhile (=='0') $ reverse $ extend $ show b
  extend s = (replicate (amountExponent-k) '0' ++ s) where k = length s

-- | Note: unlike doubleAmount, this ignores all digits after the 8th decimal.
parseAmount :: String -> Maybe Amount
parseAmount s = 
  case splitDecimalPoint s of
    [s]    -> case maybeReadWord64 s of
                Just n -> Just (Amount (n*amountMultiplier))
                _      -> Nothing
    [s,""] -> case maybeReadWord64 s of
                Just n -> Just (Amount (n*amountMultiplier))
                _      -> Nothing
    [s,t]  -> case (maybeReadWord64 s, maybeReadWord64 t) of
                (Just a, Just _) -> Just (Amount (a*amountMultiplier + b)) where 
                                      b = read 
                                        $ take amountExponent 
                                        $ (t ++ replicate amountExponent '0')
                _                -> Nothing
    _ -> Nothing

--------------------------------------------------------------------------------
-- * helpers

maybeReadWord64 :: String -> Maybe Word64
maybeReadWord64 s = case reads s of
  [(w,"")] -> Just w
  _        -> Nothing

splitDecimalPoint :: String -> [String]
splitDecimalPoint s = case findIndex (=='.') s of
  Nothing -> [s]
  Just i  -> [take i s , drop (i+1) s]

--------------------------------------------------------------------------------
