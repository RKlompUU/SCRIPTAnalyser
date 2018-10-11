
-- | Difficulty computation. See <https://en.bitcoin.it/wiki/Difficulty>

module Bitcoin.Protocol.Difficulty where

--------------------------------------------------------------------------------

import Data.Word
import Data.Bits

import Bitcoin.Misc.BigInt
import Bitcoin.Misc.OctetStream
import Bitcoin.Protocol.Hash

--------------------------------------------------------------------------------

-- | Computes the target from the packed difficulty representation
difficultyTarget :: Word32 -> Hash256
difficultyTarget hexdifficulty = fromWord8List (littleEndianInteger32 target) where
  msb  = fromIntegral (shiftR hexdifficulty 24)      :: Int
  base = fromIntegral (hexdifficulty .&. 0x00ffffff) :: Integer
  exponent = 8 * (msb - 3)
  target = base * 2 ^ exponent

-- | The highest (easiest) target
maximalTarget :: Hash256
maximalTarget = difficultyTarget 0x1d00ffff

-- | Computes the decimal difficulty from the packed representation
decimalDifficulty :: Word32 -> Double
decimalDifficulty = targetDecimalDifficulty . difficultyTarget

{-
decimalDifficulty :: Word32 -> Double
decimalDifficulty hexdifficulty = fromRational ratio where
  ratio = toRational (toIntegerLE maximalTarget) 
        / toRational (toIntegerLE currentTarget)
  currentTarget = difficultyTarget hexdifficulty
-}

-- | Computes the decimal difficulty from a target
targetDecimalDifficulty :: Hash256 -> Double
targetDecimalDifficulty currentTarget = fromRational ratio where
  ratio = toRational (toIntegerLE maximalTarget) 
        / toRational (toIntegerLE currentTarget)


--------------------------------------------------------------------------------