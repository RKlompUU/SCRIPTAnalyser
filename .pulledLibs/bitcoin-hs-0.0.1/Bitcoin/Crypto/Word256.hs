
-- | Word256 implementation, with arithmetic written in C. 
--
-- Should work on both little-endian and big-endian architectures,
-- but only tested on little-endian.

{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Bitcoin.Crypto.Word256 
  ( Word256 , toWord256 , fromWord256
  , word256Decimal, word256Hex
  , word256ToByteStringLE , word256ToByteStringBE
  , word256ToWord8ListLE 
  , makeWord256 , readWord256
  , newWord256 , withWord256 
  , peekWord256 , pokeWord256
  , shiftl256_small , shiftr256_small
  , shiftl256_fullword , shiftr256_fullword 
  , shiftr256by1 
  , highestSetBit256 
  , not256 , neg256 
  , add256 , sub256 , mul256
  , scale256 
  , equals256 , lessThan256 , lessOrEqual256 
  , isEven256 , isOdd256
  , twoToThe256 , mod256
  , littleEndianRollInteger32 , littleEndianUnrollInteger32 
  )
  where

--------------------------------------------------------------------------------

import Data.Word
import Data.Bits

import Data.List ( unfoldr )

import Foreign
import Foreign.C

import qualified System.IO.Unsafe as Unsafe
import qualified Data.ByteString  as B

import Bitcoin.Misc.HexString

#ifdef __GLASGOW_HASKELL__
import GHC.ForeignPtr ( mallocPlainForeignPtrBytes )
#endif

--------------------------------------------------------------------------------

-- | Word256 is represented internally as 8 native 32 bit unsigned integers, 
-- in little-endian order. 
newtype Word256 = Word256 { unWord256 :: ForeignPtr Word32 } 

word256Decimal :: Word256 -> String
word256Decimal = show . fromWord256

word256Hex :: Word256 -> String
word256Hex = toHexStringChars . word256ToByteStringBE

instance Show Word256 where
  show w = "0x" ++ word256Hex w 
  -- show = word256Decimal

--------------------------------------------------------------------------------

toWord256 :: Integer -> Word256
toWord256 n = Unsafe.unsafePerformIO (makeWord256 n)

fromWord256 :: Word256 -> Integer
fromWord256 w256 = Unsafe.unsafePerformIO (readWord256 w256)

--------------------------------------------------------------------------------

-- | Converts to a little-endian bytestring
word256ToByteStringLE :: Word256 -> B.ByteString
word256ToByteStringLE = B.pack . word256ToWord8ListLE

-- | Converts to a big-endian bytestring
word256ToByteStringBE :: Word256 -> B.ByteString
word256ToByteStringBE = B.pack . reverse . word256ToWord8ListLE

-- | Converts to a little-endian sequence of bytes
word256ToWord8ListLE :: Word256 -> [Word8]
word256ToWord8ListLE w256 = 
  Unsafe.unsafePerformIO $ do
    w32s <- peekWord256 w256
    return $ concatMap f w32s
  where
    f w32 = [ fromIntegral ((shiftR w32 k) .&. 255) | k<-[0,8,16,24] ]

--------------------------------------------------------------------------------

newWord256 :: IO Word256
newWord256 = do
#ifdef __GLASGOW_HASKELL__
  fptr <- mallocPlainForeignPtrBytes 32 
#else
  fptr <- mallocForeignPtrBytes 32 
#endif
  return (Word256 fptr)

withWord256 :: Word256 -> (Ptr Word32 -> IO a) -> IO a
withWord256 (Word256 fptr) action = withForeignPtr fptr action

--------------------------------------------------------------------------------

makeWord256 :: Integer -> IO Word256
makeWord256 n = do
#ifdef __GLASGOW_HASKELL__
  fptr <- mallocPlainForeignPtrBytes 32 
#else
  fptr <- mallocForeignPtrBytes 32
#endif
  withForeignPtr fptr $ \ptr -> do
    pokeWord256 (Word256 fptr) (take 8 $ littleEndianUnrollInteger32 n ++ repeat 0) 
  return (Word256 fptr)

readWord256 :: Word256 -> IO Integer
readWord256 w256 = do
  ws <- peekWord256 w256
  return $ littleEndianRollInteger32 ws

peekWord256 :: Word256 -> IO [Word32]
peekWord256 (Word256 fptr) = withForeignPtr fptr $ \ptr -> peekArray 8 ptr

pokeWord256 :: Word256 -> [Word32] -> IO ()
pokeWord256 (Word256 fptr) ws = withForeignPtr fptr $ \ptr -> pokeArray ptr ws

--------------------------------------------------------------------------------

littleEndianRollInteger32 :: [Word32] -> Integer
littleEndianRollInteger32 = foldr unstep 0 where
  unstep b a = shiftL a 32 .|. fromIntegral b

littleEndianUnrollInteger32 :: Integer -> [Word32]
littleEndianUnrollInteger32 = unfoldr step where
  step 0 = Nothing
  step i = Just (fromIntegral i, shiftR i 32)

--------------------------------------------------------------------------------

foreign import ccall unsafe "c_word256.c not256" c_not256 :: Ptr Word32 -> Ptr Word32 -> IO ()
foreign import ccall unsafe "c_word256.c neg256" c_neg256 :: Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "c_word256.c shiftl256_small"    c_shiftl256_small    :: Ptr Word32 -> CInt -> Ptr Word32 -> IO Word32
foreign import ccall unsafe "c_word256.c shiftr256_small"    c_shiftr256_small    :: Ptr Word32 -> CInt -> Ptr Word32 -> IO Word32
foreign import ccall unsafe "c_word256.c shiftl256_fullword" c_shiftl256_fullword :: Ptr Word32         -> Ptr Word32 -> IO Word32
foreign import ccall unsafe "c_word256.c shiftr256_fullword" c_shiftr256_fullword :: Ptr Word32         -> Ptr Word32 -> IO Word32
foreign import ccall unsafe "c_modp.c    shiftr256by1"       c_shiftr256by1       :: Ptr Word32         -> Ptr Word32 -> IO Word32

foreign import ccall unsafe "c_word256.c highestSetBit256"   c_highestSetBit256   :: Ptr Word32 -> IO CInt

foreign import ccall unsafe "c_word256.c add256"   c_add256   :: Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO Word32
foreign import ccall unsafe "c_word256.c sub256"   c_sub256   :: Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO Word32
foreign import ccall unsafe "c_word256.c mul256"   c_mul256   :: Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO ()
foreign import ccall unsafe "c_word256.c scale256" c_scale256 :: Ptr Word32 ->     Word32 -> Ptr Word32 -> IO Word32

foreign import ccall unsafe "c_word256.c equals256"      c_equals256      :: Ptr Word32 -> Ptr Word32 -> IO CInt
foreign import ccall unsafe "c_word256.c lessThan256"    c_lessThan256    :: Ptr Word32 -> Ptr Word32 -> IO CInt
foreign import ccall unsafe "c_word256.c lessOrEqual256" c_lessOrEqual256 :: Ptr Word32 -> Ptr Word32 -> IO CInt

--------------------------------------------------------------------------------

-- | Shifts left by at most 31 bits
shiftl256_small :: Word256 -> Int -> Word256
shiftl256_small a k = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 c $ \pc -> c_shiftl256_small pa (fromIntegral $ mod k 32) pc
  return c

-- | Shifts right by at most 31 bits
shiftr256_small :: Word256 -> Int -> Word256
shiftr256_small a k = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 c $ \pc -> c_shiftr256_small pa (fromIntegral $ mod k 32) pc
  return c

-- | Shifts left by 32 bits
shiftl256_fullword :: Word256 -> Word256
shiftl256_fullword a = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 c $ \pc -> c_shiftl256_fullword pa pc
  return c

-- | Shifts right by 32 bits
shiftr256_fullword :: Word256 -> Word256
shiftr256_fullword a = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 c $ \pc -> c_shiftr256_fullword pa pc
  return c

-- | Shifts right by 1 bit
shiftr256by1 :: Word256 -> Word256
shiftr256by1 a = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 c $ \pc -> c_shiftr256by1 pa pc
  return c

highestSetBit256 :: Word256 -> Int
highestSetBit256 a = Unsafe.unsafePerformIO $ do
  k <- withWord256 a $ \pa -> c_highestSetBit256 pa
  return (fromIntegral k)

--------------------------------------------------------------------------------

not256 :: Word256 -> Word256
not256 a = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 c $ \pc -> c_not256 pa pc
  return c

neg256 :: Word256 -> Word256
neg256 a = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 c $ \pc -> c_neg256 pa pc
  return c

add256 :: Word256 -> Word256 -> Word256
add256 a b = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 b $ \pb -> withWord256 c $ \pc -> c_add256 pa pb pc
  return c

sub256 :: Word256 -> Word256 -> Word256
sub256 a b = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 b $ \pb -> withWord256 c $ \pc -> c_sub256 pa pb pc
  return c

mul256 :: Word256 -> Word256 -> Word256
mul256 a b = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 b $ \pb -> withWord256 c $ \pc -> c_mul256 pa pb pc
  return c

scale256 :: Word256 -> Word32 -> Word256
scale256 a b = Unsafe.unsafePerformIO $ do
  c <- newWord256
  withWord256 a $ \pa -> withWord256 c $ \pc -> c_scale256 pa b pc
  return c

equals256 :: Word256 -> Word256 -> Bool
equals256 a b = Unsafe.unsafePerformIO $ do
  x <- withWord256 a $ \pa -> withWord256 b $ \pb -> c_equals256 pa pb 
  return (x/=0)

lessThan256 :: Word256 -> Word256 -> Bool
lessThan256 a b = Unsafe.unsafePerformIO $ do
  x <- withWord256 a $ \pa -> withWord256 b $ \pb -> c_lessThan256 pa pb 
  return (x/=0)

lessOrEqual256 :: Word256 -> Word256 -> Bool
lessOrEqual256 a b = Unsafe.unsafePerformIO $ do
  x <- withWord256 a $ \pa -> withWord256 b $ \pb -> c_lessOrEqual256 pa pb 
  return (x/=0)

--------------------------------------------------------------------------------

isEven256 :: Word256 -> Bool
isEven256 a = Unsafe.unsafePerformIO $ do
  c <- newWord256
  carry <- withWord256 a $ \pa -> withWord256 c $ \pc -> c_shiftr256_small pa 1 pc
  return (carry == 0)

isOdd256 :: Word256 -> Bool
isOdd256 a = Unsafe.unsafePerformIO $ do
  c <- newWord256
  carry <- withWord256 a $ \pa -> withWord256 c $ \pc -> c_shiftr256_small pa 1 pc
  return (carry /= 0)

--------------------------------------------------------------------------------

instance Eq Word256 where
  (==) = equals256

instance Ord Word256 where
  (<)  = lessThan256
  (<=) = lessOrEqual256

instance Num Word256 where
  (+) = add256
  (-) = sub256
  (*) = mul256
  negate = neg256
  fromInteger = toWord256
  abs      = id
  signum _ = toWord256 1

--------------------------------------------------------------------------------
-- * useful

instance Bounded Word256 where
  minBound = toWord256 0
  maxBound = toWord256 (twoToThe256-1)

twoToThe256 :: Integer
twoToThe256 = 2^(256::Int)

mod256 :: Integer -> Integer
mod256 n = mod n twoToThe256

--------------------------------------------------------------------------------



