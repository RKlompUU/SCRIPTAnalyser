
-- | Dealing with endianness issues

{-# LANGUAGE BangPatterns #-}
module Bitcoin.Misc.Endian where

--------------------------------------------------------------------------------

import Data.Word
import Foreign

import qualified System.IO.Unsafe as Unsafe

--------------------------------------------------------------------------------

data Endian
  = LittleEndian 
  | BigEndian 
  deriving (Eq,Show)

isLittleEndian :: Endian -> Bool
isLittleEndian e = case e of
  LittleEndian -> True
  BigEndian    -> False

isBigEndian :: Endian -> Bool
isBigEndian e = case e of
  LittleEndian -> False
  BigEndian    -> True

--------------------------------------------------------------------------------

hostEndian :: Endian
hostEndian = Unsafe.unsafePerformIO detectHostEndian

detectHostEndian :: IO Endian
detectHostEndian = do
  let be = 0x12345678 :: Word32
      le = 0x78563412 :: Word32
      ws = [0x12,0x34,0x56,0x78] :: [Word8]
  allocaArray 4 $ \p_word8 -> do
    pokeArray p_word8 ws
    x <- peek (castPtr p_word8 :: Ptr Word32)
    if x == le 
      then return LittleEndian
      else if x == be 
        then return BigEndian
        else error "fatal error: cannot detect endianness of host (neither little-endian nor big-endian)"

--------------------------------------------------------------------------------

class HasByteOrder a where
  -- | swaps the byte order
  swapByteOrder :: a -> a    

  toLilEndBytes  :: a -> [Word8]      -- ^ LE order
  toBigEndBytes  :: a -> [Word8]      -- ^ BE order

  fromLilEndBytes  :: [Word8] -> a      -- ^ LE order
  fromBigEndBytes  :: [Word8] -> a      -- ^ BE order

  toBigEndBytes  = reverse . toLilEndBytes
  toLilEndBytes = reverse . toBigEndBytes

  fromBigEndBytes  = fromLilEndBytes . reverse
  fromLilEndBytes  = fromBigEndBytes . reverse

--------------------------------------------------------------------------------

-- | Native memory order
toMachineBytes :: HasByteOrder a => a -> [Word8]      
toMachineBytes = case hostEndian of 
  LittleEndian -> toLilEndBytes 
  BigEndian    -> toBigEndBytes 

-- | Native memory order
fromMachineBytes :: HasByteOrder a => [Word8] -> a      
fromMachineBytes = case hostEndian of 
  LittleEndian -> fromLilEndBytes
  BigEndian    -> fromBigEndBytes 

--------------------------------------------------------------------------------

-- | on little-endian hosts, this is identity; on big-endian hosts, it swaps the byte order
swapByteOrderToLE :: HasByteOrder a => a -> a        
swapByteOrderToLE = case hostEndian of
  LittleEndian -> id
  BigEndian    -> swapByteOrder

-- | on big-endian hosts, this is identity; on little-endian hosts, it swaps the byte order
swapByteOrderToBE :: HasByteOrder a => a -> a        
swapByteOrderToBE = case hostEndian of
  LittleEndian -> swapByteOrder
  BigEndian    -> id

--------------------------------------------------------------------------------

instance HasByteOrder Word16 where
  swapByteOrder x = shiftL (x .&. 0x00ff) 8
                  + shiftR (x .&. 0xff00) 8

  toLilEndBytes x = map fromIntegral [        x   , shiftR x 8 ]
  toBigEndBytes x = map fromIntegral [ shiftR x 8 ,        x   ]

  fromLilEndBytes [a,b] = shiftL (fromIntegral b) 8 
                        +         fromIntegral a

  fromBigEndBytes [b,a] = shiftL (fromIntegral b) 8 
                        +         fromIntegral a

instance HasByteOrder Word32 where
  swapByteOrder x = shiftL (x .&. 0x000000ff) 24
                  + shiftL (x .&. 0x0000ff00)  8
                  + shiftR (x .&. 0x00ff0000)  8
                  + shiftR (x .&. 0xff000000) 24

  toLilEndBytes x = map fromIntegral [        x    , shiftR x 8  , shiftR x 16 , shiftR x 24 ]
  toBigEndBytes x = map fromIntegral [ shiftR x 24 , shiftR x 16 , shiftR x 8  ,        x    ]

  fromLilEndBytes [a,b,c,d] = shiftL (fromIntegral d) 24
                            + shiftL (fromIntegral c) 16
                            + shiftL (fromIntegral b) 8 
                            +         fromIntegral a

  fromBigEndBytes [d,c,b,a] = shiftL (fromIntegral d) 24
                            + shiftL (fromIntegral c) 16
                            + shiftL (fromIntegral b) 8 
                            +         fromIntegral a

instance HasByteOrder Word64 where
  swapByteOrder x = shiftL (x .&. 0x00000000000000ff) 56
                  + shiftL (x .&. 0x000000000000ff00) 40
                  + shiftL (x .&. 0x0000000000ff0000) 24
                  + shiftL (x .&. 0x00000000ff000000)  8
                  + shiftR (x .&. 0x000000ff00000000)  8
                  + shiftR (x .&. 0x0000ff0000000000) 24
                  + shiftR (x .&. 0x00ff000000000000) 40
                  + shiftR (x .&. 0xff00000000000000) 56
  toLilEndBytes x = map fromIntegral [        x    , shiftR x 8  , shiftR x 16 , shiftR x 24 , shiftR x 32 , shiftR x 40 , shiftR x 48 , shiftR x 56 ]
  toBigEndBytes x = map fromIntegral [ shiftR x 56 , shiftR x 48 , shiftR x 40 , shiftR x 32 , shiftR x 24 , shiftR x 16 , shiftR x 8  ,        x    ]

  fromLilEndBytes [a,b,c,d,e,f,g,h]
                          = shiftL (fromIntegral h) 56
                          + shiftL (fromIntegral g) 48
                          + shiftL (fromIntegral f) 40
                          + shiftL (fromIntegral e) 32
                          + shiftL (fromIntegral d) 24
                          + shiftL (fromIntegral c) 16
                          + shiftL (fromIntegral b) 8 
                          +         fromIntegral a

  fromBigEndBytes [h,g,f,e,d,c,b,a]
                          = shiftL (fromIntegral h) 56
                          + shiftL (fromIntegral g) 48
                          + shiftL (fromIntegral f) 40
                          + shiftL (fromIntegral e) 32
                          + shiftL (fromIntegral d) 24
                          + shiftL (fromIntegral c) 16
                          + shiftL (fromIntegral b) 8 
                          +         fromIntegral a

--------------------------------------------------------------------------------

