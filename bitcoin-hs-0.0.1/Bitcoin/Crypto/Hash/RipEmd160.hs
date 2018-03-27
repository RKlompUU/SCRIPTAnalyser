
-- | RipEmd-160 hash implementation: a wrapper around Antoon Bosselaers' C sample implementation.
--
-- WARNING: little-endian only

{-# LANGUAGE ForeignFunctionInterface #-}
module Bitcoin.Crypto.Hash.RipEmd160
  ( RipEmd160(..)
  , ripemd160
  , ripemdTest , testCases
  )
  where

--------------------------------------------------------------------------------

import Data.Char
import Data.Int
import Data.Word
import Data.Bits

import qualified Data.ByteString as B

import Control.Monad
import Foreign
import System.IO.Unsafe as Unsafe

import Bitcoin.Misc.OctetStream
import Bitcoin.Misc.HexString

--------------------------------------------------------------------------------

-- void MDinit(dword *MDbuf)
foreign import ccall safe "rmd160.c MDinit" c_MDInit :: Ptr Word32 -> IO ()

-- void compress(dword *MDbuf, dword *X)
foreign import ccall safe "rmd160.c compress" c_compress :: Ptr Word32 -> Ptr Word32 -> IO ()

-- void MDfinish(dword *MDbuf, byte *strptr, dword lswlen, dword mswlen)
foreign import ccall safe "rmd160.c MDfinish" c_MDfinish :: Ptr Word32 -> Ptr Word8 -> Word32 -> Word32 -> IO ()

--------------------------------------------------------------------------------

newtype RipEmd160 = RipEmd160 { unRipEmd160 :: B.ByteString } deriving (Eq,Ord)

instance Show RipEmd160 where show (RipEmd160 bs) = "RipEmd160<" ++ toHexStringChars bs ++ ">"

instance OctetStream RipEmd160 where
  toByteString = unRipEmd160
  fromByteString bs = case B.length bs of
    20 -> RipEmd160 bs
    _  -> error "RipEmd160/fromByteString: RipEmd160 is expected to be 20 bytes"

--------------------------------------------------------------------------------

ripemd160 :: OctetStream a => a -> RipEmd160
ripemd160 msg = RipEmd160 $ Unsafe.unsafePerformIO (ripemd160_IO $ toByteString msg)

ripemd160_IO :: B.ByteString -> IO B.ByteString
ripemd160_IO msg = do
  let n = B.length msg
      k = div n 64     -- computation units are 16 dwords
      (remaining, chunks) = partition k msg

  allocaBytes 20 $ \mdbuf -> do 
    c_MDInit mdbuf
    forM_ chunks $ \chunk -> B.useAsCStringLen chunk $ \(ptr,_) -> c_compress mdbuf (castPtr ptr)
    B.useAsCStringLen remaining $ \(ptr,_) -> c_MDfinish mdbuf (castPtr ptr) (fromIntegral n) 0
    B.packCStringLen (castPtr mdbuf, 20)   -- note: this works only for little-endian architectures!

partition :: Int -> B.ByteString -> (B.ByteString,[B.ByteString])
partition 0 msg = (msg,[])
partition k msg = let (rest,xs) = partition (k-1) (B.drop 64 msg) in (rest, B.take 64 msg : xs)

--------------------------------------------------------------------------------

-- | Result is a list of failed test cases. Empty list -> OK.
ripemdTest :: [String]
ripemdTest = concatMap worker list where
  list = zip [1..] testCases
  worker (i,(msg,hexhash)) = result where
    ourhash  = toHexString' False $ ripemd160 msg
    result = if hexhash==ourhash then [] else ["test case " ++ show i ++ " failed"]
 
testCases :: [(String,HexString)]
testCases =  map (\(msg,hash) -> (msg, HexString hash)) 
  [ ("" , "9c1185a5c5e9fc54612808977ee8f548b2258d31")
  , ("a" , "0bdc9d2d256b3ee9daae347be6f4dc835a467ffe")
  , ("abc" , "8eb208f7e05d987a9b044a8e98c6b087f15a0bfc")
  , ("message digest" , "5d0689ef49d2fae572b881b123a85ffa21595f36")
  , ("abcdefghijklmnopqrstuvwxyz" , "f71c27109c692c1b56bbdceb5b9d2865b3708dbc")
  , ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" , "12a053384a9c0c88e405a06c27dcf49ada62eb2b")
  , ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" , "b0e20b6e3116640286ed3a87a5713079b21f5189")
  , (concat (replicate 8 "1234567890") , "9b752e45573d4b39f4dbd3323cab82bf63326bfb")
  , (replicate 1000000 'a' , "52783243c1697bdbe16d37f97f68f08325dc1528")
  ]

--------------------------------------------------------------------------------

