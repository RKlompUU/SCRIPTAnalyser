
-- | SHA512 hash: wrapper around Aaron D. Gifford's C implementation.
-- 

{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Bitcoin.Crypto.Hash.SHA512
  ( SHA512(..)
  , sha512
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
import Foreign.C
import System.IO.Unsafe as Unsafe

import Bitcoin.Misc.OctetStream
import Bitcoin.Misc.HexString

--------------------------------------------------------------------------------

data SHA512_CTX

--typedef struct _SHA512_CTX {
--	uint64_t	state[8];
--	uint64_t	bitcount[2];
--	uint8_t	buffer[SHA512_BLOCK_LENGTH];
--} SHA512_CTX;
--
instance Storable SHA512_CTX where
  alignment _ = 8
  sizeOf _    = 8*8 + 16 + 128
  peek = error "SHA512_CTX/peek: not implemented"
  poke = error "SHA512_CTX/poke: not implemented"

--------------------------------------------------------------------------------

-- void SHA512_Init(SHA512_CTX *);
foreign import ccall safe "sha2.h SHA512_Init" c_SHA512_Init :: Ptr SHA512_CTX -> IO ()

-- void SHA512_Update(SHA512_CTX*, const uint8_t*, size_t);
foreign import ccall safe "sha2.h SHA512_Update" c_SHA512_Update :: Ptr SHA512_CTX -> Ptr Word8 -> CSize -> IO ()

-- void SHA512_Final(uint8_t[SHA512_DIGEST_LENGTH], SHA512_CTX*);
foreign import ccall safe "sha2.h SHA512_Final" c_SHA512_Final :: Ptr Word8 -> Ptr SHA512_CTX -> IO ()

-- char* SHA512_End(SHA512_CTX*, char[SHA512_DIGEST_STRING_LENGTH]);
foreign import ccall safe "sha2.h SHA512_End" c_SHA512_End :: Ptr SHA512_CTX -> Ptr Word8 -> IO (Ptr CChar)

-- char* SHA512_Data(const uint8_t*, size_t, char[SHA512_DIGEST_STRING_LENGTH]);
foreign import ccall safe "sha2.h SHA512_Data" c_SHA512_Data :: Ptr Word8 -> CSize -> Ptr SHA512_CTX -> IO (Ptr CChar)

--------------------------------------------------------------------------------

newtype SHA512 = SHA512 { unSHA512 :: B.ByteString } deriving (Eq,Ord)

instance Show SHA512 where show (SHA512 bs) = "SHA512<" ++ toHexStringChars bs ++ ">"

instance OctetStream SHA512 where
  toByteString = unSHA512
  fromByteString bs = case B.length bs of
    64 -> SHA512 bs
    _  -> error "SHA512/fromByteString: SHA512 is expected to be 64 bytes"

--------------------------------------------------------------------------------

sha512 :: OctetStream a => a -> SHA512
sha512 x = SHA512 $ Unsafe.unsafePerformIO (sha512_IO $ toByteString x)

sha512_IO :: B.ByteString -> IO B.ByteString
sha512_IO msg = do
  alloca $ \ctx -> do
    c_SHA512_Init ctx   
    B.useAsCStringLen msg $ \(cstr,len) -> c_SHA512_Update ctx (castPtr cstr) (fromIntegral len)
    allocaBytes 64 $ \pdigest -> do
      c_SHA512_Final pdigest ctx
      B.packCStringLen (castPtr pdigest,64)

{-
sha512String :: String -> HexStringLE
sha512String msg = hexEncode' False $ B.unpack $ sha512 $ B.pack $ map char_to_word8 msg
-}

--------------------------------------------------------------------------------

