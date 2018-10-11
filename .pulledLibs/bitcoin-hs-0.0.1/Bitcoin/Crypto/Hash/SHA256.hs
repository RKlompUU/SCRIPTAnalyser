
-- | SHA256 hash: wrapper around Aaron D. Gifford's C implementation.
-- 

{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Bitcoin.Crypto.Hash.SHA256 
  ( SHA256(..)
  , sha256
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

data SHA256_CTX

-- typedef struct _SHA256_CTX {
--  uint32_t state[8];
--  uint64_t sbitcount;
--  uint8_t  buffer[SHA256_BLOCK_LENGTH];
-- } SHA256_CTX;
--
instance Storable SHA256_CTX where
  alignment _ = 8
  sizeOf _    = 4*8 + 8 + 64 
  peek = error "SHA256_CTX/peek: not implemented"
  poke = error "SHA256_CTX/poke: not implemented"

--------------------------------------------------------------------------------

-- void SHA256_Init(SHA256_CTX *);
foreign import ccall safe "sha2.h SHA256_Init" c_SHA256_Init :: Ptr SHA256_CTX -> IO ()

-- void SHA256_Update(SHA256_CTX*, const uint8_t*, size_t);
foreign import ccall safe "sha2.h SHA256_Update" c_SHA256_Update :: Ptr SHA256_CTX -> Ptr Word8 -> CSize -> IO ()

-- void SHA256_Final(uint8_t[SHA256_DIGEST_LENGTH], SHA256_CTX*);
foreign import ccall safe "sha2.h SHA256_Final" c_SHA256_Final :: Ptr Word8 -> Ptr SHA256_CTX -> IO ()

-- char* SHA256_End(SHA256_CTX*, char[SHA256_DIGEST_STRING_LENGTH]);
foreign import ccall safe "sha2.h SHA256_End" c_SHA256_End :: Ptr SHA256_CTX -> Ptr Word8 -> IO (Ptr CChar)

-- char* SHA256_Data(const uint8_t*, size_t, char[SHA256_DIGEST_STRING_LENGTH]);
foreign import ccall safe "sha2.h SHA256_Data" c_SHA256_Data :: Ptr Word8 -> CSize -> Ptr SHA256_CTX -> IO (Ptr CChar)

--------------------------------------------------------------------------------

newtype SHA256 = SHA256 { unSHA256 :: B.ByteString } deriving (Eq,Ord)

instance Show SHA256 where show (SHA256 bs) = "SHA256<" ++ toHexStringChars bs ++ ">"

instance OctetStream SHA256 where
  toByteString = unSHA256
  fromByteString bs = case B.length bs of
    32 -> SHA256 bs
    _  -> error "SHA256/fromByteString: SHA256 is expected to be 32 bytes"

--------------------------------------------------------------------------------

sha256 :: OctetStream a => a -> SHA256
sha256 x = SHA256 $ Unsafe.unsafePerformIO (sha256_IO $ toByteString x)

sha256_IO :: B.ByteString -> IO B.ByteString
sha256_IO msg = do
  alloca $ \ctx -> do
    c_SHA256_Init ctx   
    B.useAsCStringLen msg $ \(cstr,len) -> c_SHA256_Update ctx (castPtr cstr) (fromIntegral len)
    allocaBytes 32 $ \pdigest -> do
      c_SHA256_Final pdigest ctx
      B.packCStringLen (castPtr pdigest,32)

{-
sha256String :: String -> HexStringLE
sha256String msg = hexEncode' False $ B.unpack $ sha256 $ B.pack $ map char_to_word8 msg
-}

--------------------------------------------------------------------------------

