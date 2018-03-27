
-- | SHA1 hash: wrapper around Steve Reid's C implementation.
--
-- (SHA1 is not actively used in Bitcoin, but the script language has a SHA1 opcode)
-- 

{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Bitcoin.Crypto.Hash.SHA1 
  ( SHA1(..)
  , sha1
  , testCases , sha1Test
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

data SHA1_CTX

-- typedef struct {
--    uint32_t state[5];
--    uint32_t count[2];
--    uint8_t  buffer[64];
-- } SHA1_CTX;
--
instance Storable SHA1_CTX where
  alignment _ = 8
  sizeOf _    = 4*5 + 8 + 64 
  peek = error "SHA1_CTX/peek: not implemented"
  poke = error "SHA1_CTX/poke: not implemented"

--------------------------------------------------------------------------------

-- void SHA1_Init  (SHA1_CTX* context);
-- void SHA1_Update(SHA1_CTX* context, const uint8_t* data, const size_t len);
-- void SHA1_Final (SHA1_CTX* context, uint8_t digest[SHA1_DIGEST_SIZE]);

foreign import ccall safe "sha1.h SHA1_Init"   c_SHA1_Init   :: Ptr SHA1_CTX -> IO ()
foreign import ccall safe "sha1.h SHA1_Update" c_SHA1_Update :: Ptr SHA1_CTX -> Ptr Word8 -> CSize -> IO ()
foreign import ccall safe "sha1.h SHA1_Final"  c_SHA1_Final  :: Ptr SHA1_CTX -> Ptr Word8 -> IO ()

--------------------------------------------------------------------------------

newtype SHA1 = SHA1 { unSHA1 :: B.ByteString } deriving (Eq,Ord)

instance Show SHA1 where show (SHA1 bs) = "SHA1<" ++ toHexStringChars bs ++ ">"

instance OctetStream SHA1 where
  toByteString = unSHA1
  fromByteString bs = case B.length bs of
    20 -> SHA1 bs
    _  -> error "SHA1/fromByteString: SHA1 is expected to be 20 bytes"

--------------------------------------------------------------------------------

sha1 :: OctetStream a => a -> SHA1
sha1 octets = SHA1 $ Unsafe.unsafePerformIO (sha1_IO $ toByteString octets)

sha1_IO :: B.ByteString -> IO B.ByteString
sha1_IO msg = do
  alloca $ \ctx -> do
    c_SHA1_Init ctx   
    B.useAsCStringLen msg $ \(cstr,len) -> c_SHA1_Update ctx (castPtr cstr) (fromIntegral len)
    allocaBytes 20 $ \pdigest -> do
      c_SHA1_Final ctx pdigest 
      B.packCStringLen (castPtr pdigest,20)

{-
sha1Hex :: OctetStream a => a -> HexString
sha1Hex = sha1String' False

sha1Hex' :: OctetStream a => Bool -> a -> HexString
sha1Hex' uppercase msg = hexEncode' uppercase $ B.unpack $ sha1 $ B.pack $ map char_to_word8 msg
-}

--------------------------------------------------------------------------------

testCases = map (\(x,y) -> (x,HexString y))
  [ ("abc" , "A9993E364706816ABA3E25717850C26C9CD0D89D")
  , ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" , "84983E441C3BD26EBAAE4AA1F95129E5E54670F1")
  , (replicate 1000000 'a' , "34AA973CD4C4DAA4F61EEB2BDBAD27316534016F")
  ]

-- | Result is a list of failed test cases. Empty list -> OK.
sha1Test :: [String]
sha1Test = concatMap worker list where
  list = zip [1..] testCases
  worker (i,(msg,hexhash)) = result where
    ourhash  = toHexString' True $ sha1 msg
    result = if hexhash==ourhash then [] else ["test case " ++ show i ++ " failed"]

--------------------------------------------------------------------------------

