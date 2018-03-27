
-- | MD5 hash (for completeness)

{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
module Bitcoin.Crypto.Hash.MD5 
  ( MD5(..)
  , md5
  )
  where

--------------------------------------------------------------------------------

import Data.Char (chr,ord)

import Data.Int
import Data.Word

import Control.Monad ( liftM , forM_ )

import qualified Data.ByteString        as B
import qualified Data.ByteString.Unsafe as B
import Data.ByteString (ByteString)

import Foreign
import Foreign.C
import Foreign.Marshal
import Foreign.Storable

import System.IO.Unsafe as Unsafe

import Bitcoin.Misc.OctetStream
import Bitcoin.Misc.HexString

--------------------------------------------------------------------------------

{-
extern void MD5_Init(MD5_CTX *ctx);
extern void MD5_Update(MD5_CTX *ctx, void *data, unsigned long size);
extern void MD5_Final(unsigned char *result, MD5_CTX *ctx);
-}

data MD5CTX = MD5CTX

instance Storable MD5CTX where
  alignment _ = 8 
  sizeOf    _ = 64 + (16+4+2) * sizeOf (undefined :: CUInt)
  peek        = error "MD5CTX/Storable/peek: not implemented"
  poke        = error "MD5CTX/Storable/poke: not implemented"

foreign import ccall safe "md5.h MD5_Init"   c_MD5_Init   :: Ptr MD5CTX -> IO ()
foreign import ccall safe "md5.h MD5_Update" c_MD5_Update :: Ptr MD5CTX -> Ptr CChar -> CULong -> IO ()
foreign import ccall safe "md5.h MD5_Final"  c_MD5_Final  :: Ptr CUChar -> Ptr MD5CTX -> IO ()

withMD5CTX :: (Ptr MD5CTX -> IO ()) -> IO MD5
withMD5CTX action = do
  alloca $ \pctx -> do
    c_MD5_Init pctx
    action pctx
    allocaBytes 16 $ \pres -> do
      c_MD5_Final pres pctx
      bytes <- peekArray 16 (castPtr pres :: Ptr Word8)
      return $ MD5 $ B.pack bytes  

--------------------------------------------------------------------------------

{-
newtype MD5 = MD5 ByteString deriving (Eq)

instance Show MD5 where
  show (MD5 bs) = concatMap showByte (B.unpack bs) where
    showByte b = let k = fromIntegral b :: Int in [ showNibble (shiftR k 4) , showNibble (k .&. 15) ] 
    showNibble n = if n<10 then (chr (n+48)) else chr (n+97-10)
               
stringMD5 :: String -> MD5
stringMD5 s = Unsafe.unsafePerformIO $ do
  withMD5CTX $ \pctx -> do
    withCStringLen s $ \(ptr,len) -> c_MD5_Update pctx ptr (fromIntegral len)

bytestringMD5 :: ByteString -> MD5
bytestringMD5 bs = Unsafe.unsafePerformIO $ do
  withMD5CTX $ \pctx -> do
    B.unsafeUseAsCStringLen bs $ \(ptr,len) -> c_MD5_Update pctx ptr (fromIntegral len)
-}

--------------------------------------------------------------------------------

newtype MD5 = MD5 { unMD5 :: B.ByteString } deriving (Eq,Ord)

instance Show MD5 where show (MD5 bs) = "MD5<" ++ toHexStringChars bs ++ ">"

instance OctetStream MD5 where
  toByteString = unMD5
  fromByteString bs = case B.length bs of
    16 -> MD5 bs
    _  -> error "MD5/fromByteString: MD5 is expected to be 16 bytes"

--------------------------------------------------------------------------------

md5 :: OctetStream a => a -> MD5
md5 x = MD5 $ Unsafe.unsafePerformIO (md5_IO $ toByteString x)

md5_IO :: B.ByteString -> IO ByteString
md5_IO msg = liftM unMD5 $ do
  withMD5CTX $ \pctx -> do
    B.unsafeUseAsCStringLen msg $ \(ptr,len) -> c_MD5_Update pctx ptr (fromIntegral len)

--------------------------------------------------------------------------------

md5_test_vectors :: [(String,String)]
md5_test_vectors = 
  [ ( "The quick brown fox jumps over the lazy dog"   , "9e107d9d372bb6826bd81d3542a419d6" )
  , ( "The quick brown fox jumps over the lazy dog."  , "e4d909c290d0fb1ca068ffaddf22cbd0" )
  , ( ""                                              , "d41d8cd98f00b204e9800998ecf8427e" )
  ]

md5_test = do
  forM_ (md5_test_vectors) $ \(msg,ref) -> do
    print (md5 msg)
    print ref
