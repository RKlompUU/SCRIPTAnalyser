
{-# LANGUAGE CPP #-}
module Bitcoin.Protocol.Hash where

--------------------------------------------------------------------------------

import Data.Word
import Data.Maybe 

import Text.Show
import Text.Read

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI

import Foreign
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import System.IO.Unsafe as Unsafe

import Bitcoin.Misc.BigInt
import Bitcoin.Misc.HexString
import Bitcoin.Misc.OctetStream
import Bitcoin.Misc.Endian

import Bitcoin.Crypto.Hash.SHA256
import Bitcoin.Crypto.Hash.RipEmd160

#ifdef __GLASGOW_HASKELL__
import GHC.ForeignPtr ( mallocPlainForeignPtrBytes )
#endif

import Debug.Trace

--------------------------------------------------------------------------------

{- ForeignPtr version

-- | Hash160(x) = RipEmd160(SHA256(x))
--
-- Note: the Show and Ord instances treat them as big-endian integers!
--
-- We use a ForeignPtr instead of a ByteString as it is 2 (or even 3?) words smaller.
newtype Hash160 = Hash160 { unHash160 :: ForeignPtr Word8 } 

-- | Hash256(x) = SHA256(SHA256(x))
-- 
-- Note: the Show and Ord instances treat them as big-endian integers!
--
-- We use a ForeignPtr instead of a ByteString as it is 2 (or even 3?) words smaller.
newtype Hash256 = Hash256 { unHash256 :: ForeignPtr Word8 } 

-}

--------------------------------------------------------------------------------

-- | Hash160(x) = RipEmd160(SHA256(x))
--
-- Note: the Show and Ord instances treat them as /big-endian/ integers!
--
-- It seems that the most compact representation is unpacked machine words 
-- ('ForeignPtr' has too much overhead even with 'mallocPlainForeignPtr').
--
-- In memory representation: @Hash160 w1 w2 w3@ means that the w1 represent the
-- first 8 bytes, in little-endian representation, then w2 the second 8 bytes,
-- finally w3 the last 4 bytes. This way, the /big-endian/ comparison can be
-- done fast (first compare @w3@, if equal compare @w2@, if that is also equal compare @w1@)
data Hash160 = Hash160 
       {-# UNPACK #-} !Word64
       {-# UNPACK #-} !Word64
       {-# UNPACK #-} !Word32

-- | Hash256(x) = SHA256(SHA256(x))
-- 
-- Note: the Show and Ord instances treat them as /big-endian/ integers!
--
-- It seems that the most compact representation is unpacked machine words 
-- ('ForeignPtr' has too much overhead even with 'mallocPlainForeignPtr').
--
data Hash256 = Hash256
       {-# UNPACK #-} !Word64
       {-# UNPACK #-} !Word64
       {-# UNPACK #-} !Word64
       {-# UNPACK #-} !Word64

--------------------------------------------------------------------------------

{- ForeignPtr 
instance Eq Hash160 where a == b = (toWord8List a) == (toWord8List b)
instance Eq Hash256 where a == b = (toWord8List a) == (toWord8List b)
-}

{- ordering is big-endian!
instance Ord Hash160 where compare a b = compare (reverse $ toWord8List a) (reverse $ toWord8List b)
instance Ord Hash256 where compare a b = compare (reverse $ toWord8List a) (reverse $ toWord8List b)
-}

instance Eq Hash160 where (Hash160 a1 a2 a3   ) == (Hash160 b1 b2 b3   )  =  a1==b1 && a2==b2 && a3==b3
instance Eq Hash256 where (Hash256 a1 a2 a3 a4) == (Hash256 b1 b2 b3 b4)  =  a1==b1 && a2==b2 && a3==b3 && a4==b4

instance Ord Hash160 where 
  compare (Hash160 a1 a2 a3) (Hash160 b1 b2 b3) = compare (a3,a2,a1) (b3,b2,b1)

instance Ord Hash256 where 
  compare (Hash256 a1 a2 a3 a4) (Hash256 b1 b2 b3 b4) = compare (a4,a3,a2,a1) (b4,b3,b2,b1)

--------------------------------------------------------------------------------

instance Show Hash160 where
  showsPrec d h = showParen (d > 10) $
    showString "hash160FromTextBE " . showChar '"' . showString (toHexStringChars (B.reverse $ toByteString h)) . showChar '"'

instance Show Hash256 where
  showsPrec d h = showParen (d > 10) $
    showString "hash256FromTextBE " . showChar '"' . showString (toHexStringChars (B.reverse $ toByteString h)) . showChar '"'

instance Read Hash160 where
  readsPrec d r = readParen (d > 10) 
    (\r -> [ (fromWord8List (reverse $ fromJust mws) , t) 
           | ("hash160FromTextBE",s) <- lex r
           , (m,t) <- readsPrec 11 s
           , length m == 40 
           , let mws = safeHexDecode m
           , isJust mws
           ]) r

instance Read Hash256 where
  readsPrec d r = readParen (d > 10) 
    (\r -> [ (fromWord8List (reverse $ fromJust mws) , t) 
           | ("hash256FromTextBE",s) <- lex r
           , (m,t) <- readsPrec 11 s 
           , length m == 64 
           , let mws = safeHexDecode m
           , isJust mws
           ]) r

--------------------------------------------------------------------------------

partitionList20 :: [Word8] -> ( [Word8] , [Word8] , [Word8] )
partitionList20 ws = (a,b,c) where
  (a,tmp1) = splitAt 8 ws
  (b,c   ) = splitAt 8 tmp1

partitionList32 :: [Word8] -> ( [Word8] , [Word8] , [Word8] , [Word8] )
partitionList32 ws = (a,b,c,d) where
  (a,tmp1) = splitAt 8 ws
  (b,tmp2) = splitAt 8 tmp1
  (c,d   ) = splitAt 8 tmp2
  
--------------------------------------------------------------------------------

instance OctetStream Hash160 where
  toWord8List (Hash160 w1 w2 w3) = toLilEndBytes w1 ++ toLilEndBytes w2 ++ toLilEndBytes w3
 
  fromWord8List ws = case length ws of
    20 -> Hash160 w1 w2 w3 where
            w1 = fromLilEndBytes xs1
            w2 = fromLilEndBytes xs2
            w3 = fromLilEndBytes xs3
            (xs1,xs2,xs3) = partitionList20 ws
    _ -> error "Hash160/fromWord8List: Hash160 is expected to be 20 bytes"

  toByteString (Hash160 w1 w2 w3) = do
          Unsafe.unsafePerformIO $ BI.create 20 $ \ptr -> do
            poke (castPtr  ptr               :: Ptr Word64) $ swapByteOrderToLE w1 
            poke (castPtr (ptr `plusPtr`  8) :: Ptr Word64) $ swapByteOrderToLE w2 
            poke (castPtr (ptr `plusPtr` 16) :: Ptr Word32) $ swapByteOrderToLE w3
                                    
  fromByteString bs = case B.length bs of
    20 -> Unsafe.unsafePerformIO $ B.useAsCString bs $ \src -> do
            w1 <- peek (castPtr  src               :: Ptr Word64)
            w2 <- peek (castPtr (src `plusPtr`  8) :: Ptr Word64)
            w3 <- peek (castPtr (src `plusPtr` 16) :: Ptr Word32)
            return $ Hash160 (swapByteOrderToLE w1) 
                             (swapByteOrderToLE w2) 
                             (swapByteOrderToLE w3)
    _  -> error "Hash160/fromByteString: Hash160 is expected to be 20 bytes"

  fromIntegerLE = fromWord8List . littleEndianInteger20
  fromIntegerBE = fromWord8List . bigEndianInteger20

----------------------------------------

instance OctetStream Hash256 where
  toWord8List (Hash256 w1 w2 w3 w4) = toLilEndBytes w1 ++ toLilEndBytes w2 ++ toLilEndBytes w3 ++ toLilEndBytes w4
 
  fromWord8List ws = case length ws of
    32 -> Hash256 w1 w2 w3 w4 where
            w1 = fromLilEndBytes xs1
            w2 = fromLilEndBytes xs2
            w3 = fromLilEndBytes xs3
            w4 = fromLilEndBytes xs4
            (xs1,xs2,xs3,xs4) = partitionList32 ws
    _ -> error "Hash256/fromWord8List: Hash256 is expected to be 32 bytes"

  toByteString (Hash256 w1 w2 w3 w4) = do
          Unsafe.unsafePerformIO $ BI.create 32 $ \ptr -> do
            poke (castPtr  ptr               :: Ptr Word64) $ swapByteOrderToLE w1 
            poke (castPtr (ptr `plusPtr`  8) :: Ptr Word64) $ swapByteOrderToLE w2 
            poke (castPtr (ptr `plusPtr` 16) :: Ptr Word64) $ swapByteOrderToLE w3
            poke (castPtr (ptr `plusPtr` 24) :: Ptr Word64) $ swapByteOrderToLE w4
                                    
  fromByteString bs = case B.length bs of
    32 -> Unsafe.unsafePerformIO $ B.useAsCString bs $ \src -> do
            w1 <- peek (castPtr  src               :: Ptr Word64)
            w2 <- peek (castPtr (src `plusPtr`  8) :: Ptr Word64)
            w3 <- peek (castPtr (src `plusPtr` 16) :: Ptr Word64)
            w4 <- peek (castPtr (src `plusPtr` 24) :: Ptr Word64)
            return $ Hash256 (swapByteOrderToLE w1) 
                             (swapByteOrderToLE w2)
                             (swapByteOrderToLE w3) 
                             (swapByteOrderToLE w4)
    _  -> error "Hash256/fromByteString: Hash256 is expected to be 32 bytes"

  fromIntegerLE = fromWord8List . littleEndianInteger32
  fromIntegerBE = fromWord8List . bigEndianInteger32

--------------------------------------------------------------------------------

{- ForeignPtr version

instance OctetStream Hash160 where
  toWord8List (Hash160 fptr) = Unsafe.unsafePerformIO $ withForeignPtr fptr $ \ptr -> peekArray 20 ptr 
  fromWord8List ws = case length ws of
    20 -> Unsafe.unsafePerformIO $ do 
#ifdef __GLASGOW_HASKELL__
            fptr <- mallocPlainForeignPtrBytes 20 
#else
            fptr <- mallocForeignPtrBytes 20 
#endif
            withForeignPtr fptr $ \ptr -> pokeArray ptr ws 
            return (Hash160 fptr) 
    _ -> error "Hash160/fromWord8List: Hash160 is expected to by 20 bytes"

  toByteString (Hash160 fptr) = Unsafe.unsafePerformIO $ withForeignPtr fptr $ \ptr -> B.packCStringLen (castPtr ptr, 20)
  fromByteString bs = case B.length bs of
    20 -> Unsafe.unsafePerformIO $ B.useAsCString bs $ \src -> do
#ifdef __GLASGOW_HASKELL__
            fptr <- mallocPlainForeignPtrBytes 20 
#else
            fptr <- mallocForeignPtrBytes 20 
#endif
            withForeignPtr fptr $ \tgt -> copyBytes tgt (castPtr src) 20
            return (Hash160 fptr) 
    _  -> error "Hash160/fromByteString: Hash160 is expected to by 20 bytes"

  fromIntegerLE = fromWord8List . littleEndianInteger20
  fromIntegerBE = fromWord8List . bigEndianInteger20

----------------------------------------

instance OctetStream Hash256 where
  toWord8List (Hash256 fptr) = Unsafe.unsafePerformIO $ withForeignPtr fptr $ \ptr -> peekArray 32 ptr 
  fromWord8List ws = case length ws of
    32 -> Unsafe.unsafePerformIO $ do 
#ifdef __GLASGOW_HASKELL__
            fptr <- mallocPlainForeignPtrBytes 32
#else
            fptr <- mallocForeignPtrBytes 32
#endif
            withForeignPtr fptr $ \ptr -> pokeArray ptr ws 
            return (Hash256 fptr) 
    _ -> error "Hash256/fromWord8List: Hash256 is expected to by 32 bytes"

  toByteString (Hash256 fptr) = Unsafe.unsafePerformIO $ withForeignPtr fptr $ \ptr -> B.packCStringLen (castPtr ptr, 32)
  fromByteString bs = case B.length bs of
    32 -> Unsafe.unsafePerformIO $ B.useAsCString bs $ \src -> do
#ifdef __GLASGOW_HASKELL__
            fptr <- mallocPlainForeignPtrBytes 32
#else
            fptr <- mallocForeignPtrBytes 32
#endif
            withForeignPtr fptr $ \tgt -> copyBytes tgt (castPtr src) 32
            return (Hash256 fptr) 

  fromIntegerLE = fromWord8List . littleEndianInteger32
  fromIntegerBE = fromWord8List . bigEndianInteger32

-}


--------------------------------------------------------------------------------

debugDoHash256 :: OctetStream a => a -> Hash256
debugDoHash256 x = Debug.Trace.trace (">>> " ++ toHexStringChars x ++ " <<<") $ doHash256 x

-- | SHA256(SHA256(x))
doHash256 :: OctetStream a => a -> Hash256
doHash256 = fromByteString . unSHA256 . sha256 . sha256 

-- | RIPEMD160(SHA256(x))
doHash160 :: OctetStream a => a -> Hash160
doHash160 = fromByteString . unRipEmd160 . ripemd160 . sha256 

--------------------------------------------------------------------------------

-- | The zero ripemd hash (not really used for anything)
zeroHash160 :: Hash160
zeroHash160 = fromWord8List (replicate 20 0)

-- | Generation (or \"coinbase\") transactions have hash set to zero
zeroHash256 :: Hash256
zeroHash256 = fromWord8List (replicate 32 0)

--------------------------------------------------------------------------------

-- | Creates a 256 bit hash from a /big-endian/ hex string (may fail with exception).
-- This is here primarily for convenience.
hash256FromTextBE :: String -> Hash256
hash256FromTextBE s 
  | length s == 64  = case safeHexDecode s of
                        Just ws -> fromWord8List $ reverse ws
                        Nothing -> error "hash256FromText: not a hex string"
  | otherwise       = error "hash256FromTextBE: length should be 64 characters"

-- | Creates a 160 bit hash from a /big-endian/ hex string (may fail with exception).
-- This is here primarily for convenience.
hash160FromTextBE :: String -> Hash160
hash160FromTextBE s 
  | length s == 40  = case safeHexDecode s of
                        Just ws -> fromWord8List $ reverse ws
                        Nothing -> error "hash160FromText: not a hex string"
  | otherwise       = error "hash160FromTextBE: length should be 40 characters"

--------------------------------------------------------------------------------
