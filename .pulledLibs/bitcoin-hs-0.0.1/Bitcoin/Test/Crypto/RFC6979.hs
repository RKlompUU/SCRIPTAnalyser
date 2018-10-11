
-- | Test vectors for the RFC 6979 based deterministic DSA

module Bitcoin.Test.Crypto.RFC6979 where

--------------------------------------------------------------------------------

import Data.Word

import Bitcoin.Crypto.EC.DSA  -- hiding ( signMessageHashRFC6979 )
import Bitcoin.Protocol
import Bitcoin.Misc
import Bitcoin.Crypto.Hash.SHA256

import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------

testgroup_RFC6979 :: TestTree
testgroup_RFC6979 = testGroup "RFC 6979 (deterministic DSA)" 
  [ testgroup_Haskoin
  , testgroup_FPGAMiner
  ]

testgroup_Haskoin :: TestTree
testgroup_Haskoin = testGroup "Haskoin test vectors" 
  [ testCase ("haskoin test vector #" ++ show i) (assertRight $ haskoin_test vec) 
  | (i,vec) <- zip [1..] haskoin_test_vectors 
  ]

testgroup_FPGAMiner :: TestTree
testgroup_FPGAMiner = testGroup "FPGAMiner test vectors"
  [ testCase ("fpgaminer test vector #" ++ show i) (assertRight $ fpgaminer_test vec) 
  | (i,vec) <- zip [1..] fpgaminer_test_vectors 
  ]

assertRight :: Either String a -> Assertion
assertRight ei = case ei of
  Left err -> assertFailure err
  Right {} -> return ()

--------------------------------------------------------------------------------

{-
runTestRFC6979 :: IO ()
runTestRFC6979 = do
  putStrLn "haskoin test vectors:"
  mapM_ print $ map haskoin_test haskoin_test_vectors
  putStrLn "fpgaminer test vectors:"
  mapM_ print $ map fpgaminer_test fpgaminer_test_vectors
-}

--------------------------------------------------------------------------------

{-  the (old - before projective) implementation (but the differences are very minor)

import Bitcoin.Crypto.Hash.HMAC
import Bitcoin.Crypto.EC.Base
import Bitcoin.Crypto.FiniteField.Fast.Fp
import Bitcoin.Crypto.FiniteField.Naive.Fn hiding ( secp256k1_n )

-- | Deterministic signature as specified by RFC 6979: 
-- <http://tools.ietf.org/html/rfc6979>, in particular section 3.2
--
signMessageHashRFC6979 :: PrivKey -> Hash256 -> (SignBits,Signature)
signMessageHashRFC6979 (PrivKey da) hash = result where

  hmac_k :: OctetStream a => a -> [Word8] -> [Word8]
  hmac_k key = toWord8List . unHMAC . hmacSha256 (hmacKeyFromString64 key)

  z    = mod (hashInteger hash) secp256k1_n           -- for z, it doesn't matter (it will be in Fn anyway), but for h1 it matters :(

  x1   = bigEndianInteger32 da    :: [Word8]          -- x = private key here
  h1   = bigEndianInteger32 z     :: [Word8]          -- step a

  v0   = replicate 32 0x01 :: [Word8]                 -- step b
  k0   = replicate 32 0x00 :: [Word8]                 -- step c
  k1   = hmac_k k0 $ v0 ++ [0x00] ++ x1 ++ h1         -- step d
  v1   = hmac_k k1 $ v0                               -- step e
  k2   = hmac_k k1 $ v1 ++ [0x01] ++ x1 ++ h1         -- step f
  v2   = hmac_k k2 $ v1                               -- step g
  
  result = step_h k2 v2

  halfn = div secp256k1_n 2

  step_h k v0 =                                       -- final step (step h)

    if dsa_k > 0 && dsa_k < secp256k1_n && ep /= ECInfinity && r/=0 && s/=0
      then (signbits,signature)
      else step_h k' v'

    where

      v = hmac_k k v0          -- step h/1 
      t = v                    -- step h/2 (always a single step in our case?)
      dsa_k = toIntegerBE t

      k' = hmac_k k  $ v ++ [0x00]
      v' = hmac_k k' $ v

      ep = mulEC secp256k1_G dsa_k
      ECPoint x y = ep
      r  = fromInteger (fromFp x)                                     :: Fn
      s0 = (fromInteger z + r * fromInteger da) / (fromInteger dsa_k) :: Fn

      -- hmm, it seems this extra rule is used for some reason ??
      -- quote: "The theory behind this is: if you negate K you get the same R and the negated S. 
      --         Hence you need to negate S as a post-processing step, i.e., S' = prime - S in both cases"
      s = if (fromFn s0) > halfn then Fn (secp256k1_n - fromFn s0) else s0   

      odd_y  = if even (fromFp y)      then 0 else 1
      add_n  = if fromFn r == fromFp x then 0 else 2
      w8 = odd_y + add_n 
  
      signbits  = SignBits w8
      signature = Signature (fromFn r) (fromFn s)

-}

--------------------------------------------------------------------------------
-- * Haskoin test vectors for RFC 6979 ECDSA (secp256k1, SHA-256)

-- | As opposed to the double hash used in Bitcoin
singleHash256 :: OctetStream a => a -> Hash256
singleHash256 = fromByteString . unSHA256 . sha256

-- | NOTE: because of the WIF encoding, this won't work when compileg with the testnet flag
--
haskoin_test :: (String,String,String,String,String) -> Either String ()
haskoin_test (privstr, wifstr, msg, rsstr, dersig) = result where
  Just (pkfmt,privkey1) 
                = privKeyWIFDecode $ WIF wifstr
  privkey2      = PrivKey 
                $ toIntegerBE        ( fromHexString $ HexString privstr :: [Word8] )
  Just (SignatureExt signat sighash)
                = decodeSignatureDER ( (fromHexString $ HexString dersig) ++ [0x01] :: [Word8] )
  msghash       = singleHash256 msg   -- doHash256 msg
  rs            = fromHexString ( HexString rsstr ) :: [Word8]
  r             = toIntegerBE (take 32 rs)
  s             = toIntegerBE (drop 32 rs)

  (mysignbits,mysignat) = signMessageHashRFC6979 privkey1 msghash
  
  result 
    | privkey1 /= privkey2  = Left "rfc6979/haskoin_test: privkey decoding failed"
    | signat   /= mysignat  = Left $ unlines $ 
        [ "signature mismatch:"
        , "  theirs: " ++ show signat 
        , "  ours:   " ++ show mysignat
        , "  (r,s):  " ++ show (r,s)
        ]
    | otherwise = Right ()
    
-- | Haskoin test vectors for RFC 6979 ECDSA (secp256k1, SHA-256)
-- (PrvKey HEX, PrvKey WIF, message, R || S as HEX, sig as DER)
--
-- from: <https://bitcointalk.org/index.php?topic=285142.msg3300992#msg3300992>
haskoin_test_vectors :: [(String,String,String,String,String)]
haskoin_test_vectors = 
  [ ( "0000000000000000000000000000000000000000000000000000000000000001"
    , "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgd9M7rFU73sVHnoWn"
    , "Everything should be made as simple as possible, but not simpler."
    , "33a69cd2065432a30f3d1ce4eb0d59b8ab58c74f27c41a7fdb5696ad4e6108c96f807982866f785d3f6418d24163ddae117b7db4d5fdf0071de069fa54342262"
    , "3044022033a69cd2065432a30f3d1ce4eb0d59b8ab58c74f27c41a7fdb5696ad4e6108c902206f807982866f785d3f6418d24163ddae117b7db4d5fdf0071de069fa54342262"
    )
    
  , ( "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364140"
    , "L5oLkpV3aqBjhki6LmvChTCV6odsp4SXM6FfU2Gppt5kFLaHLuZ9"
    , "Equations are more important to me, because politics is for the present, but an equation is something for eternity."
    , "54c4a33c6423d689378f160a7ff8b61330444abb58fb470f96ea16d99d4a2fed07082304410efa6b2943111b6a4e0aaa7b7db55a07e9861d1fb3cb1f421044a5"
    , "3044022054c4a33c6423d689378f160a7ff8b61330444abb58fb470f96ea16d99d4a2fed022007082304410efa6b2943111b6a4e0aaa7b7db55a07e9861d1fb3cb1f421044a5"
    )
    
  , ( "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364140"
    , "L5oLkpV3aqBjhki6LmvChTCV6odsp4SXM6FfU2Gppt5kFLaHLuZ9"
    , "Not only is the Universe stranger than we think, it is stranger than we can think."
    , "ff466a9f1b7b273e2f4c3ffe032eb2e814121ed18ef84665d0f515360dab3dd06fc95f5132e5ecfdc8e5e6e616cc77151455d46ed48f5589b7db7771a332b283"
    , "3045022100ff466a9f1b7b273e2f4c3ffe032eb2e814121ed18ef84665d0f515360dab3dd002206fc95f5132e5ecfdc8e5e6e616cc77151455d46ed48f5589b7db7771a332b283"
    )
    
  , ( "0000000000000000000000000000000000000000000000000000000000000001"
    , "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgd9M7rFU73sVHnoWn"
    , "How wonderful that we have met with a paradox. Now we have some hope of making progress."
    , "c0dafec8251f1d5010289d210232220b03202cba34ec11fec58b3e93a85b91d375afdc06b7d6322a590955bf264e7aaa155847f614d80078a90292fe205064d3"
    , "3045022100c0dafec8251f1d5010289d210232220b03202cba34ec11fec58b3e93a85b91d3022075afdc06b7d6322a590955bf264e7aaa155847f614d80078a90292fe205064d3"
    )
    
  , ( "69ec59eaa1f4f2e36b639716b7c30ca86d9a5375c7b38d8918bd9c0ebc80ba64"
    , "KzmcSTRmg8Gtoq8jbBCwsrvgiTKRrewQXniAHHTf7hsten8MZmBB"
    , "Computer science is no more about computers than astronomy is about telescopes."
    , "7186363571d65e084e7f02b0b77c3ec44fb1b257dee26274c38c928986fea45d0de0b38e06807e46bda1f1e293f4f6323e854c86d58abdd00c46c16441085df6"
    , "304402207186363571d65e084e7f02b0b77c3ec44fb1b257dee26274c38c928986fea45d02200de0b38e06807e46bda1f1e293f4f6323e854c86d58abdd00c46c16441085df6"
    )
    
  , ( "00000000000000000000000000007246174ab1e92e9149c6e446fe194d072637"
    , "KwDiBf89QgGbjEhKnhXJwe1E2mCa8asowBrSKuCaBV6EsPYEAFZ8"
    , "...if you aren't, at any given time, scandalized by code you wrote five or even three years ago, you're not learning anywhere near enough"
    , "fbfe5076a15860ba8ed00e75e9bd22e05d230f02a936b653eb55b61c99dda4870e68880ebb0050fe4312b1b1eb0899e1b82da89baa5b895f612619edf34cbd37"
    , "3045022100fbfe5076a15860ba8ed00e75e9bd22e05d230f02a936b653eb55b61c99dda48702200e68880ebb0050fe4312b1b1eb0899e1b82da89baa5b895f612619edf34cbd37"
    )
    
  , ( "000000000000000000000000000000000000000000056916d0f9b31dc9b637f3"
    , "KwDiBf89QgGbjEhKnhXJuH7LrciVrZiib5S9h4knkymNojPUVsWN"
    , "The question of whether computers can think is like the question of whether submarines can swim."
    , "cde1302d83f8dd835d89aef803c74a119f561fbaef3eb9129e45f30de86abbf906ce643f5049ee1f27890467b77a6a8e11ec4661cc38cd8badf90115fbd03cef"
    , "3045022100cde1302d83f8dd835d89aef803c74a119f561fbaef3eb9129e45f30de86abbf9022006ce643f5049ee1f27890467b77a6a8e11ec4661cc38cd8badf90115fbd03cef"
    )
  ]
  
--------------------------------------------------------------------------------
-- * fpgaminer test vectors 

fpgaminer_test :: (Integer,String,Integer,String) -> Either String ()
fpgaminer_test (privnum, msg, dsa_k, rsstr) = result where
  privkey       = PrivKey privnum
  msghash       = singleHash256 msg   -- doHash256 msg

  rs = fromHexString $ HexString rsstr  :: [Word8]
  r = toIntegerBE $ take 32 rs
  s = toIntegerBE $ drop 32 rs
  signat = Signature r s 

  (mysignbits,mysignat) = signMessageHashRFC6979 privkey msghash
  
  result 
    | signat   /= mysignat  = Left $ unlines $ 
        [ "signature mismatch:"
        , "  theirs: " ++ show signat 
        , "  ours:   " ++ show mysignat
        , "  k:      " ++ show dsa_k
        ]
    | otherwise = Right ()


-- | Test Vectors for RFC 6979 ECDSA, secp256k1, SHA-256
-- (private key, message, expected k, expected signature)
--
-- from <https://bitcointalk.org/index.php?topic=285142.msg3299061#msg3299061>
fpgaminer_test_vectors :: [(Integer,String,Integer,String)]
fpgaminer_test_vectors = 
  [ (0x1, "Satoshi Nakamoto", 0x8F8A276C19F4149656B280621E358CCE24F5F52542772691EE69063B74F15D15, "934b1ea10a4b3c1757e2b0c017d0b6143ce3c9a7e6a4a49860d7a6ab210ee3d82442ce9d2b916064108014783e923ec36b49743e2ffa1c4496f01a512aafd9e5")
  , (0x1, "All those moments will be lost in time, like tears in rain. Time to die...", 0x38AA22D72376B4DBC472E06C3BA403EE0A394DA63FC58D88686C611ABA98D6B3, "8600dbd41e348fe5c9465ab92d23e3db8b98b873beecd930736488696438cb6b547fe64427496db33bf66019dacbf0039c04199abb0122918601db38a72cfc21")
  , (0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364140, "Satoshi Nakamoto", 0x33A19B60E25FB6F4435AF53A3D42D493644827367E6453928554F43E49AA6F90, "fd567d121db66e382991534ada77a6bd3106f0a1098c231e47993447cd6af2d06b39cd0eb1bc8603e159ef5c20a5c8ad685a45b06ce9bebed3f153d10d93bed5")
  , (0xf8b8af8ce3c7cca5e300d33939540c10d45ce001b8f252bfbc57ba0342904181, "Alan Turing", 0x525A82B70E67874398067543FD84C83D30C175FDC45FDEEE082FE13B1D7CFDF1, "7063ae83e7f62bbb171798131b4a0564b956930092b33b07b395615d9ec7e15c58dfcc1e00a35e1572f366ffe34ba0fc47db1e7189759b9fb233c5b05ab388ea")
  , (0xe91671c46231f833a6406ccbea0e3e392c76c167bac1cb013f6f1013980455c2, "There is a computer disease that anybody who works with computers knows about. It's a very serious disease and it interferes completely with the work. The trouble with computers is that you 'play' with them!", 0x1F4B84C23A86A221D233F2521BE018D9318639D5B8BBD6374A8A59232D16AD3D, "b552edd27580141f3b2a5463048cb7cd3e047b97c9f98076c32dbdf85a68718b279fa72dd19bfae05577e06c7c0c1900c371fcd5893f7e1d56a37d30174671f6")
  ]
  
--------------------------------------------------------------------------------

