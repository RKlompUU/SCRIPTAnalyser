
-- | Test-suite using Tasty.

module Main where

--------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck
  
import Bitcoin.Test.Crypto.Word256              ( testgroup_Word256      )
import Bitcoin.Test.Crypto.FiniteField.NaiveFp  ( testgroup_NaiveFp      )
import Bitcoin.Test.Crypto.FiniteField.FastFp   ( testgroup_FastFp       )
import Bitcoin.Test.Crypto.Curve                ( testgroup_Curve        )
import Bitcoin.Test.Crypto.Projective           ( testgroup_Projective   )
import Bitcoin.Test.Crypto.Key                  ( testgroup_Key          )
import Bitcoin.Test.Crypto.RFC6979              ( testgroup_RFC6979      )

import Bitcoin.Test.Protocol.Hash               ( testgroup_ProtocolHash )
import Bitcoin.Test.Script.RunTests             ( testgroup_Script       )
import Bitcoin.Test.TxCheck                     ( testgroup_TxCheck      )

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

setN :: Int -> TestTree -> TestTree
setN n = localOption (QuickCheckTests n)

tests :: TestTree
tests 
  = setN 1000
  $ testGroup "all tests"
      [ testGroup "crypto" 
          [ testgroup_Word256
          , testgroup_NaiveFp
          , testgroup_FastFp
          , setN 250 testgroup_Curve
          , setN 250 testgroup_Projective
          , testgroup_Key
          , testgroup_RFC6979
          ]
      , testGroup "protocol" 
          [ testgroup_ProtocolHash
          ]
      , testgroup_Script
      , testgroup_TxCheck 
      ]

--------------------------------------------------------------------------------
