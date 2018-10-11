
-- | Some tests for transaction validation

module Bitcoin.Test.TxCheck where

--------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.HUnit

import Bitcoin.BlockChain.Tx
import Bitcoin.Protocol

import Bitcoin.Script.Run

import Bitcoin.Test.TxCheck.TestNet3
import Bitcoin.Test.TxCheck.MainNet

import Bitcoin.Misc.HexString

--------------------------------------------------------------------------------

testgroup_TxCheck :: TestTree
testgroup_TxCheck = testGroup "transaction checks"
  [ testgroup_testnet3
  , testgroup_mainnet
  ]

testgroup_testnet3 :: TestTree
testgroup_testnet3 = testGroup "testnet3"
  [ testCase ("testnet3 tx #" ++ show i ++ " | hash = " ++ show (_txHash tx)) (runSingle tx) 
  | (i,tx) <- zip [1..] some_testnet3_txs
  ]

testgroup_mainnet :: TestTree
testgroup_mainnet = testGroup "mainnet"
  [ testCase ("mainnet tx #" ++ show i ++ " | hash = " ++ show (_txHash tx)) (runSingle tx) 
  | (i,tx) <- zip [1..] some_mainnet_txs
  ]

--------------------------------------------------------------------------------

runSingle :: Tx (Tx RawScript RawScript, RawScript) RawScript -> Assertion
runSingle txExt = do
  let ei = checkTransaction txExt
  case ei of
    Left err -> assertFailure err
    Right b  -> case b of
      True  -> return ()
      False -> do
        let msg = "tx " ++ unHexString (toHexString (_txHash txExt))
        assertFailure ("failed: " ++ msg)

--------------------------------------------------------------------------------

{-
runTxCheckTests :: IO ()
runTxCheckTests = 
  do
    putStrLn "\nsome testnet transactions:"
    mapM_ runSingle some_testnet3_txs
    putStrLn "\nsome mainnet transactions:"
    mapM_ runSingle some_mainnet_txs
  where 
    runSingle txExt = do
      let ei = checkTransaction txExt
          result = case ei of
            Left err -> err
            Right b  -> case b of
              True  -> "ok"
              False -> "failed"
      putStrLn $ "tx " ++ unHexString (toHexString (_txHash txExt)) ++ " - " ++ result
-}

--------------------------------------------------------------------------------

