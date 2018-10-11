
-- | Running the script test
module Bitcoin.Test.Script.RunTests where

--------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.HUnit

import Data.Word
import Data.Char ( isDigit )
import qualified Data.ByteString as B

import Bitcoin.Script.Base
import Bitcoin.Script.Run
import Bitcoin.Script.Serialize

import Bitcoin.Misc.HexString
import Bitcoin.Misc.OctetStream

import Bitcoin.Protocol.Hash
import Bitcoin.BlockChain.Base

import Bitcoin.Test.Script.Valid
import Bitcoin.Test.Script.Invalid
import Bitcoin.Test.Script.Parser

--------------------------------------------------------------------------------

testgroup_Script :: TestTree
testgroup_Script = testGroup "Script" 
  [ testgroup_Valid
  , testgroup_Invalid
  ]

testgroup_Valid :: TestTree
testgroup_Valid = testGroup "valid scripts" 
  [ testCase ("valid script #" ++ show i) (assertOK $ runTestCase scr) 
  | (i,scr) <- zip [1..] valid_testcases
  ]

testgroup_Invalid :: TestTree
testgroup_Invalid = testGroup "invalid scripts"
  [ testCase ("invalid script #" ++ show i) (assertNotOK $ runTestCase scr) 
  | (i,scr) <- zip [1..] invalid_testcases
  ]

assertOK :: (Bool,String) -> Assertion
assertOK (b,msg) = case b of
  False -> assertFailure msg
  True  -> return ()

assertNotOK :: (Bool,String) -> Assertion
assertNotOK (b,msg) = assertOK (not b, msg)

--------------------------------------------------------------------------------

-- | test cases which should pass
valid_testcases :: [TestCase]
valid_testcases   = map parseInner valid_json   

-- | test cases which should not pass
invalid_testcases :: [TestCase]
invalid_testcases = map parseInner invalid_json 

--------------------------------------------------------------------------------

-- | True if passed, False if failed, 
-- and possibly an explanation if failed (including the testcase comment if exists)
runTestCase :: TestCase -> (Bool, String)
runTestCase (TestCase eiSig eiPk mbcomment) = result where

  sig = eiToRawScript eiSig :: RawScript
  pk  = eiToRawScript eiPk  :: RawScript

  prevTx = Tx 0 [] [TxOutput 666 pk] LockImmed zeroHash256        :: Tx RawScript RawScript
  inExt  = TxInput zeroHash256 0 (prevTx,sig) 0                   :: TxInput (Tx RawScript RawScript, RawScript)
  fakeTx = Tx 0 [inExt] [TxOutput 666 sig] LockImmed zeroHash256  :: Tx (Tx RawScript RawScript, RawScript) RawScript

  result = case checkTransaction fakeTx of
    Left err -> (False , err ++ " | " ++ maybe "" id mbcomment)
    Right b  -> (b     ,        " | " ++ maybe "" id mbcomment) 

--------------------------------------------------------------------------------

{-
runValid   = mapM_ print $ map runTestCase valid_testcases
runInvalid = mapM_ print $ map runTestCase invalid_testcases

--------------------------------------------------------------------------------

-- | for testing the hacked-together parser for Gavin's custom test format...
-- and also manually checking the cases
runAndSaveTestCases = do
  writeFile "_valid.txt"       $ unlines $ map show valid_testcases
  writeFile "_invalid.txt"     $ unlines $ map show invalid_testcases
  writeFile "_valid_run.txt"   $ unlines $ map show $ map runTestCase valid_testcases
  writeFile "_invalid_run.txt" $ unlines $ map show $ map runTestCase invalid_testcases
-}

--------------------------------------------------------------------------------
