
-- | Parse the script test cases from the Satoshi client (\"script_valid.json\" and \"script_invalid.json\")
--
-- Note that these do not test CHECKSIG / CHECKMULTISIG!
--
-- Fortunately the JSON readily parses as a Haskell type @[[String]]@ (inner lists are scriptSig, scriptPubKey
-- and an optional comment)
--

{-# LANGUAGE PatternGuards #-}
module Bitcoin.Test.Script.Parser where

--------------------------------------------------------------------------------

import Data.Word
import Data.Char ( isDigit )
import qualified Data.ByteString as B

import Bitcoin.Script.Base
import Bitcoin.Script.Run
import Bitcoin.Script.Serialize

import Bitcoin.Misc.HexString
import Bitcoin.Misc.OctetStream

--------------------------------------------------------------------------------

data TestCase = TestCase 
  { _testScriptSig    :: Either [Word8] Script
  , _testScriptPubKey :: Either [Word8] Script
  , _testComment      :: Maybe String
  }
  deriving Show

--------------------------------------------------------------------------------

eiToRawScript :: Either [Word8] Script -> RawScript
eiToRawScript (Left ws) = fromWord8List ws
eiToRawScript (Right s) = serializeScript s

--------------------------------------------------------------------------------

parseInner :: [String] -> TestCase
parseInner ls = case ls of
  [a,b  ] -> TestCase (parseWords a) (parseWords b) Nothing
  [a,b,c] -> TestCase (parseWords a) (parseWords b) (Just c)
  _ -> error "Bitcoin.Test.Script.Parser.parseInner"

catEither :: [Either [a] [b]] -> [Either [a] [b]]
catEither = go where
  go []  = []
  go (Left  xs : Left  ys : rest) = go (Left  (xs++ys) : rest)
  go (Right xs : Right ys : rest) = go (Right (xs++ys) : rest)
  go (ei : rest) = ei : go rest

parseWords :: String -> Either [Word8] Script
parseWords input = 
  case catEither eis of
    [Right ops] -> Right $ Script ops
    _           -> let ws = concatMap toW8 eis
                   in  case parseScript $ fromWord8List ws of
                         Just script -> Right script       -- it can happen that individually they don't parse, but together they parse 
                         Nothing     -> Left  ws           -- (because of smallnums, for example)
  where

  toW8 :: Either [Word8] [Opcode] -> [Word8]
  toW8 (Left ws) = ws
  toW8 (Right ops) = toWord8List $ serializeScript $ Script ops
  
  eis = normalWorker $ words input

  normalWorker :: [String] -> [Either [Word8] [Opcode]]
  normalWorker [] = []
  normalWorker (w:rest) 
    | sop /= OP_UNKNOWN 0     =  Right [sop] : normalWorker rest                               
    | isHex w                 =  handleStupidHexStuff (w:rest)  
    | otherwise               =  error $ "Bitcoin.Test.Script.Parser.parseWord: " ++ w
    where
      sop = singleOpcode w 

  isHex :: String -> Bool
  isHex w = take 2 w == "0x"

  -- 'gavin_was_here' - yeah, i can see that...    
  handleStupidHexStuff :: [String] -> [Either [Word8] [Opcode]]
  handleStupidHexStuff ws = this : normalWorker nonhex where
    (hex,nonhex) = span isHex ws
    bs = fromHexString $ HexString $ concat $ map (drop 2) hex
    this = case parseScript (RawScript bs) of
      Nothing               -> Left (toWord8List bs) -- error "parseWords: cannot parse stupid hex stuff"
      Just (Script opcodes) -> Right opcodes
      
  -- | if not a single opcode, we return OP_UNKNOWN 0, which should be otherwise never returned
  -- hack, but i don't care, especially after seeing what is present in the bitcoin codebase...
  singleOpcode :: String -> Opcode    
  singleOpcode w = case w of
    "0" -> OP_SMALLNUM 0
    "1" -> OP_SMALLNUM 1
    "2" -> OP_SMALLNUM 2
    "3" -> OP_SMALLNUM 3
    "4" -> OP_SMALLNUM 4
    "5" -> OP_SMALLNUM 5
    "6" -> OP_SMALLNUM 6
    "7" -> OP_SMALLNUM 7
    "8" -> OP_SMALLNUM 8
    "9" -> OP_SMALLNUM 9
    "10" -> OP_SMALLNUM 10
    "11" -> OP_SMALLNUM 11
    "12" -> OP_SMALLNUM 12
    "13" -> OP_SMALLNUM 13
    "14" -> OP_SMALLNUM 14
    "15" -> OP_SMALLNUM 15
    "16" -> OP_SMALLNUM 16

    "NOP"  -> OP_NOP 97
    "NOP1" -> OP_NOP 176
    "NOP2" -> OP_NOP 177
    "NOP3" -> OP_NOP 178
    "NOP4" -> OP_NOP 179
    "NOP5" -> OP_NOP 180
    "NOP6" -> OP_NOP 181
    "NOP7" -> OP_NOP 182
    "NOP8" -> OP_NOP 183
    "NOP9" -> OP_NOP 184
    "NOP10"-> OP_NOP 185

    "IF"     -> OP_IF
    "NOTIF"  -> OP_NOTIF
    "ELSE"   -> OP_ELSE
    "ENDIF"  -> OP_ENDIF
    "VERIFY" -> OP_VERIFY
    "RETURN" -> OP_RETURN

    "TOALTSTACK"   -> OP_TOALTSTACK          -- Puts the input onto the top of the alt stack. Removes it from the main stack. 
    "FROMALTSTACK" -> OP_FROMALTSTACK        -- Puts the input onto the top of the main stack. Removes it from the alt stack. 
    "IFDUP" -> OP_IFDUP               -- If the top stack value is not 0, duplicate it. 
    "DEPTH" -> OP_DEPTH               -- Puts the number of stack items onto the stack. 
    "DROP"  -> OP_DROP                -- Removes the top stack item. 
    "DUP"   -> OP_DUP                 -- Duplicates the top stack item. 
    "NIP"   -> OP_NIP                 -- Removes the second-to-top stack item. 
    "OVER"  -> OP_OVER                -- Copies the second-to-top stack item to the top. 
    "PICK"  -> OP_PICK                -- The item n back in the stack is copied to the top. 
    "ROLL"  -> OP_ROLL                -- The item n back in the stack is moved to the top. 
    "ROT"   -> OP_ROT                 -- The top three items on the stack are rotated to the left. 
    "SWAP"  -> OP_SWAP                -- The top two items on the stack are swapped. 
    "TUCK"  -> OP_TUCK                -- The item at the top of the stack is copied and inserted before the second-to-top item. 
    "2DROP" -> OP_2DROP               -- Removes the top two stack items. 
    "2DUP"  -> OP_2DUP                -- Duplicates the top two stack items. 
    "3DUP"  -> OP_3DUP                -- Duplicates the top three stack items. 
    "2OVER" -> OP_2OVER               -- Copies the pair of items two spaces back in the stack to the front. 
    "2ROT"  -> OP_2ROT                -- The fifth and sixth items back are moved to the top of the stack. 
    "2SWAP" -> OP_2SWAP               -- Swaps the top two pairs of items.

    -- splice
    "CAT"    -> OP_CAT        -- Concatenates two strings. Currently disabled. 
    "SUBSTR" -> OP_SUBSTR     -- Returns a section of a string. Currently disabled. 
    "LEFT"   -> OP_LEFT       -- Keeps only characters left of the specified point in a string. Currently disabled. 
    "RIGHT"  -> OP_RIGHT      -- Keeps only characters right of the specified point in a string. Currently disabled. 
    "SIZE"   -> OP_SIZE       -- Returns the length of the input string. 

    -- bitwise logic
    "INVERT" -> OP_INVERT         -- Flips all of the bits in the input. Currently disabled. 
    "AND"    -> OP_AND            -- Boolean and between each bit in the inputs. Currently disabled. 
    "OR"     -> OP_OR             -- Boolean or between each bit in the inputs. Currently disabled. 
    "XOR"    -> OP_XOR            -- Boolean exclusive or between each bit in the inputs. Currently disabled. 
    "EQUAL"  -> OP_EQUAL          -- Returns 1 if the inputs are exactly equal, 0 otherwise. 
    "EQUALVERIFY" -> OP_EQUALVERIFY    -- Same as OP_EQUAL, but runs OP_VERIFY afterward.

    -- arithmetic
    "1ADD"   -> OP_1ADD          -- 1 is added to the input. 
    "1SUB"   -> OP_1SUB          -- 1 is subtracted from the input. 
    "2MUL"   -> OP_2MUL          -- The input is multiplied by 2. Currently disabled. 
    "2DIV"   -> OP_2DIV          -- The input is divided by 2. Currently disabled. 
    "NEGATE" -> OP_NEGATE        -- The sign of the input is flipped. 
    "ABS"    -> OP_ABS           -- The input is made positive. 
    "NOT"    -> OP_NOT           -- If the input is 0 or 1, it is flipped. Otherwise the output will be 0. 
    "0NOTEQUAL" -> OP_0NOTEQUAL     -- Returns 0 if the input is 0. 1 otherwise. 
    "ADD"    -> OP_ADD           -- a is added to b. 
    "SUB"    -> OP_SUB           -- b is subtracted from a. 
    "MUL"    -> OP_MUL           -- a is multiplied by b. Currently disabled. 
    "DIV"    -> OP_DIV           -- a is divided by b. Currently disabled. 
    "MOD"    -> OP_MOD           -- Returns the remainder after dividing a by b. Currently disabled. 
    "LSHIFT" -> OP_LSHIFT            -- Shifts a left b bits, preserving sign. Currently disabled. 
    "RSHIFT" -> OP_RSHIFT             -- Shifts a right b bits, preserving sign. Currently disabled. 
    "BOOLAND"  -> OP_BOOLAND            -- If both a and b are not 0, the output is 1. Otherwise 0. 
    "BOOLOR"   -> OP_BOOLOR             -- If a or b is not 0, the output is 1. Otherwise 0. 
    "NUMEQUAL" -> OP_NUMEQUAL           -- Returns 1 if the numbers are equal, 0 otherwise. 
    "NUMEQUALVERIFY" -> OP_NUMEQUALVERIFY     -- Same as OP_NUMEQUAL, but runs OP_VERIFY afterward. 
    "NUMNOTEQUAL"    -> OP_NUMNOTEQUAL        -- Returns 1 if the numbers are not equal, 0 otherwise. 
    "LESSTHAN"       -> OP_LESSTHAN           -- Returns 1 if a is less than b, 0 otherwise. 
    "GREATERTHAN"    -> OP_GREATERTHAN        -- Returns 1 if a is greater than b, 0 otherwise. 
    "LESSTHANOREQUAL"-> OP_LESSTHANOREQUAL    -- Returns 1 if a is less than or equal to b, 0 otherwise. 
    "GREATERTHANOREQUAL" -> OP_GREATERTHANOREQUAL -- Returns 1 if a is greater than or equal to b, 0 otherwise. 
    "MIN"    -> OP_MIN                -- Returns the smaller of a and b. 
    "MAX"    -> OP_MAX                -- Returns the larger of a and b. 
    "WITHIN" -> OP_WITHIN             -- Returns 1 if x is within the specified range (left-inclusive), 0 otherwise

    -- crypto
    "RIPEMD160" -> OP_RIPEMD160           -- The input is hashed using RIPEMD-160. 
    "SHA1" -> OP_SHA1                -- The input is hashed using SHA-1. 
    "SHA256" -> OP_SHA256              -- The input is hashed using SHA-256. 
    "HASH160" -> OP_HASH160             -- The input is hashed twice: first with SHA-256 and then with RIPEMD-160. 
    "HASH256" -> OP_HASH256             -- The input is hashed two times with SHA-256. 
    "CODESEPARATOR" -> OP_CODESEPARATOR       -- All of the signature checking words will only match signatures to the data after the most recently-executed OP_CODESEPARATOR. 
    "CHECKSIG" -> OP_CHECKSIG            -- The entire transaction's outputs, inputs, and script (from the most recently-executed OP_CODESEPARATOR to the end) are hashed. The signature used by OP_CHECKSIG must be a valid signature for this hash and public key. If it is, 1 is returned, 0 otherwise. 
    "CHECKSIGVERIFY" -> OP_CHECKSIGVERIFY      -- Same as OP_CHECKSIG, but OP_VERIFY is executed afterward. 
    "CHECKMULTISIG" -> OP_CHECKMULTISIG       -- For each signature and public key pair, OP_CHECKSIG is executed. If more public keys than signatures are listed, some key/sig pairs can fail. All signatures need to match a public key. If all signatures are valid, 1 is returned, 0 otherwise. Due to a bug, one extra unused value is removed from the stack. 
    "CHECKMULTISIGVERIFY" -> OP_CHECKMULTISIGVERIFY -- Same as OP_CHECKMULTISIG, but OP_VERIFY is executed afterward.

    -- reserved words
    "RESERVED"  -> OP_RESERVED           -- Transaction is invalid unless occuring in an unexecuted OP_IF branch
    "VER"       -> OP_VER                -- Transaction is invalid unless occuring in an unexecuted OP_IF branch
    "VERIF"     -> OP_VERIF              -- Transaction is invalid even when occuring in an unexecuted OP_IF branch
    "VERNOTIF"  -> OP_VERNOTIF           -- Transaction is invalid even when occuring in an unexecuted OP_IF branch
    "RESERVED1" -> OP_RESERVED1          -- Transaction is invalid unless occuring in an unexecuted OP_IF branch
    "RESERVED2" -> OP_RESERVED2          -- Transaction is invalid unless occuring in an unexecuted OP_IF branch

    _ | head w == '\'' && last w == '\''      -> op_PUSHDATA (B.pack $ map char_to_word8 $ init $ tail w)   
    _ | all isDigit w                         -> op_BIGNUMBER (read w)
    _ | head w == '-' && all isDigit (tail w) -> op_BIGNUMBER (read w)    

    _ -> OP_UNKNOWN 0

--------------------------------------------------------------------------------
