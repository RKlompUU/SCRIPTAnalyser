
-- | Bitcoin scripts

{-# LANGUAGE PatternGuards #-}
module Bitcoin.Script.Base where

--------------------------------------------------------------------------------

import Data.Int
import Data.Word

import Text.Show

import qualified Data.ByteString as B

import Bitcoin.Misc.OctetStream
import Bitcoin.Misc.HexString

import Bitcoin.Protocol.Hash

import Bitcoin.Script.Integer

--------------------------------------------------------------------------------

-- | A raw script (octet stream)
newtype RawScript = RawScript { fromRawScript :: B.ByteString } deriving Eq

-- | A parsed script (opcode stream)
newtype Script = Script { fromScript :: [Opcode] } deriving Eq

-- | The 160-bit hash of a (raw) script
newtype ScriptHash = ScriptHash { fromScriptHash  :: Hash160 } deriving (Eq,Show)

emptyRawScript :: RawScript
emptyRawScript = RawScript (B.empty)

emptyScript :: Script
emptyScript = Script []

scriptHash :: RawScript -> ScriptHash
scriptHash = ScriptHash . doHash160 . fromRawScript

instance OctetStream ScriptHash where
  toByteString   = toByteString . fromScriptHash
  fromByteString = ScriptHash . fromByteString

instance OctetStream RawScript where
  toByteString   = fromRawScript
  fromByteString = RawScript

instance Show RawScript where
  showsPrec d (RawScript rs) = showParen (d>10) $ showString "rawScriptFromString " . shows (toHexStringChars rs)

instance Show Script where
  showsPrec d (Script opcodes) = showParen (d>10) $ showString "Script " . (showOpcodeList opcodes)

rawScriptFromString :: String -> RawScript
rawScriptFromString s = if odd (length s)
  then error "rawScriptFromString: hex string should have even length"
  else RawScript $ fromHexString $ HexString s

--------------------------------------------------------------------------------
-- * shorthands

-- | @OP_FALSE@ = @OP_0@ = 0x00 = pushes an empty array to stack. 
op_FALSE :: Opcode
op_FALSE = OP_SMALLNUM 0
-- | @OP_TRUE@ = @OP_1@ = 0x51 = pushes the number 1 onto the stack
op_TRUE :: Opcode
op_TRUE  = OP_SMALLNUM 1

-- | Pushes the corresponding number to the stack (0 is represented by an empty array)
op_0 :: Opcode
op_0 = OP_SMALLNUM 0
op_1 :: Opcode
op_1 = OP_SMALLNUM 1
op_2 :: Opcode
op_2 = OP_SMALLNUM 2
op_3 :: Opcode
op_3 = OP_SMALLNUM 3

-- | Handy shorthands for comparisons
op_LT :: Opcode
op_LT = OP_LESSTHAN
op_GT :: Opcode
op_GT = OP_GREATERTHAN
op_LE :: Opcode
op_LE = OP_LESSTHANOREQUAL
op_GE :: Opcode
op_GE = OP_GREATERTHANOREQUAL

-- | Figures out which opcode to use
op_PUSHDATA :: B.ByteString -> Opcode
op_PUSHDATA bs = let l = B.length bs in case l of
    0                  -> OP_SMALLNUM 0       -- OP_0 = push empty array
    _ | l>=1 && l<=75  -> OP_PUSHDATA (fromIntegral l) bs
    _ | l<=255         -> OP_PUSHDATA 76 bs
    _ | l<=65536       -> OP_PUSHDATA 77 bs 
    _                  -> OP_PUSHDATA 78 bs

-- | Pushes a (signed) integer onto the stack, 
-- never using OP_SMALLNUM (except for 0, which is represented by the empty array)
op_BIGNUMBER :: Integer -> Opcode
op_BIGNUMBER n = op_PUSHDATA (asByteString n)

--------------------------------------------------------------------------------
-- * opcodes

-- | See <https://en.bitcoin.it/wiki/Script>
data Opcode
  
  -- data
  = OP_SMALLNUM !Int                    -- ^ OP_0, OP_1 .. OP_16 (bytes 0,81,82..96). Pushes the number to the stack (0 is represented by an empty array)
  | OP_1NEGATE                          -- ^ The number -1 is pushed onto the stack.
  | OP_PUSHDATA !Word8 !B.ByteString    -- ^ Pushes data to the stack. The Word8 is the opcode: it can be 0, 1..75 and 76,77,78. 

  -- control flow
  | OP_NOP !Word8          -- ^ Does nothing. The argument is the opcode, either 0x61 or 0xb0-0xb9
  | OP_IF                  -- ^ If the top stack value is not 0, the statements are executed. The top stack value is removed.
  | OP_NOTIF               -- ^ If the top stack value is 0, the statements are executed. The top stack value is removed.
  | OP_ELSE                -- ^ If the preceding OP_IF or OP_NOTIF or OP_ELSE was not executed then these statements are and if the preceding OP_IF or OP_NOTIF or OP_ELSE was executed then these statements are not.
  | OP_ENDIF               -- ^ Ends an if/else block.
  | OP_VERIFY              -- ^ Marks transaction as invalid if top stack value is not true. True is removed, but false is not.
  | OP_RETURN              -- ^ Marks transaction as invalid.

  -- stack
  | OP_TOALTSTACK          -- ^ Puts the input onto the top of the alt stack. Removes it from the main stack. 
  | OP_FROMALTSTACK        -- ^ Puts the input onto the top of the main stack. Removes it from the alt stack. 
  | OP_IFDUP               -- ^ If the top stack value is not 0, duplicate it. 
  | OP_DEPTH               -- ^ Puts the number of stack items onto the stack. 
  | OP_DROP                -- ^ Removes the top stack item. 
  | OP_DUP                 -- ^ Duplicates the top stack item. 
  | OP_NIP                 -- ^ Removes the second-to-top stack item. 
  | OP_OVER                -- ^ Copies the second-to-top stack item to the top. 
  | OP_PICK                -- ^ The item n back in the stack is copied to the top. 
  | OP_ROLL                -- ^ The item n back in the stack is moved to the top. 
  | OP_ROT                 -- ^ The top three items on the stack are rotated to the left. 
  | OP_SWAP                -- ^ The top two items on the stack are swapped. 
  | OP_TUCK                -- ^ The item at the top of the stack is copied and inserted before the second-to-top item. 
  | OP_2DROP               -- ^ Removes the top two stack items. 
  | OP_2DUP                -- ^ Duplicates the top two stack items. 
  | OP_3DUP                -- ^ Duplicates the top three stack items. 
  | OP_2OVER               -- ^ Copies the pair of items two spaces back in the stack to the front. 
  | OP_2ROT                -- ^ The fifth and sixth items back are moved to the top of the stack. 
  | OP_2SWAP               -- ^ Swaps the top two pairs of items.

  -- splice
  | OP_CAT        -- ^ Concatenates two strings. Currently disabled. 
  | OP_SUBSTR     -- ^ Returns a section of a string. Currently disabled. 
  | OP_LEFT       -- ^ Keeps only characters left of the specified point in a string. Currently disabled. 
  | OP_RIGHT      -- ^ Keeps only characters right of the specified point in a string. Currently disabled. 
  | OP_SIZE       -- ^ Returns the length of the input string. 

  -- bitwise logic
  | OP_INVERT         -- ^ Flips all of the bits in the input. Currently disabled. 
  | OP_AND            -- ^ Boolean and between each bit in the inputs. Currently disabled. 
  | OP_OR             -- ^ Boolean or between each bit in the inputs. Currently disabled. 
  | OP_XOR            -- ^ Boolean exclusive or between each bit in the inputs. Currently disabled. 
  | OP_EQUAL          -- ^ Returns 1 if the inputs are exactly equal, 0 otherwise. 
  | OP_EQUALVERIFY    -- ^ Same as OP_EQUAL, but runs OP_VERIFY afterward.

  -- arithmetic
  | OP_1ADD          -- ^ 1 is added to the input. 
  | OP_1SUB          -- ^ 1 is subtracted from the input. 
  | OP_2MUL          -- ^ The input is multiplied by 2. Currently disabled. 
  | OP_2DIV          -- ^ The input is divided by 2. Currently disabled. 
  | OP_NEGATE        -- ^ The sign of the input is flipped. 
  | OP_ABS           -- ^ The input is made positive. 
  | OP_NOT           -- ^ If the input is 0 or 1, it is flipped. Otherwise the output will be 0. 
  | OP_0NOTEQUAL     -- ^ Returns 0 if the input is 0. 1 otherwise. 
  | OP_ADD           -- ^ a is added to b. 
  | OP_SUB           -- ^ b is subtracted from a. 
  | OP_MUL           -- ^ a is multiplied by b. Currently disabled. 
  | OP_DIV           -- ^ a is divided by b. Currently disabled. 
  | OP_MOD           -- ^ Returns the remainder after dividing a by b. Currently disabled. 
  | OP_LSHIFT            -- ^ Shifts a left b bits, preserving sign. Currently disabled. 
  | OP_RSHIFT             -- ^ Shifts a right b bits, preserving sign. Currently disabled. 
  | OP_BOOLAND            -- ^ If both a and b are not 0, the output is 1. Otherwise 0. 
  | OP_BOOLOR             -- ^ If a or b is not 0, the output is 1. Otherwise 0. 
  | OP_NUMEQUAL           -- ^ Returns 1 if the numbers are equal, 0 otherwise. 
  | OP_NUMEQUALVERIFY     -- ^ Same as OP_NUMEQUAL, but runs OP_VERIFY afterward. 
  | OP_NUMNOTEQUAL        -- ^ Returns 1 if the numbers are not equal, 0 otherwise. 
  | OP_LESSTHAN           -- ^ Returns 1 if a is less than b, 0 otherwise. 
  | OP_GREATERTHAN        -- ^ Returns 1 if a is greater than b, 0 otherwise. 
  | OP_LESSTHANOREQUAL    -- ^ Returns 1 if a is less than or equal to b, 0 otherwise. 
  | OP_GREATERTHANOREQUAL -- ^ Returns 1 if a is greater than or equal to b, 0 otherwise. 
  | OP_MIN                -- ^ Returns the smaller of a and b. 
  | OP_MAX                -- ^ Returns the larger of a and b. 
  | OP_WITHIN             -- ^ Returns 1 if x is within the specified range (left-inclusive), 0 otherwise

  -- crypto
  | OP_RIPEMD160           -- ^ The input is hashed using RIPEMD-160. 
  | OP_SHA1                -- ^ The input is hashed using SHA-1. 
  | OP_SHA256              -- ^ The input is hashed using SHA-256. 
  | OP_HASH160             -- ^ The input is hashed twice: first with SHA-256 and then with RIPEMD-160. 
  | OP_HASH256             -- ^ The input is hashed two times with SHA-256. 
  | OP_CODESEPARATOR       -- ^ All of the signature checking words will only match signatures to the data after the most recently-executed OP_CODESEPARATOR. 
  | OP_CHECKSIG            -- ^ The entire transaction's outputs, inputs, and script (from the most recently-executed OP_CODESEPARATOR to the end) are hashed. The signature used by OP_CHECKSIG must be a valid signature for this hash and public key. If it is, 1 is returned, 0 otherwise. 
  | OP_CHECKSIGVERIFY      -- ^ Same as OP_CHECKSIG, but OP_VERIFY is executed afterward. 
  | OP_CHECKMULTISIG       -- ^ For each signature and public key pair, OP_CHECKSIG is executed. If more public keys than signatures are listed, some key/sig pairs can fail. All signatures need to match a public key. If all signatures are valid, 1 is returned, 0 otherwise. Due to a bug, one extra unused value is removed from the stack. 
  | OP_CHECKMULTISIGVERIFY -- ^ Same as OP_CHECKMULTISIG, but OP_VERIFY is executed afterward.

  -- reserved words
  | OP_RESERVED           -- ^ Transaction is invalid unless occuring in an unexecuted OP_IF branch
  | OP_VER                -- ^ Transaction is invalid unless occuring in an unexecuted OP_IF branch
  | OP_VERIF              -- ^ Transaction is invalid even when occuring in an unexecuted OP_IF branch
  | OP_VERNOTIF           -- ^ Transaction is invalid even when occuring in an unexecuted OP_IF branch
  | OP_RESERVED1          -- ^ Transaction is invalid unless occuring in an unexecuted OP_IF branch
  | OP_RESERVED2          -- ^ Transaction is invalid unless occuring in an unexecuted OP_IF branch

  -- pseudo
  | OP_INVALIDOPCODE      -- ^ this is a pseudo opcode, but it appears in the testnet3 blockchain...
  | OP_UNKNOWN !Word8     -- ^ unknown opcodes also appear in the testnet, inside (unexecuted?) OP_IF branches...

  deriving (Eq,Show)

--------------------------------------------------------------------------------
-- * helper functions

-- | Note: this function returns 'Nothing' for @OP_0@ (which technically pushes an empty array to the stack).
-- Also we don't check for validity (see 'is_valid_pushdata')
is_op_pushdata :: Opcode -> Maybe B.ByteString
is_op_pushdata op = case op of { OP_PUSHDATA _ bs -> Just bs ; _ -> Nothing }

-- | Note that OP_0 = push empty array, so we accept that as a valid @OP_PUSHDATA@
is_valid_pushdata :: Word8 -> B.ByteString -> Bool
is_valid_pushdata op bs 
  | op>=0 && op<=75   =  (n == fromIntegral op)    
  | op==76            =  (n <= 255) 
  | op==77            =  (n <= 65535)
  | op==78            =  True
  | otherwise         =  False
  where
    n = B.length bs

is_op_smallnum :: Opcode -> Maybe Int
is_op_smallnum op = case op of { OP_SMALLNUM k  -> Just k  ; _ -> Nothing }

is_nop :: Word8 -> Bool
is_nop w8 = case w8 of
  0x61                         -> True             -- OP_NOP
  _ | w8 >= 0xb0 && w8 <= 0xb9 -> True             -- OP_NOP1 .. OP_NOP10
  _                            -> False

--------------------------------------------------------------------------------

showOpcode :: Opcode -> (String -> String)
showOpcode op = case op of
  OP_PUSHDATA w8 dat -> showString "OP_PUSHDATA <0x"
                          . showString (showHexWord8 w8) 
                          . showString "> \"" 
                          . showString (toHexStringChars dat) 
                          . showChar '\"'
  _                  -> shows op

showList' :: (a -> (String -> String)) ->  [a] -> (String -> String)
showList' _     []     s = "[]" ++ s
showList' showx (x:xs) s = '[' : showx x (showl xs)
  where
    showl []     = ']' : s
    showl (y:ys) = ',' : showx y (showl ys)

showOpcodeList :: [Opcode] -> (String -> String)
showOpcodeList list = showList' showOpcode list 

--------------------------------------------------------------------------------
