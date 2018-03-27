
-- | Parsing and serializing Bitcoin scripts

{-# LANGUAGE PatternGuards #-}
module Bitcoin.Script.Serialize where

--------------------------------------------------------------------------------

import Data.Int
import Data.Word

import Control.Monad
import Control.Applicative

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Bitcoin.Script.Base

--------------------------------------------------------------------------------

{-
import Debug.Trace
debug      x y = trace ("---" ++ show x ++ "---") y
debug' pre x y = trace ("---" ++ pre ++ ":" ++ show x ++ "---") y
-}

--------------------------------------------------------------------------------

instance Binary Opcode where
  get = getOpcode
  put = putOpcode

--------------------------------------------------------------------------------

-- Notes: Scripts are big-endian.

getOpcode :: Get Opcode
getOpcode = getWord8 >>= \op -> case op of

  -- data
  0                      -> return (OP_SMALLNUM 0)
  _  | op>=1 && op<=75   -> OP_PUSHDATA op <$> getByteString (fromIntegral op)
  76                     -> getWord8    >>= \l -> OP_PUSHDATA op <$> getByteString (fromIntegral l)
  77                     -> getWord16le >>= \l -> OP_PUSHDATA op <$> getByteString (fromIntegral l)
  78                     -> getWord32le >>= \l -> OP_PUSHDATA op <$> getByteString (fromIntegral l)
  79                     -> return (OP_1NEGATE)
  81                     -> return (OP_SMALLNUM 1)
  _  | op>=82 && op<=96  -> return (OP_SMALLNUM (fromIntegral op-80))

  -- control flow
  97  -> return (OP_NOP op)
  99  -> return OP_IF
  100 -> return OP_NOTIF
  103 -> return OP_ELSE
  104 -> return OP_ENDIF
  105 -> return OP_VERIFY
  106 -> return OP_RETURN

  -- stack
  107 -> return OP_TOALTSTACK          -- Puts the input onto the top of the alt stack. Removes it from the main stack. 
  108 -> return OP_FROMALTSTACK        -- Puts the input onto the top of the main stack. Removes it from the alt stack. 
  115 -> return OP_IFDUP               -- If the top stack value is not 0, duplicate it. 
  116 -> return OP_DEPTH               -- Puts the number of stack items onto the stack. 
  117 -> return OP_DROP                -- Removes the top stack item. 
  118 -> return OP_DUP                 -- Duplicates the top stack item. 
  119 -> return OP_NIP                 -- Removes the second-to-top stack item. 
  120 -> return OP_OVER                -- Copies the second-to-top stack item to the top. 
  121 -> return OP_PICK                -- The item n back in the stack is copied to the top. 
  122 -> return OP_ROLL                -- The item n back in the stack is moved to the top. 
  123 -> return OP_ROT                 -- The top three items on the stack are rotated to the left. 
  124 -> return OP_SWAP                -- The top two items on the stack are swapped. 
  125 -> return OP_TUCK                -- The item at the top of the stack is copied and inserted before the second-to-top item. 
  109 -> return OP_2DROP               -- Removes the top two stack items. 
  110 -> return OP_2DUP                -- Duplicates the top two stack items. 
  111 -> return OP_3DUP                -- Duplicates the top three stack items. 
  112 -> return OP_2OVER               -- Copies the pair of items two spaces back in the stack to the front. 
  113 -> return OP_2ROT                -- The fifth and sixth items back are moved to the top of the stack. 
  114 -> return OP_2SWAP               -- Swaps the top two pairs of items.

  -- splice
  126 -> return OP_CAT        
  127 -> return OP_SUBSTR     
  128 -> return OP_LEFT       
  129 -> return OP_RIGHT      
  130 -> return OP_SIZE       

  -- bitwise logic
  131 -> return OP_INVERT     
  132 -> return OP_AND        
  133 -> return OP_OR         
  134 -> return OP_XOR        
  135 -> return OP_EQUAL      
  136 -> return OP_EQUALVERIFY 

  -- arithmetic
  139 -> return OP_1ADD          
  140 -> return OP_1SUB          
  141 -> return OP_2MUL          
  142 -> return OP_2DIV          
  143 -> return OP_NEGATE        
  144 -> return OP_ABS           
  145 -> return OP_NOT           
  146 -> return OP_0NOTEQUAL     
  147 -> return OP_ADD           
  148 -> return OP_SUB           
  149 -> return OP_MUL           
  150 -> return OP_DIV           
  151 -> return OP_MOD           
  152 -> return OP_LSHIFT            
  153 -> return OP_RSHIFT             
  154 -> return OP_BOOLAND            
  155 -> return OP_BOOLOR             
  156 -> return OP_NUMEQUAL           
  157 -> return OP_NUMEQUALVERIFY     
  158 -> return OP_NUMNOTEQUAL        
  159 -> return OP_LESSTHAN           
  160 -> return OP_GREATERTHAN        
  161 -> return OP_LESSTHANOREQUAL    
  162 -> return OP_GREATERTHANOREQUAL 
  163 -> return OP_MIN                
  164 -> return OP_MAX                
  165 -> return OP_WITHIN             

  -- crypto
  166 -> return OP_RIPEMD160          
  167 -> return OP_SHA1                
  168 -> return OP_SHA256              
  169 -> return OP_HASH160             
  170 -> return OP_HASH256             
  171 -> return OP_CODESEPARATOR       
  172 -> return OP_CHECKSIG            
  173 -> return OP_CHECKSIGVERIFY      
  174 -> return OP_CHECKMULTISIG       
  175 -> return OP_CHECKMULTISIGVERIFY

  -- reserved words
  80  -> return OP_RESERVED  
  98  -> return OP_VER       
  101 -> return OP_VERIF     
  102 -> return OP_VERNOTIF  
  137 -> return OP_RESERVED1 
  138 -> return OP_RESERVED2 
  _ | op>=167 && op<=185 -> return (OP_NOP op)

  -- pseudo
  255 -> return OP_INVALIDOPCODE
  _   -> return (OP_UNKNOWN op)      -- fail ("getOpcode: unhandled or invalid opcode " ++ show op)

--------------------------------------------------------------------------------

putOpcode :: Opcode -> Put 
putOpcode op = case op of

  -- data
  OP_SMALLNUM n   -> case n of 
                       0                 -> putWord8 0
                       _ | n>=1 && n<=16 -> putWord8 (80 + fromIntegral n) 
                       _                 -> fail ("putOpcode: OP_SMALLNUM can handle integers between 0 and 16")
  OP_1NEGATE      -> putWord8 79

  OP_PUSHDATA w8 bs  ->  if (is_valid_pushdata w8 bs) 
                           then let l = B.length bs in case w8 of
                             0  -> putWord8 w8    
                             76 -> putWord8 w8 >> putWord8    (fromIntegral l) >> putByteString bs 
                             77 -> putWord8 w8 >> putWord16le (fromIntegral l) >> putByteString bs 
                             78 -> putWord8 w8 >> putWord32le (fromIntegral l) >> putByteString bs 
                             _  -> putWord8 w8 >> putByteString bs
                           else fail "putOpcode: invalid OP_PUSHDATA"

  -- control flow
  OP_NOP w8   -> if is_nop w8 then putWord8 w8 else fail "putOpcode/OP_NOP: invalid NOP opcode"
  OP_IF       -> putWord8 99
  OP_NOTIF    -> putWord8 100
  OP_ELSE     -> putWord8 103
  OP_ENDIF    -> putWord8 104
  OP_VERIFY   -> putWord8 105
  OP_RETURN   -> putWord8 106

  -- stack
  OP_TOALTSTACK      -> putWord8 107
  OP_FROMALTSTACK    -> putWord8 108
  OP_IFDUP     -> putWord8 115
  OP_DEPTH     -> putWord8 116
  OP_DROP      -> putWord8 117
  OP_DUP       -> putWord8 118
  OP_NIP       -> putWord8 119
  OP_OVER      -> putWord8 120
  OP_PICK      -> putWord8 121
  OP_ROLL      -> putWord8 122
  OP_ROT       -> putWord8 123
  OP_SWAP      -> putWord8 124
  OP_TUCK      -> putWord8 125
  OP_2DROP     -> putWord8 109
  OP_2DUP      -> putWord8 110
  OP_3DUP      -> putWord8 111
  OP_2OVER     -> putWord8 112
  OP_2ROT      -> putWord8 113
  OP_2SWAP     -> putWord8 114

  -- splice
  OP_CAT       -> putWord8 126
  OP_SUBSTR    -> putWord8 127
  OP_LEFT      -> putWord8 128
  OP_RIGHT     -> putWord8 129 
  OP_SIZE      -> putWord8 130

  -- bitwise logic
  OP_INVERT      -> putWord8 131
  OP_AND         -> putWord8 132
  OP_OR          -> putWord8 133
  OP_XOR         -> putWord8 134
  OP_EQUAL       -> putWord8 135
  OP_EQUALVERIFY -> putWord8 136

  -- arithmetic
  OP_1ADD           -> putWord8 139
  OP_1SUB           -> putWord8 140
  OP_2MUL           -> putWord8 141
  OP_2DIV           -> putWord8 142
  OP_NEGATE         -> putWord8 143
  OP_ABS            -> putWord8 144
  OP_NOT            -> putWord8 145
  OP_0NOTEQUAL      -> putWord8 146
  OP_ADD            -> putWord8 147
  OP_SUB            -> putWord8 148
  OP_MUL            -> putWord8 149
  OP_DIV            -> putWord8 150
  OP_MOD            -> putWord8 151
  OP_LSHIFT              -> putWord8 152
  OP_RSHIFT              -> putWord8 153
  OP_BOOLAND             -> putWord8 154
  OP_BOOLOR              -> putWord8 155
  OP_NUMEQUAL            -> putWord8 156
  OP_NUMEQUALVERIFY      -> putWord8 157
  OP_NUMNOTEQUAL         -> putWord8 158
  OP_LESSTHAN            -> putWord8 159
  OP_GREATERTHAN         -> putWord8 160
  OP_LESSTHANOREQUAL     -> putWord8 161
  OP_GREATERTHANOREQUAL  -> putWord8 162
  OP_MIN                 -> putWord8 163
  OP_MAX                 -> putWord8 164
  OP_WITHIN              -> putWord8 165

  -- crypto
  OP_RIPEMD160       -> putWord8 166
  OP_SHA1            -> putWord8 167
  OP_SHA256          -> putWord8 168
  OP_HASH160         -> putWord8 169
  OP_HASH256         -> putWord8 170
  OP_CODESEPARATOR   -> putWord8 171
  OP_CHECKSIG        -> putWord8 172
  OP_CHECKSIGVERIFY      -> putWord8 173 
  OP_CHECKMULTISIG       -> putWord8 174
  OP_CHECKMULTISIGVERIFY -> putWord8 175

  -- reserved words
  OP_RESERVED       -> putWord8 80
  OP_VER            -> putWord8 98
  OP_VERIF          -> putWord8 101
  OP_VERNOTIF       -> putWord8 102
  OP_RESERVED1      -> putWord8 137
  OP_RESERVED2      -> putWord8 138

  -- pseudo
  OP_INVALIDOPCODE  -> putWord8 255       
  OP_UNKNOWN w8     -> putWord8 w8

  _  -> fail ("putOpcode: unhandled or invalid opcode " ++ show op)

--------------------------------------------------------------------------------

instance Binary Script where
  get = Script <$> getMany 
  put (Script ops) = putMany ops

-- | The default Binry instance for lists (naturally) encodes the 
-- length of the list at the start, so we need this...
getMany :: Binary a => Get [a]
getMany = do
  b <- isEmpty 
  if b 
    then return []
    else do
      x  <- get
      xs <- getMany
      return (x:xs)

putMany :: Binary a => [a] -> Put
putMany xs = mapM_ put xs

--------------------------------------------------------------------------------

parseScript :: RawScript -> Maybe Script   
parseScript (RawScript bs) = case decodeOrFail (L.fromChunks [bs]) of
  Left _ -> Nothing
  Right (remaining, consumedbytes, x) -> if L.null remaining
    then Just x
    else Nothing

serializeScript :: Script -> RawScript
serializeScript script = RawScript $ B.concat $ L.toChunks $ encode script

--------------------------------------------------------------------------------
