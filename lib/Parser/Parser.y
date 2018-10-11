{
module Parser.Parser where

import Data.Bitcoin.Script.Types
import Parser.AST
}

%name buildAST
%tokentype { ScriptOp }
%error { parseError }

%token
  if    { OP_IF }
  ifn   { OP_NOTIF }
  else  { OP_ELSE }
  fi    { OP_ENDIF }
  ifdup { OP_IFDUP }
  op    { $$ }

%%

scriptAst : stmnts { $1 }

stmnts : if stmnts else stmnts fi stmnts { ScriptITE 0 $2 0 $4 0 $6 }
       | if stmnts fi stmnts { ScriptITE 0 $2 0 ScriptTail 0 $4 }
       | ifn stmnts else stmnts fi stmnts { ScriptITE 0 $4 0 $2 0 $6 }
       | ifn stmnts fi stmnts { ScriptITE 0 ScriptTail 0 $2 0 $4 }
       | ifdup stmnts { ScriptOp 0 OP_DUP $ ScriptITE 0 (ScriptOp 0 OP_DUP ScriptTail) 0
                                                        (ScriptOp 0 OP_DROP ScriptTail) 0
                                                        $2 }
       | op stmnts { ScriptOp 0 $1 $2 }
       | { ScriptTail }


{

parseError t = error $ "Parse error.. " ++ show t

}
