{
module Script.Parser where

import Data.Bitcoin.Script.Types
import Script.AST
}

%name buildAST
%tokentype { ScriptOp }
%error { parseError }

%token
  if   { OP_IF }
  ifn  { OP_NOTIF }
  else { OP_ELSE }
  fi   { OP_ENDIF }
  op   { $$ }

%%

scriptAst : stmnts { $1 }

stmnts : if stmnts else stmnts fi stmnts { ScriptITE 0 $2 $4 $6 }
       | if stmnts fi stmnts { ScriptITE 0 $2 ScriptTail $4 }
       | ifn stmnts else stmnts fi stmnts { ScriptITE 0 $4 $2 $6 }
       | ifn stmnts fi stmnts { ScriptITE 0 ScriptTail $2 $4 }
       | op stmnts { ScriptOp 0 $1 $2 }
       | { ScriptTail }


{

parseError _ = error "Parse error"

}
