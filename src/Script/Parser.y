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

scriptAst : stmnts { ScriptBlock 0 $1 }

stmnts : stmnts stmnt { $1 ++ [$2] }
       | { [] }

stmnt : if stmnt else stmnt fi { ScriptITE 0 $2 $4 }
      | if stmnt fi { ScriptITE 0 $2 (ScriptBlock 0 []) }
      | ifn stmnt else stmnt fi { ScriptITE 0 $4 $2 }
      | ifn stmnt fi { ScriptITE 0 (ScriptBlock 0 []) $2 }
      | op { ScriptOp 0 $1 }


{

parseError _ = error "Parse error"

}
