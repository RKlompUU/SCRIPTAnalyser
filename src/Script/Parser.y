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

scriptAst : stmntsBlock { $1 }

stmntsBlock : stmnts { ScriptBlock 0 $1 }

stmnts : stmnts stmnt { $1 ++ [$2] }
       | { [] }

stmnt : if stmntsBlock else stmntsBlock fi { ScriptITE 0 $2 $4 }
      | if stmntsBlock fi { ScriptITE 0 $2 (ScriptBlock 0 []) }
      | ifn stmntsBlock else stmntsBlock fi { ScriptITE 0 $4 $2 }
      | ifn stmntsBlock fi { ScriptITE 0 (ScriptBlock 0 []) $2 }
      | op { ScriptOp 0 $1 }


{

parseError _ = error "Parse error"

}
