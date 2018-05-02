{
module Script.Parser where

import Data.Bitcoin.Script.Types
import Script.AST
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

stmnts : if stmnts else stmnts fi stmnts { ScriptITE $2 $4 $6 }
       | if stmnts fi stmnts { ScriptITE $2 ScriptTail $4 }
       | ifn stmnts else stmnts fi stmnts { ScriptITE $4 $2 $6 }
       | ifn stmnts fi stmnts { ScriptITE ScriptTail $2 $4 }
       | ifdup stmnts { ScriptOp OP_DUP $ ScriptITE (ScriptOp OP_DUP ScriptTail)
                                                        (ScriptOp OP_DROP ScriptTail)
                                                        $2 }
       | op stmnts { ScriptOp $1 $2 }
       | { ScriptTail }


{

parseError t = error $ "Parse error.. " ++ show t

}
