S |= op : S'
S' |= ops : S''
----------- Script
S |= op,ops : S''


S {stack = v1:ss}
S' {stack = v1:v1:ss}
----------- Dup
S |= OP_DUP : S'


S {stack = ss,
   vp = i}
S' {stack = Var i:ss,
    vp = i + 1}
------- VirtualStack
S |= op : S'


S {stack = v1:v2,ss,
   constrs = c}
S' {stack = ss,
    constrs = c && v1 == v2}
------------- ==
S |= OP_EQUAL : S'


S {stack = ss}
S' {stack = data:ss}
--------------------- Push data
S |= OP_PUSHDATA data : S'
