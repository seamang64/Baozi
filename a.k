MODULE Main 0 0
IMPORT Lib 0
ENDHDR

DEFINE Object.%desc
WORD 0
WORD Object.%anc
WORD 3
WORD Object.equals
WORD Object.GetType
WORD Object.Is

DEFINE Object.%anc
WORD 1
WORD Object.%desc

PROC Object.equals 0 0 0
LOCAL 12
LOADW
LOCAL 16
LOADW
EQ
GLOBAL Bool.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

PROC Object.GetType 0 0 0
LOCAL 12
LOADW
LOADW
GLOBAL Bool.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

PROC Object.Is 12 0 0
LOCAL 16
LOADW
CONST 4
OFFSET
LOADW
LOCAL -4
STOREW
CONST 0
LOCAL 12
LOADW
LOADW
CONST 4
OFFSET
LOADW
DUP 0
LOADW
LOCAL -8
STOREW
LABEL 4
SWAP
DUP 0
LOCAL -8
LOADW
JEQ 6
CONST 4
PLUS
SWAP
CONST 4
OFFSET
DUP 0
LOADW
LOCAL -4
LOADW
JEQ 5
JUMP 4
LABEL 5
CONST 1
GLOBAL baozi.makePrim
CALLW 1
RETURNW
LABEL 6
CONST 0
GLOBAL baozi.makePrim
CALLW 1
RETURNW
END

PROC Integer.add 4 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
LOCAL 16
LOADW
CONST 4
OFFSET
LOADW
PLUS
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

PROC Integer.times 4 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
LOCAL 16
LOADW
CONST 4
OFFSET
LOADW
TIMES
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

PROC Integer.sub 4 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
LOCAL 16
LOADW
CONST 4
OFFSET
LOADW
MINUS
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

PROC Integer.div 4 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
LOCAL 16
LOADW
CONST 4
OFFSET
LOADW
DIV
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

PROC Integer.mod 4 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
LOCAL 16
LOADW
CONST 4
OFFSET
LOADW
MOD
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

PROC Integer.uminus 4 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
UMINUS
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

PROC Integer.equals 4 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
LOCAL 16
LOADW
CONST 4
OFFSET
LOADW
EQ
GLOBAL Bool.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

PROC Integer.notEquals 4 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
LOCAL 16
LOADW
CONST 4
OFFSET
LOADW
NEQ
GLOBAL Bool.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

PROC Integer.lessThan 4 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
LOCAL 16
LOADW
CONST 4
OFFSET
LOADW
LT
GLOBAL Bool.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

PROC Integer.greaterThan 4 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
LOCAL 16
LOADW
CONST 4
OFFSET
LOADW
GT
GLOBAL Bool.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

PROC Integer.lessThanEq 4 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
LOCAL 16
LOADW
CONST 4
OFFSET
LOADW
LEQ
GLOBAL Bool.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

PROC Integer.greaterThanEq 4 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
LOCAL 16
LOADW
CONST 4
OFFSET
LOADW
GEQ
GLOBAL Bool.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

DEFINE Integer.%desc
WORD 0
WORD Integer.%anc
WORD 12
WORD Integer.equals
WORD Integer.add
WORD Integer.times
WORD Integer.sub
WORD Integer.div
WORD Integer.mod
WORD Integer.uminus
WORD Integer.notEquals
WORD Integer.lessThan
WORD Integer.greaterThan
WORD Integer.lessThanEq
WORD Integer.greaterThanEq

DEFINE Integer.%anc
WORD 2
WORD Integer.%desc
WORD Object.%desc

PROC Output.Int 0 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
CONST 0
GLOBAL lib.print_num
PCALLW 1
GLOBAL lib.newline
PCALLW 0
RETURN
END

DEFINE Output.%desc
WORD 0
WORD 1
WORD Output.Int

PROC Bool.equals 4 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
LOCAL 16
LOADW
CONST 4
OFFSET
LOADW
EQ
GLOBAL Bool.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

PROC Bool.and 4 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
LOCAL 16
LOADW
CONST 4
OFFSET
LOADW
EQ
GLOBAL Bool.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

PROC Bool.or 4 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
LOCAL 16
LOADW
CONST 4
OFFSET
LOADW
EQ
GLOBAL Bool.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

PROC Bool.not 4 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
NOT
GLOBAL Bool.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

DEFINE Bool.%desc
WORD 0
WORD Bool.%anc
WORD 4
WORD Bool.equals
WORD Bool.and
WORD Bool.or
WORD Bool.not

DEFINE Bool.%anc
WORD Bool.%desc
WORD Object.%desc

DEFINE Array.%desc
WORD 0
WORD Array.%anc
WORD 1
WORD Array.equals

DEFINE Array.%anc
WORD Array.%desc
WORD Object.%desc

PROC Array.equals 0 0 0
LOCAL 12
LOADW
LOCAL 16
LOADW
EQ
GLOBAL Bool.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

DEFINE Type.%desc
WORD 0
WORD 1
WORD Type.%anc
WORD Type.equals

DEFINE Type.%anc
WORD 2
WORD Type.%desc
WORD Object.%desc

PROC Type.equals 0 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
LOCAL 16
LOADW
CONST 4
OFFSET
LOADW
EQ
GLOBAL Bool.%desc
GLOBAL baozi.makePrim
CALLW 2
RETURNW
END

PROC Program.Main 4 0 0
GLOBAL B.%desc
CONST 4
GLOBAL baozi.make
CALLW 2
LOCAL -4
STOREW
LOCAL -4
LOADW
DUP 0
LOADW
CONST 24
OFFSET
LOADW
CALLW 1
RETURN
END

PROC A.DoSomething 0 0 0
CONST 57
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
GLOBAL Output.Int
CALLW 1
RETURN
END

PROC B.DoSomething 0 0 0
CONST 6
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
GLOBAL Output.Int
CALLW 1
LOCAL 12
LOADW
DUP 0
LOADW
CONST 4
OFFSET
LOADW
CONST 8
OFFSET
LOADW
CONST 24
OFFSET
LOADW
CALLW 1
RETURN
END

DEFINE Program.%desc
WORD 0
WORD Program.%anc
WORD 4
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Program.Main

DEFINE Program.%anc
WORD 2
WORD Program.%desc
WORD Object.%desc

DEFINE A.%desc
WORD 0
WORD A.%anc
WORD 4
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD A.DoSomething

DEFINE A.%anc
WORD 2
WORD A.%desc
WORD Object.%desc

DEFINE B.%desc
WORD 0
WORD B.%anc
WORD 4
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD B.DoSomething

DEFINE B.%anc
WORD 3
WORD B.%desc
WORD A.%desc
WORD Object.%desc

PROC MAIN 0 0 0
GLOBAL Program.Main
CALLW 0
RETURN
END
! End
