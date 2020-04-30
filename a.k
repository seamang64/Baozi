MODULE Main 0 0
IMPORT Lib 0
ENDHDR

DEFINE Object.%desc
WORD 0
WORD Object.%anc
WORD Object.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print

DEFINE Object.%anc
WORD 1
WORD Object.%desc

DEFINE Object.%string
STRING 4F626A65637400

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
LABEL 5
SWAP
DUP 0
LOCAL -8
LOADW
JEQ 7
CONST 4
PLUS
SWAP
CONST 4
OFFSET
DUP 0
LOADW
LOCAL -4
LOADW
JEQ 6
JUMP 5
LABEL 6
CONST 1
GLOBAL baozi.makePrim
CALLW 1
RETURNW
LABEL 7
CONST 0
GLOBAL baozi.makePrim
CALLW 1
RETURNW
END

PROC Object.Print 0 0 0
LOCAL 12
LOADW
LOADW
CONST 8
OFFSET
LOADW
CONST 0
GLOBAL lib.print_string
PCALLW 1
RETURNW
END

PROC Integer.equals 0 0 0
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

PROC Integer.Print 0 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
CONST 0
GLOBAL lib.print_num
PCALLW 1
RETURN
END

PROC Integer.add 0 0 0
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

PROC Integer.times 0 0 0
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

PROC Integer.sub 0 0 0
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

PROC Integer.div 0 0 0
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

PROC Integer.mod 0 0 0
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

PROC Integer.uminus 0 0 0
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

PROC Integer.notEquals 0 0 0
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

PROC Integer.lessThan 0 0 0
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

PROC Integer.greaterThan 0 0 0
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

PROC Integer.lessThanEq 0 0 0
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

PROC Integer.greaterThanEq 0 0 0
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
WORD Integer.%string
WORD Integer.equals
WORD Object.GetType
WORD Object.Is
WORD Integer.Print
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

DEFINE Integer.%string
STRING 496E7400

DEFINE Output.%desc
WORD 0
WORD 1
WORD Output.Int
WORD Output.IntLn
WORD Output.String
WORD Output.StringLn
WORD Output.Ln

PROC Output.Int 0 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
CONST 0
GLOBAL lib.print_num
PCALLW 1
RETURN
END

PROC Output.IntLn 0 0 0
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

PROC Output.String 0 0 0
LOCAL 12
LOADW
CONST 8
OFFSET
LOADW
CONST 0
GLOBAL lib.print_string
PCALLW 1
RETURN
END

PROC Output.StringLn 0 0 0
LOCAL 12
LOADW
CONST 8
OFFSET
LOADW
CONST 0
GLOBAL lib.print_string
PCALLW 1
GLOBAL lib.newline
PCALLW 0
RETURN
END

PROC Output.Ln 0 0 0
GLOBAL lib.newline
PCALLW 0
RETURN
END

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

PROC Bool.Print 4 0 0
LOCAL 12
LOADW
CONST 4
OFFSET
LOADW
CONST 0
JEQ 4
GLOBAL Bool.TrueString
CONST 0
GLOBAL lib.print_string
PCALLW 1
RETURN
LABEL 4
GLOBAL Bool.FalseString
CONST 0
GLOBAL lib.print_string
PCALLW 1
RETURN
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
WORD Bool.%string
WORD Bool.equals
WORD Object.GetType
WORD Object.Is
WORD Bool.Print
WORD Bool.and
WORD Bool.or
WORD Bool.not

DEFINE Bool.%anc
WORD Bool.%desc
WORD Object.%desc

DEFINE Bool.%string
STRING 426F6F6C00

DEFINE Bool.TrueString
STRING 5472756500

DEFINE Bool.FalseString
STRING 46616C736500

DEFINE Array.%desc
WORD 0
WORD Array.%anc
WORD Array.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print

DEFINE Array.%anc
WORD 2
WORD Array.%desc
WORD Object.%desc

DEFINE Array.%string
STRING 417272617900

DEFINE Type.%desc
WORD 0
WORD Type.%anc
WORD Type.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print

DEFINE Type.%anc
WORD 2
WORD Type.%desc
WORD Object.%desc

DEFINE Type.%string
STRING 5479706500

PROC String.Print 0 0 0
LOCAL 12
LOADW
GLOBAL Output.String
CALLW 1
RETURN
END

DEFINE String.%desc
WORD 0
WORD String.%anc
WORD String.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD String.Print

DEFINE String.%anc
WORD 2
WORD String.%desc
WORD Object.%desc

DEFINE String.%string
STRING 537472696E6700

PROC Program.Main 8 0 0
GLOBAL Nil
LOCAL -4
STOREW
GLOBAL Nil
LOCAL -8
STOREW
LOCAL -8
LOADW
LOCAL -4
LOADW
DUP 0
LOADW
CONST 12
OFFSET
LOADW
CALLW 2
CONST 4
OFFSET
LOADW
CONST 0
JNEQ 9
JUMP 10
LABEL 9
GLOBAL g8
CONST 13
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
GLOBAL baozi.makeString
CALLW 2
GLOBAL Output.StringLn
CALLW 1
JUMP 11
LABEL 10
LABEL 11
RETURN
END

DEFINE Program.%desc
WORD 0
WORD Program.%anc
WORD Program.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print
WORD Program.Main

DEFINE Program.%anc
WORD 2
WORD Program.%desc
WORD Object.%desc

DEFINE Program.%string
STRING 50726F6772616D00

DEFINE A.%desc
WORD 0
WORD A.%anc
WORD A.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print

DEFINE A.%anc
WORD 2
WORD A.%desc
WORD Object.%desc

DEFINE A.%string
STRING 4100

DEFINE B.%desc
WORD 0
WORD B.%anc
WORD B.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print

DEFINE B.%anc
WORD 2
WORD B.%desc
WORD Object.%desc

DEFINE B.%string
STRING 4200

DEFINE g8
STRING 48656C6C6F2C20576F726C642100

PROC MAIN 0 0 0
GLOBAL Program.Main
CALLW 0
RETURN
END
! End
