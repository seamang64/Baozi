MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC Program.Main 8 0 0x18001
GLOBAL IntVec.%desc
CONST 3
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
GLOBAL baozi.makeArray
CALLW 2
STLW -4
CONST 1
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
LDLW -4
LDNW 4
CONST 4
OFFSET
CONST 0
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
LDNW 4
STIW
CONST 4
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
LDLW -4
LDNW 4
CONST 4
OFFSET
CONST 1
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
LDNW 4
STIW
CONST 9
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
LDLW -4
LDNW 4
CONST 4
OFFSET
CONST 2
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
LDNW 4
STIW
CONST 0
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
STLW -8
JUMP 16
LABEL 15
LDLW -4
LDNW 4
CONST 4
OFFSET
LDLW -8
LDNW 4
LDIW
GLOBAL Output.Int
CALLW 1
GLOBAL g14
CONST 2
GLOBAL baozi.makeString
CALLW 2
GLOBAL Output.String
CALLW 1
CONST 1
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
LDLW -8
DUP 0
LOADW
LDNW 28
CALLW 2
STLW -8
LABEL 16
LDLW -4
LDNW 8
LDLW -8
DUP 0
LOADW
LDNW 56
CALLW 2
LDNW 4
JNEQZ 15
LABEL 17
GLOBAL Output.Ln
CALLW 0
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

DEFINE IntVec.%desc
WORD 0
WORD IntVec.%anc
WORD IntVec.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print

DEFINE IntVec.%anc
WORD 2
WORD IntVec.%desc
WORD Array.%desc

DEFINE IntVec.%string
STRING 496E7456656300

DEFINE g14
STRING 2C2000

PROC MAIN 0 0 0x0
GLOBAL Object.%desc
CONST 4
GLOBAL baozi.make
CALLW 2
STGW Nil
GLOBAL Program.Main
CALLW 0
RETURN
END
! End
