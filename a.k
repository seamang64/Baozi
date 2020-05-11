MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC Program.Main 4 0 0x10001
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
LDLW -4
LDNW 4
CONST 4
OFFSET
CONST 0
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
LDNW 4
LDIW
GLOBAL Output.IntLn
CALLW 1
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
WORD 0x7
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

PROC MAIN 0 0 0x0
CONST g2
CONST 0
GLOBAL lib.gc_debug
PCALL 1
GLOBAL Object.%desc
CONST 4
GLOBAL baozi.make
CALLW 2
STGW Nil
GLOBAL Program.Main
CALLW 0
RETURN
END

DEFINE g2
STRING 737a00
! END