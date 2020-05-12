MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC Program.Main 8 0 0x18001
! 		xs : IntVec = New IntVec<-[3].
LINE 5
GLOBAL IntVec.%desc
CONST 3
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
STKMAP 0x9
CALLW 2
GLOBAL baozi.makeArray
CALLW 2
STLW -4
! 		xs->[0] = 1.
LINE 6
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
STKMAP 0x19
CALLW 2
LDNW 4
STIW
! 		xs->[1] = 4.
LINE 7
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
STKMAP 0x19
CALLW 2
LDNW 4
STIW
! 		xs->[2] = 9.
LINE 8
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
STKMAP 0x19
CALLW 2
LDNW 4
STIW
! 		For i : Int = 0. Step i = i + 1. Test i < xs->Length.
LINE 10
CONST 0
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
STLW -8
JUMP 16
LABEL 15
! 			Output->Int<-{xs->[i]}.
LINE 12
LDLW -4
LDNW 4
CONST 4
OFFSET
LDLW -8
LDNW 4
LDIW
GLOBAL Output.Int
CALLW 1
! 			Output->String<-{", "}.
LINE 13
GLOBAL g14
CONST 2
GLOBAL baozi.makeString
CALLW 2
GLOBAL Output.String
CALLW 1
! 		For i : Int = 0. Step i = i + 1. Test i < xs->Length.
LINE 10
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
! 		Output->Ln<-{}.
LINE 15
GLOBAL Output.Ln
CALLW 0
! 		Return.
LINE 16
RETURN
END

! Descriptor for Program
DEFINE Program.%desc
WORD 0
WORD Program.%anc
WORD Program.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print
WORD Program.Main

! Ancestor Table for Program
DEFINE Program.%anc
WORD 2
WORD Program.%desc
WORD Object.%desc

! String "Program"
DEFINE Program.%string
STRING 50726F6772616D00

! Descriptor for IntVec
DEFINE IntVec.%desc
WORD 0x7
WORD IntVec.%anc
WORD IntVec.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print

! Ancestor Table for IntVec
DEFINE IntVec.%anc
WORD 3
WORD IntVec.%desc
WORD Array.%desc
WORD Object.%desc

! String "IntVec"
DEFINE IntVec.%string
STRING 496E7456656300

! String ", "
DEFINE g14
STRING 2C2000

PROC MAIN 0 0 0x0
CONST g2
CONST 0
GLOBAL lib.gc_debug
PCALL 1
GLOBAL Program.Main
CALLW 0
RETURN
END

! String "sz"
DEFINE g2
STRING 737a00
! End