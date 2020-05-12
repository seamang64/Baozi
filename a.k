MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC Program.Main 8 0 0x18001
! 		xs : IntVec = New IntVec<-[3].
LINE 5
GLOBAL IntVec.%desc
GLOBAL baozi.%const.%3
GLOBAL baozi.makeArray
CALLW 2
STLW -4
! 		xs->[0] = 1.
LINE 6
GLOBAL baozi.%const.%1
LDLW -4
LDNW 4
STOREW
! 		xs->[1] = 4.
LINE 7
GLOBAL baozi.%const.%4
LDLW -4
LDNW 4
STNW 4
! 		xs->[2] = 9.
LINE 8
GLOBAL baozi.%const.%9
LDLW -4
LDNW 4
STNW 8
! 		<$
LINE 14
GLOBAL baozi.%const.%0
STLW -8
JUMP 16
LABEL 15
! 			Output->Int<-{xs->[i]}.
LINE 12
LDLW -4
LDNW 4
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
GLOBAL baozi.%const.%1
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
CONST 0
GLOBAL lib.start_clock
PCALLW 0
GLOBAL Program.Main
CALLW 0
CONST 0
GLOBAL lib.end_clock
PCALLW 0
RETURN
END
! End
