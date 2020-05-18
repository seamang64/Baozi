MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC Program.Main 0 0 0x0
! 		Output->IntLn<-{5 + 4 * 2}.
LINE 5
GLOBAL baozi.%const.%2
GLOBAL baozi.%const.%4
DUP 0
LOADW
LDNW 32
CALLW 2
GLOBAL baozi.%const.%5
DUP 0
LOADW
LDNW 28
CALLW 2
GLOBAL Output.IntLn
CALLW 1
! 		Return.
LINE 6
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
WORD Object.%constructor
WORD Program.Main

! Ancestor Table for Program
DEFINE Program.%anc
WORD 2
WORD Program.%desc
WORD Object.%desc

! String "Program"
DEFINE Program.%string
STRING 50726F6772616D00

PROC MAIN 0 0 0x0
GLOBAL Program.Main
CALLW 0
RETURN
END
! End
