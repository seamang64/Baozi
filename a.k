MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC Program.Main 4 0 0x10001
!         x : A = New A.
LINE 5
GLOBAL A.%desc
CONST 4
GLOBAL baozi.%make
CALLW 2
STLW -4
!         Program->castMethod<-{x}.
LINE 8
LDLW -4
GLOBAL Program.castMethod
CALLW 1
!         Return.
LINE 9
RETURN
END

PROC Program.castMethod 0 0 0x100001
!         Output->IntLn<-{(Cast(a)->B)->p}.
LINE 14
LDLW 12
DUP 0
GLOBAL B.%desc
GLOBAL baozi.%typeCheck
STKMAP 0x9
CALL 2
LDNW 4
GLOBAL Output.IntLn
STKMAP 0x5
CALLW 1
!         Return.
LINE 15
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
WORD Program.castMethod

! Ancestor Table for Program
DEFINE Program.%anc
WORD 2
WORD Program.%desc
WORD Object.%desc

! String "Program"
DEFINE Program.%string
STRING 50726F6772616D00

! Descriptor for A
DEFINE A.%desc
WORD 0
WORD A.%anc
WORD A.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print
WORD Object.%constructor

! Ancestor Table for A
DEFINE A.%anc
WORD 2
WORD A.%desc
WORD Object.%desc

! String "A"
DEFINE A.%string
STRING 4100

! Descriptor for B
DEFINE B.%desc
WORD 0x3
WORD B.%anc
WORD B.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print
WORD Object.%constructor

! Ancestor Table for B
DEFINE B.%anc
WORD 3
WORD B.%desc
WORD A.%desc
WORD Object.%desc

! String "B"
DEFINE B.%string
STRING 4200

PROC MAIN 0 0 0x0
GLOBAL Program.Main
CALLW 0
RETURN
END
! End
