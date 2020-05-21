MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC Program.Main 8 0 0x18001
!     x : Int = 12.
LINE 5
CONST 12
GLOBAL baozi.%makeInt
CALLW 1
STLW -4
!     r : Int = Program->twoPow<-{x}.
LINE 6
LDLW -4
GLOBAL Program.twoPow
CALLW 1
STLW -8
!     Output->String<-{"Result: "}.
LINE 7
GLOBAL g14
GLOBAL baozi.%print
CALLW 1
!     Output->IntLn<-{r}.
LINE 8
LDLW -8
GLOBAL Output.IntLn
CALLW 1
!     Return.
LINE 9
RETURN
END

PROC Program.twoPow 0 0 0x100001
!     Else Return Program->twoPow<-{x - 1} + Program->twoPow<-{x - 1}.
LINE 15
GLOBAL baozi.%const.%0
LDLW 12
LDNW 4
SWAP
LDNW 4
JNEQ 16
LABEL 15
!     If x == 0 Then Return 1.
LINE 14
GLOBAL baozi.%const.%1
RETURNW
LABEL 16
!     Else Return Program->twoPow<-{x - 1} + Program->twoPow<-{x - 1}.
LINE 15
GLOBAL baozi.%const.%1
LDLW 12
DUP 0
LOADW
LDNW 36
CALLW 2
GLOBAL Program.twoPow
CALLW 1
GLOBAL baozi.%const.%1
LDLW 12
DUP 0
LOADW
LDNW 36
STKMAP 0x9
CALLW 2
GLOBAL Program.twoPow
STKMAP 0x5
CALLW 1
DUP 0
LOADW
LDNW 28
CALLW 2
RETURNW
LABEL 17
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
WORD Program.twoPow

! Ancestor Table for Program
DEFINE Program.%anc
WORD 2
WORD Program.%desc
WORD Object.%desc

! String "Program"
DEFINE Program.%string
STRING 50726F6772616D00

! String "Result: "
DEFINE g14
STRING 526573756C743A2000

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
