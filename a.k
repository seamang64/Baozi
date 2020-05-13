MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC Program.Main 4 0 0x10001
!         xs : Array Of Array Of Int = New Array Of Array Of Int<-[3].
LINE 5
GLOBAL baozi.%const.%3
GLOBAL baozi.makeArray
CALLW 1
STLW -4
!         xs->[2] = New Array Of Int<-[4].
LINE 6
GLOBAL baozi.%const.%4
GLOBAL baozi.makeArray
CALLW 1
LDLW -4
LDNW 4
STNW 8
!         xs->[2]->[1] = 14.
LINE 7
CONST 14
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
LDLW -4
LDNW 4
LDNW 8
LDNW 4
STNW 4
!         Output->IntLn<-{xs->[2]->[1]}.
LINE 8
LDLW -4
LDNW 4
LDNW 8
LDNW 4
LDNW 4
GLOBAL Output.IntLn
CALLW 1
!         Return.
LINE 9
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

PROC MAIN 0 0 0x0
GLOBAL Program.Main
CALLW 0
RETURN
END
! End
