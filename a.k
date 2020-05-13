MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC Program.Main 4 0 0x10001
!         v : ThreeDVector = New ThreeDVector<-{5, 9, 12}.
LINE 5
CONST 12
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
GLOBAL baozi.%const.%9
GLOBAL baozi.%const.%5
GLOBAL ThreeDVector.%desc
CONST 16
GLOBAL baozi.make
STKMAP 0x39
CALLW 2
DUP 0
LOADW
LDNW 28
CALLW 4
STLW -4
!         Output->String<-{"Created 3D Vector with: x = "}.
LINE 6
GLOBAL g14
GLOBAL baozi.print
CALLW 1
!         Output->Int<-{v->x}.
LINE 7
LDLW -4
LDNW 4
GLOBAL Output.Int
CALLW 1
!         Output->String<-{", y = "}.
LINE 8
GLOBAL g15
GLOBAL baozi.print
CALLW 1
!         Output->Int<-{v->y}.
LINE 9
LDLW -4
LDNW 8
GLOBAL Output.Int
CALLW 1
!         Output->String<-{", z = "}.
LINE 10
GLOBAL g16
GLOBAL baozi.print
CALLW 1
!         Output->IntLn<-{v->z}.
LINE 11
LDLW -4
LDNW 12
GLOBAL Output.IntLn
CALLW 1
!         Return.
LINE 12
RETURN
END

PROC Vector.%constructor 0 0 0x700001
!         My->x = a.
LINE 26
LDLW 16
LDLW 12
STNW 4
!         My->y = b.
LINE 27
LDLW 20
LDLW 12
STNW 8
!         Return Me.
LINE 28
LDLW 12
RETURNW
END

PROC ThreeDVector.%constructor 0 0 0xF00001
!         Parent<-{a, b}.
LINE 41
LDLW 20
LDLW 16
LDLW 12
DUP 0
LOADW
LDNW 4
LDNW 8
LDNW 28
CALLW 3
!         My->z = c.
LINE 42
LDLW 24
LDLW 12
STNW 12
!         Return Me.
LINE 43
LDLW 12
RETURNW
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

! Descriptor for Vector
DEFINE Vector.%desc
WORD 0x7
WORD Vector.%anc
WORD Vector.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print
WORD Vector.%constructor

! Ancestor Table for Vector
DEFINE Vector.%anc
WORD 2
WORD Vector.%desc
WORD Object.%desc

! String "Vector"
DEFINE Vector.%string
STRING 566563746F7200

! Descriptor for ThreeDVector
DEFINE ThreeDVector.%desc
WORD 0xF
WORD ThreeDVector.%anc
WORD ThreeDVector.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print
WORD ThreeDVector.%constructor

! Ancestor Table for ThreeDVector
DEFINE ThreeDVector.%anc
WORD 3
WORD ThreeDVector.%desc
WORD Vector.%desc
WORD Object.%desc

! String "ThreeDVector"
DEFINE ThreeDVector.%string
STRING 546872656544566563746F7200

! String ", z = "
DEFINE g16
STRING 2C207A203D2000

! String ", y = "
DEFINE g15
STRING 2C2079203D2000

! String "Created 3D Vector with: x = "
DEFINE g14
STRING 4372656174656420334420566563746F7220776974683A2078203D2000

PROC MAIN 0 0 0x0
GLOBAL Program.Main
CALLW 0
RETURN
END
! End
