MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC Program.Main 4 0 0x10001
!         l : List With (A, A) = New List With (A, A).
LINE 5
GLOBAL List.%desc
CONST 16
GLOBAL baozi.make
CALLW 2
STLW -4
!         Return.
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

! Descriptor for List
DEFINE List.%desc
WORD 0xF
WORD List.%anc
WORD List.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print
WORD Object.%constructor

! Ancestor Table for List
DEFINE List.%anc
WORD 2
WORD List.%desc
WORD Object.%desc

! String "List"
DEFINE List.%string
STRING 4C69737400

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
WORD 0
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
