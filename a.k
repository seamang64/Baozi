MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC Program.Main 4 0 0x10001
!         l : List With (Int) = New List With (Int).
LINE 5
GLOBAL List.%desc
CONST 12
GLOBAL baozi.make
CALLW 2
STLW -4
!         l->value = 12.
LINE 6
CONST 12
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
LDLW -4
STNW 4
!         l->next = New List With (Int).
LINE 7
GLOBAL List.%desc
CONST 12
GLOBAL baozi.make
CALLW 2
LDLW -4
STNW 8
!         Return.
LINE 8
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
WORD 0x7
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

PROC MAIN 0 0 0x0
GLOBAL Program.Main
CALLW 0
RETURN
END
! End
