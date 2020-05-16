MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC Program.Main 4 0 0x10001
!         l : List = New List.
LINE 5
GLOBAL List.%desc
CONST 12
GLOBAL baozi.make
CALLW 2
STLW -4
!         l->value = 15.
LINE 6
CONST 15
GLOBAL Integer.%desc
GLOBAL baozi.makePrim
CALLW 2
LDLW -4
STNW 4
!         Return.
LINE 7
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

! Descriptor for Collection
DEFINE Collection.%desc
WORD 0x7
WORD Collection.%anc
WORD Collection.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print
WORD Object.%constructor

! Ancestor Table for Collection
DEFINE Collection.%anc
WORD 2
WORD Collection.%desc
WORD Object.%desc

! String "Collection"
DEFINE Collection.%string
STRING 436F6C6C656374696F6E00

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
WORD 3
WORD List.%desc
WORD Collection.%desc
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

PROC MAIN 0 0 0x0
GLOBAL Program.Main
CALLW 0
RETURN
END
! End
