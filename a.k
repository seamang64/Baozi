MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC Program.Main 4 0 0x10001
!         bs : Array Of Array Of A = New Array Of Array Of B<-[3].
LINE 5
GLOBAL array.%array.%B.%desc
GLOBAL baozi.%const.%3
GLOBAL baozi.%makeArray
CALLW 2
STLW -4
!         bs->[0] = New Array Of C<-[2].
LINE 6
GLOBAL array.%C.%desc
GLOBAL baozi.%const.%2
GLOBAL baozi.%makeArray
CALLW 2
LDLW -4
DUP 1
GLOBAL baozi.%typeCheck
STKMAP 0x9
CALLW 2
LDNW 4
STOREW
!         Return.
LINE 7
RETURN
END

! Descriptor for Program
DEFINE Program.%desc
WORD 0
WORD Program.%anc
WORD Program.%string
WORD 0
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

! Descriptor for A
DEFINE A.%desc
WORD 0
WORD A.%anc
WORD A.%string
WORD 0
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
WORD 0
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

! Descriptor for C
DEFINE C.%desc
WORD 0
WORD C.%anc
WORD C.%string
WORD 0
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print
WORD Object.%constructor

! Ancestor Table for C
DEFINE C.%anc
WORD 3
WORD C.%desc
WORD A.%desc
WORD Object.%desc

! String "C"
DEFINE C.%string
STRING 4300

! Descriptor for D
DEFINE D.%desc
WORD 0
WORD D.%anc
WORD D.%string
WORD 0
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print
WORD Object.%constructor

! Ancestor Table for D
DEFINE D.%anc
WORD 4
WORD D.%desc
WORD B.%desc
WORD A.%desc
WORD Object.%desc

! String "D"
DEFINE D.%string
STRING 4400

! Descriptor for Array of A
DEFINE array.%A.%desc
WORD 0xF
WORD array.%A.%anc
WORD array.%A.%string
WORD A.%desc
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print

! Ancestor table for Array of A
DEFINE array.%A.%anc
WORD 3
WORD array.%A.%desc
WORD array.%Object.%desc
WORD Object.%desc

! String "Array of A"
DEFINE array.%A.%string
STRING 4172726179206F66204100

! Descriptor for Array of B
DEFINE array.%B.%desc
WORD 0xF
WORD array.%B.%anc
WORD array.%B.%string
WORD B.%desc
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print

! Ancestor table for Array of B
DEFINE array.%B.%anc
WORD 4
WORD array.%B.%desc
WORD array.%A.%desc
WORD array.%Object.%desc
WORD Object.%desc

! String "Array of B"
DEFINE array.%B.%string
STRING 4172726179206F66204200

! Descriptor for Array of C
DEFINE array.%C.%desc
WORD 0xF
WORD array.%C.%anc
WORD array.%C.%string
WORD C.%desc
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print

! Ancestor table for Array of C
DEFINE array.%C.%anc
WORD 4
WORD array.%C.%desc
WORD array.%A.%desc
WORD array.%Object.%desc
WORD Object.%desc

! String "Array of C"
DEFINE array.%C.%string
STRING 4172726179206F66204300

! Descriptor for Array of Object
DEFINE array.%Object.%desc
WORD 0xF
WORD array.%Object.%anc
WORD array.%Object.%string
WORD Object.%desc
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print

! Ancestor table for Array of Object
DEFINE array.%Object.%anc
WORD 2
WORD array.%Object.%desc
WORD Object.%desc

! String "Array of Object"
DEFINE array.%Object.%string
STRING 4172726179206F66204F626A65637400

! Descriptor for Array of Array of A
DEFINE array.%array.%A.%desc
WORD 0xF
WORD array.%array.%A.%anc
WORD array.%array.%A.%string
WORD array.%A.%desc
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print

! Ancestor table for Array of Array of A
DEFINE array.%array.%A.%anc
WORD 3
WORD array.%array.%A.%desc
WORD array.%array.%Object.%desc
WORD Object.%desc

! String "Array of Array of A"
DEFINE array.%array.%A.%string
STRING 4172726179206F66204172726179206F66204100

! Descriptor for Array of Array of B
DEFINE array.%array.%B.%desc
WORD 0xF
WORD array.%array.%B.%anc
WORD array.%array.%B.%string
WORD array.%B.%desc
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print

! Ancestor table for Array of Array of B
DEFINE array.%array.%B.%anc
WORD 4
WORD array.%array.%B.%desc
WORD array.%array.%A.%desc
WORD array.%array.%Object.%desc
WORD Object.%desc

! String "Array of Array of B"
DEFINE array.%array.%B.%string
STRING 4172726179206F66204172726179206F66204200

! Descriptor for Array of Array of Object
DEFINE array.%array.%Object.%desc
WORD 0xF
WORD array.%array.%Object.%anc
WORD array.%array.%Object.%string
WORD array.%Object.%desc
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print

! Ancestor table for Array of Array of Object
DEFINE array.%array.%Object.%anc
WORD 2
WORD array.%array.%Object.%desc
WORD Object.%desc

! String "Array of Array of Object"
DEFINE array.%array.%Object.%string
STRING 4172726179206F66204172726179206F66204F626A65637400

! MAIN procedure
PROC MAIN 0 0 0x0
GLOBAL Program.Main
CALLW 0
RETURN
END
! End
