MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC A.DoNothing 0 0 0x300001
!     Return.
LINE 5
RETURN
END

PROC B.DoNothing 0 0 0x300001
!     Return.
LINE 13
RETURN
END

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
WORD A.DoNothing

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
WORD B.DoNothing

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
GLOBAL 
CALLW 0
RETURN
END
! End
