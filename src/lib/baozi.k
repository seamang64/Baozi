MODULE Baozi 0 0
IMPORT Lib 0
ENDHDR

DEFINE Nil
WORD 0

PROC baozi.%make 0 0 0
LDLW 12
LDLW 16
CONST 0
GLOBAL lib.newgc
PCALLW 2
DUP 0
CONST 0
LABEL 1
SWAP
CONST 4
OFFSET
DUP 0
GLOBAL Nil
SWAP
STOREW
SWAP
CONST 4
PLUS
DUP 0
LDLW 12
JEQ 1
DUP 2
RETURNW
END

! Procedure for creating a new primitive object
PROC baozi.%makePrim 0 0 0
! Need 8 bytes for a primitive
CONST 8
! Load the class pointer
LDLW 12
! Allocate the memory
CONST 0
GLOBAL lib.newgc
PCALLW 2
DUP 0
! Store the primitve value at offset 4
LDLW 16
SWAP
STNW 4
! Return the primitive
RETURNW
END

PROC baozi.%makeInt 0 0 0
LDLW 12
CONST 10
JLEQ 10
LABEL 11
LDLW 12
GLOBAL Integer.%desc
GLOBAL baozi.%makePrim
CALLW 2
RETURNW
LABEL 10
LDLW 12
CONST -10
JLT 11
GLOBAL boazi.%const.%ints
LDLW 12
LDIW
RETURNW
END

! Procedure for allocating and initialising a new array object
PROC baozi.%makeArray 4 0 0x110001
! 	Allocate 12 bytes for Array
CONST 12
GLOBAL Array.%desc
CONST 0
GLOBAL lib.newgc
PCALLW 2
STLW -4
! 	Create the body of the Array
LDLW 12
LDNW 4
GLOBAL baozi.%makeArrayData
CALLW 1
!	Offset, so we are pointing to the first element
CONST 4
OFFSET
LDLW -4
STNW 4
! 	Store the length in the array
LDLW 12
LDLW -4
STNW 8
! 	Return
LDLW -4
RETURNW
END

! Procedure for allocating and initialising the elements and GC map in an array
PROC baozi.%makeArrayData 0 0 0
! 	Allocate 4*n + 32 bytes of memory
LDLW 12
CONST 4
TIMES
CONST 32
PLUS
CONST 0
CONST 0
GLOBAL lib.newgc
PCALLW 2
DUP 0
! 	Store a pointer to p+4n+4 at p
DUP 0
LDLW 12
CONST 4
TIMES
CONST 4
PLUS
OFFSET
SWAP
STOREW
! 	Loop through the elements of the array storing Nil
DUP 0
CONST 0
JUMP 9
LABEL 8
SWAP
CONST 4
OFFSET
DUP 0
GLOBAL Nil
SWAP
STOREW
SWAP
CONST 1
PLUS
LABEL 9
DUP 0
LDLW 12
JNEQ 8
POP 1
! 	Store a pointer to p+4n+8 at p+4n+4
DUP 0
CONST 8
OFFSET
DUP 1
STNW 4
! 	Store -4
CONST -4
DUP 1
STNW 8
! 	Store 4n
LDLW 12
CONST 4
TIMES
DUP 1
STNW 12
! 	Store GC_BLOCK
CONST 22
DUP 1
STNW 16
! 	Store 0
CONST 0
DUP 1
STNW 20
! 	Store n
LDLW 12
DUP 1
STNW 24
! 	Store GC_END
CONST 10
DUP 1
STNW 28

POP 1
RETURNW
END

PROC baozi.%makeString 0 0 0x200001
CONST 12
GLOBAL String.%desc
CONST 0
GLOBAL lib.newgc
PCALLW 2
DUP 0
CONST 4
OFFSET
LDLW 12
GLOBAL Integer.%desc
GLOBAL baozi.%makePrim
STKMAP 0x19
CALLW 2
SWAP
STOREW
DUP 0
CONST 8
OFFSET
LDLW 16
SWAP
STOREW
RETURNW
END

PROC baozi.%typeCheck 8 0 0x310001
CONST 0
LDLW 16
LOADW
LDNW 4
DUP 0
LOADW
STLW -4

LABEL 5
SWAP
DUP 0
LDLW -4
JEQ 7
CONST 1
PLUS
SWAP
CONST 4
OFFSET
DUP 0
LOADW
LDLW 12
JNEQ 5

LABEL 6
RETURN

LABEL 7
ERROR 1 0
RETURN
END

PROC baozi.%print 0 0 0x100001
LDLW 12
CONST 0
GLOBAL lib.print_string
PCALLW 1
RETURN
END

PROC baozi.%println 0 0 0x100001
LDLW 12
CONST 0
GLOBAL lib.print_string
PCALLW 1
GLOBAL lib.newline
PCALLW 0
RETURN
END

PROC baozi.%printnum 0 0 0x100001
LDLW 12
CONST 0
GLOBAL lib.print_num
PCALLW 1
RETURN
END

PROC baozi.%printnumln 0 0 0x100001
LDLW 12
CONST 0
GLOBAL lib.print_num
PCALLW 1
GLOBAL lib.newline
PCALLW 0
RETURN
END

DEFINE Object.%desc
WORD 0
WORD Object.%anc
WORD Object.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print

DEFINE Object.%anc
WORD 1
WORD Object.%desc

DEFINE Object.%string
STRING 4F626A65637400

DEFINE Integer.%desc
WORD 0
WORD Integer.%anc
WORD Integer.%string
WORD Integer.equals
WORD Object.GetType
WORD Object.Is
WORD Integer.Print
WORD Integer.add
WORD Integer.times
WORD Integer.sub
WORD Integer.div
WORD Integer.mod
WORD Integer.uminus
WORD Integer.notEquals
WORD Integer.lessThan
WORD Integer.greaterThan
WORD Integer.lessThanEq
WORD Integer.greaterThanEq

DEFINE Integer.%anc
WORD 2
WORD Integer.%desc
WORD Object.%desc

DEFINE Integer.%string
STRING 496E7400

DEFINE Bool.%desc
WORD 0
WORD Bool.%anc
WORD Bool.%string
WORD Bool.equals
WORD Object.GetType
WORD Object.Is
WORD Bool.Print
WORD Bool.and
WORD Bool.or
WORD Bool.not

DEFINE Bool.%anc
WORD Bool.%desc
WORD Object.%desc

DEFINE Bool.%string
STRING 426F6F6C00

DEFINE Bool.TrueString
STRING 5472756500

DEFINE Bool.FalseString
STRING 46616C736500

DEFINE Array.%desc
WORD 0x7
WORD Array.%anc
WORD Array.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print

DEFINE Array.%anc
WORD 2
WORD Array.%desc
WORD Object.%desc

DEFINE Array.%string
STRING 417272617900

DEFINE Type.%desc
WORD 0
WORD Type.%anc
WORD Type.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD Object.Print

DEFINE Type.%anc
WORD 2
WORD Type.%desc
WORD Object.%desc

DEFINE Type.%string
STRING 5479706500

DEFINE String.%desc
WORD 3
WORD String.%anc
WORD String.%string
WORD Object.equals
WORD Object.GetType
WORD Object.Is
WORD String.Print

DEFINE String.%anc
WORD 2
WORD String.%desc
WORD Object.%desc

DEFINE String.%string
STRING 537472696E6700

DEFINE Output.%desc
WORD 0
WORD 1
WORD Output.Int
WORD Output.IntLn
WORD Output.String
WORD Output.StringLn
WORD Output.Ln

PROC Object.equals 0 0 0x300001
LDLW 12
LDLW 16
JEQ 12
GLOBAL baozi.%const.%false
RETURNW
LABEL 12
GLOBAL baozi.%const.%true
RETURNW
END

PROC Object.GetType 0 0 0x100001
LDLW 12
LOADW
GLOBAL Type.%desc
GLOBAL baozi.%makePrim
CALLW 2
RETURNW
END

PROC Object.Is 8 0 0x310001
LDLW 16
LDNW 4
STLW -4
CONST 0
LDLW 12
LOADW
LDNW 4
DUP 0
LOADW
STLW -8
LABEL 5
SWAP
DUP 0
LDLW -8
JEQ 7
CONST 1
PLUS
SWAP
CONST 4
OFFSET
DUP 0
LOADW
LDLW -4
JNEQ 5
LABEL 6
GLOBAL baozi.%const.%true
RETURNW
LABEL 7
GLOBAL baozi.%const.%false
RETURNW
END

PROC Object.Print 0 0 0x100001
LDLW 12
LOADW
LDNW 8
CONST 0
GLOBAL lib.print_string
PCALLW 1
RETURNW
END

PROC Object.%constructor 0 0 0x100001
LDLW 12
RETURNW
END

PROC Integer.equals 0 0 0x300001
LDLW 12
LDNW 4
LDLW 16
LDNW 4
JEQ 13
GLOBAL baozi.%const.%false
RETURNW
LABEL 13
GLOBAL baozi.%const.%true
RETURNW
END

PROC Integer.Print 0 0 0x186A1
LDLW 12
LDNW 4
CONST 0
GLOBAL lib.print_num
PCALLW 1
RETURN
END

PROC Integer.add 0 0 0x300001
LDLW 12
LDNW 4
LDLW 16
LDNW 4
PLUS
GLOBAL baozi.%makeInt
CALLW 1
RETURNW
END

PROC Integer.times 0 0 0x300001
LDLW 12
LDNW 4
LDLW 16
LDNW 4
TIMES
GLOBAL baozi.%makeInt
CALLW 1
RETURNW
END

PROC Integer.sub 0 0 0x300001
LDLW 12
LDNW 4
LDLW 16
LDNW 4
MINUS
GLOBAL baozi.%makeInt
CALLW 1
RETURNW
END

PROC Integer.div 0 0 0x300001
LDLW 12
LDNW 4
LDLW 16
LDNW 4
DIV
GLOBAL baozi.%makeInt
CALLW 1
RETURNW
END

PROC Integer.mod 0 0 0x300001
LDLW 12
LDNW 4
LDLW 16
LDNW 4
MOD
GLOBAL baozi.%makeInt
CALLW 1
RETURNW
END

PROC Integer.uminus 0 0 0x100001
LDLW 12
LDNW 4
UMINUS
GLOBAL baozi.%makeInt
CALLW 1
RETURNW
END

PROC Integer.notEquals 0 0 0x300001
LDLW 12
LDNW 4
LDLW 16
LDNW 4
JNEQ 14
GLOBAL baozi.%const.%false
RETURNW
LABEL 14
GLOBAL baozi.%const.%true
RETURNW
END

PROC Integer.lessThan 0 0 0x300001
LDLW 12
LDNW 4
LDLW 16
LDNW 4
JLT 14
GLOBAL baozi.%const.%false
RETURNW
LABEL 14
GLOBAL baozi.%const.%true
RETURNW
END

PROC Integer.greaterThan 0 0 0x300001
LDLW 12
LDNW 4
LDLW 16
LDNW 4
JGT 15
GLOBAL baozi.%const.%false
RETURNW
LABEL 15
GLOBAL baozi.%const.%true
RETURNW
END

PROC Integer.lessThanEq 0 0 0x300001
LDLW 12
LDNW 4
LDLW 16
LDNW 4
JLEQ 16
GLOBAL baozi.%const.%false
RETURNW
LABEL 16
GLOBAL baozi.%const.%true
RETURNW
END

PROC Integer.greaterThanEq 0 0 0x300001
LDLW 12
LDNW 4
LDLW 16
LDNW 4
JGEQ 17
GLOBAL baozi.%const.%false
RETURNW
LABEL 17
GLOBAL baozi.%const.%true
RETURNW
END

PROC Bool.equals 4 0 0x300001
LDLW 12
LDNW 4
LDLW 16
LDNW 4
JEQ 18
GLOBAL baozi.%const.%false
RETURNW
LABEL 18
GLOBAL baozi.%const.%true
RETURNW
END

PROC Bool.Print 4 0 0x100001
LDLW 12
LDNW 4
JEQZ 4
GLOBAL Bool.TrueString
CONST 0
GLOBAL lib.print_string
PCALLW 1
RETURN
LABEL 4
GLOBAL Bool.FalseString
CONST 0
GLOBAL lib.print_string
PCALLW 1
RETURN
END

PROC Bool.and 4 0 0x300001
LDLW 12
LDNW 4
LDLW 16
LDNW 4
AND
JEQZ 19
GLOBAL baozi.%const.%true
RETURNW
LABEL 19
GLOBAL baozi.%const.%false
RETURNW
END

PROC Bool.or 4 0 0x300001
LDLW 12
LDNW 4
LDLW 16
LDNW 4
OR
JEQZ 20
GLOBAL baozi.%const.%true
RETURNW
LABEL 20
GLOBAL baozi.%const.%false
RETURNW
END

PROC Bool.not 4 0 0x100001
LDLW 12
LDNW 4
JEQZ 21
GLOBAL baozi.%const.%false
RETURNW
LABEL 21
GLOBAL baozi.%const.%true
RETURNW
END

PROC String.Print 0 0 0x100001
LDLW 12
GLOBAL Output.String
CALLW 1
RETURN
END

PROC Output.Int 0 0 0x100001
LDLW 12
LDNW 4
CONST 0
GLOBAL lib.print_num
PCALLW 1
RETURN
END

PROC Output.IntLn 0 0 0x100001
LDLW 12
LDNW 4
CONST 0
GLOBAL lib.print_num
PCALLW 1
GLOBAL lib.newline
PCALLW 0
RETURN
END

PROC Output.String 0 0 0x100001
LDLW 12
LDNW 8
CONST 0
GLOBAL lib.print_string
PCALLW 1
RETURN
END

PROC Output.StringLn 0 0 0x100001
LDLW 12
LDNW 8
CONST 0
GLOBAL lib.print_string
PCALLW 1
GLOBAL lib.newline
PCALLW 0
RETURN
END

PROC Output.Ln 0 0 0x0
CONST 0
GLOBAL lib.newline
PCALLW 0
RETURN
END

! Constant 0
DEFINE baozi.%const.%0
WORD Integer.%desc
WORD 0

! Constant 1
DEFINE baozi.%const.%1
WORD Integer.%desc
WORD 1

! Constant 2
DEFINE baozi.%const.%2
WORD Integer.%desc
WORD 2

! Constant 3
DEFINE baozi.%const.%3
WORD Integer.%desc
WORD 3

! Constant 4
DEFINE baozi.%const.%4
WORD Integer.%desc
WORD 4

! Constant 5
DEFINE baozi.%const.%5
WORD Integer.%desc
WORD 5

! Constant 6
DEFINE baozi.%const.%6
WORD Integer.%desc
WORD 6

! Constant 7
DEFINE baozi.%const.%7
WORD Integer.%desc
WORD 7

! Constant 8
DEFINE baozi.%const.%8
WORD Integer.%desc
WORD 8

! Constant 9
DEFINE baozi.%const.%9
WORD Integer.%desc
WORD 9

! Constant 10
DEFINE baozi.%const.%10
WORD Integer.%desc
WORD 10

! Constant -1
DEFINE baozi.%const.%-1
WORD Integer.%desc
WORD -1

! Constant -2
DEFINE baozi.%const.%-2
WORD Integer.%desc
WORD -2

! Constant -3
DEFINE baozi.%const.%-3
WORD Integer.%desc
WORD -3

! Constant -4
DEFINE baozi.%const.%-4
WORD Integer.%desc
WORD -4

! Constant -5
DEFINE baozi.%const.%-5
WORD Integer.%desc
WORD -5

! Constant -6
DEFINE baozi.%const.%-6
WORD Integer.%desc
WORD -6

! Constant -7
DEFINE baozi.%const.%-7
WORD Integer.%desc
WORD -7

! Constant -8
DEFINE baozi.%const.%-8
WORD Integer.%desc
WORD -8

! Constant -9
DEFINE baozi.%const.%-9
WORD Integer.%desc
WORD -9

! Constant -10
DEFINE baozi.%const.%-10
WORD Integer.%desc
WORD -10

! Constant false
DEFINE baozi.%const.%false
WORD Bool.%desc
WORD 0

! Constant true
DEFINE baozi.%const.%true
WORD Bool.%desc
WORD 1

WORD baozi.%const.%-10
WORD baozi.%const.%-9
WORD baozi.%const.%-8
WORD baozi.%const.%-7
WORD baozi.%const.%-6
WORD baozi.%const.%-5
WORD baozi.%const.%-4
WORD baozi.%const.%-3
WORD baozi.%const.%-2
WORD baozi.%const.%-1
DEFINE boazi.%const.%ints
WORD baozi.%const.%0
WORD baozi.%const.%1
WORD baozi.%const.%2
WORD baozi.%const.%3
WORD baozi.%const.%4
WORD baozi.%const.%5
WORD baozi.%const.%6
WORD baozi.%const.%7
WORD baozi.%const.%8
WORD baozi.%const.%9
WORD baozi.%const.%10