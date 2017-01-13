PROGRAM test

USE test_module
IMPLICIT NONE
INTEGER(KIND=4) :: num, N

num = COMMAND_ARGUMENT_COUNT()
IF (num == 0) THEN 
   WRITE(*,*) "No arguments found!"
END IF

ALLOCATE(argument(num))

DO N=1,num
CALL GET_COMMAND_ARGUMENT(N, argument(N))
END DO

WRITE(*,*) argument(1)

IF (num > 2) THEN
WRITE(*,*) argument(3)
END IF

CALL writer

END PROGRAM test
