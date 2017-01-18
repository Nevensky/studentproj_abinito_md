! writetest

SUBROUTINE numbering

    USE ARGUMENTS
    USE VARS

    IMPLICIT NONE

    INTEGER :: ios, i
    INTEGER (KIND=0) :: blank_0
    CHARACTER(LEN=128) :: check_line

    blank_0=0
    i=0

    OPEN (UNIT=56, FILE=xyz_arg, iostat=ios, STATUS="OLD")
    IF (ios /= 0) STOP "Error in opening file"

    DO WHILE (blank_0 == 0)
        READ(56,"(a128)") check_line
        blank_0 = SCAN (check_line, "aeiour123456789i0!")
        IF (blank_0 == 0) THEN
            i = i + 1 
         END IF
   END DO
REWIND(56)

DO N=1, i+1
    READ (56,*)
END DO

! DO N=1, n_atoms
!     IF 



