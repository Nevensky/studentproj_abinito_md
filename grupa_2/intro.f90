!Host 193.198.195.249
!User tc
!Password tc999iwq678 

PROGRAM UGA
   IMPLICIT NONE 

INTEGER :: A = 1, B=1

DO WHILE (A<10)
   PRINT*,A
   A=A+1
   DO WHILE (B<20)
      PRINT*,B
      B=B+1
  END DO
END DO

!PRINT *,A

END PROGRAM