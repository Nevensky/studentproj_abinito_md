! normalno raspodjeljeni slucajni brojevi
! prema https://en.wikipedia.org/wiki/Box–Muller_transform

MODULE gaussrand
    IMPLICIT NONE
    SAVE
    CONTAINS

    SUBROUTINE randn(x_1,x_2)
      integer,parameter :: dp=kind(1.0d0)
      real(dp),parameter :: pi=4.*ATAN(1.)
      real(dp) :: u_1,u_2
      real(dp),intent(out) :: x_1,x_2
      real(dp) :: mean=0.,sigma=1.
      ! sigma ce ovisit o MB rasp, dodatni naknadno

      CALL RANDOM_SEED()
      CALL RANDOM_NUMBER(u_1)
      CALL RANDOM_NUMBER(u_2)
      x_1 = SQRT(-2.*LOG(u_1))*COS(2.*pi*u_2)
      x_2 = SQRT(-2.*LOG(u_1))*SIN(2.*pi*u_2)
      x_1 = sigma*x_1+mean
      x_2 = sigma*x_2+mean
      ! PRINT *,'x_1 ',x_1
      ! PRINT *,'x_2 ',x_2
    END SUBROUTINE randn
END MODULE gaussrand