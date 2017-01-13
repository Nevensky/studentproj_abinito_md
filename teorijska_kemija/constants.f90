MODULE constants
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!
!
!   Purpose:      Module which includes all frequently used constants
!
!
!   Record of revision:
!
!      Date          Programmer         Revision   
!    ========      ==============     =====================
!   21/07/2016       Karlo Sovic      Original code
!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!

IMPLICIT NONE

REAL(KIND=8) :: PI = 4*ATAN(1.0_8)

REAL(KIND=8), PARAMETER :: Kb = 1.38065D-23
REAL(KIND=8), PARAMETER :: h_planck = 6.626070D-34
REAL(KIND=8), PARAMETER :: u_mass = 1.660539040D-27


END MODULE constants
