MODULE mdp
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!
!
!   Purpose:      Variable and data storage for program MD
!
!
!   Record of revision:
!
!      Date          Programmer            
!    ========      ==============     =====================
!
!    02/12/16           tc              
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!

IMPLICIT NONE
SAVE
CONTAINS
SUBROUTINE create_mdp 

!****************************************************************
!****************************************************************
                                                                
CHARACTER(LEN=128) :: xyz_arg, mdp_arg, trj_arg, vel_arg                                              
CHARACTER(LEN=128) :: em_arg                                              

OPEN(UNIT=21, FILE='MD.mdp', STATUS='NEW', ACTION='WRITE')
WRITE(21,'(a)') "temp        =    298.15    ! in K"
WRITE(21,'(a)') "int_step    =    1.0       ! integration step in fs"
WRITE(21,'(a)') "max_steps   =    500       ! maximum number of steps"

WRITE(*,*) 'MESSAGE: .mdp file generated!'


END SUBROUTINE create_mdp
END MODULE mdp

