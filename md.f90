 PROGRAM md
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!
!
!   Purpose:      This program performs molecular dynamics 
!                 
!              
!   Record of revision:
!
!      Date          Programmer        Revision
!    ========      ==============    =========================
!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!******** MODULES USED IN THIS PROGRAM:

USE arguments                                    !
USE mdp
USE vars
USE str
!***********************************


IMPLICIT NONE

LOGICAL :: file_exists
CHARACTER(LEN=128) :: argument
INTEGER(KIND=4) :: num_args=0, xyz_check=0


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


num_args = COMMAND_ARGUMENT_COUNT()

   IF (num_args == 0) THEN
           !CALL help_page
           WRITE(*,*) "No input arguments found!"
           STOP
   END IF

   IF (num_args > 1) THEN
           !CALL help_page
           WRITE(*,*) "Too much arguments found!"
           STOP
   END IF
 
     CALL GET_COMMAND_ARGUMENT( 1, argument)

                xyz_check = INDEX(argument, ".xyz")
                IF (xyz_check == 0) THEN
                    WRITE(*,*) "Wrong input argument"
                    STOP
                END IF

                         xyz_arg = argument
                         mdp_arg = "MD.mdp"
                         trj_arg = "MD.trj"
                         vel_arg = "MD.vel"
                         em_arg  = "EM.mop"
                        ! out_arg = xyz_arg(1:(xyz_check-1))//".out"
                        ! log_arg = xyz_arg(1:(xyz_check-1))//".log"

     INQUIRE(FILE=mdp_arg, EXIST=file_exists)
           IF (file_exists) THEN
                   CONTINUE
           ELSE
                    CALL create_mdp
                    STOP
           END IF

CALL structure_read
CALL ingen
CALL EXECUTE_COMMAND_LINE('mopac EM.mop')

WRITE(*,*) "Program completed!"
          STOP


END PROGRAM md
