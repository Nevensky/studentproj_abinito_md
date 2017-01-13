 PROGRAM md
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!
!
!   Purpose:      This program performs molecular dynamics. 
!                 
!              
!   Authors:      Students of Department of Chemistry, Faculty of Science,
!
!                 University of Zagreb
!   
!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!******** MODULES USED IN THIS PROGRAM:

USE arguments
USE vars
USE str

!*****************************************************************


IMPLICIT NONE

LOGICAL :: file_exists
CHARACTER(LEN=128) :: argument
INTEGER(KIND=4) :: num_args=0, xyz_check=0, N=0


!*****************************************************************

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
                    WRITE(*,*) "Wrong input argument!"
                    STOP
                END IF


                         xyz_arg = argument
                         mdp_arg = "MD.mdp"
                         trj_arg = "MD.trj"
                         vel_arg = "MD.vel"
                         em_arg  = "EM.mop"
                        ! out_arg = xyz_arg(1:(xyz_check-1))//".out"
                        ! log_arg = xyz_arg(1:(xyz_check-1))//".log"

          CALL structure_read
          CALL ingen  



         WRITE(*,*) ""      
         WRITE(*,*) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"      
         WRITE(*,*) ""
         WRITE(*,*) "                      Program completed!                       "
         WRITE(*,*) ""
         WRITE(*,*) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"      
         WRITE(*,*) ""      

          STOP


END PROGRAM md
