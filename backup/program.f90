 PROGRAM md_program
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!
!
!   Purpose:      This program performs molecular structure analysis 
!              which includes: input generation for optimisation of    
!              structure, conformational analysis, generation of        
!              initial velocities for molecular dynamics and guided
!              MD.
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
USE chem_structure                               !
USE help                                         !
USE symmetry                                     !
USE normal_coords                                !
USE int_coords                                   !
USE md_normal_coords                             !
USE guided_md                                    !
USE rotate

!***********************************


IMPLICIT NONE

LOGICAL :: file_exists
CHARACTER(LEN=128), ALLOCATABLE, DIMENSION(:) :: argument
INTEGER(KIND=4) :: ix=0, num_args=0


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


num_args = COMMAND_ARGUMENT_COUNT()


   SELECT CASE(num_args)

          CASE(0)

                CALL help_info

          CASE(1)
 
                ALLOCATE(argument(1))

                      CALL GET_COMMAND_ARGUMENT( ix, argument(ix))

                xyz_check = INDEX(argument(1), ".xyz")
                help_check = INDEX(argument(1), "-help")

                   IF ( xyz_check > 0 ) THEN
                         argument(1) = xyz_arg
                         par_arg = xyz_arg(1:(xyz_check-1))//".par"
                         info_arg = xyz_arg(1:(xyz_check-1))//".info"
                         qpar_arg = xyz_arg(1:(xyz_check-1))//".qpar"
                         out_arg = xyz_arg(1:(xyz_check-1))//".out"
                         log_arg = xyz_arg(1:(xyz_check-1))//".log"
                   ELSE IF ( help_check > 0 ) THEN
                      CALL help_info
                   ELSE
                      DO
                         WRITE(*,*) ""
                         WRITE(*,*) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                         WRITE(*,*) ""
                         WRITE(*,*) "  WARNING: Incorrect input argument!"
                         WRITE(*,*) ""
                         WRITE(*,*) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                         WRITE(*,*) ""
                         STOP
                      END DO
                   END IF

          CASE(2)

                ALLOCATE(argument(2))
                   DO ix = 1, 2
                      CALL GET_COMMAND_ARGUMENT( ix, argument(ix))
                   END DO

                xyz_check = INDEX(argument(2), ".xyz")
                help_check = INDEX(argument(1), "-help")
                MD_check = INDEX(argument(2), "__")
                   IF ( xyz_check > 0 ) THEN
                      DO
                         command_xyz_arg = argument(1)
                         xyz_arg = argument(2)
                         par_arg = xyz_arg(1:(xyz_check-1))//".par"
                         info_arg = xyz_arg(1:(xyz_check-1))//".info"
                         qpar_arg = xyz_arg(1:(xyz_check-1))//".qpar"
                         initial_arg = xyz_arg(1:(xyz_check-1))//".initial"
                         out_arg = xyz_arg(1:(xyz_check-1))//".out"
                         log_arg = xyz_arg(1:(xyz_check-1))//".log"
                         minp_arg = xyz_arg(1:(xyz_check-1))//".minp"
                         mout_arg = xyz_arg(1:(xyz_check-1))//".mout"
                         share_arg = xyz_arg(1:(xyz_check-1))//".shares"
                         test_arg = xyz_arg(1:(xyz_check-1))//".test"
                         normcoords_arg = xyz_arg(1:(xyz_check-1))//".normcoords"
                         restart_arg = xyz_arg(1:(MD_check+4))//"MD_1__"//xyz_arg((MD_check+5):(xyz_check-1))//".restart"
                         WRITE(*,*) restart_arg
                         EXIT
                      END DO
                   ELSE IF (help_check > 0 ) THEN
                      help_arg = argument(2)
                   ELSE
                      DO
                         WRITE(*,*) ""
                         WRITE(*,*) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                         WRITE(*,*) ""
                         WRITE(*,*) "  WARNING: Incorrect input arguments!"
                         WRITE(*,*) ""
                         WRITE(*,*) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                         WRITE(*,*) ""
                         STOP
                      END DO
                   END IF

          CASE(3)

                ALLOCATE(argument(3))
                   DO ix = 1, 3
                      CALL GET_COMMAND_ARGUMENT( ix, argument(ix))
                   END DO

                convert_check = INDEX(argument(1), "-convert")
                out_check = INDEX(argument(3), ".out")
                int_check = INDEX(argument(3), ".int")
                   IF (out_check == 0 .AND. int_check == 0) THEN
                      DO
                         WRITE(*,*) ""
                         WRITE(*,*) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                         WRITE(*,*) ""
                         WRITE(*,*) "  WARNING: Incorrect input file argument!"
                         WRITE(*,*) ""
                         WRITE(*,*) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                         WRITE(*,*) ""
                         STOP
                      END DO
                   ELSE IF ( convert_check > 0 ) THEN
                      DO
                         command_conv_arg = argument(2)
                         file_arg = argument(3)
                         
                            IF (int_check > 0) THEN
                                  int_arg = file_arg(1:(int_check-1))//".intcoords"
                               
                            ELSE IF (out_check > 0) THEN
                               DO
                                  out_arg = file_arg(1:(out_check-1))//".out"
                                  xyz_arg = file_arg(1:(out_check-1))//".xyz"
                                  log_arg = file_arg(1:(out_check-1))//".log"
                                  EXIT
                               END DO
                             END IF
                          EXIT
                       END DO
                   ELSE
                      DO
                         WRITE(*,*) ""
                         WRITE(*,*) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                         WRITE(*,*) ""
                         WRITE(*,*) "  WARNING: Incorrect input file arguments!"
                         WRITE(*,*) ""
                         WRITE(*,*) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                         WRITE(*,*) ""
                         STOP
                      END DO
                  END IF

          CASE DEFAULT
   
                      DO
                         WRITE(*,*) ""
                         WRITE(*,*) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                         WRITE(*,*) ""
                         WRITE(*,*) "  WARNING: Too much input arguments!"
                         WRITE(*,*) ""
                         WRITE(*,*) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                         WRITE(*,*) ""
                         STOP
                      END DO

   END SELECT

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   IF (int2int_check > 0 ) THEN
         INQUIRE(FILE=int_arg, EXIST=file_exists)
            IF (file_exists) THEN
               CALL intcoords(int_arg)
            ELSE
               DO
                  WRITE(*,*) ""
                  WRITE(*,*) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                  WRITE(*,*) ""
                  WRITE(*,*) "  WARNING: Input .intcoords file not found!"
                  WRITE(*,*) ""
                  WRITE(*,*) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                  WRITE(*,*) ""
                  STOP
               END DO
            END IF
   END IF


WRITE(*,*) "Program completed!" 
STOP
END PROGRAM md_program

