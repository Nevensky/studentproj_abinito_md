MODULE guided_md
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!
!
!   Purpose:      To read molecular structure from .xyz file and per-
!              forms guided molecular dynamics with specified normal    
!              coordinates from .qpar file.       
!              
!
!
!   Record of revision:
!
!      Date          Programmer         Revision   
!    ========      ==============     ==========================
!   20/10/2016       Karlo Sovic      Subroutine structure_read
!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!

USE constants
USE data_storage

IMPLICIT NONE
SAVE
CONTAINS


SUBROUTINE structure_read
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!

IMPLICIT NONE

INTEGER(KIND=4) :: blank_0=0, i=0, N=0, K=0
INTEGER(KIND=4) :: istat=0
CHARACTER(LEN=128) :: check_line

blank_0=0
OPEN(UNIT=10, FILE=xyz_arg, STATUS="OLD", ACTION="READ")
READ(10,*) n_atoms
   DO WHILE (blank_0 == 0)
      READ(10,"(a128)") check_line
      blank_0 = SCAN (check_line, "aeiour123456789i0!")
         IF (blank_0 == 0) THEN
            i = i + 1 
         END IF
   END DO
REWIND(10)
!***** Reads number of atoms and i is number of blank spaces until x, y, z coordinates



ALLOCATE (atoms(n_atoms), STAT=istat)
   IF (istat /= 0) THEN
      DO
         WRITE(*,*) ""      
         WRITE(*,*) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"      
         WRITE(*,*) ""      
         WRITE(*,*) "    WARNING: Error during allocation of molecule structure! "      
         WRITE(*,*) ""      
         WRITE(*,*) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"      
         WRITE(*,*) ""      
         STOP 
      END DO
   END IF

   DO N=1,i+1
      READ(10,*)
   END DO
      DO N=1,n_atoms
         READ(10,*) atoms(N)%symbol, atoms(N)%x_comp, atoms(N)%y_comp, atoms(N)%z_comp
      END DO
CLOSE(10)
!***** Reads atom symbol and coordinates for each atom in molecule


      DO N=1, n_atoms
         IF (INDEX( atoms(N)%symbol, "H" ) /= 0 ) THEN
            atoms(N)%rel_mass = 1.00078250
         ELSE IF (INDEX( atoms(N)%symbol, "D") /= 0 ) THEN
            atoms(N)%rel_mass = 2.01410178
         ELSE IF (INDEX( atoms(N)%symbol, "T") /= 0 ) THEN
            atoms(N)%rel_mass = 3.01604928
         ELSE IF (INDEX( atoms(N)%symbol, "He") /= 0 ) THEN
            atoms(N)%rel_mass = 4.00260300
         ELSE IF (INDEX( atoms(N)%symbol, "Li") /= 0 ) THEN
            atoms(N)%rel_mass = 6.94000000
         ELSE IF (INDEX( atoms(N)%symbol, "Be") /= 0 ) THEN
            atoms(N)%rel_mass = 9.01218200
         ELSE IF (INDEX( atoms(N)%symbol, "B") /= 0 ) THEN
            atoms(N)%rel_mass = 10.8100000
         ELSE IF (INDEX( atoms(N)%symbol, "C") /= 0 ) THEN
            atoms(N)%rel_mass = 12.0000000
         ELSE IF (INDEX( atoms(N)%symbol, "N") /= 0 ) THEN
            atoms(N)%rel_mass = 14.0030740
         ELSE IF (INDEX( atoms(N)%symbol, "O") /= 0 ) THEN
            atoms(N)%rel_mass = 15.9949146
         ELSE IF (INDEX( atoms(N)%symbol, "F") /= 0 ) THEN
            atoms(N)%rel_mass = 18.9984031
         ELSE IF (INDEX( atoms(N)%symbol, "P") /= 0 ) THEN
            atoms(N)%rel_mass = 30.9737620
         ELSE
            atoms(N)%rel_mass = 0.00000000
         END IF
      END DO 
!***** Reads relative atomic masses - each isotope specificaly


      DO N=1, n_atoms
         IF (INDEX( atoms(N)%symbol, "H" ) /= 0 ) THEN
            atoms(N)%a_number = 1
         ELSE IF (INDEX( atoms(N)%symbol, "D") /= 0 ) THEN
            atoms(N)%a_number = 1
         ELSE IF (INDEX( atoms(N)%symbol, "T") /= 0 ) THEN
            atoms(N)%a_number = 1
         ELSE IF (INDEX( atoms(N)%symbol, "He") /= 0 ) THEN
            atoms(N)%a_number = 2
         ELSE IF (INDEX( atoms(N)%symbol, "Li") /= 0 ) THEN
            atoms(N)%a_number = 3
         ELSE IF (INDEX( atoms(N)%symbol, "Be") /= 0 ) THEN
            atoms(N)%a_number = 4
         ELSE IF (INDEX( atoms(N)%symbol, "B") /= 0 ) THEN
            atoms(N)%a_number = 5
         ELSE IF (INDEX( atoms(N)%symbol, "C") /= 0 ) THEN
            atoms(N)%a_number = 6
         ELSE IF (INDEX( atoms(N)%symbol, "N") /= 0 ) THEN
            atoms(N)%a_number = 7
         ELSE IF (INDEX( atoms(N)%symbol, "O") /= 0 ) THEN
            atoms(N)%a_number = 8
         ELSE IF (INDEX( atoms(N)%symbol, "F") /= 0 ) THEN
            atoms(N)%a_number = 9
         ELSE IF (INDEX( atoms(N)%symbol, "Na") /= 0 ) THEN
            atoms(N)%a_number = 11
         ELSE IF (INDEX( atoms(N)%symbol, "Mg") /= 0 ) THEN
            atoms(N)%a_number = 12
         ELSE IF (INDEX( atoms(N)%symbol, "P") /= 0 ) THEN
            atoms(N)%a_number = 15
         ELSE IF (INDEX( atoms(N)%symbol, "S") /= 0 ) THEN
            atoms(N)%a_number = 16
         ELSE IF (INDEX( atoms(N)%symbol, "Cl") /= 0 ) THEN
            atoms(N)%a_number = 17
         ELSE
            atoms(N)%a_number = 0
         END IF
      END DO
!*****Designates information about atomic number of element



           END SUBROUTINE structure_read
!****************************************************!
!****************************************************!






SUBROUTINE everything
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!

IMPLICIT NONE

INTEGER(KIND=4) :: blank_blank=0, blank_0=0, i=0, N=0, K=0
INTEGER(KIND=4) :: istat=0, temp_numb=1, nc_numb=1, ios=0
CHARACTER(LEN=128) :: check_line, temp_char, temp_format 
CHARACTER(LEN=128) :: check_line_2 


OPEN(UNIT=30, FILE=qpar_arg, STATUS="OLD", ACTION="READ")
   DO WHILE (blank_0 == 0)
      READ(30, "(a128)") check_line
      blank_0 = INDEX(check_line, "Number of steps")
         IF (blank_0 == 0) THEN
            K = K + 1
         END IF
   END DO  
REWIND(30)

WRITE(arg_1, "(a)") "./trajectory_1/"
new_restart_arg = arg_1(1:15)//restart_arg(1:LEN(restart_arg))


INQUIRE(FILE=new_restart_arg, EXIST=file_exist)
   IF (file_exist) THEN
      GO TO 1000
   ELSE
      CALL qcc_start
   END IF

1000  OPEN(UNIT=40, FILE=new_restart_arg, STATUS="OLD", ACTION="READ")
          READ(40, *) check_line, test_step_number
             IF (test_step_number < Number_of_steps) THEN
                REWIND(40)
                CLOSE(40)
                CALL atom_displacement
             ELSE
         WRITE(*,*) ""      
         WRITE(*,*) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"      
         WRITE(*,*) ""      
         WRITE(*,*) "    MESSAGE: Guided molecular dynamics has finished! "      
         WRITE(*,*) ""      
         WRITE(*,*) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"      
         WRITE(*,*) ""      
                STOP
             END IF


blank_blank=0
   DO WHILE (blank_blank == 0)
      READ(30,"(a128)") check_line
      blank_blank = INDEX (check_line, "temperature")
         IF (blank_blank == 0) THEN
            temp_numb = temp_numb + 1 
         END IF
   END DO
REWIND(30)


   DO N=1, temp_numb-1
      READ(30,*)
   END DO
      READ(30, "(t37, a12)") temp_char
      BACKSPACE(30)
WRITE(temp_format, "(a7, I1, a3)") "(t37, F", LEN(TRIM(temp_char)), ".2)"
      READ(30, temp_format ) temperature
REWIND(30)


             DO K = 3*n_atoms-6, 1, -1
                ALLOCATE(NC_input_vector(K))
                  READ(30, *, IOSTAT=ios) NC_input_vector
                     IF ( ios == 0 ) THEN
                        EXIT 
                     ELSE
                        REWIND(30)
                        DO N=1, nc_numb
                           READ(30,*)
                        END DO
                        DEALLOCATE(NC_input_vector) 
                     END IF
              END DO 
REWIND(30)
CLOSE(30)
!***** Reads which vibrations will be affected during mdynamics



CALL init_random_seed
CALL RANDOM_NUMBER(x)
p = INT(6*x) + 1
CALL RANDOM_NUMBER(x)
q = INT(6*x) + 1
WRITE(*,*) p , q

CONTAINS
SUBROUTINE  init_random_seed
            IMPLICIT NONE
            INTEGER :: i, n, clock
            INTEGER, DIMENSION(:), ALLOCATABLE :: seed
          
            CALL RANDOM_SEED(size = n)
            ALLOCATE(seed(n))
          
            CALL SYSTEM_CLOCK(COUNT=clock)
          
            seed = clock + 37 * (/ (i , i = 1, n) /)
            CALL RANDOM_SEED(PUT = seed)
            
            DEALLOCATE(seed) 
        END SUBROUTINE init_random_seed


           END SUBROUTINE qpar_read
!****************************************************!
!****************************************************!


