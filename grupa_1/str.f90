MODULE str
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!
!
!   Purpose:     To read molecular structure from .xyz file and generate input
!                 
!                for MOPAC. 
!
!
!   Authors:     Students of Department of Chemistry, Faculty of Science,
!
!                University of Zagreb
!  
!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!****** MODULES USED IN THIS PROGRAM:

USE arguments
USE vars

!**************************************************************


IMPLICIT NONE
SAVE

CONTAINS


SUBROUTINE structure_read
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!

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
!***** Reads number of atoms and i is number of blank spaces until x, y, z coordinates.



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
         READ(10,*) atoms(N)%symbol, atoms(N)%coord(1), atoms(N)%coord(2), atoms(N)%coord(3)
      END DO
CLOSE(10)
!***** Reads atom symbol and coordinates for each atom in molecule.


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
!***** Reads relative atomic masses - each isotope specificaly.


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
!***** Designates information about atomic number of element.


           END SUBROUTINE structure_read
!****************************************************!
!****************************************************!


SUBROUTINE ingen
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!

IMPLICIT NONE

INTEGER(KIND=4) :: N=0

OPEN(10, file='EM.mop', STATUS='NEW', ACTION='WRITE' )

WRITE(10,*) "PM7 OPT XYZ" 
WRITE(10,*) "Optimization of structure given as argument"
WRITE(10,*) "       "

DO N=1, n_atoms
     WRITE(10,*) atoms(N)%symbol, atoms(N)%coord
END DO
WRITE(10,*) "       "

CLOSE(10)

         END SUBROUTINE ingen
!****************************************************!
!****************************************************!

END MODULE str
