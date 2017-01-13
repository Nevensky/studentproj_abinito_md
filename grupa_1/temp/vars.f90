MODULE vars
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!
!
!   Purpose:      Variable and data storage for program MD.
!
!
!   Authors:      Students of Department of Chemistry, Faculty of Science,
!
!                 University of Zagreb     
!  
!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!****** MODULES USED IN THIS PROGRAM:

USE arguments

!****************************************************************


IMPLICIT NONE
SAVE


!****************************************************************
!GRUPA_1
!****************************************************************


INTEGER(KIND=4) :: n_atoms              
                                                                
TYPE :: atom                                                     
CHARACTER :: symbol                                              
REAL(KIND=8) :: rel_mass                                      
INTEGER(KIND=4) :: a_number                                      
REAL(KIND=8), DIMENSION(3) :: coord   
REAL(KIND=8), DIMENSION(3) :: init_velocity
REAL(KIND=8), DIMENSION(3) :: velocity
REAL(KIND=8), DIMENSION(3) :: acceleration
END TYPE atom                                                    
                                                                 
TYPE(atom), ALLOCATABLE, DIMENSION(:) :: atoms
TYPE(atom), ALLOCATABLE, DIMENSION(:) :: new_atoms


!****************************************************************
!GRUPA_2
!****************************************************************


REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: r   
REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: r_init   
REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: r_fin   


!****************************************************************
!GRUPA_3
!****************************************************************


REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: v   


END MODULE vars

