MODULE data_storage
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!
!
!
!   Purpose:      Variable and data storage for module chem_structure
!
!
!   Record of revision:
!
!      Date          Programmer         Revision   
!    ========      ==============     =====================
!   12/07/2016       Karlo Sovic      Original code
!
!   27/07/2016       Karlo Sovic      Last revision
!
!=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~!

USE constants

IMPLICIT NONE
SAVE


!****************************************************************
!Variables for subroutine adjancency matrix
!****************************************************************

INTEGER(KIND=4) :: n_atoms              
                                                                
TYPE :: atom                                                     
CHARACTER :: symbol                                              
REAL(KIND=8) :: rel_mass                                      
INTEGER(KIND=4) :: a_number                                      
REAL(KIND=8) :: x_comp                                            
REAL(KIND=8) :: y_comp                                           
REAL(KIND=8) :: z_comp   
INTEGER(KIND=4) :: occurrence                                         
REAL(KIND=8), DIMENSION(3) :: i_velocity
REAL(KIND=8), DIMENSION(3) :: i_acceleration
END TYPE atom                                                    
                                                                 
TYPE(atom), ALLOCATABLE, DIMENSION(:) :: atoms
TYPE(atom), ALLOCATABLE, DIMENSION(:) :: new_atoms
                                                                 
INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:,:) :: adj_matrix        



!****************************************************************
!Variables for subroutine get_rotation_bonds
!****************************************************************

INTEGER(KIND=4) :: n_bonds                                        
                                                                 
INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:,:) :: Rot_bond_matrix   
                                                                  
TYPE :: type_rotatable_atoms                                        
INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:) :: Aff_atoms_vector    
END TYPE type_rotatable_atoms                                      
                                                                  
TYPE(type_rotatable_atoms), ALLOCATABLE, DIMENSION(:) :: rot_bond 




!****************************************************************
!Variables for subroutine get_rotation_axis
!****************************************************************


CONTAINS
FUNCTION cross_product(a,b)
TYPE(atom), INTENT(IN) :: a
TYPE(atom), INTENT(IN) :: b
REAL(KIND=8), DIMENSION(3) :: cross_product
REAL(KIND=8) :: norm

norm= SQRT((a%y_comp*b%z_comp - a%z_comp*b%y_comp)**(2) + (a%z_comp*b%x_comp - a%x_comp*b%z_comp)**(2) + &
(a%x_comp*b%y_comp - a%y_comp*b%x_comp)**(2))

cross_product(1) = (a%y_comp*b%z_comp - a%z_comp*b%y_comp)/(norm)
cross_product(2) = (a%z_comp*b%x_comp - a%x_comp*b%z_comp)/(norm)
cross_product(3) = (a%x_comp*b%y_comp - a%y_comp*b%x_comp)/(norm)

END FUNCTION cross_product



FUNCTION scalar_angle(a,b)
TYPE(atom), INTENT(IN) :: a
TYPE(atom), INTENT(IN) :: b
REAL(KIND=8) :: scalar_angle
REAL(KIND=8) :: norm
 
norm= SQRT((a%x_comp)**2 + (a%y_comp)**2 + (a%z_comp)**2) * SQRT((b%x_comp)**2 + &
            (b%y_comp)**2 + (b%z_comp)**2)

scalar_angle= ACOS(((a%x_comp*b%x_comp) + (a%y_comp*b%y_comp) + (a%z_comp*b%z_comp))/norm)

END FUNCTION scalar_angle



FUNCTION rotation(a,b,angle)
REAL(KIND=8), DIMENSION(3), INTENT(IN) :: a
REAL(KIND=8), DIMENSION(3), INTENT(IN) :: b
REAL(KIND=8), INTENT(IN) :: angle
REAL(KIND=8), DIMENSION(3) :: rotation
REAL(KIND=8) :: scalar_product

scalar_product = a(1)*b(1) + a(2)*b(2) + a(3)*b(3)

rotation(1) = COS(angle)*a(1) + SIN(angle)*(b(2)*a(3)-b(3)*a(2)) + &
              (1-COS(angle))*(scalar_product)*b(1)

rotation(2) = COS(angle)*a(2) + SIN(angle)*(b(3)*a(1)-b(1)*a(3)) + &
              (1-COS(angle))*(scalar_product)*b(2)

rotation(3) = COS(angle)*a(3) + SIN(angle)*(b(1)*a(2)-b(2)*a(1)) + &
              (1-COS(angle))*(scalar_product)*b(3)

END FUNCTION rotation
END MODULE data_storage

