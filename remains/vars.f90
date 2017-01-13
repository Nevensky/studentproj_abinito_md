MODULE vars
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

USE arguments
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
REAL(KIND=8), DIMENSION(3) :: i_velocity
REAL(KIND=8), DIMENSION(3) :: v_velocity
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
END MODULE vars

