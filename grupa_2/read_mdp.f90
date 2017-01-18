module read_mdp
  implicit none
  save ! Save sluzi da se ne pobrisu podaci nakon koristenja drugih modula ili podrutina
  contains 
  
  subroutine mdp_vars

  ! MDP varijable
  integer,parameter :: dp = kind(1.0d0)
  real(dp),allocatable :: temp,int_step
  integer,allocatable :: max_steps
  character(len=200) :: mdp_arg = './MD.mdp'

  ! fileop varijable
  character(len=100) :: buffer, label
  integer :: pos1,pos2
  integer, parameter :: fh = 15 ! UNIT=15 za fajlove
  integer :: ios = 0 ! IOstatus >0 greska <0 kraj fajla
  integer :: line = 0


  open(fh, file=mdp_arg)
  do while (ios == 0)
     read(fh, '(A)', iostat=ios) buffer
     if (ios == 0) then
        line = line + 1

        ! Find the first instance of whitespace.  Split label and data.
        pos1 = scan(buffer, '    ') ! polozaj prvog whitespacea
        label = buffer(1:pos1) ! ime opcije
        buffer = buffer(pos1:) ! preostali dio linije
        pos2 = scan(buffer, '=')
        buffer = buffer(pos2+1:) ! vrijednost nakon =

        select case (label)
        case ('temp')
          allocate(temp)
           read(buffer, *, iostat=ios) temp
           print *, 'Temperature [K]: ',temp
        case ('int_step')
          allocate(int_step)
           read(buffer, *, iostat=ios) int_step
           print *, 'Integration step [ps]: ', int_step
         case ('max_steps')
           allocate(max_steps)
            read(buffer, *, iostat=ios) max_steps
            print *, 'Maximum num of steps: ', max_steps
        case default
           print *, 'Skipping invalid option in MDP file at line: ', line
        end select
     end if
  end do

  ! check if temp , int_step and max_steps are defined
  if (.NOT. allocated(temp)) then
    print *,"\033[1;31m Temperature not defined in MDP file. \033[0m"
    stop
  elseif (.NOT. allocated(int_step) ) then
    print *, "\033[1;31m Integration step not defined in MDP file. \033[0m"
    stop
  elseif (.NOT. allocated(max_steps) ) then
    print *, "\033[1;31m Maximum num of steps not defined in MDP file. \033[0m"
    stop
  else
    print*,"\033[01;32m MDP variables initialized successfully. \033[0m"
  end if
end module read_mdp
