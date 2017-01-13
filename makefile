
gfortran-6 -c ./arguments.f90
gfortran-6 -c ./vars.f90
gfortran-6 -c ./mdp.f90
gfortran-6 -c ./grupa_1/str.f90
gfortran-6 md.f90 -o md.out ./arguments.o vars.o str.o mdp.o
