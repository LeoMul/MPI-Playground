mpif90 mpitest.f90 module.f90 -g -fbacktrace -Wall -fcheck=all -ffpe-trap=invalid -ffree-line-length-none -o mpichecking
mpirun -n 6 ./mpichecking

