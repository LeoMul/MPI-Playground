program fftmpi 
    USE MPI
    use, intrinsic :: iso_c_binding
    implicit none 

    !include 'mpi.h'

    !fft stuff
    include 'fftw3-mpi.f03'
    type(C_PTR) :: p
    complex*16 :: in(10),out(10)

    !mpi stuff
    integer :: ierr 
    integer :: rank ,root
    integer :: nprocs 
    call MPI_INIT(ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)


    in = 1.0d0 
    p = fftw_plan_dft_1d(size(in), in, out, FFTW_FORWARD, FFTW_ESTIMATE);
    call fftw_execute_dft(p, in, out)
    call fftw_destroy_plan(p)
    print*,out

    call MPI_FINALIZE(ierr)
end program