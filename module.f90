module paradot

    implicit none 
    !include 'mpi.h'
    contains 

    function parallel_dot(localx,localy,localn) result(localdot)
        real * 8,intent(in) :: localx(:),localy(:)
        integer,intent(in) ::localn
        real * 8 :: localdot ,dot
        integer :: i 
        !integer :: ierr 
        !integer :: rank ,root
        !integer :: nprocs 
        !call MPI_INIT(ierr)
        !call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,ierr)
        !call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)

        dot = 0.0d0
        localdot = 0.0d0
        do i = 1,localn 
            localdot = localdot + localx(i) * localy(i)
        end do 
    end function

end module 