program myprogram
    USE paradot
    implicit none 
    include 'mpif.h'

    real * 8 :: x(10), y(10),z(10)
    real*8 , allocatable :: localx(:),localy(:),localz(:)
    integer,allocatable :: dispvec(:),counts(:)
    integer :: n,nperproc,j,DISP

    integer :: ierr 
    integer :: rank ,root
    integer :: nprocs 
    call MPI_INIT(ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
    root = 0
   
    n = size(x)
    nperproc = n / nprocs

    x(:) = 1.0d0 
    y(:) = 3.0d0


    allocate(dispvec(nprocs),counts(nprocs))
    counts = nperproc 
    counts(nprocs) = nperproc + modulo(n,nprocs)

    do j = 0,nprocs-1
        dispvec(j+1) = j* (n / nprocs) 
    end do 
    
    if (rank .eq. (nprocs -1)) then 
        nperproc = nperproc + modulo(n,nprocs)
    end if 

    allocate(localx(nperproc),localy(nperproc),localz(nperproc))

    if (rank .ne. (nprocs -1)) then 
        DISP = rank*nperproc+1
        localx = x(DISP:(rank+1)*nperproc)
        localy = y(DISP:(rank+1)*nperproc)
    else 
        DISP = (n-nperproc+1)
        localx = x(DISP:n)
        localy = y(DISP:n)
    end if 

    if (rank .eq. root) then 
        print*,"dispvec,",dispvec
    end if  


    localz = localx*localy
    print*,"localz,",localz

    call MPI_GATHERV(localz,nperproc,MPI_DOUBLE_PRECISION,z,counts,dispvec,MPI_DOUBLE_PRECISION,root,MPI_COMM_WORLD,ierr)

    if (rank .eq. root) then 
        print*,z
    end if 

    call MPI_FINALIZE(ierr)
end program 

