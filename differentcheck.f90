program greetings
    include 'mpif.h'
    integer my_rank
    integer p
    integer source
    integer dest
    integer tag
    character*100 message
    character*10 digit_string
    integer size
    integer status(MPI_STATUS_SIZE)
    integer ierr

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, p, ierr)
    
    if (my_rank .NE. 0) then
        write(digit_string,FMT="(I3)") my_rank
        message = 'Greetings from process ' //trim(digit_string) // ' !'
        dest = 0
        tag = 0
        call MPI_Send(message, len_trim(message), MPI_CHARACTER, dest, tag, MPI_COMM_WORLD, ierr)
        else
        do source = 1, p-1
            tag = 0
            call MPI_Recv(message, 100, MPI_CHARACTER,source, tag, MPI_COMM_WORLD, status, ierr)
            write(6,FMT="(A)") message
        enddo
    endif
call MPI_Finalize(ierr)
end program greetings