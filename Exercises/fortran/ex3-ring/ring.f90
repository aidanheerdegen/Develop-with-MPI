program ring

  use mpi
  use ifport ! Intel fortran specific

  implicit none

  integer :: ierr, rrequest, srequest
  integer :: rank, size, left, right
  integer :: sum

  integer :: recbuf, sendbuf

  integer, parameter :: tag = 99

  integer :: status(MPI_STATUS_SIZE)


  call mpi_init(ierr)

  call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)
  call mpi_comm_size(MPI_COMM_WORLD, size, ierr)

  ! Work out neighbouring ranks
  left = mod((rank-1)+size,size)
  right = mod(rank+1,size)

  print *,'Rank = ', rank, ' left neigbour: ', left, ' right neighbour: ',right

  ! Initialise the sum
  sum = 0

  ! Initialise recbuf to a number that cannot be the same as a rank
  recbuf = -1
  sendbuf = rank

  do while (recbuf /= rank) 

    ! Send message to the right
    call mpi_isend(sendbuf, 1, MPI_INTEGER, right, tag, MPI_COMM_WORLD, srequest, ierr)

    print *,'Rank ',rank,' sent ',sendbuf,' to ',right

    ! Receive message from the left
    call mpi_irecv(recbuf, 1, MPI_INTEGER, left, tag, MPI_COMM_WORLD, rrequest, ierr)

    call mpi_wait(rrequest, status, ierr)

    ! Sleep a variable number of seconds based on number just sent 
    print *,'Rank ',rank,' is sleeping ',sendbuf,' seconds'
    call sleep(sendbuf)
    print *,'Rank ',rank,' received ',recbuf,' from ',left

    sum = sum + recbuf

    call mpi_wait(srequest, status, ierr)

    sendbuf = recbuf

  end do

  ! Display the result on all ranks, along with the correct answer
  write (unit = *, fmt = '(a,i3,a,i4,a,i4)') &
       "Rank ", rank, " has sum of ranks ", sum, " Answer ", (size - 1)*size/2

  call mpi_finalize(ierr)


end program ring
