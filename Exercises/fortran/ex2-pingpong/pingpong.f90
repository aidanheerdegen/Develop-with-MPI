program pingpong

  use mpi
  use ifport ! Intel fortran specific

  implicit none

  integer(4) :: hold_time = 1  ! sleep 1s

  integer :: i, ierr
  integer :: rank, size
  ! character(len=4) :: sendbuf, recvbuf
  integer :: sendbuf, recvbuf
  integer, parameter :: limit = 10
  integer :: proc1 = 0, proc2 = 1 
  integer, parameter :: tag = 1, ping = 0, pong = 1
  integer, dimension(MPI_STATUS_SIZE) :: status

  real*8 timer

  call mpi_init(ierr)

  call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)
  call mpi_comm_size(MPI_COMM_WORLD, size, ierr)

  if (size /= 2) then
     write (unit = *, fmt = *) "Must use exactly two MPI tasks"
     call mpi_abort(MPI_COMM_WORLD, -1, ierr)
  end if

  do i=1,limit
    

    if (rank == proc1) then
      ! Send a ping to proc2
      sendbuf = ping
      timer = laptime()
      call mpi_send(sendbuf, 1, MPI_INTEGER, proc2, tag, MPI_COMM_WORLD, ierr)
      ! Wait for pong from proc2
      call mpi_recv(recvbuf, 1, MPI_INTEGER, proc2, tag, MPI_COMM_WORLD, status, ierr)
      timer = laptime()
      if (recvbuf /= pong) then
        stop 'proc1 received incorrect message'
      else
        print *,timer,i,'proc1 received pong'
      end if
      call sleep(hold_time)
    elseif (rank == proc2) then
      ! Send a pong to proc1
      sendbuf = pong
      timer = laptime()
      call mpi_send(sendbuf, 1, MPI_INTEGER, proc1, tag, MPI_COMM_WORLD, ierr)
      ! Wait for pong from proc2
      call mpi_recv(recvbuf, 1, MPI_INTEGER, proc1, tag, MPI_COMM_WORLD, status, ierr)
      timer = laptime()
      if (recvbuf /= ping) then
        stop 'proc2 received incorrect message'
      else
        print *,timer,i,'proc2 received ping'
      end if
      call sleep(hold_time)
    end if

  end do

  call mpi_finalize(ierr)

contains

  real*8 function laptime(restart)

    ! Interface variables
    logical, optional :: restart

    logical, save :: timing = .false.
    real(kind=8), save ::  start, finish

    if (.not. timing) then
      start  = MPI_Wtime()
      timing = .true.
      laptime = 0.
    else
      finish = MPI_Wtime()
      laptime = finish - start
      if (present(restart) .and. restart) then
        start  = MPI_Wtime()
      end if
    end if

  end function laptime

end program pingpong
