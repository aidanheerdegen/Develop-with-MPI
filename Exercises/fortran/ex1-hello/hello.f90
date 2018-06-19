program hello

  use mpi

  implicit none

  integer :: ntasks, rank, ierr;

  call mpi_init(ierr)

  call mpi_comm_size(MPI_COMM_WORLD, ntasks, ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr);

  print *,"Hello World from rank ",rank," out of ",ntasks," processes"

  call mpi_finalize(ierr)

end program hello
