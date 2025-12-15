module component_mod
  use mpi
  implicit none
contains

  subroutine comp_init(comm, name)
    integer, intent(in) :: comm
    character(*), intent(in) :: name
    integer :: rank, ierr

    call MPI_Comm_rank(comm, rank, ierr)
    if (rank == 0) then
      write(*,*) trim(name), ": init"
    end if
  end subroutine comp_init

  subroutine comp_step(comm, name, nsteps)
    integer, intent(in) :: comm, nsteps
    character(*), intent(in) :: name
    integer :: rank, step, ierr

    call MPI_Comm_rank(comm, rank, ierr)

    do step = 1, nsteps
      if (rank == 0) then
        write(*,*) trim(name), ": step ", step
      end if
      call MPI_Barrier(comm, ierr)
    end do
  end subroutine comp_step

  subroutine comp_finalize(comm, name)
    integer, intent(in) :: comm
    character(*), intent(in) :: name
    integer :: rank, ierr

    call MPI_Comm_rank(comm, rank, ierr)
    if (rank == 0) then
      write(*,*) trim(name), ": finalize"
    end if
  end subroutine comp_finalize

end module component_mod
