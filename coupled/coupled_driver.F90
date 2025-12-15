program coupled_driver
  use mpi
  use component_mod
  implicit none

  integer :: ierr, world_size, world_rank
  integer :: comp_comm
  integer :: color, key
  integer :: nsteps

  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, world_size, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, world_rank, ierr)

  !----------------------------------------
  ! Split world into two components
  !----------------------------------------
  ! First half -> component A
  ! Second half -> component B
  !----------------------------------------
  if (world_rank < world_size/2) then
    color = 0   ! component A
  else
    color = 1   ! component B
  end if
  key = world_rank

  call MPI_Comm_split(MPI_COMM_WORLD, color, key, comp_comm, ierr)

  nsteps = 5

  select case (color)
  case (0)
    call comp_init(comp_comm, "Component A")
    call comp_step(comp_comm, "Component A", nsteps)
    call comp_finalize(comp_comm, "Component A")

  case (1)
    call comp_init(comp_comm, "Component B")
    call comp_step(comp_comm, "Component B", nsteps)
    call comp_finalize(comp_comm, "Component B")
  end select

  call MPI_Comm_free(comp_comm, ierr)
  call MPI_Finalize(ierr)

end program coupled_driver
