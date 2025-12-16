program coupled_driver
  ! Driver orchestrates the execution of A and B components
  ! and is responsible for transferring data between A and B
  ! at the start (A -> B) and the end (B -> A) of each step.
  use mpi
  use component_mod
  implicit none

  integer :: ierr, world_rank, world_size
  integer :: color
  integer :: comp_comm
  integer :: coupler_comm
  integer :: coupler_rank

  logical :: isA, isB
  type(component_type) :: compA, compB

  integer, parameter :: n = 4
  integer :: step

  ! Coupler-owned buffers and windows
  real(dp), allocatable :: A2B_buf(:), B2A_buf(:)
  integer :: A2B_win, B2A_win

  !----------------------------------------
  ! Init MPI
  !----------------------------------------
  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, world_rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, world_size, ierr)

  !----------------------------------------
  ! Split WORLD into A and B
  !----------------------------------------
  if (world_rank < world_size/2) then
    color = 0
    isA   = .true.
    isB   = .false.
  else
    color = 1
    isA   = .false.
    isB   = .true.
  end if

  call MPI_Comm_split(MPI_COMM_WORLD, color, world_rank, comp_comm, ierr)

  if (isA) call comp_init(compA, comp_comm, n, "Component A")
  if (isB) call comp_init(compB, comp_comm, n, "Component B")

  !----------------------------------------
  ! Coupler communicator: rank 0 of A and B
  !----------------------------------------
  if ((isA .and. compA%comp_rank == 0) .or. &
      (isB .and. compB%comp_rank == 0)) then
    call MPI_Comm_split(MPI_COMM_WORLD, 0, world_rank, coupler_comm, ierr)
  else
    call MPI_Comm_split(MPI_COMM_WORLD, MPI_UNDEFINED, world_rank, coupler_comm, ierr)
  end if

  if (coupler_comm /= MPI_COMM_NULL) then
    call MPI_Comm_rank(coupler_comm, coupler_rank, ierr)

    ! Allocate buffers to hold the A/B data
    allocate(A2B_buf(n), B2A_buf(n))
    A2B_buf = -100.0_dp
    B2A_buf = -200.0_dp

    ! Create MPI windows for the A/B fields
    call MPI_Win_create(A2B_buf, int(n*8, kind=MPI_ADDRESS_KIND), 8, MPI_INFO_NULL, coupler_comm, A2B_win, ierr)
    call MPI_Win_create(B2A_buf, int(n*8, kind=MPI_ADDRESS_KIND), 8, MPI_INFO_NULL, coupler_comm, B2A_win, ierr)
  else
    ! MPI_Win_create is collective. Need to set the windows to null for the ranks that don't contribute to 
    ! data exchange
    A2B_win = MPI_WIN_NULL
    B2A_win = MPI_WIN_NULL
  end if

  !----------------------------------------
  ! Time stepping
  !----------------------------------------
  do step = 1, 3

    ! A pushes to B
    if (isA .and. compA%comp_rank == 0) then
      call MPI_Win_lock(MPI_LOCK_SHARED, 1, 0, A2B_win, ierr)
      call MPI_Put(compA%export_data, n, MPI_DOUBLE_PRECISION, &
                   1, 0_MPI_ADDRESS_KIND, n, MPI_DOUBLE_PRECISION, A2B_win, ierr)
      call MPI_Win_unlock(1, A2B_win, ierr)
    end if

    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    ! B pulls from A
    if (isB .and. compB%comp_rank == 0) then
      call MPI_Win_lock(MPI_LOCK_SHARED, 0, 0, A2B_win, ierr)
      call MPI_Get(compB%import_data, n, MPI_DOUBLE_PRECISION, &
                   0, 0_MPI_ADDRESS_KIND, n, MPI_DOUBLE_PRECISION, A2B_win, ierr)
      call MPI_Win_unlock(0, A2B_win, ierr)
    end if

    if (isB) then
      call MPI_Bcast(compB%import_data, n, MPI_DOUBLE_PRECISION, 0, comp_comm, ierr)
    end if

    !==============================
    ! Component A step
    !==============================
    if (isA) call comp_step(compA, step, "Component A")

    !==============================
    ! Component B step
    !==============================
    if (isB) call comp_step(compB, step, "Component B")

    ! B pushes to A
    if (isB .and. compB%comp_rank == 0) then
      call MPI_Win_lock(MPI_LOCK_SHARED, 0, 0, B2A_win, ierr)
      call MPI_Put(compB%export_data, n, MPI_DOUBLE_PRECISION, &
                   0, 0_MPI_ADDRESS_KIND, n, MPI_DOUBLE_PRECISION, B2A_win, ierr)
      call MPI_Win_unlock(0, B2A_win, ierr)
    end if

    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    ! A pulls from B
    if (isA .and. compA%comp_rank == 0) then
      call MPI_Win_lock(MPI_LOCK_SHARED, 1, 0, B2A_win, ierr)
      call MPI_Get(compA%import_data, n, MPI_DOUBLE_PRECISION, &
                   1, 0_MPI_ADDRESS_KIND, n, MPI_DOUBLE_PRECISION, B2A_win, ierr)
      call MPI_Win_unlock(1, B2A_win, ierr)
    end if

    if (isA) then
      call MPI_Bcast(compA%import_data, n, MPI_DOUBLE_PRECISION, 0, comp_comm, ierr)
    end if

  end do

  !----------------------------------------
  ! Finalize
  !----------------------------------------
  if (coupler_comm /= MPI_COMM_NULL) then
    call MPI_Win_free(A2B_win, ierr)
    call MPI_Win_free(B2A_win, ierr)
    deallocate(A2B_buf, B2A_buf)
    call MPI_Comm_free(coupler_comm, ierr)
  end if

  if (isA) call comp_finalize(compA, "Component A")
  if (isB) call comp_finalize(compB, "Component B")

  call MPI_Comm_free(comp_comm, ierr)
  call MPI_Finalize(ierr)

end program coupled_driver
