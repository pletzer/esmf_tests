module component_mod
  use mpi
  implicit none
  integer, parameter :: dp = kind(1.0d0)

  type :: component_type
     integer :: comp_comm
     integer :: comp_rank, comp_size
     real(dp), allocatable :: export_data(:)
     real(dp), allocatable :: import_data(:)
  end type component_type

contains

  subroutine comp_init(comp, comm, n, name)
    type(component_type), intent(inout) :: comp
    integer, intent(in)                 :: comm, n
    character(*), intent(in)            :: name
    integer :: ierr

    comp%comp_comm = comm
    call MPI_Comm_rank(comm, comp%comp_rank, ierr)
    call MPI_Comm_size(comm, comp%comp_size, ierr)

    allocate(comp%export_data(n))
    allocate(comp%import_data(n))

    comp%export_data = 0.0_dp
    comp%import_data = -1.0_dp

    if (comp%comp_rank == 0) then
      write(*,*) trim(name), ": init"
    end if
  end subroutine comp_init


  subroutine comp_step(comp, step, name)
    type(component_type), intent(inout) :: comp
    integer, intent(in)                 :: step
    character(*), intent(in)            :: name

    ! Dummy physics
    comp%export_data = step + comp%comp_rank

    if (comp%comp_rank == 0) then
      write(*,*) trim(name), ": step", step
    end if
  end subroutine comp_step


  subroutine comp_finalize(comp, name)
    type(component_type), intent(inout) :: comp
    character(*), intent(in)            :: name

    deallocate(comp%export_data, comp%import_data)

    if (comp%comp_rank == 0) then
      write(*,*) trim(name), ": finalize"
    end if
  end subroutine comp_finalize

end module component_mod
