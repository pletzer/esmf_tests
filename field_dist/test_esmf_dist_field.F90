program esmf_2d_dist_grid_example
  use ESMF
  implicit none

  !-----------------------------------------------------------------
  ! Type declarations
  !-----------------------------------------------------------------
  integer, parameter :: dp = ESMF_KIND_R8
  integer, parameter :: I4 = ESMF_KIND_I4
  real(ESMF_KIND_R8) :: xmin, xmax, ymin, ymax

  ! ESMF objects
  type(ESMF_Grid)     :: grid
  type(ESMF_VM)       :: vm

  ! Grid parameters
  integer(I4) :: nx, ny ! number of cells
  integer(I4) :: regDecomp(2)

  ! Coordinate arrays

  ! Local grid bounds
  integer(I4) :: iBeg(2), iEnd(2)

  ! Coordinate pointers
  real(dp), allocatable :: x(:), y(:)
  real(dp), pointer :: x2d(:,:), y2d(:,:)

  ! MPI info
  integer(I4) :: pet, npets

  ! Misc
  integer :: i, j, rc, localDE

  !-----------------------------------------------------------------
  ! Initialize ESMF
  !-----------------------------------------------------------------
  call ESMF_Initialize(rc=rc)
  if (rc /= ESMF_SUCCESS) stop "ESMF_Initialize failed"

  call ESMF_VMGetCurrent(vm, rc=rc)
  call ESMF_VMGet(vm, localPet=pet, petCount=npets, rc=rc)

  !-----------------------------------------------------------------
  ! USER GRID PARAMETERS
  !-----------------------------------------------------------------
  nx = 16
  ny = 24
  xmin = 0.0_8
  xmax = 16.0_8
  ymin = 0.0_8
  ymax = 24.0_8
  regDecomp = (/2,3/)

  if (regDecomp(1)*regDecomp(2) /= npets) then
    if (pet == 0) write(*,*) "ERROR: npx*npy must equal MPI tasks"
    call ESMF_Finalize(rc=rc)
    stop
  end if

  if (pet == 0) then
    write(*,*) "Grid size:", nx, "x", ny
    write(*,*) "Decomposition:", regDecomp(1), "x", regDecomp(2)
    write(*,*) "MPI tasks:", npets
  end if

  !-----------------------------------------------------------------
  ! Create global lon/lat arrays
  !-----------------------------------------------------------------
  allocate(x(nx), y(ny))
  do i = 1, nx
     x(i) = xmin + (i-0.5)*(xmax - xmin)/nx
  end do
  do j = 1, ny
     y(j) = ymin + (j-0.5)*(ymax - ymin)/ny
  end do

  !-----------------------------------------------------------------
  ! Create distributed grid with user decomposition
  !-----------------------------------------------------------------
  grid = ESMF_GridCreateNoPeriDim( &
           minIndex=(/1,1/), &
           maxIndex=(/nx,ny/), &
           regDecomp=regDecomp, &
           coordSys=ESMF_COORDSYS_CART, &
           indexflag=ESMF_INDEX_GLOBAL, &
           rc=rc)
  if (rc /= ESMF_SUCCESS) stop "GridCreate failed"

  !-----------------------------------------------------------------
  ! Add coordinates
  !-----------------------------------------------------------------
  call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
                                rc=rc)
  if (rc /= ESMF_SUCCESS) stop "GridAddCoord failed"

  call ESMF_GridGetCoord(grid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
                         farrayPtr=x2d, exclusiveLBound=iBeg, exclusiveUBound=iEnd, rc=rc)
  call ESMF_GridGetCoord(grid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
                         farrayPtr=y2d)

  !-----------------------------------------------------------------
  ! Fill local coordinates
  !-----------------------------------------------------------------
  do j = iBeg(2), iEnd(2)
     do i = iBeg(1), iEnd(1)
        x2d(i,j) = x(i)
        y2d(i,j) = y(j)
     end do
  end do

  !-----------------------------------------------------------------
  ! Print ownership
  !-----------------------------------------------------------------
  !write(*,'(A,I3,A,2I5,A,I25,A)') "PET ", pet, " owns i=[", iBeg(1), iEnd(1), "] j=[", iBeg(2), iEnd(2), "]"
  print*, 'PET: ', pet, ' iBeg = ', iBeg, ' iEnd = ', iEnd

  !-----------------------------------------------------------------
  ! Finalize
  !-----------------------------------------------------------------
  ! no need to destroy the grid, will be taken care of ESMF_Finalize
  call ESMF_Finalize(rc=rc)

end program esmf_2d_dist_grid_example
