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
  real(dp) :: dx, dy
  real(dp), pointer :: x2dCorner(:,:), y2dCorner(:,:)
  real(dp), pointer :: x2dCentre(:,:), y2dCentre(:,:)

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
  call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, &
                                rc=rc)

  !-----------------------------------------------------------------
  ! Fill local coordinates
  !-----------------------------------------------------------------
  dx = (xmax - xmin) / real(nx, dp)
  dy = (ymax - ymin) / real(ny, dp)

  ! cell centres
  call ESMF_GridGetCoord(grid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
                         farrayPtr=x2dCentre, &
                         exclusiveLBound=iBeg, exclusiveUBound=iEnd, rc=rc)
  call ESMF_GridGetCoord(grid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
                         farrayPtr=y2dCentre)
  do j = iBeg(2), iEnd(2)
     do i = iBeg(1), iEnd(1)
        x2dCentre(i,j) = xmin + (i - 0.5_dp)*dx
        y2dCentre(i,j) = ymin + (j - 0.5_dp)*dy
     end do
  end do

  ! nodes
  call ESMF_GridGetCoord(grid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CORNER, &
                         farrayPtr=x2dCorner, &
                         exclusiveLBound=iBeg, exclusiveUBound=iEnd, rc=rc)
  call ESMF_GridGetCoord(grid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CORNER, &
                         farrayPtr=y2dCorner)
  do j = iBeg(2), iEnd(2)
     do i = iBeg(1), iEnd(1)
        x2dCorner(i,j) = xmin + (i - 1)*dx
        y2dCorner(i,j) = ymin + (j - 1)*dy
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
