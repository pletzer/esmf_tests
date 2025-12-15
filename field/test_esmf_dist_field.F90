program esmf_2d_dist_grid_example
  use ESMF
  implicit none

  !-----------------------------------------------------------------
  ! Type declarations
  !-----------------------------------------------------------------
  integer, parameter :: dp = ESMF_KIND_R8
  integer, parameter :: I4 = ESMF_KIND_I4
  integer, parameter :: MAXDIM = ESMF_MAXDIM

  ! ESMF objects
  type(ESMF_Grid)     :: grid
  type(ESMF_DistGrid) :: distgrid
  type(ESMF_VM)       :: vm

  ! Grid parameters
  integer(I4) :: nx, ny
  integer(I4) :: maxIndex(2)
  integer(I4) :: regDecomp(2)

  ! Coordinate arrays
  real(dp), allocatable :: lons(:), lats(:)

  ! Local grid bounds
  integer(I4) :: iStart(MAXDIM), iEnd(MAXDIM)

  ! Coordinate pointers
  real(dp), pointer :: lon2d(:,:), lat2d(:,:)

  ! MPI info
  integer(I4) :: pet, npets

  ! Misc
  integer :: i, j, rc

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
  ny = 8

  ! User-defined decomposition (npx x npy)
  regDecomp = (/ 2, 2 /)
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
  allocate(lons(nx), lats(ny))
  do i = 1, nx
     lons(i) = -180.0_dp + (i-1)*360.0_dp/nx
  end do
  do j = 1, ny
     lats(j) = -90.0_dp + (j-1)*180.0_dp/(ny-1)
  end do

  maxIndex = (/ nx, ny /)

  !-----------------------------------------------------------------
  ! Create distributed grid with user decomposition
  !-----------------------------------------------------------------
  distgrid = ESMF_DistGridCreate(minIndex = (/1,1/), &
                                 maxIndex = maxIndex, &
                                 regDecomp = regDecomp, &
                                 rc = rc)
  if (rc /= ESMF_SUCCESS) stop "DistGridCreate failed"

  !-----------------------------------------------------------------
  ! Create Grid from DistGrid
  !-----------------------------------------------------------------
  grid = ESMF_GridCreate(distgrid = distgrid, &
                         coordSys  = ESMF_COORDSYS_SPH_DEG, &
                         rc        = rc)
  if (rc /= ESMF_SUCCESS) stop "GridCreate failed"

  !-----------------------------------------------------------------
  ! Add coordinates
  !-----------------------------------------------------------------
  call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
                                rc=rc)
  call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
                                rc=rc)
  if (rc /= ESMF_SUCCESS) stop "GridAddCoord failed"

  !-----------------------------------------------------------------
  ! Get local grid bounds
  !-----------------------------------------------------------------
  call ESMF_GridGet(grid=grid, tile=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
                    minIndex=iStart, maxIndex=iEnd, rc=rc)
  if (rc /= ESMF_SUCCESS) stop "GridGet failed"

  !-----------------------------------------------------------------
  ! Get coordinate pointers
  !-----------------------------------------------------------------
  call ESMF_GridGetCoord(grid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
                         farrayPtr=lon2d)
  call ESMF_GridGetCoord(grid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
                         farrayPtr=lat2d)

  !-----------------------------------------------------------------
  ! Fill local coordinates
  !-----------------------------------------------------------------
  do j = iStart(2), iEnd(2)
     do i = iStart(1), iEnd(1)
        lon2d(i,j) = lons(i)
        lat2d(i,j) = lats(j)
     end do
  end do

  !-----------------------------------------------------------------
  ! Print ownership
  !-----------------------------------------------------------------
  !write(*,'(A,I3,A,2I5,A,I25,A)') "PET ", pet, " owns i=[", iStart(1), iEnd(1), "] j=[", iStart(2), iEnd(2), "]"
  print*, 'PET: ', pet, ' iStart = ', iStart(1:2), ' iEnd = ', iEnd(1:2)

  !-----------------------------------------------------------------
  ! Finalize
  !-----------------------------------------------------------------
  call ESMF_Finalize(rc=rc)

end program esmf_2d_dist_grid_example
