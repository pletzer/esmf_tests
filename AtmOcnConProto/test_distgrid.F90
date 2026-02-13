program test_distgrid
    use ESMF
    use distgrid_mod
    use distfield_mod
    implicit none
    type(ESMF_VM)       :: vm
    integer :: pet, npets, rc
    type(distgrid_type), target :: dgrid
    type(distfield_type) :: dfield
    integer, parameter :: regDecomp(2) = (/2, 3/)
    integer, parameter :: nx = 10, ny=12
    real(8), parameter :: xmin = 0.0_8, xmax = 1.0_8, ymin = 0.0_8, ymax = 2.0_8
    real(8) :: dx, dy, xCentre, yCentre
    integer :: i, j
    real(8), allocatable :: x2dCorner(:, :), y2dCorner(:, :)
    real(8), pointer :: localDataPtr(:, :)
    integer :: iBeg(2), iEnd(2)


    call ESMF_Initialize(rc=rc)
    if (rc /= ESMF_SUCCESS) stop "ESMF_Initialize failed"

    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=pet, petCount=npets, rc=rc)

    if (regDecomp(1) * regDecomp(2) /= npets) then
        call ESMF_Finalize(rc=rc)
        stop 'Invalid number of procs, should be 6'
    endif

    ! create grid
    allocate(x2dCorner(nx+1, ny+1), y2dCorner(nx+1, ny+1))
    dx = (xmax - xmin) / real(nx, 8)
    dy = (ymax - ymin) / real(ny, 8)
    do j = 1, ny+1
        do i = 1, nx+1
            x2dCorner(i, j) = xmin + (i - 1)*dx
            y2dCorner(i, j) = ymin + (j - 1)*dy
        enddo
    enddo
    call distgrid_new(dgrid, x2dCorner, y2dCorner, regDecomp)

    ! create a field, from this point on everything is distributed
    ! so we're working with global indices but only on the local 
    ! subdomain
    call distfield_new(dfield, 'sst', dgrid, ESMF_STAGGERLOC_CENTER)
    call distfield_getLocalBounds(dfield, iBeg, iEnd)
    call distfield_getLocalDataPtr(dfield, localDataPtr)

    write(*,*) 'PET', pet, 'iBeg=', iBeg, 'iEnd=', iEnd

    do j = iBeg(2), iEnd(2)
        do i = iBeg(1), iEnd(1)
            xCentre = dgrid%x2dCentrePtr(i, j)
            yCentre = dgrid%y2dCentrePtr(i, j)
            localDataPtr(i, j) = xCentre + yCentre ! for example
        enddo
    enddo

    ! clean up
    call distfield_del(dfield)
    call distgrid_del(dgrid)
    deallocate(x2dCorner, y2dCorner)
    call ESMF_Finalize(rc=rc)

end program 