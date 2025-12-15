program test_esmf_field
    use ESMF
    use esmf_field_mod
    implicit none

    type(esmf_field_type) :: F
    integer :: rc
    integer, parameter :: nx=4, ny=3
    real(8) :: lons(ny,nx), lats(ny,nx), data(ny,nx)
    real(8), pointer :: dptr(:,:)
    integer :: i, j
    type(ESMF_VM) :: vm

    !------------------------------------------
    ! Initialize ESMF
    !------------------------------------------
    call ESMF_Initialize(vm=vm, defaultlogfilename='test.log', rc=rc)
    if (rc /= ESMF_SUCCESS) then
       print *, "ESMF_Initialize FAILED, rc=", rc
       stop
    end if

    !------------------------------------------
    ! Build simple test coordinate grid
    !------------------------------------------
    do j = 1, ny
        do i = 1, nx
            lons(j,i) = 160.0d0 + (i-1)*0.5d0
            lats(j,i) = -45.0d0 + (j-1)*0.5d0
            data(j,i) = 10.0d0*j + i   ! Some recognizable pattern
        end do
    end do

    !------------------------------------------
    ! Create ESMF field through wrapper
    !------------------------------------------
    call esmf_field_new(F, "demo_field", lons, lats, data, rc)
    if (rc /= ESMF_SUCCESS) then
        print *, "esmf_field_new FAILED, rc=", rc
        stop
    end if

    !------------------------------------------
    ! Retrieve data pointer
    !------------------------------------------
    call esmf_field_getDataPtr(F, dptr, rc)
    if (rc /= ESMF_SUCCESS) then
        print *, "getDataPtr FAILED, rc=", rc
        stop
    end if

    !------------------------------------------
    ! Print part of the data
    !------------------------------------------
    print *, "Field data:"
    do j = 1, ny
        print '(4F10.3)', (dptr(j,i), i=1,nx)
    end do

    !------------------------------------------
    ! Destroy field
    !------------------------------------------
    call esmf_field_del(F, rc)
    if (rc /= ESMF_SUCCESS) then
        print *, "esmf_field_del FAILED, rc=", rc
    end if

    call ESMF_Finalize(rc=rc)
    print *, "ESMF_Finalize rc=", rc

end program test_esmf_field
