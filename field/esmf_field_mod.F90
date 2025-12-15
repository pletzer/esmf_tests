module esmf_field_mod
    use ESMF
    implicit none

    type esmf_field_type
       type(ESMF_Grid)  :: grid
       type(ESMF_Field) :: field
       real(8), pointer :: data(:, :)
    end type esmf_field_type

contains

!====================================================================
subroutine esmf_field_new(self, name, lons, lats, data_in, rc)
!====================================================================
    type(esmf_field_type)             :: self
    character(len=*), intent(in)      :: name
    real(8), intent(in)               :: lons(:, :), lats(:, :)
    real(8), intent(in)               :: data_in(:, :)
    integer, intent(out)              :: rc

    integer :: nx, ny
    integer :: localrc
    real(8), pointer :: lonPtr(:,:), latPtr(:,:), dataPtr(:,:)

    rc = ESMF_RC_NOT_IMPL

    ny = size(lons, 1)
    nx = size(lons, 2)

    !--------------------------------------------------------------
    ! Create logically-rectangular grid
    !--------------------------------------------------------------
    self%grid = ESMF_GridCreateNoPeriDim(      &
                   maxIndex=(/nx, ny/),        &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   name=name//'_grid',         &
                   rc=localrc )
    if (localrc /= ESMF_SUCCESS) return

    ! Add coordinate arrays to the grid
    call ESMF_GridAddCoord(self%grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=localrc)
    if (localrc /= ESMF_SUCCESS) return
    call ESMF_GridAddCoord(self%grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=localrc)
    if (localrc /= ESMF_SUCCESS) return

    !--------------------------------------------------------------
    ! Access coordinate storage from grid
    !--------------------------------------------------------------
    call ESMF_GridGetCoord(self%grid, coordDim=1, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           farrayPtr=lonPtr, rc=localrc)
    if (localrc /= ESMF_SUCCESS) return

    call ESMF_GridGetCoord(self%grid, coordDim=2, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           farrayPtr=latPtr, rc=localrc)
    if (localrc /= ESMF_SUCCESS) return

    ! copy
    lonPtr = lons
    latPtr = lats

    !--------------------------------------------------------------
    ! Create Field on Grid
    !--------------------------------------------------------------
    self%field = ESMF_FieldCreate( grid=self%grid,         &
                                   name=name,              &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   rc=localrc )
    if (localrc /= ESMF_SUCCESS) return

    !--------------------------------------------------------------
    ! Access Field data array
    !--------------------------------------------------------------
    call ESMF_FieldGet(self%field, farrayPtr=dataPtr,      &
                       rc=localrc )
    if (localrc /= ESMF_SUCCESS) return

    dataPtr = data_in          ! copy user data into ESMF field
    self%data => dataPtr       ! save pointer

    rc = ESMF_SUCCESS
end subroutine esmf_field_new

!====================================================================
subroutine esmf_field_getDataPtr(self, dataPtr, rc)
!====================================================================
    type(esmf_field_type), intent(in) :: self
    real(8), pointer                  :: dataPtr(:, :)
    integer, intent(out)              :: rc

    dataPtr => self%data
    rc = ESMF_SUCCESS
end subroutine esmf_field_getDataPtr

!====================================================================
subroutine esmf_field_del(self, rc)
!====================================================================
    type(esmf_field_type) :: self
    integer, intent(out)  :: rc
    integer :: localrc

    call ESMF_FieldDestroy(self%field, rc=localrc)
    call ESMF_GridDestroy (self%grid,  rc=localrc)

    nullify(self%data)

    rc = ESMF_SUCCESS
end subroutine esmf_field_del

end module esmf_field_mod
