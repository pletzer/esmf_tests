module distgrid_mod
    use ESMF
    implicit none

    type distgrid_type
        type(ESMF_Grid) :: egrid
        integer :: iBegCorner(2), iEndCorner(2)
        integer :: iBegCentre(2), iEndCentre(2)
        real(8), pointer :: x2dCornerPtr(:, :), y2dCornerPtr(:, :)
        real(8), pointer :: x2dCentrePtr(:, :), y2dCentrePtr(:, :)
    end type

contains

    subroutine distgrid_new(obj, x2dCorner, y2dCorner, regDecomp)
        type(distgrid_type), intent(inout) :: obj
        real(8), intent(in) :: x2dCorner(:, :), y2dCorner(:, :)
        integer, intent(in) :: regDecomp(:)

        integer :: nx, ny, rc, i, j, i0, j0, i1, j1

        ! number of cells
        nx = size(x2dCorner, 1) - 1
        ny = size(x2dCorner, 2) - 1

        ! distributed grid with user decomp
        obj%egrid = ESMF_GridCreateNoPeriDim( &
           minIndex=(/1,1/), &
           maxIndex=(/nx,ny/), &
           regDecomp=regDecomp, &
           coordSys=ESMF_COORDSYS_CART, &
           indexflag=ESMF_INDEX_GLOBAL, &
           rc=rc)

        ! add coordinates, corner and centre
        call ESMF_GridAddCoord(obj%egrid, staggerloc=ESMF_STAGGERLOC_CORNER, &
                                rc=rc)
        call ESMF_GridAddCoord(obj%egrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
                                rc=rc)

        ! set the corner coordinates
        call ESMF_GridGetCoord(obj%egrid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CORNER, &
                    farrayPtr=obj%x2dCornerPtr, &
                    exclusiveLBound=obj%iBegCorner, exclusiveUBound=obj%iEndCorner, rc=rc)
        call ESMF_GridGetCoord(obj%egrid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CORNER, &
                    farrayPtr=obj%y2dCornerPtr, rc=rc)
        do j = obj%iBegCorner(2), obj%iEndCorner(2)
            do i = obj%iBegCorner(1), obj%iEndCorner(1)
                obj%x2dCornerPtr(i, j) = x2dCorner(i, j)
                obj%y2dCornerPtr(i, j) = y2dCorner(i, j)
            enddo
        enddo

        ! set the centre coordinates, assume no halo...
        call ESMF_GridGetCoord(obj%egrid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
                    farrayPtr=obj%x2dCentrePtr, &
                    exclusiveLBound=obj%iBegCentre, exclusiveUBound=obj%iEndCentre, rc=rc)
        call ESMF_GridGetCoord(obj%egrid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
                    farrayPtr=obj%y2dCentrePtr, rc=rc)
        do j0 = obj%iBegCentre(2), obj%iEndCentre(2)
            j1 = j0 + 1
            do i0 = obj%iBegCentre(1), obj%iEndCentre(1)
                i1 = i0 + 1
                obj%x2dCentrePtr(i0, j0) = 0.25_8*(x2dCorner(i0, j0) + x2dCorner(i1, j0) + x2dCorner(i1, j1) + x2dCorner(i0, j1))
                obj%y2dCentrePtr(i0, j0) = 0.25_8*(y2dCorner(i0, j0) + y2dCorner(i1, j0) + y2dCorner(i1, j1) + y2dCorner(i0, j1))
            enddo
        enddo

    end subroutine

    subroutine distgrid_del(obj)
        type(distgrid_type), intent(inout) :: obj
        call ESMF_GridDestroy(obj%egrid)
        nullify(obj%x2dCornerPtr, obj%y2dCornerPtr, &
                obj%x2dCentrePtr, obj%y2dCentrePtr)
    end subroutine

end module

module distfield_mod
    use ESMF
    use distgrid_mod
    implicit none

    type distfield_type
        character(len=32) :: name
        type(distgrid_type), pointer :: dgridPtr
        real(8), pointer :: localDataPtr(:, :)
        type(ESMF_Field) :: efield
        type(ESMF_StaggerLoc) :: staggerLoc
    end type

contains

    subroutine distfield_new(obj, name, dgrid, staggerLoc)
        type(distfield_type), intent(inout) :: obj
        character(len=*), intent(in) :: name
        type(distgrid_type), target, intent(in) :: dgrid
        type(ESMF_StaggerLoc), intent(in) :: staggerLoc

        integer :: i, j, rc

        obj%name = name
        obj%dgridPtr => dgrid
        obj%staggerLoc = staggerLoc

        obj%efield = ESMF_FieldCreate(dgrid%egrid, name=name, staggerloc=staggerloc, typekind=ESMF_TYPEKIND_R8, rc=rc)
    end subroutine

    subroutine distfield_getLocalBounds(obj, iBeg, iEnd)
        type(distfield_type), intent(inout) :: obj
        integer, intent(out) :: iBeg(:), iEnd(:)
        if (obj%staggerLoc == ESMF_STAGGERLOC_CENTER) then
            iBeg = obj%dgridPtr%iBegCentre
            iEnd = obj%dgridPtr%iEndCentre
        else if (obj%staggerLoc == ESMF_STAGGERLOC_CENTER) then
            iBeg = obj%dgridPtr%iBegCorner
            iEnd = obj%dgridPtr%iEndCorner
        end if
    end subroutine

    subroutine distfield_getLocalDataPtr(obj, localDataPtr)
        type(distfield_type), intent(inout) :: obj
        real(8), pointer, intent(out) :: localDataPtr(:, :)
        integer :: rc
        call ESMF_FieldGet(obj%efield, farrayPtr=localDataPtr, rc=rc)
    end subroutine

    subroutine distfield_del(obj)
        type(distfield_type), intent(inout) :: obj
        call ESMF_FieldDestroy(obj%efield)
        nullify(obj%localDataPtr)
    end subroutine


end module 



