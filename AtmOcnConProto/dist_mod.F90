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
        type(distgrid_type) :: obj
        real(8), intent(in) :: x2dCorner(:, :), y2dCorner(:, :)
        integer :: regDecomp(:)

        real(8), pointer :: x2dPtr(:, :), y2dPtr(:, :)
        integer :: nx, ny, rc, i, j, i0, j0, i1, j1

        nx = size(x2dCorner, 1)
        ny = size(x2dCorner, 2)

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
                    farrayPtr=x2dPtr, &
                    exclusiveLBound=obj%iBegCorner, exclusiveUBound=obj%iEndCorner, rc=rc)
        call ESMF_GridGetCoord(obj%egrid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CORNER, &
                    farrayPtr=y2dPtr, rc=rc)
        do j = obj%iBegCorner(2), obj%iEndCorner(2)
            do i = 1, obj%iBegCorner(1), obj%iEndCorner(1)
                x2dPtr(i, j) = x2dCorner(i, j)
                y2dPtr(i, j) = y2dCorner(i, j)
            enddo
        enddo

        ! set the centre coordinates
        call ESMF_GridGetCoord(obj%egrid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
                    farrayPtr=x2dPtr, &
                    exclusiveLBound=obj%iBegCentre, exclusiveUBound=obj%iEndCentre, rc=rc)
        call ESMF_GridGetCoord(obj%egrid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
                    farrayPtr=y2dPtr, rc=rc)
        do j0 = obj%iBegCentre(2), obj%iEndCentre(2)
            j1 = j0 + 1
            do i0 = 1, obj%iBegCentre(1), obj%iEndCentre(1)
                i1 = i0 + 1
                x2dPtr(i0, j0) = 0.25_8*(x2dCorner(i0, j0) + x2dCorner(i1, j0) + x2dCorner(i1, j1) + x2dCorner(i0, j1))
                y2dPtr(i0, j0) = 0.25_8*(y2dCorner(i0, j0) + y2dCorner(i1, j0) + y2dCorner(i1, j1) + y2dCorner(i0, j1))
            enddo
        enddo

    end subroutine

    subroutine distgrid_del(obj)
        type(distgrid_type) :: obj
        call ESMF_GridDestroy(obj%egrid)
    end subroutine

end module

module distfield_mod
    use ESMF
    use distgrid_mod
    implicit none

    type distfield_type
        type(distgrid_type) :: dgrid
    end type

end module