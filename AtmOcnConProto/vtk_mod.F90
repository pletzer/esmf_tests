module vtk_mod
use ESMF
implicit none

contains 

    subroutine write_vtk(field, filename)
        type(ESMF_Field), intent(in) :: field
        character(len=*), intent(in) :: filename

        integer :: i, j, lbCenter(2), ubCenter(2), rc, iu, npoints
        type(ESMF_Grid) :: grid
        type(ESMF_StaggerLoc) :: staggerLoc
        real(8), pointer :: xCenterPtr(:, :), yCenterPtr(:, :), dataPtr(:, :)


        call ESMF_FieldGet(field, grid=grid, rc=rc)
        call ESMF_FieldGet(field, staggerloc=staggerLoc, rc=rc)

        do i = 1, 2
        call ESMF_GridGetCoordBounds(grid, coordDim=i, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                exclusiveLBound=lbCenter, exclusiveUBound=ubCenter, rc=rc)
        enddo

        call ESMF_GridGetCoord(grid, coordDim=1, staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=xCenterPtr, rc=rc)
        call ESMF_GridGetCoord(grid, coordDim=2, staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=yCenterPtr, rc=rc)

        call ESMF_FieldGet(field, farrayPtr=dataPtr, rc=rc)

        npoints = (ubCenter(1) - lbCenter(1) + 1) * (ubCenter(2) - lbCenter(2) + 1)

        open(file=filename, status='replace', action='write', newunit=iu)
        write(iu, '(A)') '# vtk DataFile Version 3.0'
        write(iu, '(A)') 'data'
        write(iu, '(A)') 'ASCII'
        write(iu, '(A)') 'DATASET STRUCTURED_GRID'
        ! need at least two z levels to have a single cell layer
        write(iu, '(A,I4,A,I4,A,I4)') 'DIMENSIONS ', ubCenter(1) - lbCenter(1) + 1, ' ', ubCenter(2) - lbCenter(2) + 1, ' ', 1
        write(iu, '(A,I4,A)') 'POINTS ', npoints, ' DOUBLE'
        do j = lbCenter(2), ubCenter(2)
            do i = lbCenter(1), ubCenter(1)
                write(iu, '(E20.12, A, E20.12, A, E20.12)') xCenterPtr(i, j), ' ', yCenterPtr(i, j), ' ', 0.0
            enddo
        enddo

        write(iu, '(A,I4)') 'POINT_DATA ', npoints
        write(iu, '(A)') 'SCALARS data double 1'
        write(iu, '(A)') 'LOOKUP_TABLE default'
        do j = lbCenter(2), ubCenter(2)
            do i = lbCenter(1), ubCenter(1)
                write(iu, '(E20.12,A)') dataPtr(i, j)
            enddo
        enddo

        close(iu)

    end subroutine

end module