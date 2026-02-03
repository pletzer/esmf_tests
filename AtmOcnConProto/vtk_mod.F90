module vtk_mod
use ESMF
implicit none

contains 

    subroutine write_vtk(field, filename)
        type(ESMF_Field), intent(in) :: field
        character(len=*), intent(in) :: filename

        integer :: i, j, lbCorner(2), ubCorner(2), lbCenter(2), ubCenter(2), rc, iu, npoints, ncells
        type(ESMF_Grid) :: grid
        type(ESMF_StaggerLoc) :: staggerLoc
        real(8), pointer :: xCornerPtr(:, :), yCornerPtr(:, :), dataPtr(:, :)


        call ESMF_FieldGet(field, grid=grid, rc=rc)
        call ESMF_FieldGet(field, staggerloc=staggerLoc, rc=rc)

        call ESMF_GridGetCoordBounds(grid, 1, staggerLoc=ESMF_STAGGERLOC_CORNER, &
                exclusiveLBound=lbCorner, exclusiveUBound=ubCorner, rc=rc)
        call ESMF_GridGetCoordBounds(grid, 2, staggerLoc=ESMF_STAGGERLOC_CORNER, &
                exclusiveLBound=lbCorner, exclusiveUBound=ubCorner, rc=rc)
        call ESMF_GridGetCoordBounds(grid, 1, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                exclusiveLBound=lbCenter, exclusiveUBound=ubCenter, rc=rc)
        call ESMF_GridGetCoordBounds(grid, 2, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                exclusiveLBound=lbCenter, exclusiveUBound=ubCenter, rc=rc)

        call ESMF_GridGetCoord(grid, 1, staggerLoc=ESMF_STAGGERLOC_CORNER, farrayPtr=xCornerPtr, rc=rc)
        call ESMF_GridGetCoord(grid, 2, staggerLoc=ESMF_STAGGERLOC_CORNER, farrayPtr=yCornerPtr, rc=rc)

        call ESMF_FieldGet(field, farrayPtr=dataPtr, rc=rc)

        npoints = (ubCorner(1) - lbCorner(1) + 1) * (ubCorner(2) - lbCorner(2) + 1)
        ncells = (ubCenter(1) - lbCenter(1) + 1) * (ubCenter(2) - lbCenter(2) + 1)

        open(file=filename, status='replace', action='write', newunit=iu)
        write(iu, *) 'vtk DataFile Version 2.0'
        write(iu, *) 'data'
        write(iu, *) 'ASCII'
        write(iu, *) 'DATASET STRUCTURED_GRID'
        write(iu, *) 'DIMENSIONS ', ubCorner(1) - lbCorner(1) + 1, ' ', ubCorner(2) - lbCorner(2) + 1, ' ', 1
        write(iu, *) 'POINTS ', npoints, ' DOUBLE'
        do j = lbCorner(2), ubCorner(2)
            do i = lbCorner(1), ubCorner(1)
                write(iu, '(E20.12, A, E20.12, A)') xCornerPtr(i, j), ' ', yCornerPtr(i, j), ' 0.0'
            enddo
        enddo
        write(iu, *) 'CELL_DATA ', ncells
        write(iu, *) 'SCALARS data double 1'
        write(iu, *) 'LOOKUP_TABLE default'
        do j = lbCenter(2), ubCenter(2)
            do i = lbCenter(1), ubCenter(1)
                write(iu, '(E20.12,A)') dataPtr(i, j)
            enddo
        enddo

        close(iu)



    end subroutine

end module