!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2026, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module ICE

  !-----------------------------------------------------------------------------
  ! OCN Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    modelSS    => SetServices
  use vtk_mod

  implicit none

  private

  real(8), parameter :: xmin = 0._8, xmax = 1._8, ymin = 0._8, ymax = 1._8

  public SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Model
    call NUOPC_CompDerive(model, modelSS, rc=rc)
    if (rc /= ESMF_SUCCESS) print*,'rc=', rc, ' at line ', __LINE__, ' in file ', __FILE__

    ! specialize model
    call NUOPC_CompSpecialize(model, specLabel=label_Advertise, &
      specRoutine=Advertise, rc=rc)
    if (rc /= ESMF_SUCCESS) print*,'rc=', rc, ' at line ', __LINE__, ' in file ', __FILE__


    call NUOPC_CompSpecialize(model, specLabel=label_RealizeProvided, &
      specRoutine=Realize, rc=rc)
    if (rc /= ESMF_SUCCESS) print*,'rc=', rc, ' at line ', __LINE__, ' in file ', __FILE__


    call NUOPC_CompSpecialize(model, specLabel=label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (rc /= ESMF_SUCCESS) print*,'rc=', rc, ' at line ', __LINE__, ' in file ', __FILE__


    call NUOPC_CompSpecialize(model, specLabel=label_Advance, &
      specRoutine=Advance, rc=rc)
    if (rc /= ESMF_SUCCESS) print*,'rc=', rc, ' at line ', __LINE__, ' in file ', __FILE__


  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advertise(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)

    ! Disabling the following macro, e.g. renaming to WITHIMPORTFIELDS_disable,
    ! will result in a model component that does not advertise any importable
    ! Fields. Use this if you want to drive the model independently.

    ! importable field: air_pressure_at_sea_level
    call NUOPC_Advertise(importState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)

    ! importable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(importState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)

    ! exportable field: albedo
    call NUOPC_Advertise(exportState, &
      StandardName="ice_albedo", name="ice_albedo", rc=rc)


  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Realize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    type(ESMF_TimeInterval) :: stabilityTimeStep
    type(ESMF_Field)        :: field_albedo, field_pmsl, field_rsns

    integer :: nx, ny, i, j
    integer :: lbCorner(2), ubCorner(2), lbCenter(2), ubCenter(2)
    real(ESMF_KIND_R8), pointer :: xCornerPtr(:,:), yCornerPtr(:,:)
    real(ESMF_KIND_R8), pointer :: xCenterPtr(:,:), yCenterPtr(:,:)
    real(ESMF_KIND_R8), pointer :: albedoPtr(:,:)
    real(ESMF_KIND_R8) :: x, y, pi
    type(ESMF_Grid) :: grid

    rc = ESMF_SUCCESS

    pi = acos(-1.0_8)

    ! Define the grid
    nx = 12
    ny = 24

    ! Create the grid with both CENTER and CORNER stagger locations
    grid = ESMF_GridCreateNoPeriDim( &
          regDecomp=(/1, 2/), &  ! assumes component is running on 2 processors
          coordDep1=(/1, 2/), & ! 1st coord is 2D and depends on both Grid dim
          coordDep2=(/1, 2/), &
          indexflag=ESMF_INDEX_GLOBAL, &
          maxIndex=(/nx, ny/), & ! number of cells?
          rc=rc)
    if (rc /= ESMF_SUCCESS) stop 'ESMF_GridCreateNoPeriDim failed'

    call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
    if (rc /= ESMF_SUCCESS) stop 'ESMF_GridAddCoord CORNER failed'
    call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (rc /= ESMF_SUCCESS) stop 'ESMF_GridAddCoord CENTER failed'

    !--------------------------------------------------------
    ! Get bounds for all coordinates and stagger locations
    !--------------------------------------------------------
    ! Need to call ESMF_GridGetCoordBounds for each coordinate to fill in the bounds along
    ! each axis
    ! Fill in corner bounds
    call ESMF_GridGetCoordBounds(grid, coordDim=1, staggerLoc=ESMF_STAGGERLOC_CORNER, &
                                exclusiveLBound=lbCorner, exclusiveUBound=ubCorner, rc=rc)
    call ESMF_GridGetCoordBounds(grid, coordDim=2, staggerLoc=ESMF_STAGGERLOC_CORNER, &
                                exclusiveLBound=lbCorner, exclusiveUBound=ubCorner, rc=rc)
    ! Fill in center bounds
    call ESMF_GridGetCoordBounds(grid, coordDim=1, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                                exclusiveLBound=lbCenter, exclusiveUBound=ubCenter, rc=rc)
    call ESMF_GridGetCoordBounds(grid, coordDim=2, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                                exclusiveLBound=lbCenter, exclusiveUBound=ubCenter, rc=rc)

    print*,'*** ICE corner bounds ', lbCorner, ubCorner
    print*,'*** ICE centre bounds ', lbCenter, ubCenter

    !--------------------------------------------------------
    ! Then: get the actual coordinate arrays
    !--------------------------------------------------------
    call ESMF_GridGetCoord(grid, 1, staggerLoc=ESMF_STAGGERLOC_CORNER, &
                          farrayPtr=xCornerPtr, &
                          rc=rc)
    print*,'rc = ', rc
    if (rc /= ESMF_SUCCESS) stop 'ESMF_GridGetCoord xCorner failed'

    call ESMF_GridGetCoord(grid, 2, staggerLoc=ESMF_STAGGERLOC_CORNER, &
                          farrayPtr=yCornerPtr, &
                          rc=rc)
    if (rc /= ESMF_SUCCESS) stop 'ESMF_GridGetCoord yCorner failed'

    call ESMF_GridGetCoord(grid, 1, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                          farrayPtr=xCenterPtr, &
                          rc=rc)
    if (rc /= ESMF_SUCCESS) stop 'ESMF_GridGetCoord xCenter failed'

    call ESMF_GridGetCoord(grid, 2, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                          farrayPtr=yCenterPtr, &
                          rc=rc)
    if (rc /= ESMF_SUCCESS) stop 'ESMF_GridGetCoord yCenter failed'

    !--------------------------------------------------------
    ! Coordinates can now be filled safely
    !--------------------------------------------------------
    do j = lbCorner(2), ubCorner(2)
        do i = lbCorner(1), ubCorner(1)
            xCornerPtr(i,j) = xmin + (i-1) * (xmax - xmin)/ real(nx, 8)
            yCornerPtr(i,j) = ymin + (j-1) * (ymax - ymin)/ real(ny, 8)
        enddo
    enddo

    do j = lbCenter(2), ubCenter(2)
        do i = lbCenter(1), ubCenter(1)
            xCenterPtr(i,j) = xmin + (i - 0.5_8) * (xmax - xmin)/ real(nx, 8)
            yCenterPtr(i,j) = ymin + (j - 0.5_8) * (ymax - ymin)/ real(ny, 8)
        enddo
    enddo


    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)

    ! importable field: air_pressure_at_sea_level
    field_pmsl = ESMF_FieldCreate(name="pmsl", grid=grid, &
      staggerloc=ESMF_STAGGERLOC_CENTER, typekind=ESMF_TYPEKIND_R8, rc=rc)

    call NUOPC_Realize(importState, field=field_pmsl, rc=rc)

    ! importable field: surface_net_downward_shortwave_flux
    field_rsns = ESMF_FieldCreate(name="rsns", grid=grid, &
      staggerloc=ESMF_STAGGERLOC_CENTER, typekind=ESMF_TYPEKIND_R8, rc=rc)

    call NUOPC_Realize(importState, field=field_rsns, rc=rc)

    ! exportable field
    field_albedo = ESMF_FieldCreate(name="ice_albedo", grid=grid, &
      staggerloc=ESMF_STAGGERLOC_CENTER, typekind=ESMF_TYPEKIND_R8, rc=rc)

    ! initialize
    call ESMF_FieldGet(field=field_albedo, farrayPtr=albedoPtr, rc=rc)

    ! set the albedo
    do j = lbCenter(2), ubCenter(2)
      do i = lbCenter(1), ubCenter(1)
        x = xCenterPtr(i, j)
        y = yCenterPtr(i, j)
        ! between 0 and 1
        albedoPtr(i, j) = 0.5_8 + 0.5_8 * sin(pi*x/(xmax - xmin)) * cos((pi*y - 1.2_8)/(ymax - ymin))
      enddo
    enddo
  
    call NUOPC_Realize(exportState, field=field_albedo, rc=rc)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetClock(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: stabilityTimeStep

    rc = ESMF_SUCCESS

    ! query for clock
    call NUOPC_ModelGet(model, modelClock=clock, rc=rc)

    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    !TODO: stabilityTimeStep should be read in from configuation
    !TODO: or computed from internal Grid information
    call ESMF_TimeIntervalSet(stabilityTimeStep, m=5, rc=rc) ! 5 minute steps

    call NUOPC_CompSetClock(model, clock, stabilityTimeStep, rc=rc)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: currTime
    type(ESMF_TimeInterval)     :: timeStep
    character(len=160)          :: msgString
    integer :: istep = 0
    type(ESMF_Field) :: field_albedo
    type(ESMF_Grid) :: grid
    real(8), pointer :: albedoPtr(:, :), xCenterPtr(:, :), yCenterPtr(:, :)
    integer :: i, j, lbCenter(2), ubCenter(2)
    real(8) :: pi, x, y
    type(ESMF_VM) :: compVM
    character(len=32) :: filename
    integer :: pe, comm
    type(ESMF_Field) :: field_pmsl
    real(8) :: chksum
    real(8), pointer :: dataPtr(:, :)

    rc = ESMF_SUCCESS
    pi = acos(-1._8)

    istep = istep + 1

    ! query for clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)

    ! update the export fields
    call ESMF_StateGet(exportState, itemName='ice_albedo', field=field_albedo, rc=rc)
    call ESMF_FieldGet(field_albedo, farrayPtr=albedoPtr, rc=rc)
    call ESMF_FieldGet(field_albedo, grid=grid, rc=rc)

    ! Fill in center bounds
    call ESMF_GridGetCoord(grid, coordDim=1, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                              farrayPtr=xCenterPtr, rc=rc)
    call ESMF_GridGetCoord(grid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
                              farrayPtr=yCenterPtr, rc=rc)
    call ESMF_GridGetCoordBounds(grid, coordDim=1, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                                exclusiveLBound=lbCenter, exclusiveUBound=ubCenter, rc=rc)
    call ESMF_GridGetCoordBounds(grid, coordDim=2, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                                exclusiveLBound=lbCenter, exclusiveUBound=ubCenter, rc=rc)

    ! set the fields
    do j = lbCenter(2), ubCenter(2)
      do i = lbCenter(1), ubCenter(1)
        x = xCenterPtr(i, j)
        y = yCenterPtr(i, j)
        ! move the field in time
        albedoPtr(i, j) = 0.5_8 + 0.5_8 *sin(pi*(x - istep*0.05_8)/(xmax - xmin)) * cos((pi*y - 1.2_8 - istep*0.07_8)/(ymax - ymin))
      enddo
    enddo


    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in
    ! multiple calls to the Advance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing ICE from: ", unit=msgString, rc=rc)

    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)

    call ESMF_TimePrint(currTime + timeStep, &
      preString="---------------------> to: ", unit=msgString, rc=rc)

    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

    ! query for importState
    call NUOPC_ModelGet(model, importState=importState, rc=rc)
    call ESMF_StateGet(importState, itemName='pmsl', field=field_pmsl, rc=rc)

    ! Get the VM associated with the component
    call ESMF_GridCompGet(model, vm=compVM, rc=rc)
  
    ! Get the specific MPI communicator
    call ESMF_VMGet(compVM, mpiCommunicator=comm, rc=rc)

    call MPI_Comm_rank(comm, pe, rc)
    write(filename, '(A,I4.4,A,I4.4,A)') 'ice_', pe, 'pe_', istep,'.vtk'
    call write_vtk(field_pmsl, filename)

    chksum = 0
    call ESMF_FieldGet(field_pmsl, farrayPtr=dataPtr, rc=rc)
    do j = lbCenter(2), ubCenter(2)
      do i = lbCenter(1), ubCenter(1)
        chksum = chksum + abs(dataPtr(i, j))
      enddo
    enddo
    print*,'ICE step=', istep, ' pe=', pe, ' chksum pmsl = ', chksum


  end subroutine

  !-----------------------------------------------------------------------------

end module
