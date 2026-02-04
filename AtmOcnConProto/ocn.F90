!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2026, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module OCN

  !-----------------------------------------------------------------------------
  ! OCN Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    modelSS    => SetServices

  implicit none

  private

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

    ! specialize model
    call NUOPC_CompSpecialize(model, specLabel=label_Advertise, &
      specRoutine=Advertise, rc=rc)

    call NUOPC_CompSpecialize(model, specLabel=label_RealizeProvided, &
      specRoutine=Realize, rc=rc)

    call NUOPC_CompSpecialize(model, specLabel=label_SetClock, &
      specRoutine=SetClock, rc=rc)

    call NUOPC_CompSpecialize(model, specLabel=label_Advance, &
      specRoutine=Advance, rc=rc)

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

    ! exportable field: sea_surface_temperature
    call NUOPC_Advertise(exportState, &
      StandardName="sea_surface_temperature", name="sst", rc=rc)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Realize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    type(ESMF_TimeInterval) :: stabilityTimeStep
    type(ESMF_Field)        :: field_sst, field_pmsl, field_rsns

    integer :: nx, ny, i, j
    integer :: lbCorner(2), ubCorner(2), lbCenter(2), ubCenter(2)
    real(ESMF_KIND_R8), pointer :: xCornerPtr(:,:), yCornerPtr(:,:)
    real(ESMF_KIND_R8), pointer :: xCenterPtr(:,:), yCenterPtr(:,:)
    real(ESMF_KIND_R8), pointer :: sstPtr(:,:)
    real(ESMF_KIND_R8) :: x, y, xmin, xmax, ymin, ymax, pi
    type(ESMF_Grid) :: grid

    rc = ESMF_SUCCESS

    pi = acos(-1.0_8)

    ! Define the grid
    nx = 16
    ny = 32
    xmin = 0.0_8
    xmax = 1.0_8
    ymin = 0.0_8
    ymax = 1.0_8

    ! Create the grid with both CENTER and CORNER stagger locations
    grid = ESMF_GridCreateNoPeriDim( &
          regDecomp=(/1, 2/), &
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

    print*,'*** OCN corner bounds ', lbCorner, ubCorner
    print*,'*** OCN centre bounds ', lbCenter, ubCenter

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

    ! exportable field: sea_surface_temperature
    field_sst = ESMF_FieldCreate(name="sst", grid=grid, &
      staggerloc=ESMF_STAGGERLOC_CENTER, typekind=ESMF_TYPEKIND_R8, rc=rc)

    ! initialize
    call ESMF_FieldGet(field=field_sst, farrayPtr=sstPtr, rc=rc)

    ! set the SST 
    do j = lbCenter(2), ubCenter(2)
      do i = lbCenter(1), ubCenter(1)
        x = xCenterPtr(i, j)
        y = yCenterPtr(i, j)
        sstPtr(i, j) = sin(pi*x/(xmax - xmin)) * cos((pi*y - 1.2_8)/(ymax - ymin))
      enddo
    enddo
  
    call NUOPC_Realize(exportState, field=field_sst, rc=rc)

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

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in
    ! multiple calls to the Advance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing OCN from: ", unit=msgString, rc=rc)

    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)

    call ESMF_TimePrint(currTime + timeStep, &
      preString="---------------------> to: ", unit=msgString, rc=rc)

    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

  end subroutine

  !-----------------------------------------------------------------------------

end module
