!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2026, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module ATM

  !-----------------------------------------------------------------------------
  ! ATM Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    modelSS    => SetServices
  use vtk_mod
  use mpi

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

    ! importable field: sea_surface_temperature
    call NUOPC_Advertise(importState, &
      StandardName="sea_surface_temperature", name="sst", rc=rc)

    call NUOPC_Advertise(importState, &
      StandardName="ice_albedo", name="ice_albedo", rc=rc)

    ! exportable field: air_pressure_at_sea_level
    call NUOPC_Advertise(exportState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)

    ! exportable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(exportState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Realize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    type(ESMF_TimeInterval) :: stabilityTimeStep
    type(ESMF_Field)        :: field_sst, field_pmsl, field_rsns, field_albedo

    integer :: nx, ny, i, j
    integer :: lbCorner(2), ubCorner(2), lbCenter(2), ubCenter(2)
    real(ESMF_KIND_R8), pointer :: xCornerPtr(:,:), yCornerPtr(:,:)
    real(ESMF_KIND_R8), pointer :: xCenterPtr(:,:), yCenterPtr(:,:)
    real(ESMF_KIND_R8), pointer :: pmslPtr(:,:)
    real(ESMF_KIND_R8) :: x, y, xmin, xmax, ymin, ymax
    type(ESMF_Grid) :: grid
    real(8) :: pi

    rc = ESMF_SUCCESS
    pi = acos(-1._8)

    ! Define grid 
    nx = 24
    ny = 12
    xmin = 0.0_8
    xmax = 1.0_8
    ymin = 0.0_8
    ymax = 1.0_8

    ! Create the grid with both CENTER and CORNER stagger locations
    grid = ESMF_GridCreateNoPeriDim( &
          regDecomp=(/1, 2/), &  ! assumes component is running on 2 processors!!!
          coordDep1=(/1, 2/), &  ! 1st coord is 2D and depends on both Grid dim
          coordDep2=(/1, 2/), &
          indexflag=ESMF_INDEX_GLOBAL, &
          maxIndex=(/nx, ny/), &
          rc=rc)
    if (rc /= ESMF_SUCCESS) stop 'ESMF_GridCreateNoPeriDim failed'

    call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
    if (rc /= ESMF_SUCCESS) stop 'ESMF_GridAddCoord CORNER failed'

    call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (rc /= ESMF_SUCCESS) stop 'ESMF_GridAddCoord CENTER failed'

    !--------------------------------------------------------
    ! Get bounds for all coordinates and stagger locations
    !--------------------------------------------------------
    call ESMF_GridGetCoordBounds(grid, coordDim=1, staggerLoc=ESMF_STAGGERLOC_CORNER, &
                                exclusiveLBound=lbCorner, exclusiveUBound=ubCorner, rc=rc)
    call ESMF_GridGetCoordBounds(grid, coordDim=2, staggerLoc=ESMF_STAGGERLOC_CORNER, &
                                exclusiveLBound=lbCorner, exclusiveUBound=ubCorner, rc=rc)

    call ESMF_GridGetCoordBounds(grid, coordDim=1, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                                exclusiveLBound=lbCenter, exclusiveUBound=ubCenter, rc=rc)
    call ESMF_GridGetCoordBounds(grid, coordDim=2, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                                exclusiveLBound=lbCenter, exclusiveUBound=ubCenter, rc=rc)

    print*,'*** ATM corner bounds ', lbCorner, ubCorner
    print*,'*** ATM centre bounds ', lbCenter, ubCenter

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
    ! Coordinates can now be filled safely, e.g. uniform
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

    ! exportable field: air_pressure_at_sea_level
    field_pmsl = ESMF_FieldCreate(name="pmsl", grid=grid, &
      staggerloc=ESMF_STAGGERLOC_CENTER, typekind=ESMF_TYPEKIND_R8, rc=rc)

    ! initialize
    call ESMF_FieldGet(field=field_pmsl, farrayPtr=pmslPtr, rc=rc)

    ! set the PMSL
    do j = lbCenter(2), ubCenter(2)
      do i = lbCenter(1), ubCenter(1)
        x = xCenterPtr(i, j)
        y = yCenterPtr(i, j)
        pmslPtr(i, j) = cos(pi*x/(xmax - xmin)) * sin((pi*y - 1.2_8)/(ymax - ymin))
      enddo
    enddo

    call NUOPC_Realize(exportState, field=field_pmsl, rc=rc)

    ! exportable field: surface_net_downward_shortwave_flux
    field_rsns = ESMF_FieldCreate(name="rsns", grid=grid, &
      staggerloc=ESMF_STAGGERLOC_CENTER, typekind=ESMF_TYPEKIND_R8, rc=rc)

    call NUOPC_Realize(exportState, field=field_rsns, rc=rc)

    ! importable field: sea_surface_temperature
    field_sst = ESMF_FieldCreate(name="sst", grid=grid, &
      staggerloc=ESMF_STAGGERLOC_CENTER, typekind=ESMF_TYPEKIND_R8, rc=rc)
 
    call NUOPC_Realize(importState, field=field_sst, rc=rc)

    ! importable field: ice albedo
    field_albedo = ESMF_FieldCreate(name="ice_albedo", grid=grid, &
      staggerloc=ESMF_STAGGERLOC_CENTER, typekind=ESMF_TYPEKIND_R8, rc=rc)
 
    call NUOPC_Realize(importState, field=field_albedo, rc=rc)


  end subroutine
  !-----------------------------------------------------------------------------

  subroutine Run(model, rc)
    
    ! Use this to initialize the values of the fields

    type(ESMF_GridComp) :: model
    integer, intent(out) :: rc

    type(ESMF_State) :: exportState
    type(ESMF_Field) :: sst, pmsl, rsns
    real(ESMF_KIND_R8), pointer :: ptr(:,:)

    rc = ESMF_SUCCESS

    ! Get the export state
    call NUOPC_ModelGet(model, exportState=exportState, rc=rc)

    ! --- PMSL ---
    call ESMF_StateGet(exportState, itemName="pmsl", field=pmsl, rc=rc)
    call ESMF_FieldGet(pmsl, farrayPtr=ptr, rc=rc)
    ptr = 101325.0_ESMF_KIND_R8

    ! --- RSNS ---
    call ESMF_StateGet(exportState, itemName="rsns", field=rsns, rc=rc)
    call ESMF_FieldGet(rsns, farrayPtr=ptr, rc=rc)
    ptr = 200.0_ESMF_KIND_R8
  end subroutine


  !-----------------------------------------------------------------------------

  subroutine Advance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Field)            :: field_sst
    real(8), pointer :: sstPtr(:, :)
    character(len=160)          :: msgString
    integer :: i, j
    character(len=32) :: filename
    integer :: pe, comm
    integer :: istep = 0
    type(ESMF_Time) :: currTime, startTime
    type(ESMF_VM) :: compVM
    real(8) :: chksum
    real(8), pointer :: dataPtr(:, :)
    integer :: lbCenter(2), ubCenter(2)
    type(ESMF_Grid) :: grid

    rc = ESMF_SUCCESS

    istep = istep + 1

    ! query for clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Because of the way that the internal Clock was set by default,
    ! its timeStep is equal to the parent timeStep. As a consequence the
    ! currTime + timeStep is equal to the stopTime of the internal Clock
    ! for this call of the Advance() routine.

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing ATM from: ", unit=msgString, rc=rc)

    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="---------------------> to: ", unit=msgString, rc=rc)

    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

    ! query for importState
    call NUOPC_ModelGet(model, importState=importState, rc=rc)
    call ESMF_StateGet(importState, itemName='sst', field=field_sst, rc=rc)

    ! Get the VM associated with the component
    call ESMF_GridCompGet(model, vm=compVM, rc=rc)
  
    ! Get the specific MPI communicator
    call ESMF_VMGet(compVM, mpiCommunicator=comm, rc=rc)

    call MPI_Comm_rank(comm, pe, rc)
    write(filename, '(A,I4.4,A,I4.4,A)') 'atm_', pe, 'pe_', istep,'.vtk'
    call write_vtk(field_sst, filename)

    chksum = 0
    call ESMF_FieldGet(field_sst, farrayPtr=dataPtr, rc=rc)
    call ESMF_FieldGet(field_sst, grid=grid, rc=rc)

    ! Fill in center bounds
    do i = 1, 2
      call ESMF_GridGetCoordBounds(grid, coordDim=i, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                              exclusiveLBound=lbCenter, exclusiveUBound=ubCenter, rc=rc)
    enddo
    do j = lbCenter(2), ubCenter(2)
      do i = lbCenter(1), ubCenter(1)
        chksum = chksum + abs(dataPtr(i, j))
      enddo
    enddo
    print*,'ATM step=', istep, ' pe=', pe, ' chksum sst = ', chksum

  end subroutine

  !-----------------------------------------------------------------------------

end module
