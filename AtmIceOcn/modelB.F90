!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2026, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module ModelB

  !-----------------------------------------------------------------------------
  ! ModelB Component.
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

    ! exportable field: PINT
    call NUOPC_Advertise(exportState, &
      StandardName="PINT", rc=rc)

    ! exportable field: air_pressure_at_sea_level
    call NUOPC_Advertise(exportState, &
      StandardName="air_pressure_at_sea_level", rc=rc)

    ! exportable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(exportState, &
      StandardName="surface_net_downward_shortwave_flux", rc=rc)

    ! importable field: sea_surface_temperature
    call NUOPC_Advertise(importState, &
      StandardName="sea_surface_temperature", rc=rc)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Realize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)                  :: importState, exportState
    type(ESMF_Field)                  :: field
    type(ESMF_Grid)                   :: gridIn, gridOut
    integer                           :: i, j
    real(kind=ESMF_KIND_R8),  pointer :: lonPtr(:,:), latPtr(:,:)

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)

    ! create Grid objects for Fields
    gridIn = ESMF_GridCreate1PeriDim(minIndex=(/1,1/), maxIndex=(/100,150/), &
      indexflag=ESMF_INDEX_GLOBAL, coordSys=ESMF_COORDSYS_SPH_DEG, rc=rc)

    call ESMF_GridAddCoord(gridIn, rc=rc)

    call ESMF_GridGetCoord(gridIn, coordDim=1, farrayPtr=lonPtr, rc=rc)

    call ESMF_GridGetCoord(gridIn, coordDim=2, farrayPtr=latPtr, rc=rc)
 
    do j=lbound(lonPtr,2),ubound(lonPtr,2)
    do i=lbound(lonPtr,1),ubound(lonPtr,1)
      lonPtr(i,j) = 360./real(100) * (i-1)
      latPtr(i,j) = 100./real(150) * (j-1) - 50.
    enddo
    enddo

    gridOut = gridIn ! for now out same as in

    ! exportable field: PINT
    field = ESMF_FieldCreate(name="PINT", &
      grid=gridIn, typekind=ESMF_TYPEKIND_R8, &
      ungriddedLBound=(/1/), ungriddedUBound=(/35/), rc=rc)

    call NUOPC_Realize(exportState, field=field, rc=rc)

    ! exportable field: air_pressure_at_sea_level
    field = ESMF_FieldCreate(name="air_pressure_at_sea_level", &
      grid=gridIn, typekind=ESMF_TYPEKIND_R8, rc=rc)

    call NUOPC_Realize(exportState, field=field, rc=rc)

    ! exportable field: surface_net_downward_shortwave_flux
    field = ESMF_FieldCreate(name="surface_net_downward_shortwave_flux", &
      grid=gridIn, typekind=ESMF_TYPEKIND_R8, rc=rc)

    call NUOPC_Realize(exportState, field=field, rc=rc)

    ! importable field: sea_surface_temperature
    field = ESMF_FieldCreate(name="sea_surface_temperature", &
      grid=gridOut, typekind=ESMF_TYPEKIND_R8, rc=rc)

    call NUOPC_Realize(importState, field=field, rc=rc)

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
    call ESMF_GridCompGet(model, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing ModelB from: ", unit=msgString, rc=rc)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)

    call ESMF_TimePrint(currTime + timeStep, &
      preString="------------------------> to: ", unit=msgString, rc=rc)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

  end subroutine

  !-----------------------------------------------------------------------------

end module
