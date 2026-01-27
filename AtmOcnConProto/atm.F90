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

    ! exportable field: air_pressure_at_sea_level
    call NUOPC_Advertise(exportState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)

    ! exportable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(exportState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Realize(model, rc)
    ! Create grid, fields and advertise iomport/export

    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Field)        :: field_sst, field_pmsl, field_rsns
    type(ESMF_Grid)         :: gridIn
    type(ESMF_Grid)         :: gridOut

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)

    ! create a Grid object for Fields
    gridIn = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/10, 100/), &
      minCornerCoord=(/10._ESMF_KIND_R8, 20._ESMF_KIND_R8/), &
      maxCornerCoord=(/100._ESMF_KIND_R8, 200._ESMF_KIND_R8/), &
      coordSys=ESMF_COORDSYS_CART, &
      staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
      rc=rc)

    gridOut = gridIn ! for now out same as in

    ! importable field: sea_surface_temperature
    field_sst = ESMF_FieldCreate(name="sst", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)

    ! fill in default values
    call ESMF_FieldFill(field_sst, dataFillScheme="const", const1=292.0_8, rc=rc)

    call NUOPC_Realize(importState, field=field_sst, rc=rc)

    ! exportable field: air_pressure_at_sea_level
    field_pmsl = ESMF_FieldCreate(name="pmsl", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)

    ! fill in default values
    call ESMF_FieldFill(field_pmsl, dataFillScheme="const", const1=101000.0_8, rc=rc)


    call NUOPC_Realize(exportState, field=field_pmsl, rc=rc)

    ! exportable field: surface_net_downward_shortwave_flux
    field_rsns = ESMF_FieldCreate(name="rsns", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)

    ! fill in default values
    call ESMF_FieldFill(field_rsns, dataFillScheme="const", const1=300.0_8, rc=rc)    

    call NUOPC_Realize(exportState, field=field_rsns, rc=rc)

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
    character(len=160)          :: msgString

    rc = ESMF_SUCCESS

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

  end subroutine

  !-----------------------------------------------------------------------------

end module
