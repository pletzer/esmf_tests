module CON

  use ESMF
  use NUOPC
  use NUOPC_Connector, &
    conSS      => SetServices

  implicit none
  private
  public SetServices

  contains

  subroutine SetServices(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS
    call NUOPC_CompDerive(connector, conSS, rc=rc)

    call NUOPC_CompSpecialize(connector, specLabel=label_ComputeRouteHandle, &
      specRoutine=ComputeRH, rc=rc)
    call NUOPC_CompSpecialize(connector, specLabel=label_ExecuteRouteHandle, &
      specRoutine=ExecuteRH, rc=rc)
    call NUOPC_CompSpecialize(connector, specLabel=label_ReleaseRouteHandle, &
      specRoutine=ReleaseRH, rc=rc)
  end subroutine

  subroutine ComputeRH(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc

    type(ESMF_State)              :: state
    type(ESMF_FieldBundle)        :: dstFields, srcFields
    type(ESMF_RouteHandle)        :: rh

    rc = ESMF_SUCCESS

    call NUOPC_ConnectorGet(connector, srcFields=srcFields, &
      dstFields=dstFields, state=state, rc=rc)

    ! Compute a single RouteHandle that handles BOTH interpolation 
    ! and redistribution across different PET sets/communicators.
    call ESMF_FieldBundleRegridStore(srcFields, dstFields, &
      regridMethod=ESMF_REGRIDMETHOD_CONSERVE, &
      routehandle=rh, rc=rc)
    
    call ESMF_RouteHandleSet(rh, name="src2dstRH", rc=rc)

    ! Add the single combined handle to the state
    call ESMF_StateAdd(state, (/rh/), rc=rc)
  end subroutine

  subroutine ExecuteRH(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc

    type(ESMF_RouteHandle)        :: rh
    type(ESMF_State)              :: state
    type(ESMF_FieldBundle)        :: dstFields, srcFields
    type(ESMF_Clock)              :: clock
    character(len=160)            :: msgString

    rc = ESMF_SUCCESS

    call NUOPC_ConnectorGet(connector, srcFields=srcFields, &
      dstFields=dstFields, state=state, driverClock=clock, rc=rc)

    ! Retrieve the combined handle
    call ESMF_StateGet(state, "src2dstRH", rh, rc=rc)

    ! Single call: Regrid (math) and Redistribute (comm) across components
    call ESMF_FieldBundleRegrid(srcFields, dstFields, &
      routehandle=rh, rc=rc)
  end subroutine

  subroutine ReleaseRH(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc

    type(ESMF_State)              :: state
    type(ESMF_RouteHandle)        :: rh

    rc = ESMF_SUCCESS

    call NUOPC_ConnectorGet(connector, state=state, rc=rc)

    ! Retrieve and release the single combined handle
    call ESMF_StateGet(state, "src2dstRH", rh, rc=rc)
    call ESMF_FieldBundleRegridRelease(rh, rc=rc)
  end subroutine

end module

