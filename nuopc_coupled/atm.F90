! atm.F90
!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
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
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! specialize model
    call NUOPC_CompSpecialize(model, specLabel=label_Advertise, &
      specRoutine=Advertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=label_RealizeProvided, &
      specRoutine=Realize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=label_Advance, &
      specRoutine=Advance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

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
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Disabling the following macro, e.g. renaming to WITHIMPORTFIELDS_disable,
    ! will result in a model component that does not advertise any importable
    ! Fields. Use this if you want to drive the model independently.
#define WITHIMPORTFIELDS
#ifdef WITHIMPORTFIELDS
    ! importable field: sea_surface_temperature
    call NUOPC_Advertise(importState, &
      StandardName="sea_surface_temperature", name="sst", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! exportable field: air_pressure_at_sea_level
    call NUOPC_Advertise(exportState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(exportState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Realize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Field)        :: field
    type(ESMF_Grid)         :: gridIn
    type(ESMF_Grid)         :: gridOut
 
    real(8) :: minCornerCoord(2), maxCornerCoord(2)
    integer :: maxIndexOcn(2), maxIndexAtm(2), unit_num 
                                                                                                                                                                                                              
    namelist /domain/ minCornerCoord, maxCornerCoord 
    namelist /ocn/ maxIndexOcn
    namelist /atm/ maxIndexAtm 
                                                                                                                                                                                                              
    rc = ESMF_SUCCESS 
    ! read the namelist 
    open(newunit=unit_num, file='2comp_time_example.nml', status='old', iostat=rc)
    if (rc /= 0) then 
      print*,'cannot open 2comp_time_example.nml'
      return
    endif

    read(unit_num, nml=domain, iostat=rc) 
    if (rc /= 0) then 
      print*,'cannot read domain namelist section'
      return
    endif

    read(unit_num, nml=atm, iostat=rc)
    if (rc /= 0) then 
      print*,'cannot read atm namelist section'
      return
    endif

    read(unit_num, nml=ocn, iostat=rc)
    if (rc /= 0) then 
      print*,'cannot read ocn namelist section'
      return
    endif

    close(unit_num)

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    print*,'ATM: maxIndexOcn = ', maxIndexOcn, ' maxIndexAtm = ', maxIndexAtm

    ! create a Grid object for Fields
    gridIn = ESMF_GridCreateNoPeriDimUfrm(maxIndex=maxIndexOcn, &
      minCornerCoord=minCornerCoord, &
      maxCornerCoord=maxCornerCoord, &
      coordSys=ESMF_COORDSYS_CART, &
      staggerLocList=(/ESMF_STAGGERLOC_CORNER, ESMF_STAGGERLOC_CENTER/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    gridOut = ESMF_GridCreateNoPeriDimUfrm(maxIndex=maxIndexAtm, &
      minCornerCoord=minCornerCoord, &                                                                                                                                                                        
      maxCornerCoord=maxCornerCoord, &                                                                                                                                                                        
      coordSys=ESMF_COORDSYS_CART, &
      staggerLocList=(/ESMF_STAGGERLOC_CORNER, ESMF_STAGGERLOC_CENTER/), &                                                                                                                               
      rc=rc)                                                                                                                                                                                                  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &                                                                                                                                          
      line=__LINE__, &                                                                                                                                                                                        
      file=__FILE__)) &                                                                                                                                                                                       
      return  ! bail out       

#ifdef WITHIMPORTFIELDS
    ! importable field: sea_surface_temperature
    field = ESMF_FieldCreate(name="sst", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! exportable field: air_pressure_at_sea_level
#ifdef CREATE_AND_REALIZE
    ! This branch shows the standard procedure of creating a complete field
    ! with Grid and memory allocation, and then calling Realize() for it.
    field = ESMF_FieldCreate(name="pmsl", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#else
    ! This branch shows the alternative way of "realizing" an advertised field.
    ! It accesses the empty field that was created during advertise, and
    ! finishes it, setting a Grid on it, and then calling FieldEmptyComplete().
    ! No formal Realize() is then needed.
    call ESMF_StateGet(exportState, field=field, itemName="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldEmptySet(field, grid=gridOut, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#define WITH_FORMAL_REALIZE
#ifdef WITH_FORMAL_REALIZE
    ! There is not need to formally call Realize() when completing the
    ! adverised field directly. However, calling Realize() also works.
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
#endif

    ! exportable field: surface_net_downward_shortwave_flux
    field = ESMF_FieldCreate(name="rsns", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    character(len=160)          :: msgString

    type(ESMF_Field) :: field
    type(ESMF_Grid) :: grid
    real(8), pointer :: xPtr(:), yPtr(:), dataPtr(:, :)
    real(8) :: x, y, error
    integer :: xLBound(1), xUBound(1), yLBound(1), yUBound(1), i, j
    logical :: isConnected
    integer :: localDECount, lDE

#define NUOPC_TRACE__OFF
#ifdef NUOPC_TRACE
    call ESMF_TraceRegionEnter("ATM:Advance")
#endif

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Because of the way that the internal Clock was set by default,
    ! its timeStep is equal to the parent timeStep. As a consequence the
    ! currTime + timeStep is equal to the stopTime of the internal Clock
    ! for this call of the Advance() routine.

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing ATM from: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="---------------------> to: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! checking if the imported field is connected
    isConnected = NUOPC_IsConnected(importState, fieldName="sst", rc=rc)
    if (.not. isConnected) then
      rc = ESMF_SUCCESS
      print*,'ERROR import state sst is not connected'
      return
    endif

    ! checking the imported fields
    call ESMF_StateGet(importState, itemName="sst", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

    call ESMF_FieldGet(field, farrayPtr=dataPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out


    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

    ! get the grid coordinates. THIS CRASHES!
    xPtr => null()
    xLBound = 0
    xUBound = 0
    call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
                          staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=xPtr, &
                          computationalLBound=xLBound, &
                          computationalUBound=xUBound, &
                          rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

    yPtr => null()
    yLBound = 0
    yUBound = 0
    call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
                          staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=yPtr, &
                          computationalLBound=yLBound, &
                          computationalUBound=yUBound, &
                          rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

    ! check sst
    error = 0_8
    do j = yLBound(1), yUBound(1)
      y = yPtr(j)
      do i = xLBound(1), xUBound(1)
        x = xPtr(i)
        error = error + abs(dataPtr(i,j) - x*(y + 2*x))
      enddo
    enddo
    print *,'ATM: sst regridding error = ', error
    if (error > 1.e-3) print *, dataPtr



#ifdef NUOPC_TRACE
    call ESMF_TraceRegionExit("ATM:Advance")
#endif

  end subroutine Advance

  !-----------------------------------------------------------------------------

end module

