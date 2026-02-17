!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2026, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module Driver

  !-----------------------------------------------------------------------------
  ! Code that specializes generic driver Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driverSS             => SetServices

  use Mediator, only: medSS => SetServices
  use ModelA, only: modASS => SetServices
  use ModelB, only: modBSS => SetServices

  use NUOPC_Connector, only: cplSS => SetServices

  implicit none

  private

  public SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Driver
    call NUOPC_CompDerive(driver, driverSS, rc=rc)

    ! specialize driver
    call NUOPC_CompSpecialize(driver, specLabel=label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
   call NUOPC_CompSpecialize(driver, specLabel=label_SetRunSequence, &
      specRoutine=SetRunSequence, rc=rc)

    call NUOPC_CompSpecialize(driver, specLabel=label_ModifyCplLists, &
      specRoutine=ModifyCplLists, rc=rc)

    ! set driver verbosity
    call NUOPC_CompAttributeSet(driver, name="Verbosity", value="high", rc=rc)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: internalClock
    integer                       :: petCount, i
    integer, allocatable          :: petList(:)
    type(ESMF_GridComp)           :: child

    rc = ESMF_SUCCESS

    ! get the petCount
    call ESMF_GridCompGet(driver, petCount=petCount, rc=rc)

     ! SetServices for Mediator with petList on first half of PETs
    allocate(petList(petCount/2))

    do i=1, petCount/2
      petList(i) = i-1 ! PET labeling goes from 0 to petCount-1
    enddo
    call NUOPC_DriverAddComp(driver, "Mediator", medSS, petList=petList, &
      comp=child, rc=rc)

    deallocate(petList)

    call NUOPC_CompAttributeSet(child, name="Verbosity", value="1", rc=rc)

    ! SetServices for ModelA and ModelB with petList on second half of PETs
    allocate(petList(petCount/2))

    do i=1, petCount/2
      petList(i) = petCount/2 + i-1 ! PET labeling goes from 0 to petCount-1
    enddo

    call NUOPC_DriverAddComp(driver, "ModelA", modASS, petList=petList, &
      comp=child, rc=rc)

    call NUOPC_CompAttributeSet(child, name="Verbosity", value="1", rc=rc)

    call NUOPC_DriverAddComp(driver, "ModelB", modBSS, petList=petList, &
      comp=child, rc=rc)

    call NUOPC_CompAttributeSet(child, name="Verbosity", value="1", rc=rc)

    ! set the model clock
    call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc) ! 15 minute steps
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeSet(startTime, yy=2010, mm=6, dd=1, h=0, m=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeSet(stopTime, yy=2010, mm=6, dd=1, h=1, m=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    internalClock = ESMF_ClockCreate(name="Application Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetRunSequence(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)              :: name
    type(NUOPC_FreeFormat)              :: runSeqFF

    rc = ESMF_SUCCESS

    ! query the driver for its name
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! set up free format run sequence
    runSeqFF = NUOPC_FreeFormatCreate(stringList=(/ &
      " @*                    ",    &
      "   ModelA -> Mediator  ",    &
      "   ModelB -> Mediator  ",    &
      "   Mediator            ",    &
      "   Mediator -> ModelA  ",    &
      "   Mediator -> ModelB  ",    &
      "   ModelA              ",    &
      "   ModelB              ",    &
      " @                     " /), &
      rc=rc)

    ! ingest FreeFormat run sequence
    call NUOPC_DriverIngestRunSequence(driver, runSeqFF, &
      autoAddConnectors=.true., rc=rc)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModifyCplLists(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(len=160)              :: msg
    type(ESMF_CplComp), pointer     :: connectorList(:)
    integer                         :: i, j, cplListSize
    character(len=160), allocatable :: cplList(:)
    character(len=160)              :: tempString

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("Driver is in ModifyCplLists()", ESMF_LOGMSG_INFO, rc=rc)

    nullify(connectorList)
    call NUOPC_DriverGetComp(driver, compList=connectorList, rc=rc)

    write (msg,*) "Found ", size(connectorList), " Connectors."// &
      " Modifying CplList Attribute...."
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)

    do i=1, size(connectorList)
      ! query the cplList for connector i
      call NUOPC_CompAttributeGet(connectorList(i), name="CplList", &
        itemCount=cplListSize, rc=rc)
      if (cplListSize>0) then
        allocate(cplList(cplListSize))
        call NUOPC_CompAttributeGet(connectorList(i), name="CplList", &
          valueList=cplList, rc=rc)
       ! go through all of the entries in the cplList and switch to redist
        do j=1, cplListSize
          ! switch remapping to redist
          cplList(j) = trim(cplList(j))//":REMAPMETHOD=redist"
        enddo
        ! store the modified cplList in CplList attribute of connector i
        call NUOPC_CompAttributeSet(connectorList(i), &
          name="CplList", valueList=cplList, rc=rc)
        deallocate(cplList)
      endif
    enddo
  
    deallocate(connectorList)

  end subroutine

  !-----------------------------------------------------------------------------

end module
