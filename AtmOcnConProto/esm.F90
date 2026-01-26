!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2026, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module ESM

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  ! Enabling the followng macro, i.e. setting it to WITHPETLISTS_on, will
  ! activate sections of code that demonstrate how
  ! the ATM and OCN components can run on exclusive sets of PETs. Turning this
  ! on/off does not affect how the Connector component is specialized.

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driverSS             => SetServices

  use ATM, only: atmSS => SetServices
  use OCN, only: ocnSS => SetServices

  use CON, only: cplSS => SetServices

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

    ! set driver verbosity
    call NUOPC_CompAttributeSet(driver, name="Verbosity", value="high", rc=rc)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Grid)               :: grid
    type(ESMF_Field)              :: field
    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: internalClock
    type(ESMF_GridComp)           :: child
    type(ESMF_CplComp)            :: conn
    integer                       :: petCount, i
    integer, allocatable          :: petList(:)
    integer                       :: verbosity
    character(len=10)             :: attrStr

    rc = ESMF_SUCCESS

   ! get the petCount
    call ESMF_GridCompGet(driver, petCount=petCount, rc=rc)

    ! SetServices for ATM with petList on first half of PETs
    allocate(petList(petCount/2))
    do i=1, petCount/2
      petList(i) = i-1 ! PET labeling goes from 0 to petCount-1
    enddo
    call NUOPC_DriverAddComp(driver, "ATM", atmSS, &
      petList=petList, &
      comp=child, rc=rc)
    deallocate(petList)
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    write(attrStr,"(I10)") verbosity
    call NUOPC_CompAttributeSet(child, name="Verbosity", value=attrStr, rc=rc)

    ! SetServices for OCN with petList on second half of PETs
    allocate(petList(petCount/2))
    do i=1, petCount/2
      petList(i) = petCount/2 + i-1 ! PET labeling goes from 0 to petCount-1
    enddo
    call NUOPC_DriverAddComp(driver, "OCN", ocnSS, &
      petList=petList, &
      comp=child, rc=rc)
    deallocate(petList)

    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    write(attrStr,"(I10)") verbosity
    call NUOPC_CompAttributeSet(child, name="Verbosity", value=attrStr, rc=rc)

    ! SetServices for atm2ocn
    call NUOPC_DriverAddComp(driver, srcCompLabel="ATM", dstCompLabel="OCN", &
      compSetServicesRoutine=cplSS, comp=conn, rc=rc)

    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    verbosity = ibset(verbosity,12) ! log CplList loop
    write(attrStr,"(I10)") verbosity
    call NUOPC_CompAttributeSet(conn, name="Verbosity", value=attrStr, rc=rc)

    ! SetServices for ocn2atm
    call NUOPC_DriverAddComp(driver, srcCompLabel="OCN", dstCompLabel="ATM", &
      compSetServicesRoutine=cplSS, comp=conn, rc=rc)

    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    verbosity = ibset(verbosity,12) ! log CplList loop
    write(attrStr,"(I10)") verbosity
    call NUOPC_CompAttributeSet(conn, name="Verbosity", value=attrStr, rc=rc)

    ! set the driver clock
    call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc) ! 15 minute steps

    call ESMF_TimeSet(startTime, yy=2010, mm=6, dd=1, h=0, m=0, &
      calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)

    call ESMF_TimeSet(stopTime, yy=2010, mm=6, dd=1, h=1, m=0, &
      calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)

    internalClock = ESMF_ClockCreate(name="Application Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)

    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)

  end subroutine

  !-----------------------------------------------------------------------------

end module
