! type make make coupled_model.x to compile
! and rm PET*; mpiexec -n 5 coupled_model.x; cat PET0.ESMF_LogFile

!==============================================================================
! ATM Component
!==============================================================================
module ATM
  use ESMF
  use NUOPC
  use NUOPC_Model, modelSS => SetServices
  implicit none

  contains

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    call NUOPC_CompDerive(gcomp, modelSS, rc=rc)
    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advertise, specRoutine=Advertise, rc=rc)
    call NUOPC_CompSpecialize(gcomp, specLabel=label_RealizeProvided, specRoutine=RealizeGrid, rc=rc)
    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advance, specRoutine=Advance, rc=rc)
  end subroutine

  subroutine Advertise(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    type(ESMF_State) :: importState, exportState
    ! query for importState and exportState
    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    call NUOPC_Advertise(importState, StandardName="sea_surface_temperature", rc=rc)
    call NUOPC_Advertise(exportState, StandardName="air_pressure_at_sea_level", rc=rc)
  end subroutine

  subroutine RealizeGrid(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    type(ESMF_Grid)      :: grid
    real(ESMF_KIND_R8), pointer :: coordX(:), coordY(:)
    integer :: i, lbnd(2), ubnd(2)
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Field)     :: field_sst, field_pmsl

    ! 1. Create the Grid and Coordinates
    grid = ESMF_GridCreate(name="atm_grid", maxIndex=(/20,30/), &
        coordSys=ESMF_COORDSYS_CART, regDecomp=(/2,1/), rc=rc)
    call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    call ESMF_GridGetCoord(grid, coordDim=1, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           localDe=0, farrayPtr=coordX, exclusiveLBound=lbnd, &
                           exclusiveUBound=ubnd, rc=rc)
    do i=lbnd(1), ubnd(1)
        coordX(i) = (real(i, 8)-0.5_8)*(100.0_8/20.0_8)
    end do
    call ESMF_GridGetCoord(grid, coordDim=2, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           localDe=0, farrayPtr=coordY, exclusiveLBound=lbnd, &
                           exclusiveUBound=ubnd, rc=rc)
    do i=lbnd(2), ubnd(2); coordY(i) = (real(i, 8)-0.5_8)*(100.0_8/30.0_8); end do
    
    ! 2. Get the States from the Component
    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)

    ! 3. Create the Fields on the Grid
    ! Use the same StandardNames used in Advertise
    field_sst = ESMF_FieldCreate(name="sea_surface_temperature", grid=grid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    field_pmsl = ESMF_FieldCreate(name="air_pressure_at_sea_level", grid=grid, typekind=ESMF_TYPEKIND_R8, rc=rc)

    ! 4. Realize the Fields into the NUOPC system
    ! This attaches the created field to the NUOPC state
    call NUOPC_Realize(importState, field=field_sst, rc=rc)
    call NUOPC_Realize(exportState, field=field_pmsl, rc=rc)
  end subroutine

  subroutine Advance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
  end subroutine
end module ATM

!==============================================================================
! OCN Component
!==============================================================================
module OCN
  use ESMF
  use NUOPC
  use NUOPC_Model, modelSS => SetServices
  implicit none

  contains

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    call NUOPC_CompDerive(gcomp, modelSS, rc=rc)
    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advertise, specRoutine=Advertise, rc=rc)
    call NUOPC_CompSpecialize(gcomp, specLabel=label_RealizeProvided, specRoutine=RealizeGrid, rc=rc)
    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advance, specRoutine=Advance, rc=rc)
  end subroutine

  subroutine Advertise(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    type(ESMF_State) :: importState, exportState
    ! query for importState and exportState
    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    call NUOPC_Advertise(importState, standardName="air_pressure_at_sea_level", rc=rc)
    call NUOPC_Advertise(exportState, standardName="sea_surface_temperature", rc=rc)
  end subroutine

  subroutine RealizeGrid(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    type(ESMF_Grid)      :: grid
    real(ESMF_KIND_R8), pointer :: coordX(:), coordY(:)
    integer :: i, lbnd(2), ubnd(2)
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Field)     :: field_sst, field_pmsl

    ! 1. Create the Grid and Coordinates
    grid = ESMF_GridCreate(name="ocn_grid", maxIndex=(/30,20/), &
        coordSys=ESMF_COORDSYS_CART, regDecomp=(/3,1/), rc=rc)

    call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    call ESMF_GridGetCoord(grid, coordDim=1, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           localDe=0, farrayPtr=coordX, exclusiveLBound=lbnd, &
                           exclusiveUBound=ubnd, rc=rc)
    do i=lbnd(1), ubnd(1)
        coordX(i) = (real(i, 8)-0.5_8)*(100.0_8/30.0_8)
    end do
    call ESMF_GridGetCoord(grid, coordDim=2, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           localDe=0, farrayPtr=coordY, exclusiveLBound=lbnd, &
                           exclusiveUBound=ubnd, rc=rc)
    do i=lbnd(2), ubnd(2)
        coordY(i) = (real(i, 8)-0.5_8)*(100.0_8/20.0_8)
    end do

    ! 2. Get the States from the Component
    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)

    ! 3. Create the Fields on the Grid
    ! Use the same StandardNames used in Advertise
    field_sst = ESMF_FieldCreate(name="sea_surface_temperature", grid=grid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    field_pmsl = ESMF_FieldCreate(name="air_pressure_at_sea_level", grid=grid, typekind=ESMF_TYPEKIND_R8, rc=rc)

    ! 4. Realize the Fields into the NUOPC system
    ! This attaches the created field to the NUOPC state
    call NUOPC_Realize(importState, field=field_pmsl, rc=rc)
    call NUOPC_Realize(exportState, field=field_sst, rc=rc)

  end subroutine

  subroutine Advance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
  end subroutine
end module OCN

!==============================================================================
! Driver Module
!==============================================================================
module MyDriver
  use ESMF
  use NUOPC
  use NUOPC_Driver, driverSS => SetServices
  use NUOPC_Base
  use ATM, only: atmSS => SetServices
  use OCN, only: ocnSS => SetServices
  implicit none

  ! Make calendar global so it survives routine scope
  type(ESMF_Calendar) :: calendar

contains

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! 1. Derive from base driver
    call NUOPC_CompDerive(gcomp, driverSS, rc=rc)

    ! 2. Add driver specialization to set up components
    !    Use reserved label for driver component setup
    call NUOPC_CompSpecialize(gcomp, specRoutine=SetModelComponents, rc=rc)

    ! 3. Create the driver clock
    call CreateDriverClock(gcomp, rc)
  end subroutine

  !------------------------------------------------------
  subroutine CreateDriverClock(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc
    type(ESMF_Clock) :: clock
    type(ESMF_Time) :: startTime, stopTime
    type(ESMF_TimeInterval) :: timeStep

    ! Create the calendar (global)
    calendar = ESMF_CalendarCreate(rc=rc)
    call ESMF_CalendarSet(calendar, ESMF_CALKIND_GREGORIAN, rc=rc)

    ! Create start/stop times
    call ESMF_TimeSet(startTime, yy=2026, mm=1, dd=22, calendar=calendar, rc=rc)
    call ESMF_TimeSet(stopTime,  yy=2026, mm=1, dd=22, calendar=calendar, h=1, rc=rc)

    ! Set the timestep
    call ESMF_TimeIntervalSet(timeStep, m=15, calendar=calendar, rc=rc)

    ! Create the clock and attach to the driver
    clock = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, rc=rc)
    call ESMF_GridCompSet(gcomp, clock=clock, rc=rc)
  end subroutine

  !------------------------------------------------------
  subroutine SetModelComponents(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Add components
    call NUOPC_DriverAddComp(gcomp, "ATM", atmSS, petList=(/0,1/), rc=rc)
    call NUOPC_DriverAddComp(gcomp, "OCN", ocnSS, petList=(/2,3,4/), rc=rc)

    ! Provide the run sequence using NUOPC API
    !call NUOPC_DriverSetRunSequence(gcomp, "runSeq: ATM -> OCN :runSeq", rc=rc)
    call NUOPC_DriverSetRunSequence(gcomp, slot=1, rc=rc)

  end subroutine

end module MyDriver


!==============================================================================
! Program
!==============================================================================
program coupled_system
  use ESMF
  use MyDriver, only: driverSS => SetServices
  implicit none
  integer :: rc
  type(ESMF_GridComp) :: driver

  call ESMF_Initialize(rc=rc)
  driver = ESMF_GridCompCreate(name="Driver", rc=rc)
  call ESMF_GridCompSetServices(driver, driverSS, rc=rc)
  call ESMF_GridCompInitialize(driver, rc=rc)
  call ESMF_GridCompRun(driver, rc=rc)
  call ESMF_GridCompFinalize(driver, rc=rc)
  call ESMF_Finalize(rc=rc)
end program
