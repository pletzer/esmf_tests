!==============================================================================
! ATM Component
!==============================================================================
module ATM
  use ESMF
  use ESMF_GridMod, only: ESMF_Grid, ESMF_GridCreate, ESMF_GridAddCoord, ESMF_GridSetCoord
  use NUOPC
  use NUOPC_Model, modelSS => SetServices
  implicit none

contains

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc

    call NUOPC_CompDerive(gcomp, modelSS, rc=rc)
    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advertise, specRoutine=Advertise, rc=rc)
    call NUOPC_CompSpecialize(gcomp, specLabel=label_RealizeProvided, specRoutine=RealizeGrid, rc=rc)
    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advance, specRoutine=Advance, rc=rc)
  end subroutine

  subroutine Advertise(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc
    type(ESMF_State) :: importState, exportState

    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    call NUOPC_Advertise(importState, standardName="sea_surface_temperature", rc=rc)
    call NUOPC_Advertise(exportState, standardName="air_pressure_at_sea_level", rc=rc)
  end subroutine

  subroutine RealizeGrid(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc
    type(ESMF_Grid) :: grid
    real(ESMF_KIND_R8), pointer :: xc(:), yc(:)
    integer :: i, j, nx, ny
    type(ESMF_State) :: importState, exportState
    type(ESMF_Field) :: field_sst, field_pmsl
    real(8) :: dx, dy

    ! Create simple 2D grid
    nx = 10
    ny = 30
    dx = 1.0_8
    dy = 1.0_8
    grid = ESMF_GridCreateNoPeriDim(maxIndex=(/nx, ny/), &
      coordSys=ESMF_COORDSYS_CART, &
      indexflag=ESMF_INDEX_GLOBAL, &
      coordDep1=(/1/), &
      coordDep2=(/2/), &
      rc=rc)
    call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
        rc=rc)
    call ESMF_GridGetCoord(grid, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=1, farrayPtr=xc, rc=rc)
    call ESMF_GridGetCoord(grid, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=2, farrayPtr=yc, rc=rc)
    do i = lbound(xc, 1), ubound(xc, 1)
      xc(i) = (i - 0.5_8) * dx
    enddo
    do j = lbound(yc, 1), ubound(yc, 1)
      yc(j) = (j - 0.5_8) * dy
    enddo

    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    field_sst  = ESMF_FieldCreate(name="sea_surface_temperature", grid=grid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    field_pmsl = ESMF_FieldCreate(name="air_pressure_at_sea_level", grid=grid, typekind=ESMF_TYPEKIND_R8, rc=rc)

    call NUOPC_Realize(importState, field=field_sst, rc=rc)
    call NUOPC_Realize(exportState, field=field_pmsl, rc=rc)
  end subroutine

  subroutine Advance(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
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
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc

    call NUOPC_CompDerive(gcomp, modelSS, rc=rc)
    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advertise, specRoutine=Advertise, rc=rc)
    call NUOPC_CompSpecialize(gcomp, specLabel=label_RealizeProvided, specRoutine=RealizeGrid, rc=rc)
    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advance, specRoutine=Advance, rc=rc)
  end subroutine

  subroutine Advertise(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc
    type(ESMF_State) :: importState, exportState

    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    call NUOPC_Advertise(importState, standardName="air_pressure_at_sea_level", rc=rc)
    call NUOPC_Advertise(exportState, standardName="sea_surface_temperature", rc=rc)
  end subroutine

  subroutine RealizeGrid(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc
    type(ESMF_Grid) :: grid
    real(ESMF_KIND_R8), pointer :: xc(:), yc(:)
    integer :: i, j, nx, ny
    type(ESMF_State) :: importState, exportState
    type(ESMF_Field) :: field_sst, field_pmsl
    real(8) :: dx, dy

    ! Create simple 2D grid
    nx = 30
    ny = 10
    dx = 1.0_8
    dy = 1.0_8
    grid = ESMF_GridCreateNoPeriDim(maxIndex=(/nx, ny/), &
      coordSys=ESMF_COORDSYS_CART, &
      indexflag=ESMF_INDEX_GLOBAL, &
      coordDep1=(/1/), &
      coordDep2=(/2/), &
      rc=rc)
    call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
        rc=rc)
    call ESMF_GridGetCoord(grid, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=1, farrayPtr=xc, rc=rc)
    call ESMF_GridGetCoord(grid, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=2, farrayPtr=yc, rc=rc)
    do i = lbound(xc, 1), ubound(xc, 1)
      xc(i) = (i - 0.5_8) * dx
    enddo
    do j = lbound(yc, 1), ubound(yc, 1)
      yc(j) = (j - 0.5_8) * dy
    enddo

    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    field_sst  = ESMF_FieldCreate(name="sea_surface_temperature", grid=grid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    field_pmsl = ESMF_FieldCreate(name="air_pressure_at_sea_level", grid=grid, typekind=ESMF_TYPEKIND_R8, rc=rc)

    call NUOPC_Realize(importState, field=field_pmsl, rc=rc)
    call NUOPC_Realize(exportState, field=field_sst, rc=rc)
  end subroutine

  subroutine Advance(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc

    type(ESMF_State) :: exportState
    type(ESMF_Field) :: field_sst
    type(ESMF_Clock) :: clock
    type(ESMF_Time)  :: currTime

    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(gcomp, exportState=exportState, driverClock=clock, rc=rc)

    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)

    call ESMF_StateGet(exportState, &
       itemName="sea_surface_temperature", field=field_sst, rc=rc)

    ! Dummy SST value
    call ESMF_FieldFill(field_sst, 300.0_8, rc=rc)

    call ESMF_FieldSetTimestamp(field_sst, timeStamp=currTime, rc=rc)

  end subroutine

end module OCN

!==============================================================================
! Driver Module
!==============================================================================
module MyDriver
  use ESMF
  use NUOPC
  use NUOPC_Driver, driverSS => SetServices
  use ATM, only: atmSS => SetServices
  use OCN, only: ocnSS => SetServices
  implicit none

  type(ESMF_Calendar) :: calendar

contains

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc

    call NUOPC_CompDerive(gcomp, driverSS, rc=rc)
    call NUOPC_CompSpecialize(gcomp, specLabel=label_SetModelServices, specRoutine=SetModelComponents, rc=rc)
    call CreateDriverClock(gcomp, rc)
  end subroutine

  subroutine SetModelComponents(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc

    call NUOPC_DriverAddComp(gcomp, "ATM", atmSS, petList=(/0,1/), rc=rc)
    call NUOPC_DriverAddComp(gcomp, "OCN", ocnSS, petList=(/2,3,4/), rc=rc)


    ! Minimal run sequence
    !call NUOPC_DriverSetRunSequence(gcomp, slot=1, rc=rc)
  end subroutine

  subroutine CreateDriverClock(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc
    type(ESMF_Clock) :: clock
    type(ESMF_Time) :: startTime, stopTime
    type(ESMF_TimeInterval) :: timeStep

    calendar = ESMF_CalendarCreate(rc=rc)
    call ESMF_CalendarSet(calendar, ESMF_CALKIND_GREGORIAN, rc=rc)

    call ESMF_TimeSet(startTime, yy=2026, mm=1, dd=22, calendar=calendar, rc=rc)
    call ESMF_TimeSet(stopTime,  yy=2026, mm=1, dd=22, calendar=calendar, h=1, rc=rc)
    call ESMF_TimeIntervalSet(timeStep, m=15, calendar=calendar, rc=rc)

    clock = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, rc=rc)
    call ESMF_GridCompSet(gcomp, clock=clock, rc=rc)
  end subroutine

end module MyDriver

!==============================================================================
! Coupled Model: ATM + OCN
! Minimal working NUOPC example for ESMF 8.6
!==============================================================================

program coupled_system
  use ESMF
  use MyDriver, only: driverSS => SetServices
  implicit none
  integer :: rc
  type(ESMF_GridComp) :: driver

  ! Initialize ESMF
  call ESMF_Initialize(rc=rc)

  ! Create driver grid component
  driver = ESMF_GridCompCreate(name="Driver", rc=rc)

  ! Set driver services
  call ESMF_GridCompSetServices(driver, driverSS, rc=rc)

  ! Initialize, run, finalize
  call ESMF_GridCompInitialize(driver, rc=rc)
  call ESMF_GridCompRun(driver, rc=rc)
  call ESMF_GridCompFinalize(driver, rc=rc)

  ! Finalize ESMF
  call ESMF_Finalize(rc=rc)
end program coupled_system

