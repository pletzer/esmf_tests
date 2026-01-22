!==============================================================================
! ATM Component Module
!==============================================================================
module ATM
  use ESMF
  use NUOPC
  use NUOPC_Model, modelSS    => SetServices
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
    call NUOPC_AdvertiseField(gcomp, StateIntent=ESMF_STATEINTENT_IMPORT, &
      FieldName="sst", rc=rc)
    call NUOPC_AdvertiseField(gcomp, StateIntent=ESMF_STATEINTENT_EXPORT, &
      FieldName="pmsl", rc=rc)
  end subroutine

  subroutine RealizeGrid(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    type(ESMF_Grid)      :: grid
    real(ESMF_KIND_R8), pointer :: coordX(:), coordY(:)
    integer :: i, j, lbnd(2), ubnd(2)

    ! 1. Create Grid (20x30)
    grid = ESMF_GridCreate(name="atm_grid", maxIndex=(/20,30/), &
        coordSys=ESMF_COORDSYS_CART, regDecomp=(/2,1/), rc=rc)

    ! 2. Add coordinates at Center (for field data) and Corner (for conservative regrid)
    call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    
    ! 3. Fill Center Coordinates (Simple 0-100 range)
    call ESMF_GridGetCoord(grid, coordDim=1, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           localDe=0, farrayPtr=coordX, exclusiveLBound=lbnd, &
                           exclusiveUBound=ubnd, rc=rc)
    do i=lbnd(1), ubnd(1)
      coordX(i) = (real(i, ESMF_KIND_R8) - 0.5_ESMF_KIND_R8) * (100.0_ESMF_KIND_R8 / 20.0_ESMF_KIND_R8)
    end do
    
    call ESMF_GridGetCoord(grid, coordDim=2, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           localDe=0, farrayPtr=coordY, exclusiveLBound=lbnd, &
                           exclusiveUBound=ubnd, rc=rc)
    do j=lbnd(1), ubnd(1)
      coordY(j) = (real(j, ESMF_KIND_R8) - 0.5_ESMF_KIND_R8) * (100.0_ESMF_KIND_R8 / 30.0_ESMF_KIND_R8)
    end do

    ! 4. Realize Fields
    call NUOPC_RealizeField(gcomp, FieldName="pmsl", grid=grid, rc=rc)
    call NUOPC_RealizeField(gcomp, FieldName="sst",  grid=grid, rc=rc)
  end subroutine

  subroutine Advance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    ! Physics/Dynamics code goes here
    rc = ESMF_SUCCESS
  end subroutine
end module ATM

!==============================================================================
! OCN Component Module
!==============================================================================
module OCN
  use ESMF
  use NUOPC
  implicit none

  contains

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    call NUOPC_CompDerive(gcomp, NUOPC_MODEL_SET_SERVICES, rc=rc)
    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advertise, Routine=Advertise, rc=rc)
    call NUOPC_CompSpecialize(gcomp, specLabel=label_RealizeGrid, Routine=RealizeGrid, rc=rc)
    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advance, Routine=Advance, rc=rc)
  end subroutine

  subroutine Advertise(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    call NUOPC_AdvertiseField(gcomp, StateKind=ESMF_STATEKIND_IMPORT, &
      FieldName="pmsl", rc=rc)
    call NUOPC_AdvertiseField(gcomp, StateKind=ESMF_STATEKIND_EXPORT, &
      FieldName="sst", rc=rc)
  end subroutine

  subroutine RealizeGrid(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    type(ESMF_Grid)      :: grid
    real(ESMF_KIND_R8), pointer :: coordX(:), coordY(:)
    integer :: i, j, lbnd(2), ubnd(2)

    grid = ESMF_GridCreate(name="ocn_grid", maxIndex=(/30,20/), &
        coordSys=ESMF_COORDSYS_CART, regDecomp=(/3,1/), rc=rc)

    call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    
    ! Fill X (30 cells)
    call ESMF_GridGetCoord(grid, coordDim=1, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           localDe=0, farrayPtr=coordX, exclusiveLBound=lbnd, rc=rc)
    do i=lbnd(1), ubnd(1)
      coordX(i) = (real(i, ESMF_KIND_R8) - 0.5_ESMF_KIND_R8) * (100.0_ESMF_KIND_R8 / 30.0_ESMF_KIND_R8)
    end do
    
    ! Fill Y (20 cells)
    call ESMF_GridGetCoord(grid, coordDim=2, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           localDe=0, farrayPtr=coordY, exclusiveLBound=lbnd, rc=rc)
    do j=lbnd(1), ubnd(1)
      coordY(j) = (real(j, ESMF_KIND_R8) - 0.5_ESMF_KIND_R8) * (100.0_ESMF_KIND_R8 / 20.0_ESMF_KIND_R8)
    end do

    call NUOPC_RealizeField(gcomp, FieldName="pmsl", grid=grid, rc=rc)
    call NUOPC_RealizeField(gcomp, FieldName="sst",  grid=grid, rc=rc)
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
  use ATM, only: atmSS => SetServices
  use OCN, only: ocnSS => SetServices
  implicit none

  contains

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    call NUOPC_CompDerive(gcomp, NUOPC_DRIVER_SET_SERVICES, rc=rc)
    call NUOPC_CompSpecialize(gcomp, label_SetModelComponents, &
      Routine=SetModelComponents, rc=rc)
  end subroutine

  subroutine SetModelComponents(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    ! Add ATM with 2 PETs (0,1) and OCN with 3 PETs (2,3,4)
    call NUOPC_DriverAddComp(gcomp, "ATM", atmSS, petList=(/0,1/), rc=rc)
    call NUOPC_DriverAddComp(gcomp, "OCN", ocnSS, petList=(/2,3,4/), rc=rc)
  end subroutine
end module MyDriver

!==============================================================================
! Main Program
!==============================================================================
program coupled_system
  use ESMF
  use NUOPC
  use MyDriver, only: driverSS => SetServices
  implicit none
  integer :: rc
  type(ESMF_GridComp) :: driver

  call ESMF_Initialize(rc=rc)
  driver = ESMF_GridCompCreate(name="Driver", rc=rc)
  call ESMF_GridCompSetServices(driver, driverSS, rc=rc)
  
  ! Run loop could go here (Initialize, Run, Finalize)
  call ESMF_GridCompInitialize(driver, rc=rc)
  call ESMF_GridCompRun(driver, rc=rc)
  call ESMF_GridCompFinalize(driver, rc=rc)

  call ESMF_Finalize(rc=rc)
end program
