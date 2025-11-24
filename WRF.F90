!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module WRF

  !-----------------------------------------------------------------------------
  ! WRF model component
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    modelSS    => SetServices
  
  ! WRF modules
  use module_wrf_component_top
  use module_domain, only : head_grid

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
    call NUOPC_CompSpecialize(model, specLabel=label_DataInitialize, &
      specRoutine=DataInitialize, rc=rc)
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

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine Advertise(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)     :: importState, exportState

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("WRF in Advertise", ESMF_LOGMSG_INFO, rc=rc)

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: XLAT
    call NUOPC_Advertise(exportState, StandardName="XLAT", name="XLAT", &
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: XLONG
    call NUOPC_Advertise(exportState, StandardName="XLONG", name="XLONG", &
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: Z
    call NUOPC_Advertise(exportState, StandardName="Z", name="Z", &
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_LogWrite("WRF leaving Advertise", ESMF_LOGMSG_INFO, rc=rc) 

  end subroutine Advertise

  !-----------------------------------------------------------------------------

  subroutine Realize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: clock ! uninitialised, WRF does not use it in init
    type(ESMF_Grid)         :: grid2D, grid3D
    character(len=160)      :: msgString

    ! WRF domain info
    INTEGER(ESMF_KIND_I4)   :: intvals(19)
    integer                 :: ids, ide, jds, jde, kds, kde

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("WRF in Realize", ESMF_LOGMSG_INFO, rc=rc)

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Do WRF init
    call wrf_component_init1(model, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call wrf_component_init2(model, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Get WRF domain info
    call ESMF_AttributeGet(importState, 'DecompositionIntegers', intvals, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ids = intvals(1)
    ide = intvals(2)
    jds = intvals(3)
    jde = intvals(4)
    kds = intvals(5)
    kde = intvals(6)

    call ESMF_LogWrite("Creating grids with dimensions read from WRF:", ESMF_LOGMSG_INFO, rc=rc)
    write(msgString, '(A,I4,A,I4,A,I4)') "ids = ", ids, ", jds = ", jds, ", kds = ", kds
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
    write(msgString, '(A,I4,A,I4,A,I4)') "ide = ", ide, ", jde = ", jde, ", kde = ", kde
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
    
    ! Create grids with correct size but bogus coords
    ! (we will exclusively use WRF's coords)
    grid2D = ESMF_GridCreateNoPeriDimUfrm( &
      minIndex=(/ ids, jds /), &
      maxIndex=(/ ide, jde /), &
      minCornerCoord=(/ real(ids, ESMF_KIND_R8), real(jds, ESMF_KIND_R8) /), &
      maxCornerCoord=(/ real(ide, ESMF_KIND_R8), real(jde, ESMF_KIND_R8) /), &
      coordSys=ESMF_COORDSYS_CART, &
      !staggerLocList=(/ ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER /), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Match WRF index order: ikj
    grid3D = ESMF_GridCreateNoPeriDimUfrm( &
      minIndex=(/ ids, kds, jds /), &
      maxIndex=(/ ide, kde, jde /), &
      !minCornerCoord=(/ &
      !  real(ids, ESMF_KIND_R8), real(kds, ESMF_KIND_R8), real(jds, ESMF_KIND_R8) /), &
      !maxCornerCoord=(/ &
      !  real(ide, ESMF_KIND_R8), real(kde, ESMF_KIND_R8), real(jde, ESMF_KIND_R8) /), &
      minCornerCoord=(/ &
        1._ESMF_KIND_R8, 1._ESMF_KIND_R8, 1._ESMF_KIND_R8 /), &
      maxCornerCoord=(/ &
        1000._ESMF_KIND_R8, 1000._ESMF_KIND_R8, 1000._ESMF_KIND_R8 /), &
      coordSys=ESMF_COORDSYS_CART, &
      !staggerLocList=(/ ESMF_STAGGERLOC_CENTER_VCENTER, ESMF_STAGGERLOC_CORNER_VCENTER, &
      !  ESMF_STAGGERLOC_CENTER_VFACE, ESMF_STAGGERLOC_CORNER_VFACE /), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field on Grid: XLAT
    call NUOPC_Realize(exportState, grid=grid2D, fieldName="XLAT", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field on Grid: XLONG
    call NUOPC_Realize(exportState, grid=grid2D, fieldName="XLONG", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field on Grid: Z
    call NUOPC_Realize(exportState, grid=grid3D, fieldName="Z", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_LogWrite("WRF leaving Realize", ESMF_LOGMSG_INFO, rc=rc) 

  end subroutine Realize

  !-----------------------------------------------------------------------------

  subroutine DataInitialize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Field)            :: field
    real(ESMF_KIND_R4), pointer :: ESMF_ptr_2D(:,:), ESMF_ptr_3D(:,:,:)
    integer                     :: i, j, k

    ! WRF domain info
    INTEGER(ESMF_KIND_I4)       :: intvals(19)
    integer                     :: ips, ipe, jps, jpe, kps, kpe

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("WRF in DataInitialize", ESMF_LOGMSG_INFO, rc=rc) 

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Get WRF domain info
    call ESMF_AttributeGet(importState, 'DecompositionIntegers', intvals, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ips = intvals(13)
    ipe = intvals(14)
    jps = intvals(15)
    jpe = intvals(16)
    kps = intvals(17)
    kpe = intvals(18)

    ! -------------------- XLAT --------------------

    ! Get XLAT field
    call ESMF_StateGet(exportState, itemName="XLAT", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Get pointer from field
    call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_2D, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    do i = ips, ipe
      do j = jps, jpe
        ESMF_ptr_2D(i, j) = head_grid%xlat(i, j)
      end do
    end do

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- XLONG --------------------

    ! Get XLONG field
    call ESMF_StateGet(exportState, itemName="XLONG", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Get pointer from field
    call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_2D, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    do i = ips, ipe
      do j = jps, jpe
        ESMF_ptr_2D(i, j) = head_grid%xlong(i, j)
      end do
    end do

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! -------------------- Z --------------------

    ! Get Z field
    call ESMF_StateGet(exportState, itemName="Z", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Get pointer from field
    call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    do i = ips, ipe
      do k = kpe, kps
        do j = jps, jpe
          ESMF_ptr_3D(i, k, j) = head_grid%z(i, k, j)
        end do
      end do
    end do

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -----------------------------------------------
    
    ! Indicate that the model has everything it needs
    call NUOPC_CompAttributeSet(model, &
      name="InitializeDataComplete", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_LogWrite("WRF leaving DataInitialize", ESMF_LOGMSG_INFO, rc=rc) 
  
  end subroutine DataInitialize

  !-----------------------------------------------------------------------------

  subroutine Advance(model, rc)
!$  use omp_lib
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Field)            :: field
    real(ESMF_KIND_R4), pointer :: ESMF_ptr_2D(:,:), ESMF_ptr_3D(:,:,:)
    integer                     :: i, j, k
    character(len=160)          :: msgString

    ! WRF domain info
    INTEGER(ESMF_KIND_I4)       :: intvals(19)
    integer                     :: ips, ipe, jps, jpe, kps, kpe

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Get WRF domain info
    call ESMF_AttributeGet(importState, 'DecompositionIntegers', intvals, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ips = intvals(13)
    ipe = intvals(14)
    jps = intvals(15)
    jpe = intvals(16)
    kps = intvals(17)
    kpe = intvals(18)

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing WRF from: ", unit=msgString, rc=rc)
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

    ! Imports

    call wrf_component_run(model, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Does the field need to be gotten from the exportState before its use?
    ! So
    ! 1. Get in fields, get their pointers, read from their pointers
    ! 2. Do run
    ! 3. Get out fields, get their pointers, write to their pointers, then don't need to put back in?
    ! WRF seems to say so
    ! Is there any StateAdd then? Maybe not
    
    ! Do ESMF_StateGet() again?

    ! Exports

    ! -------------------- Z --------------------

    ! Get Z field
    call ESMF_StateGet(exportState, itemName="Z", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Get pointer from field
    call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    do i = ips, ipe
      do k = kpe, kps
        do j = jps, jpe
          ESMF_ptr_3D(i, k, j) = head_grid%z(i, k, j)
        end do
      end do
    end do

    ! -----------------------------------------------

  end subroutine Advance

  !-----------------------------------------------------------------------------

end module WRF
