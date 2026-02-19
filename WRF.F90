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
  use module_state_description, only : P_qv, P_qi, P_qni

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
    type(ESMF_Clock)     :: clock ! uninitialised, WRF does not use it in init

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("WRF in Advertise", ESMF_LOGMSG_INFO, rc=rc)

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
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

    ! exportable field: XLAT
    call NUOPC_Advertise(exportState, StandardName="XLAT", name="XLAT", &
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

    ! exportable field: Z_AT_W
    call NUOPC_Advertise(exportState, StandardName="Z_AT_W", name="Z_AT_W", &
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: DRYMASS
    call NUOPC_Advertise(exportState, StandardName="DRYMASS", name="DRYMASS", &
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: T_POT
    call NUOPC_Advertise(exportState, StandardName="T_POT", name="T_POT", &
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: P
    call NUOPC_Advertise(exportState, StandardName="P", name="P", &
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: U
    call NUOPC_Advertise(exportState, StandardName="U", name="U", &
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: V
    call NUOPC_Advertise(exportState, StandardName="V", name="V", &
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: W
    call NUOPC_Advertise(exportState, StandardName="W", name="W", &
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: TNSR
    call NUOPC_Advertise(exportState, StandardName="TNSR", name="TNSR", &
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: OLR
    call NUOPC_Advertise(exportState, StandardName="OLR", name="OLR", &
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: QV
    call NUOPC_Advertise(exportState, StandardName="QV", name="QV", &
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: deltaQV
    call NUOPC_Advertise(importState, StandardName="deltaQV", name="deltaQV", &
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: QI
    call NUOPC_Advertise(exportState, StandardName="QI", name="QI", &
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: deltaQI
    call NUOPC_Advertise(importState, StandardName="deltaQI", name="deltaQI", &
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: deltaNI
    call NUOPC_Advertise(importState, StandardName="deltaNI", name="deltaNI", &
      TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: QIcontrail
    call NUOPC_Advertise(importState, StandardName="QIcontrail", name="QIcontrail", &
      TransferOfferGeomObject="will provide", rc=rc)
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
    
    call ESMF_LogWrite("WRF leaving Advertise", ESMF_LOGMSG_INFO, rc=rc) 

  end subroutine Advertise

  !-----------------------------------------------------------------------------

  subroutine Realize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_VM)           :: vm
    type(ESMF_State)        :: importState, exportState
    integer                 :: petCount
    type(ESMF_DistGrid)     :: distgrid2D, distgrid3D
    type(ESMF_Grid)         :: grid2D, grid3D
    character(len=160)      :: msgString
    integer                 :: i

    ! WRF domain info
    integer(ESMF_KIND_I4)              :: intvals(19)
    integer                            :: ids, ide, jds, jde, kds, kde, &
                                          ips, ipe, jps, jpe, kps, kpe
    integer(ESMF_KIND_I4)              :: patchIndicesSend(6) ! (/ ips, kps, jps, ipe, kpe, jpe /)
    integer(ESMF_KIND_I4), allocatable :: patchIndicesRecv(:)
    integer, allocatable               :: deBlockList2D(:,:,:), deBlockList3D(:,:,:)

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("WRF in Realize", ESMF_LOGMSG_INFO, rc=rc)

    ! query for vm, and petCount
    call ESMF_GridCompGet(model, vm=vm, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

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
    ids = intvals(1)
    ide = intvals(2)
    jds = intvals(3)
    jde = intvals(4)
    kds = intvals(5)
    kde = intvals(6)
    ips = intvals(13)
    ipe = intvals(14)
    jps = intvals(15)
    jpe = intvals(16)
    kps = intvals(17)
    kpe = intvals(18)

    patchIndicesSend = (/ ips, kps, jps, ipe, kpe, jpe /)

    allocate(patchIndicesRecv(6*petCount))
    allocate(deBlockList2D(2, 2, petCount)) ! (dims, 2 (min, max), petCount)
    allocate(deBlockList3D(3, 2, petCount)) ! (dims, 2 (min, max), petCount)

    ! Each PET (MPI rank) sends local patch indices and receives all patch indices
    call ESMF_VMAllGather(vm, patchIndicesSend, patchIndicesRecv, count=6, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Create decomposition element block lists (deBlockLists)
    ! to tell ESMF how WRF is decomposing the grid by MPI rank
    do i=1, petCount
      ! min indices
      deBlockList2D(:,1,i) = (/ patchIndicesRecv(6*(i-1)+1), patchIndicesRecv(6*(i-1)+3) /)
      deBlockList3D(:,1,i) = patchIndicesRecv(6*(i-1)+1 : 6*(i-1)+3)
      ! max indices
      deBlockList2D(:,2,i) = (/ patchIndicesRecv(6*(i-1)+4), patchIndicesRecv(6*(i-1)+6) /)
      deBlockList3D(:,2,i) = patchIndicesRecv(6*(i-1)+4 : 6*(i-1)+6)
    end do

    call ESMF_LogWrite("Creating grids with dimensions read from WRF:", ESMF_LOGMSG_INFO, rc=rc)
    write(msgString, '(A,I4,A,I4,A,I4)') "ids = ", ids, ", jds = ", jds, ", kds = ", kds
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
    write(msgString, '(A,I4,A,I4,A,I4)') "ide = ", ide, ", jde = ", jde, ", kde = ", kde
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    distgrid2D = ESMF_DistGridCreate( &
      minIndex=(/ ids, jds /), &
      maxIndex=(/ ide, jde /), &
      deBlockList=deBlockList2D, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    distgrid3D = ESMF_DistGridCreate( &
      minIndex=(/ ids, kds, jds /), &
      maxIndex=(/ ide, kde, jde /), &
      deBlockList=deBlockList3D, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Create grids from distgrids; no need for coord info since we redist not regrid
    grid2D = ESMF_GridCreate(distgrid2D, &
      indexFlag=ESMF_INDEX_GLOBAL, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    grid3D = ESMF_GridCreate(distgrid3D, &
      indexFlag=ESMF_INDEX_GLOBAL, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    deallocate(patchIndicesRecv, deBlockList2D, deBlockList3D)

    ! exportable field on Grid: XLONG
    call NUOPC_Realize(exportState, grid=grid2D, fieldName="XLONG", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
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

    ! exportable field on Grid: Z
    call NUOPC_Realize(exportState, grid=grid3D, fieldName="Z", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field on Grid: Z_AT_W
    call NUOPC_Realize(exportState, grid=grid3D, fieldName="Z_AT_W", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field on Grid: DRYMASS
    call NUOPC_Realize(exportState, grid=grid3D, fieldName="DRYMASS", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field on Grid: T_POT
    call NUOPC_Realize(exportState, grid=grid3D, fieldName="T_POT", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field on Grid: P
    call NUOPC_Realize(exportState, grid=grid3D, fieldName="P", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field on Grid: U
    call NUOPC_Realize(exportState, grid=grid3D, fieldName="U", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field on Grid: V
    call NUOPC_Realize(exportState, grid=grid3D, fieldName="V", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field on Grid: W
    call NUOPC_Realize(exportState, grid=grid3D, fieldName="W", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field on Grid: TNSR
    call NUOPC_Realize(exportState, grid=grid2D, fieldName="TNSR", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field on Grid: OLR
    call NUOPC_Realize(exportState, grid=grid2D, fieldName="OLR", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field on Grid: QV
    call NUOPC_Realize(exportState, grid=grid3D, fieldName="QV", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field on Grid: deltaQV
    call NUOPC_Realize(importState, grid=grid3D, fieldName="deltaQV", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field on Grid: QI
    call NUOPC_Realize(exportState, grid=grid3D, fieldName="QI", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field on Grid: deltaQI
    call NUOPC_Realize(importState, grid=grid3D, fieldName="deltaQI", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field on Grid: deltaNI
    call NUOPC_Realize(importState, grid=grid3D, fieldName="deltaNI", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field on Grid: QIcontrail
    call NUOPC_Realize(importState, grid=grid3D, fieldName="QIcontrail", &
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
    integer(ESMF_KIND_I4)       :: intvals(19)
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

    ESMF_ptr_2D(ips:ipe, jps:jpe) = head_grid%xlat(ips:ipe, jps:jpe)

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
    
    ESMF_ptr_2D(ips:ipe, jps:jpe) = head_grid%xlong(ips:ipe, jps:jpe)

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

    ! Before WRF runs, Z is not initialised, so we calculate it from geopotential
    ESMF_ptr_3D(ips:ipe, kps:kpe-1, jps:jpe) = 0.5d0 / 9.807d0 * &
                                               (head_grid%ph_2(ips:ipe, kps:kpe-1, jps:jpe) + &
                                                head_grid%phb(ips:ipe, kps:kpe-1, jps:jpe) + &
                                                head_grid%ph_2(ips:ipe, kps+1:kpe, jps:jpe) + &
                                                head_grid%phb(ips:ipe, kps+1:kpe, jps:jpe))

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- Z_AT_W --------------------

    ! Get Z_AT_W field
    call ESMF_StateGet(exportState, itemName="Z_AT_W", field=field, rc=rc)
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

    ! Before WRF runs, Z_AT_W is not initialised, so we calculate it from geopotential
    ESMF_ptr_3D(ips:ipe, kps:kpe, jps:jpe) = 1.d0 / 9.807d0 * &
                                               (head_grid%ph_2(ips:ipe, kps:kpe, jps:jpe) + &
                                                head_grid%phb(ips:ipe, kps:kpe, jps:jpe))

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- DRYMASS --------------------

    ! Get DRYMASS field
    call ESMF_StateGet(exportState, itemName="DRYMASS", field=field, rc=rc)
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
    
    do j = jps, jpe
      do k = kps, kpe
        do i = ips, ipe
          ESMF_ptr_3D(i, k, j) = 1.d0 / 9.807d0 * head_grid%area2d(i, j) &
                                 * (head_grid%mu_2(i, j) + head_grid%mub(i, j)) &
                                 * (-head_grid%dnw(k))
        end do
      end do
    end do

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- T_POT --------------------

    ! Get T_POT field
    call ESMF_StateGet(exportState, itemName="T_POT", field=field, rc=rc)
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

    ESMF_ptr_3D(ips:ipe, kps:kpe, jps:jpe) = (300. + head_grid%th_phy_m_t0(ips:ipe, kps:kpe, jps:jpe))

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- P --------------------

    ! Get P field
    call ESMF_StateGet(exportState, itemName="P", field=field, rc=rc)
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

    ESMF_ptr_3D(ips:ipe, kps:kpe, jps:jpe) = (head_grid%p(ips:ipe, kps:kpe, jps:jpe) + &
                                              head_grid%pb(ips:ipe, kps:kpe, jps:jpe))

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- U --------------------

    ! Get U field
    call ESMF_StateGet(exportState, itemName="U", field=field, rc=rc)
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

    ESMF_ptr_3D(ips:ipe-1, kps:kpe, jps:jpe) =  0.5d0 * (head_grid%u_2(ips:ipe-1, kps:kpe, jps:jpe) + &
                                                         head_grid%u_2(ips+1:ipe, kps:kpe, jps:jpe))

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- V --------------------

    ! Get V field
    call ESMF_StateGet(exportState, itemName="V", field=field, rc=rc)
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

    ESMF_ptr_3D(ips:ipe, kps:kpe, jps:jpe-1) =  0.5d0 * (head_grid%v_2(ips:ipe, kps:kpe, jps:jpe-1) + &
                                                         head_grid%v_2(ips:ipe, kps:kpe, jps+1:jpe))

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- W --------------------

    ! Get W field
    call ESMF_StateGet(exportState, itemName="W", field=field, rc=rc)
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

    ESMF_ptr_3D(ips:ipe, kps:kpe-1, jps:jpe) =  0.5d0 * (head_grid%w_2(ips:ipe, kps:kpe-1, jps:jpe) + &
                                                         head_grid%w_2(ips:ipe, kps+1:kpe, jps:jpe))

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- TNSR --------------------

    ! Get TNSR field
    call ESMF_StateGet(exportState, itemName="TNSR", field=field, rc=rc)
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

    ESMF_ptr_2D(ips:ipe, jps:jpe) = head_grid%swdnt(ips:ipe, jps:jpe) &
                                    - head_grid%swupt(ips:ipe, jps:jpe)

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- OLR --------------------

    ! Get OLR field
    call ESMF_StateGet(exportState, itemName="OLR", field=field, rc=rc)
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

    ESMF_ptr_2D(ips:ipe, jps:jpe) = head_grid%olr(ips:ipe, jps:jpe)

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- QV --------------------

    ! Get QV field
    call ESMF_StateGet(exportState, itemName="QV", field=field, rc=rc)
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

    ESMF_ptr_3D(ips:ipe, kps:kpe, jps:jpe) = head_grid%moist(ips:ipe, kps:kpe, jps:jpe, P_qv)

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- deltaQV --------------------

    ! not initialised

    ! -------------------- QI --------------------

    ! Get QI field
    call ESMF_StateGet(exportState, itemName="QI", field=field, rc=rc)
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

    ESMF_ptr_3D(ips:ipe, kps:kpe, jps:jpe) = head_grid%moist(ips:ipe, kps:kpe, jps:jpe, P_qi)

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- deltaQI --------------------

    ! not initialised

    ! -------------------- deltaNI --------------------

    ! not initialised

    ! -------------------- QIcontrail --------------------

    ! not initialised

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
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Field)            :: field
    real(ESMF_KIND_R4), pointer :: ESMF_ptr_2D(:,:), ESMF_ptr_3D(:,:,:)
    character(len=160)          :: msgString
    integer                     :: i, j, k

    ! WRF domain info
    integer(ESMF_KIND_I4)       :: intvals(19)
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

    ! -----------------------------------------------

    ! Imports

    ! -------------------- deltaQV --------------------

    ! Get deltaQV field
    call ESMF_StateGet(importState, itemName="deltaQV", field=field, rc=rc)
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

    head_grid%moist(ips:ipe, kps:kpe, jps:jpe, P_qv) = &
      head_grid%moist(ips:ipe, kps:kpe, jps:jpe, P_qv) + ESMF_ptr_3D(ips:ipe, kps:kpe, jps:jpe)

    ! -------------------- deltaQI --------------------

    ! Get deltaQI field
    call ESMF_StateGet(importState, itemName="deltaQI", field=field, rc=rc)
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

    head_grid%moist(ips:ipe, kps:kpe, jps:jpe, P_qi) = &
      head_grid%moist(ips:ipe, kps:kpe, jps:jpe, P_qi) + ESMF_ptr_3D(ips:ipe, kps:kpe, jps:jpe)

    ! -------------------- deltaNI --------------------

    ! Get deltaNI field
    call ESMF_StateGet(importState, itemName="deltaNI", field=field, rc=rc)
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

    head_grid%scalar(ips:ipe, kps:kpe, jps:jpe, P_qni) = &
      head_grid%scalar(ips:ipe, kps:kpe, jps:jpe, P_qni) + ESMF_ptr_3D(ips:ipe, kps:kpe, jps:jpe)

    ! -------------------- QIcontrail --------------------

    ! Get QIcontrail field
    call ESMF_StateGet(importState, itemName="QIcontrail", field=field, rc=rc)
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

    head_grid%qicontrail(ips:ipe, kps:kpe, jps:jpe) = ESMF_ptr_3D(ips:ipe, kps:kpe, jps:jpe)

    ! -----------------------------------------------
    
    ! Run WRF
    call wrf_component_run(model, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -----------------------------------------------

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

    ESMF_ptr_3D(ips:ipe, kps:kpe, jps:jpe) = head_grid%z(ips:ipe, kps:kpe, jps:jpe)

    ! -------------------- Z_AT_W --------------------

    ! Get Z_AT_W field
    call ESMF_StateGet(exportState, itemName="Z_AT_W", field=field, rc=rc)
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

    ESMF_ptr_3D(ips:ipe, kps:kpe, jps:jpe) = head_grid%z_at_w(ips:ipe, kps:kpe, jps:jpe)

    ! -------------------- DRYMASS --------------------

    ! Get DRYMASS field
    call ESMF_StateGet(exportState, itemName="DRYMASS", field=field, rc=rc)
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
    
    do j = jps, jpe
      do k = kps, kpe
        do i = ips, ipe
          ESMF_ptr_3D(i, k, j) = 1.d0 / 9.807d0 * head_grid%area2d(i, j) &
                                 * (head_grid%mu_2(i, j) + head_grid%mub(i, j)) &
                                 * (-head_grid%dnw(k))
        end do
      end do
    end do

    ! -------------------- T_POT --------------------

    ! Get T_POT field
    call ESMF_StateGet(exportState, itemName="T_POT", field=field, rc=rc)
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

    ESMF_ptr_3D(ips:ipe, kps:kpe, jps:jpe) = (300. + head_grid%th_phy_m_t0(ips:ipe, kps:kpe, jps:jpe))

    ! -------------------- P --------------------

    ! Get P field
    call ESMF_StateGet(exportState, itemName="P", field=field, rc=rc)
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

    ESMF_ptr_3D(ips:ipe, kps:kpe, jps:jpe) = (head_grid%p(ips:ipe, kps:kpe, jps:jpe) + &
                                              head_grid%pb(ips:ipe, kps:kpe, jps:jpe))

    ! -------------------- U --------------------

    ! Get U field
    call ESMF_StateGet(exportState, itemName="U", field=field, rc=rc)
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

    ESMF_ptr_3D(ips:ipe-1, kps:kpe, jps:jpe) =  0.5d0 * (head_grid%u_2(ips:ipe-1, kps:kpe, jps:jpe) + &
                                                         head_grid%u_2(ips+1:ipe, kps:kpe, jps:jpe))

    ! -------------------- V --------------------

    ! Get V field
    call ESMF_StateGet(exportState, itemName="V", field=field, rc=rc)
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

    ESMF_ptr_3D(ips:ipe, kps:kpe, jps:jpe-1) =  0.5d0 * (head_grid%v_2(ips:ipe, kps:kpe, jps:jpe-1) + &
                                                         head_grid%v_2(ips:ipe, kps:kpe, jps+1:jpe))

    ! -------------------- W --------------------

    ! Get W field
    call ESMF_StateGet(exportState, itemName="W", field=field, rc=rc)
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

    ESMF_ptr_3D(ips:ipe, kps:kpe-1, jps:jpe) =  0.5d0 * (head_grid%w_2(ips:ipe, kps:kpe-1, jps:jpe) + &
                                                         head_grid%w_2(ips:ipe, kps+1:kpe, jps:jpe))

    ! -------------------- TNSR --------------------

    ! Get TNSR field
    call ESMF_StateGet(exportState, itemName="TNSR", field=field, rc=rc)
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

    ESMF_ptr_2D(ips:ipe, jps:jpe) = head_grid%swdnt(ips:ipe, jps:jpe) &
                                    - head_grid%swupt(ips:ipe, jps:jpe)

    ! -------------------- OLR --------------------

    ! Get OLR field
    call ESMF_StateGet(exportState, itemName="OLR", field=field, rc=rc)
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

    ESMF_ptr_2D(ips:ipe, jps:jpe) = head_grid%olr(ips:ipe, jps:jpe)

    ! -------------------- QV --------------------

    ! Get QV field
    call ESMF_StateGet(exportState, itemName="QV", field=field, rc=rc)
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

    ESMF_ptr_3D(ips:ipe, kps:kpe, jps:jpe) = head_grid%moist(ips:ipe, kps:kpe, jps:jpe, P_qv)

    ! -------------------- QI --------------------

    ! Get QI field
    call ESMF_StateGet(exportState, itemName="QI", field=field, rc=rc)
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

    ESMF_ptr_3D(ips:ipe, kps:kpe, jps:jpe) = head_grid%moist(ips:ipe, kps:kpe, jps:jpe, P_qi)

    ! -----------------------------------------------

  end subroutine Advance

  !-----------------------------------------------------------------------------

end module WRF
