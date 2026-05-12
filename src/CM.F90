!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module CM

  !-----------------------------------------------------------------------------
  ! COntrail Manager Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    modelSS      => SetServices

  ! Contrail Manager interfaces
  use CM_interface
  use, intrinsic :: iso_c_binding

  implicit none

  private

  ! C pointer to Contrail Manager object
  type(c_ptr), save :: CMptr

  ! Effective radius coupling; read from config in SetServices
  logical :: re_coupling

  ! WRF domain indices
  integer(c_int), save :: ids = 0, ide = 0, jds = 0, jde = 0, kds = 0, kde = 0
  integer(c_int), save :: i_size = 0, j_size = 0, k_size = 0

  public SetVM, SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    type(ESMF_Config)             :: config
    type(NUOPC_FreeFormat)        :: ff
    character(len=800)            :: re_coupling_char

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

    ! Get the config
    call ESMF_GridCompGet(model, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Get effective radius coupling logical from config
    ff = NUOPC_FreeFormatCreate(config, label="Effective radius coupling:", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_FreeFormatGetLine(ff, 1, lineString=re_coupling_char)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    read(re_coupling_char, *) re_coupling

    if (re_coupling) then
      call ESMF_LogWrite("Effective radius is coupled.", ESMF_LOGMSG_INFO, rc=rc)
    else
      call ESMF_LogWrite("Effective radius is not coupled.", ESMF_LOGMSG_INFO, rc=rc)
    end if
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

    call ESMF_LogWrite("CM in Advertise", ESMF_LOGMSG_INFO, rc=rc)

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: XLONG
    call NUOPC_Advertise(importState, StandardName="XLONG", name="XLONG", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: XLAT
    call NUOPC_Advertise(importState, StandardName="XLAT", name="XLAT", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: Z
    call NUOPC_Advertise(importState, StandardName="Z", name="Z", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: Z_AT_W
    call NUOPC_Advertise(importState, StandardName="Z_AT_W", name="Z_AT_W", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: DRYMASS
    call NUOPC_Advertise(importState, StandardName="DRYMASS", name="DRYMASS", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: T_POT
    call NUOPC_Advertise(importState, StandardName="T_POT", name="T_POT", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: deltaT_POT
    call NUOPC_Advertise(exportState, StandardName="deltaT_POT", name="deltaT_POT", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: P
    call NUOPC_Advertise(importState, StandardName="P", name="P", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: U
    call NUOPC_Advertise(importState, StandardName="U", name="U", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: V
    call NUOPC_Advertise(importState, StandardName="V", name="V", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: W
    call NUOPC_Advertise(importState, StandardName="W", name="W", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: TNSR
    call NUOPC_Advertise(importState, StandardName="TNSR", name="TNSR", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: OLR
    call NUOPC_Advertise(importState, StandardName="OLR", name="OLR", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: QV
    call NUOPC_Advertise(importState, StandardName="QV", name="QV", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: deltaQV
    call NUOPC_Advertise(exportState, StandardName="deltaQV", name="deltaQV", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: QI
    call NUOPC_Advertise(importState, StandardName="QI", name="QI", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: deltaQI
    call NUOPC_Advertise(exportState, StandardName="deltaQI", name="deltaQI", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: deltaNI
    call NUOPC_Advertise(exportState, StandardName="deltaNI", name="deltaNI", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: QIcontrail
    call NUOPC_Advertise(exportState, StandardName="QIcontrail", name="QIcontrail",rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: REIcontrail
    call NUOPC_Advertise(exportState, StandardName="REIcontrail", name="REIcontrail",rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Initialise Contrail Manager
    CMptr = create_ContrailManager()
    call ContrailManager_init(CMptr)

    call ESMF_LogWrite("CM leaving Advertise", ESMF_LOGMSG_INFO, rc=rc) 

  end subroutine Advertise

  !-----------------------------------------------------------------------------

  subroutine Realize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    type(ESMF_DistGrid)     :: distgrid2D, distgrid3D
    type(ESMF_Grid)         :: grid2D, grid3D
    character(len=160)      :: msgString

    ! WRF domain info
    integer(ESMF_KIND_I4)   :: intvals(19)

    ! Projection info
    integer(c_int)          :: proj_code
    real(c_float)           :: lat1, lon1, knowni, knownj, dx, stdlon, &
                               truelat1, truelat2
    
    rc = ESMF_SUCCESS

    call ESMF_LogWrite("CM in Realize", ESMF_LOGMSG_INFO, rc=rc)

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

    ! Dimension sizes in CM (1 less than in WRF)
    i_size = ide - ids
    j_size = jde - jds
    k_size = kde - kds

    ! Projection variables
    call ESMF_AttributeGet(importState, 'proj_code', proj_code, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeGet(importState, 'lat1', lat1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeGet(importState, 'lon1', lon1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeGet(importState, 'knowni', knowni, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeGet(importState, 'knownj', knownj, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeGet(importState, 'dx', dx, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeGet(importState, 'stdlon', stdlon, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeGet(importState, 'truelat1', truelat1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeGet(importState, 'truelat2', truelat2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (proj_code .eq. PROJ_LC) then
      call init_domainlc(CMptr, ids, ide-1, jds, jde-1, kds, kde-1, &
        lat1, lon1, knowni, knownj, dx, stdlon, truelat1, truelat2)

    else
      write(msgString, *) "proj_code ", proj_code, " not recognised"
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_ERROR, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    endif

    distgrid2D = ESMF_DistGridCreate( &
      minIndex=(/ ids, jds /), &
      maxIndex=(/ ide, jde /), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    distgrid3D = ESMF_DistGridCreate( &
      minIndex=(/ ids, kds, jds /), &
      maxIndex=(/ ide, kde, jde /), &
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

    call NUOPC_Realize(importState, grid=grid2D, fieldName="XLONG", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(importState, grid=grid2D, fieldName="XLAT", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(importState, grid=grid3D, fieldName="Z", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(importState, grid=grid3D, fieldName="Z_AT_W", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(importState, grid=grid3D, fieldName="DRYMASS", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(importState, grid=grid3D, fieldName="T_POT", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(exportState, grid=grid3D, fieldName="deltaT_POT", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(importState, grid=grid3D, fieldName="P", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(importState, grid=grid3D, fieldName="U", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(importState, grid=grid3D, fieldName="V", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(importState, grid=grid3D, fieldName="W", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(importState, grid=grid2D, fieldName="TNSR", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(importState, grid=grid2D, fieldName="OLR", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(importState, grid=grid3D, fieldName="QV", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(exportState, grid=grid3D, fieldName="deltaQV", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(importState, grid=grid3D, fieldName="QI", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(exportState, grid=grid3D, fieldName="deltaQI", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(exportState, grid=grid3D, fieldName="deltaNI", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(exportState, grid=grid3D, fieldName="QIcontrail", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(exportState, grid=grid3D, fieldName="REIcontrail", &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite("CM leaving Realize", ESMF_LOGMSG_INFO, rc=rc)

  end subroutine Realize

  !-----------------------------------------------------------------------------

  subroutine DataInitialize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: time
    type(ESMF_Field)            :: field
    real(ESMF_KIND_R4), pointer :: ESMF_ptr_2D(:,:), ESMF_ptr_3D(:,:,:)
    logical                     :: isAvailable
    character(len=160)          :: msgString
    integer(c_int)              :: i, j, k
    type(c_ptr)                 :: c_arr_ptr
    real(c_float), pointer      :: f_arr_ptr_2D(:,:), f_arr_ptr_3D(:,:,:)

    ! Data dependency flags
    logical, save :: XLONG_satisfied = .false.
    logical, save :: XLAT_satisfied = .false.
    logical, save :: Z_satisfied = .false.
    logical, save :: Z_AT_W_satisfied = .false.
    logical, save :: DRYMASS_satisfied = .false.
    logical, save :: T_POT_satisfied = .false.
    logical, save :: P_satisfied = .false.
    logical, save :: U_satisfied = .false.
    logical, save :: V_satisfied = .false.
    logical, save :: W_satisfied = .false.
    logical, save :: TNSR_satisfied = .false.
    logical, save :: OLR_satisfied = .false.
    logical, save :: QV_satisfied = .false.
    logical, save :: QI_satisfied = .false.

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("CM in DataInitialize", ESMF_LOGMSG_INFO, rc=rc)

    ! query for clock, importState, and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Get the current time from the clock
    call ESMF_ClockGet(clock, currTime=time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- XLONG --------------------

    ! Get XLONG field
    call ESMF_StateGet(importState, itemName="XLONG", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Check if field has been given current time
    isAvailable = NUOPC_IsAtTime(field, time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (isAvailable) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_2D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      c_arr_ptr = get_XLONG(CMptr)
      ! Swap order of sizes from row-major to column-major
      call c_f_pointer(c_arr_ptr, f_arr_ptr_2D, [j_size, i_size])

      do i = 1, i_size
        do j = 1, j_size
          f_arr_ptr_2D(j, i) = ESMF_ptr_2D(ids+i-1, jds+j-1)
        end do
      end do
      
      XLONG_satisfied = .true.
      call ESMF_LogWrite("XLONG dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    end if

    ! -------------------- XLAT --------------------

    ! Get XLAT field
    call ESMF_StateGet(importState, itemName="XLAT", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Check if field has been given current time
    isAvailable = NUOPC_IsAtTime(field, time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (isAvailable) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_2D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      c_arr_ptr = get_XLAT(CMptr)
      ! Swap order of sizes from row-major to column-major
      call c_f_pointer(c_arr_ptr, f_arr_ptr_2D, [j_size, i_size])

      do i = 1, i_size
        do j = 1, j_size
          f_arr_ptr_2D(j, i) = ESMF_ptr_2D(ids+i-1, jds+j-1)
        end do
      end do
      
      XLAT_satisfied = .true.
      call ESMF_LogWrite("XLAT dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    end if

    ! -------------------- Z --------------------

    ! Get Z field
    call ESMF_StateGet(importState, itemName="Z", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Check if field has been given current time
    isAvailable = NUOPC_IsAtTime(field, time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (isAvailable) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      c_arr_ptr = get_Z(CMptr)
      ! Swap order of sizes from row-major to column-major
      call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

      do i = 1, i_size
        do j = 1, j_size
          do k = 1, k_size
            f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
          end do
        end do
      end do
      
      Z_satisfied = .true.
      call ESMF_LogWrite("Z dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    end if

    ! -------------------- Z_AT_W --------------------

    ! Get Z_AT_W field
    call ESMF_StateGet(importState, itemName="Z_AT_W", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Check if field has been given current time
    isAvailable = NUOPC_IsAtTime(field, time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (isAvailable) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      c_arr_ptr = get_Z_AT_W(CMptr)
      ! Swap order of sizes from row-major to column-major
      call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size+1, j_size, i_size])

      ! Z_AT_W is only staggered field
      do i = 1, i_size
        do j = 1, j_size
          do k = 1, k_size+1
            f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
          end do
        end do
      end do
      
      Z_AT_W_satisfied = .true.
      call ESMF_LogWrite("Z_AT_W dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    end if

    ! -------------------- DRYMASS --------------------

    ! Get DRYMASS field
    call ESMF_StateGet(importState, itemName="DRYMASS", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Check if field has been given current time
    isAvailable = NUOPC_IsAtTime(field, time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (isAvailable) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      c_arr_ptr = get_DRYMASS(CMptr)
      ! Swap order of sizes from row-major to column-major
      call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

      do i = 1, i_size
        do j = 1, j_size
          do k = 1, k_size
            f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
          end do
        end do
      end do
      
      DRYMASS_satisfied = .true.
      call ESMF_LogWrite("DRYMASS dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    end if

    ! -------------------- T_POT --------------------

    ! Get T_POT field
    call ESMF_StateGet(importState, itemName="T_POT", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Check if field has been given current time
    isAvailable = NUOPC_IsAtTime(field, time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (isAvailable) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      c_arr_ptr = get_T_POT(CMptr)
      ! Swap order of sizes from row-major to column-major
      call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

      do i = 1, i_size
        do j = 1, j_size
          do k = 1, k_size
            f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
          end do
        end do
      end do
      
      T_POT_satisfied = .true.
      call ESMF_LogWrite("T_POT dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    end if

    ! -------------------- deltaT_POT --------------------

    ! Get deltaT_POT field
    call ESMF_StateGet(exportState, itemName="deltaT_POT", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldFill(field, dataFillScheme="const", const1=0.D0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- P --------------------

    ! Get P field
    call ESMF_StateGet(importState, itemName="P", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Check if field has been given current time
    isAvailable = NUOPC_IsAtTime(field, time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (isAvailable) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      c_arr_ptr = get_P(CMptr)
      ! Swap order of sizes from row-major to column-major
      call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

      do i = 1, i_size
        do j = 1, j_size
          do k = 1, k_size
            f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
          end do
        end do
      end do
      
      P_satisfied = .true.
      call ESMF_LogWrite("P dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    end if

    ! -------------------- U --------------------

    ! Get U field
    call ESMF_StateGet(importState, itemName="U", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Check if field has been given current time
    isAvailable = NUOPC_IsAtTime(field, time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (isAvailable) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      c_arr_ptr = get_U(CMptr)
      ! Swap order of sizes from row-major to column-major
      call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

      do i = 1, i_size
        do j = 1, j_size
          do k = 1, k_size
            f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
          end do
        end do
      end do
      
      U_satisfied = .true.
      call ESMF_LogWrite("U dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    end if

    ! -------------------- V --------------------

    ! Get V field
    call ESMF_StateGet(importState, itemName="V", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Check if field has been given current time
    isAvailable = NUOPC_IsAtTime(field, time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (isAvailable) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      c_arr_ptr = get_V(CMptr)
      ! Swap order of sizes from row-major to column-major
      call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

      do i = 1, i_size
        do j = 1, j_size
          do k = 1, k_size
            f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
          end do
        end do
      end do
      
      V_satisfied = .true.
      call ESMF_LogWrite("V dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    end if

    ! -------------------- W --------------------

    ! Get W field
    call ESMF_StateGet(importState, itemName="W", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Check if field has been given current time
    isAvailable = NUOPC_IsAtTime(field, time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (isAvailable) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      c_arr_ptr = get_W(CMptr)
      ! Swap order of sizes from row-major to column-major
      call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

      do i = 1, i_size
        do j = 1, j_size
          do k = 1, k_size
            f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
          end do
        end do
      end do
      
      W_satisfied = .true.
      call ESMF_LogWrite("W dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    end if

    ! -------------------- TNSR --------------------

    ! Get TNSR field
    call ESMF_StateGet(importState, itemName="TNSR", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Check if field has been given current time
    isAvailable = NUOPC_IsAtTime(field, time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (isAvailable) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_2D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      c_arr_ptr = get_TNSR(CMptr)
      ! Swap order of sizes from row-major to column-major
      call c_f_pointer(c_arr_ptr, f_arr_ptr_2D, [j_size, i_size])

      do i = 1, i_size
        do j = 1, j_size
          f_arr_ptr_2D(j, i) = ESMF_ptr_2D(ids+i-1, jds+j-1)
        end do
      end do
      
      TNSR_satisfied = .true.
      call ESMF_LogWrite("TNSR dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    end if

    ! -------------------- OLR --------------------

    ! Get OLR field
    call ESMF_StateGet(importState, itemName="OLR", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Check if field has been given current time
    isAvailable = NUOPC_IsAtTime(field, time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (isAvailable) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_2D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      c_arr_ptr = get_OLR(CMptr)
      ! Swap order of sizes from row-major to column-major
      call c_f_pointer(c_arr_ptr, f_arr_ptr_2D, [j_size, i_size])

      do i = 1, i_size
        do j = 1, j_size
          f_arr_ptr_2D(j, i) = ESMF_ptr_2D(ids+i-1, jds+j-1)
        end do
      end do
      
      OLR_satisfied = .true.
      call ESMF_LogWrite("OLR dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    end if

    ! -------------------- QV --------------------

    ! Get QV field
    call ESMF_StateGet(importState, itemName="QV", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Check if field has been given current time
    isAvailable = NUOPC_IsAtTime(field, time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (isAvailable) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      c_arr_ptr = get_QV(CMptr)
      ! Swap order of sizes from row-major to column-major
      call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

      do i = 1, i_size
        do j = 1, j_size
          do k = 1, k_size
            f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
          end do
        end do
      end do
      
      QV_satisfied = .true.
      call ESMF_LogWrite("QV dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    end if

    ! -------------------- deltaQV --------------------

    ! Get deltaQV field
    call ESMF_StateGet(exportState, itemName="deltaQV", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldFill(field, dataFillScheme="const", const1=0.D0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- QI --------------------

    ! Get QI field
    call ESMF_StateGet(importState, itemName="QI", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Check if field has been given current time
    isAvailable = NUOPC_IsAtTime(field, time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (isAvailable) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      c_arr_ptr = get_QI(CMptr)
      ! Swap order of sizes from row-major to column-major
      call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

      do i = 1, i_size
        do j = 1, j_size
          do k = 1, k_size
            f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
          end do
        end do
      end do
      
      QI_satisfied = .true.
      call ESMF_LogWrite("QI dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    end if

    ! -------------------- deltaQI --------------------

    ! Get deltaQI field
    call ESMF_StateGet(exportState, itemName="deltaQI", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldFill(field, dataFillScheme="const", const1=0.D0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- deltaNI --------------------

    ! Get deltaNI field
    call ESMF_StateGet(exportState, itemName="deltaNI", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldFill(field, dataFillScheme="const", const1=0.D0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- QIcontrail --------------------

    ! Get QIcontrail field
    call ESMF_StateGet(exportState, itemName="QIcontrail", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldFill(field, dataFillScheme="const", const1=0.D0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -------------------- REIcontrail --------------------

    ! Get REIcontrail field
    call ESMF_StateGet(exportState, itemName="REIcontrail", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldFill(field, dataFillScheme="const", const1=0.D0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Indicate that the field has been updated
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -----------------------------------------------

    ! Check if all dependencies satisfied; if so, mark complete

    if (Z_satisfied .and. &
        XLONG_satisfied .and. &
        XLAT_satisfied .and. &
        Z_AT_W_satisfied .and. &
        DRYMASS_satisfied .and. &
        T_POT_satisfied .and. &
        P_satisfied .and. &
        U_satisfied .and. &
        V_satisfied .and. &
        W_satisfied .and. &
        TNSR_satisfied .and. &
        OLR_satisfied .and. &
        QV_satisfied .and. &
        QI_satisfied) then
      call NUOPC_CompAttributeSet(model, &
        name="InitializeDataComplete", value="true", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    end if

    call ESMF_LogWrite("CM leaving DataInitialize", ESMF_LOGMSG_INFO, rc=rc) 
  
  end subroutine DataInitialize

  !-----------------------------------------------------------------------------

  subroutine Advance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: ESMF_startTime, ESMF_stopTime
    type(CMTime_F)              :: CM_startTime, CM_stopTime
    integer(c_int)              :: year, month, day, hour, minute, second
    type(ESMF_Field)            :: field
    real(ESMF_KIND_R4), pointer :: ESMF_ptr_2D(:,:), ESMF_ptr_3D(:,:,:)
    integer                     :: i, j, k
    type(c_ptr)                 :: c_arr_ptr
    real(c_float), pointer      :: f_arr_ptr_2D(:,:), f_arr_ptr_3D(:,:,:)
    character(len=160)          :: msgString

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing CM from: ", unit=msgString, rc=rc)
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

    ! Get start and stop times from clock
    call ESMF_ClockGet(clock, currTime=ESMF_startTime, stopTime=ESMF_stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Translate start time to CM type
    call ESMF_TimeGet(ESMF_startTime, yy=year, mm=month, dd=day, h=hour, m=minute, & 
      s=second, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    CM_startTime%yy = year
    CM_startTime%mm = month
    CM_startTime%dd = day
    CM_startTime%h  = hour
    CM_startTime%m  = minute
    CM_startTime%s  = second

    ! Translate stop time to CM type
    call ESMF_TimeGet(ESMF_stopTime, yy=year, mm=month, dd=day, h=hour, m=minute, & 
      s=second, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    CM_stopTime%yy = year
    CM_stopTime%mm = month
    CM_stopTime%dd = day
    CM_stopTime%h  = hour
    CM_stopTime%m  = minute
    CM_stopTime%s  = second

    ! -----------------------------------------------

    ! Imports

    ! -------------------- Z --------------------

    ! Get Z field
    call ESMF_StateGet(importState, itemName="Z", field=field, rc=rc)
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

    c_arr_ptr = get_Z(CMptr)
    ! Swap order of sizes from row-major to column-major
    call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

    do i = 1, i_size
      do j = 1, j_size
        do k = 1, k_size
          f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
        end do
      end do
    end do

    ! -------------------- Z_AT_W --------------------

    ! Get Z_AT_W field
    call ESMF_StateGet(importState, itemName="Z_AT_W", field=field, rc=rc)
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

    c_arr_ptr = get_Z_AT_W(CMptr)
    ! Swap order of sizes from row-major to column-major
    call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size+1, j_size, i_size])

    ! Z_AT_W is only staggered field
    do i = 1, i_size
      do j = 1, j_size
        do k = 1, k_size+1
          f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
        end do
      end do
    end do

    ! -------------------- DRYMASS --------------------

    ! Get DRYMASS field
    call ESMF_StateGet(importState, itemName="DRYMASS", field=field, rc=rc)
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

    c_arr_ptr = get_DRYMASS(CMptr)
    ! Swap order of sizes from row-major to column-major
    call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

    do i = 1, i_size
      do j = 1, j_size
        do k = 1, k_size
          f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
        end do
      end do
    end do

    ! -------------------- T_POT --------------------

    ! Get T_POT field
    call ESMF_StateGet(importState, itemName="T_POT", field=field, rc=rc)
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

    c_arr_ptr = get_T_POT(CMptr)
    ! Swap order of sizes from row-major to column-major
    call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

    do i = 1, i_size
      do j = 1, j_size
        do k = 1, k_size
          f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
        end do
      end do
    end do

    ! -------------------- P --------------------

    ! Get P field
    call ESMF_StateGet(importState, itemName="P", field=field, rc=rc)
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

    c_arr_ptr = get_P(CMptr)
    ! Swap order of sizes from row-major to column-major
    call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

    do i = 1, i_size
      do j = 1, j_size
        do k = 1, k_size
          f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
        end do
      end do
    end do

    ! -------------------- U --------------------

    ! Get U field
    call ESMF_StateGet(importState, itemName="U", field=field, rc=rc)
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

    c_arr_ptr = get_U(CMptr)
    ! Swap order of sizes from row-major to column-major
    call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

    do i = 1, i_size
      do j = 1, j_size
        do k = 1, k_size
          f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
        end do
      end do
    end do

    ! -------------------- V --------------------

    ! Get V field
    call ESMF_StateGet(importState, itemName="V", field=field, rc=rc)
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

    c_arr_ptr = get_V(CMptr)
    ! Swap order of sizes from row-major to column-major
    call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

    do i = 1, i_size
      do j = 1, j_size
        do k = 1, k_size
          f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
        end do
      end do
    end do

    ! -------------------- W --------------------

    ! Get W field
    call ESMF_StateGet(importState, itemName="W", field=field, rc=rc)
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

    c_arr_ptr = get_W(CMptr)
    ! Swap order of sizes from row-major to column-major
    call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

    do i = 1, i_size
      do j = 1, j_size
        do k = 1, k_size
          f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
        end do
      end do
    end do

    ! -------------------- TNSR --------------------

    ! Get TNSR field
    call ESMF_StateGet(importState, itemName="TNSR", field=field, rc=rc)
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

    c_arr_ptr = get_TNSR(CMptr)
    ! Swap order of sizes from row-major to column-major
    call c_f_pointer(c_arr_ptr, f_arr_ptr_2D, [j_size, i_size])

    do i = 1, i_size
      do j = 1, j_size
        f_arr_ptr_2D(j, i) = ESMF_ptr_2D(ids+i-1, jds+j-1)
      end do
    end do

    ! -------------------- OLR --------------------

    ! Get TNSR field
    call ESMF_StateGet(importState, itemName="OLR", field=field, rc=rc)
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

    c_arr_ptr = get_OLR(CMptr)
    ! Swap order of sizes from row-major to column-major
    call c_f_pointer(c_arr_ptr, f_arr_ptr_2D, [j_size, i_size])

    do i = 1, i_size
      do j = 1, j_size
        f_arr_ptr_2D(j, i) = ESMF_ptr_2D(ids+i-1, jds+j-1)
      end do
    end do

    ! -------------------- QV --------------------

    ! Get QV field
    call ESMF_StateGet(importState, itemName="QV", field=field, rc=rc)
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

    c_arr_ptr = get_QV(CMptr)
    ! Swap order of sizes from row-major to column-major
    call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

    do i = 1, i_size
      do j = 1, j_size
        do k = 1, k_size
          f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
        end do
      end do
    end do

    ! -------------------- QI --------------------

    ! Get QI field
    call ESMF_StateGet(importState, itemName="QI", field=field, rc=rc)
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

    c_arr_ptr = get_QI(CMptr)
    ! Swap order of sizes from row-major to column-major
    call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

    do i = 1, i_size
      do j = 1, j_size
        do k = 1, k_size
          f_arr_ptr_3D(k, j, i) = ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1)
        end do
      end do
    end do

    ! -----------------------------------------------

    ! Run the Contrail Manager
    call ContrailManager_run(CMptr, CM_startTime, CM_stopTime)

    ! -----------------------------------------------

    ! Exports

    ! -------------------- deltaT_POT --------------------

    ! Get deltaT_POT field
    call ESMF_StateGet(exportState, itemName="deltaT_POT", field=field, rc=rc)
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

    c_arr_ptr = get_deltaT_POT(CMptr)
    ! Swap order of sizes from row-major to column-major
    call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

    do i = 1, i_size
      do j = 1, j_size
        do k = 1, k_size
          ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1) = f_arr_ptr_3D(k, j, i)
        end do
      end do
    end do

    ! -------------------- deltaQV --------------------

    ! Get deltaQV field
    call ESMF_StateGet(exportState, itemName="deltaQV", field=field, rc=rc)
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

    c_arr_ptr = get_deltaQV(CMptr)
    ! Swap order of sizes from row-major to column-major
    call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

    do i = 1, i_size
      do j = 1, j_size
        do k = 1, k_size
          ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1) = f_arr_ptr_3D(k, j, i)
        end do
      end do
    end do

    ! -------------------- deltaQI --------------------

    ! Get deltaQI field
    call ESMF_StateGet(exportState, itemName="deltaQI", field=field, rc=rc)
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

    c_arr_ptr = get_deltaQI(CMptr)
    ! Swap order of sizes from row-major to column-major
    call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

    do i = 1, i_size
      do j = 1, j_size
        do k = 1, k_size
          ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1) = f_arr_ptr_3D(k, j, i)
        end do
      end do
    end do

    ! -------------------- deltaNI --------------------

    ! Get deltaNI field
    call ESMF_StateGet(exportState, itemName="deltaNI", field=field, rc=rc)
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

    c_arr_ptr = get_deltaNI(CMptr)
    ! Swap order of sizes from row-major to column-major
    call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

    do i = 1, i_size
      do j = 1, j_size
        do k = 1, k_size
          ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1) = f_arr_ptr_3D(k, j, i)
        end do
      end do
    end do

    ! -------------------- QIcontrail --------------------

    ! Get QIcontrail field
    call ESMF_StateGet(exportState, itemName="QIcontrail", field=field, rc=rc)
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

    c_arr_ptr = get_QIcontrail(CMptr)
    ! Swap order of sizes from row-major to column-major
    call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

    do i = 1, i_size
      do j = 1, j_size
        do k = 1, k_size
          ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1) = f_arr_ptr_3D(k, j, i)
        end do
      end do
    end do

    ! -------------------- REIcontrail --------------------

    if (re_coupling) then
      ! Get REIcontrail field
      call ESMF_StateGet(exportState, itemName="REIcontrail", field=field, rc=rc)
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

      c_arr_ptr = get_REIcontrail(CMptr)
      ! Swap order of sizes from row-major to column-major
      call c_f_pointer(c_arr_ptr, f_arr_ptr_3D, [k_size, j_size, i_size])

      do i = 1, i_size
        do j = 1, j_size
          do k = 1, k_size
            ESMF_ptr_3D(ids+i-1, kds+k-1, jds+j-1) = f_arr_ptr_3D(k, j, i)
          end do
        end do
      end do
    end if

    ! -----------------------------------------------

  end subroutine Advance

  !-----------------------------------------------------------------------------

end module CM
