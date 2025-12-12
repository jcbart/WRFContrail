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

  ! WRF domain indices
  integer(c_int), save :: ids = 0, ide = 0, jds = 0, jde = 0, kds = 0, kde = 0

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
    call NUOPC_CompSpecialize(model, specLabel=label_RealizeAccepted, &
      specRoutine=RealizeAccepted, rc=rc)
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

    call ESMF_LogWrite("CM in Advertise", ESMF_LOGMSG_INFO, rc=rc)

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: XLAT
    call NUOPC_Advertise(importState, StandardName="XLAT", name="XLAT", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: XLONG
    call NUOPC_Advertise(importState, StandardName="XLONG", name="XLONG", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: Z
    call NUOPC_Advertise(importState, StandardName="Z", name="Z", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: Z_AT_W
    call NUOPC_Advertise(importState, StandardName="Z_AT_W", name="Z_AT_W", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: DRYMASS
    call NUOPC_Advertise(importState, StandardName="DRYMASS", name="DRYMASS", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: T_POT
    call NUOPC_Advertise(importState, StandardName="T_POT", name="T_POT", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: P
    call NUOPC_Advertise(importState, StandardName="P", name="P", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: U
    call NUOPC_Advertise(importState, StandardName="U", name="U", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: V
    call NUOPC_Advertise(importState, StandardName="V", name="V", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: W
    call NUOPC_Advertise(importState, StandardName="W", name="W", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: QV
    call NUOPC_Advertise(importState, StandardName="QV", name="QV", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: deltaQV
    call NUOPC_Advertise(exportState, StandardName="deltaQV", name="deltaQV", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: deltaQI
    call NUOPC_Advertise(exportState, StandardName="deltaQI", name="deltaQI", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: deltaNI
    call NUOPC_Advertise(exportState, StandardName="deltaNI", name="deltaNI", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: QIcon
    call NUOPC_Advertise(exportState, StandardName="QIcon", name="QIcon", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: NIcon
    call NUOPC_Advertise(exportState, StandardName="NIcon", name="NIcon", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite("CM leaving Advertise", ESMF_LOGMSG_INFO, rc=rc) 

  end subroutine Advertise

  !-----------------------------------------------------------------------------

  subroutine RealizeAccepted(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    
    rc = ESMF_SUCCESS

    call ESMF_LogWrite("CM in RealizeAccepted", ESMF_LOGMSG_INFO, rc=rc)

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Initialise Contrail Manager
    CMptr = create_ContrailManager()
    call ContrailManager_init(CMptr)

    ! Realize importable field derived from WRF grid object: XLAT
    call NUOPC_Realize(importState, fieldName="XLAT", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Realize importable field derived from WRF grid object: XLONG
    call NUOPC_Realize(importState, fieldName="XLONG", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Realize importable field derived from WRF grid object: Z
    call NUOPC_Realize(importState, fieldName="Z", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Realize importable field derived from WRF grid object: Z_AT_W
    call NUOPC_Realize(importState, fieldName="Z_AT_W", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Realize importable field derived from WRF grid object: DRYMASS
    call NUOPC_Realize(importState, fieldName="DRYMASS", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Realize importable field derived from WRF grid object: T_POT
    call NUOPC_Realize(importState, fieldName="T_POT", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Realize importable field derived from WRF grid object: P
    call NUOPC_Realize(importState, fieldName="P", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Realize importable field derived from WRF grid object: U
    call NUOPC_Realize(importState, fieldName="U", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Realize importable field derived from WRF grid object: V
    call NUOPC_Realize(importState, fieldName="V", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Realize importable field derived from WRF grid object: W
    call NUOPC_Realize(importState, fieldName="W", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Realize importable field derived from WRF grid object: QV
    call NUOPC_Realize(importState, fieldName="QV", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Realize exportable field derived from WRF grid object: deltaQV
    call NUOPC_Realize(exportState, fieldName="deltaQV", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Realize exportable field derived from WRF grid object: deltaQI
    call NUOPC_Realize(exportState, fieldName="deltaQI", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Realize exportable field derived from WRF grid object: deltaNI
    call NUOPC_Realize(exportState, fieldName="deltaNI", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Realize exportable field derived from WRF grid object: QIcon
    call NUOPC_Realize(exportState, fieldName="QIcon", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Realize exportable field derived from WRF grid object: NIcon
    call NUOPC_Realize(exportState, fieldName="NIcon", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine RealizeAccepted

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
    type(c_ptr)                 :: c_data_ptr
    real(c_float), pointer      :: f_data_ptr
    integer(c_int)              :: proj_code
    real(c_float)               :: lat1, lon1, knowni, knownj, dx, stdlon, &
                                   truelat1, truelat2

    ! Data dependency flags
    logical, save :: vars_initialised = .false.
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
    logical, save :: QV_satisfied = .false.
    logical, save :: proj_satisfied = .false.

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

    ! -------------------- Z --------------------
    ! Do Z first to get bounds for all dimensions and init all CM vars

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
      
      ids = lbound(ESMF_ptr_3D, dim=1)
      kds = lbound(ESMF_ptr_3D, dim=2)
      jds = lbound(ESMF_ptr_3D, dim=3)
      ide = ubound(ESMF_ptr_3D, dim=1)
      kde = ubound(ESMF_ptr_3D, dim=2)
      jde = ubound(ESMF_ptr_3D, dim=3)

      call init_CM_vars(CMptr, ids, ide-1, jds, jde-1, kds, kde-1)
      vars_initialised = .true.

      do i = ids, ide-1
        do k = kds, kde-1
          do j = jds, jde-1
            ! WRF is ikj, CM is ijk
            c_data_ptr = get_Z_element(CMptr, i, j, k)
            call c_f_pointer(c_data_ptr, f_data_ptr)
            f_data_ptr = ESMF_ptr_3D(i, k, j)
          end do
        end do
      end do

      write(msgString, *) "Z in DataInitialize: ESMF_ptr_3D(i=100, k=10, j=200) = ", ESMF_ptr_3D(100, 10, 200)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      
      Z_satisfied = .true.
      call ESMF_LogWrite("Z dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    else
      call ESMF_LogWrite("Z dependency not yet satisfied", ESMF_LOGMSG_INFO, rc=rc)
    end if

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

    if (isAvailable .and. vars_initialised) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_2D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do i = ids, ide-1
        do j = jds, jde-1
          c_data_ptr = get_XLONG_element(CMptr, i, j)
          call c_f_pointer(c_data_ptr, f_data_ptr)
          f_data_ptr = ESMF_ptr_2D(i, j)
        end do
      end do
      
      XLONG_satisfied = .true.
      call ESMF_LogWrite("XLONG dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    else
      call ESMF_LogWrite("XLONG dependency not yet satisfied", ESMF_LOGMSG_INFO, rc=rc)
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

    if (isAvailable .and. vars_initialised) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_2D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do i = ids, ide-1
        do j = jds, jde-1
          c_data_ptr = get_XLAT_element(CMptr, i, j)
          call c_f_pointer(c_data_ptr, f_data_ptr)
          f_data_ptr = ESMF_ptr_2D(i, j)
        end do
      end do
      
      XLAT_satisfied = .true.
      call ESMF_LogWrite("XLAT dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    else
      call ESMF_LogWrite("XLAT dependency not yet satisfied", ESMF_LOGMSG_INFO, rc=rc)
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

    if (isAvailable .and. vars_initialised) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do i = ids, ide-1
        do k = kds, kde ! Z_AT_W is only staggered field
          do j = jds, jde-1
            ! WRF is ikj, CM is ijk
            c_data_ptr = get_Z_AT_W_element(CMptr, i, j, k)
            call c_f_pointer(c_data_ptr, f_data_ptr)
            f_data_ptr = ESMF_ptr_3D(i, k, j)
          end do
        end do
      end do
      
      Z_AT_W_satisfied = .true.
      call ESMF_LogWrite("Z_AT_W dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    else
      call ESMF_LogWrite("Z_AT_W dependency not yet satisfied", ESMF_LOGMSG_INFO, rc=rc)
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

    if (isAvailable .and. vars_initialised) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do i = ids, ide-1
        do k = kds, kde-1
          do j = jds, jde-1
            ! WRF is ikj, CM is ijk
            c_data_ptr = get_DRYMASS_element(CMptr, i, j, k)
            call c_f_pointer(c_data_ptr, f_data_ptr)
            f_data_ptr = ESMF_ptr_3D(i, k, j)
          end do
        end do
      end do
      
      DRYMASS_satisfied = .true.
      call ESMF_LogWrite("DRYMASS dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    else
      call ESMF_LogWrite("DRYMASS dependency not yet satisfied", ESMF_LOGMSG_INFO, rc=rc)
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

    if (isAvailable .and. vars_initialised) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do i = ids, ide-1
        do k = kds, kde-1
          do j = jds, jde-1
            ! WRF is ikj, CM is ijk
            c_data_ptr = get_T_POT_element(CMptr, i, j, k)
            call c_f_pointer(c_data_ptr, f_data_ptr)
            f_data_ptr = ESMF_ptr_3D(i, k, j)
          end do
        end do
      end do
      
      T_POT_satisfied = .true.
      call ESMF_LogWrite("T_POT dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    else
      call ESMF_LogWrite("T_POT dependency not yet satisfied", ESMF_LOGMSG_INFO, rc=rc)
    end if

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

    if (isAvailable .and. vars_initialised) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do i = ids, ide-1
        do k = kds, kde-1
          do j = jds, jde-1
            ! WRF is ikj, CM is ijk
            c_data_ptr = get_P_element(CMptr, i, j, k)
            call c_f_pointer(c_data_ptr, f_data_ptr)
            f_data_ptr = ESMF_ptr_3D(i, k, j)
          end do
        end do
      end do
      
      P_satisfied = .true.
      call ESMF_LogWrite("P dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    else
      call ESMF_LogWrite("P dependency not yet satisfied", ESMF_LOGMSG_INFO, rc=rc)
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

    if (isAvailable .and. vars_initialised) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do i = ids, ide-1
        do k = kds, kde-1
          do j = jds, jde-1
            ! WRF is ikj, CM is ijk
            c_data_ptr = get_U_element(CMptr, i, j, k)
            call c_f_pointer(c_data_ptr, f_data_ptr)
            f_data_ptr = ESMF_ptr_3D(i, k, j)
          end do
        end do
      end do
      
      U_satisfied = .true.
      call ESMF_LogWrite("U dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    else
      call ESMF_LogWrite("U dependency not yet satisfied", ESMF_LOGMSG_INFO, rc=rc)
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

    if (isAvailable .and. vars_initialised) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do i = ids, ide-1
        do k = kds, kde-1
          do j = jds, jde-1
            ! WRF is ikj, CM is ijk
            c_data_ptr = get_V_element(CMptr, i, j, k)
            call c_f_pointer(c_data_ptr, f_data_ptr)
            f_data_ptr = ESMF_ptr_3D(i, k, j)
          end do
        end do
      end do
      
      V_satisfied = .true.
      call ESMF_LogWrite("V dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    else
      call ESMF_LogWrite("V dependency not yet satisfied", ESMF_LOGMSG_INFO, rc=rc)
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

    if (isAvailable .and. vars_initialised) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do i = ids, ide-1
        do k = kds, kde-1
          do j = jds, jde-1
            ! WRF is ikj, CM is ijk
            c_data_ptr = get_W_element(CMptr, i, j, k)
            call c_f_pointer(c_data_ptr, f_data_ptr)
            f_data_ptr = ESMF_ptr_3D(i, k, j)
          end do
        end do
      end do
      
      W_satisfied = .true.
      call ESMF_LogWrite("W dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    else
      call ESMF_LogWrite("W dependency not yet satisfied", ESMF_LOGMSG_INFO, rc=rc)
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

    if (isAvailable .and. vars_initialised) then
      ! Get pointer from field
      call ESMF_FieldGet(field, localDe=0, farrayPtr=ESMF_ptr_3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do i = ids, ide-1
        do k = kds, kde-1
          do j = jds, jde-1
            ! WRF is ikj, CM is ijk
            c_data_ptr = get_QV_element(CMptr, i, j, k)
            call c_f_pointer(c_data_ptr, f_data_ptr)
            f_data_ptr = ESMF_ptr_3D(i, k, j)
          end do
        end do
      end do
      
      QV_satisfied = .true.
      call ESMF_LogWrite("QV dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)
    else
      call ESMF_LogWrite("QV dependency not yet satisfied", ESMF_LOGMSG_INFO, rc=rc)
    end if

    ! -------------------- Projection variables --------------------

    if (.not. proj_satisfied) then
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

      call init_projection(CMptr, proj_code, lat1, lon1, knowni, knownj, &
        dx, stdlon, truelat1, truelat2)

      proj_satisfied = .true.
    end if

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
        QV_satisfied .and. &
        proj_satisfied) then
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
!$  use omp_lib
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
    type(c_ptr)                 :: c_data_ptr
    real(c_float), pointer      :: f_data_ptr
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

    do i = ids, ide-1
      do k = kds, kde-1
        do j = jds, jde-1
          ! WRF is ikj, CM is ijk
          c_data_ptr = get_Z_element(CMptr, i, j, k)
          call c_f_pointer(c_data_ptr, f_data_ptr)
          f_data_ptr = ESMF_ptr_3D(i, k, j)
        end do
      end do
    end do

    write(msgString, *) "Z: ESMF_ptr_3D(i=100, k=10, j=200) = ", ESMF_ptr_3D(100, 10, 200)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

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

    do i = ids, ide-1
      do k = kds, kde ! Z_AT_W is only staggered field
        do j = jds, jde-1
          ! WRF is ikj, CM is ijk
          c_data_ptr = get_Z_AT_W_element(CMptr, i, j, k)
          call c_f_pointer(c_data_ptr, f_data_ptr)
          f_data_ptr = ESMF_ptr_3D(i, k, j)
        end do
      end do
    end do

    write(msgString, *) "Z_AT_W: ESMF_ptr_3D(i=100, k=10, j=200) = ", ESMF_ptr_3D(100, 10, 200)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

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

    do i = ids, ide-1
      do k = kds, kde-1
        do j = jds, jde-1
          ! WRF is ikj, CM is ijk
          c_data_ptr = get_DRYMASS_element(CMptr, i, j, k)
          call c_f_pointer(c_data_ptr, f_data_ptr)
          f_data_ptr = ESMF_ptr_3D(i, k, j)
        end do
      end do
    end do

    write(msgString, *) "DRYMASS: ESMF_ptr_3D(i=100, k=10, j=200) = ", ESMF_ptr_3D(100, 10, 200)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

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

    do i = ids, ide-1
      do k = kds, kde-1
        do j = jds, jde-1
          ! WRF is ikj, CM is ijk
          c_data_ptr = get_T_POT_element(CMptr, i, j, k)
          call c_f_pointer(c_data_ptr, f_data_ptr)
          f_data_ptr = ESMF_ptr_3D(i, k, j)
        end do
      end do
    end do

    write(msgString, *) "T_POT: ESMF_ptr_3D(i=100, k=10, j=200) = ", ESMF_ptr_3D(100, 10, 200)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

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

    do i = ids, ide-1
      do k = kds, kde-1
        do j = jds, jde-1
          ! WRF is ikj, CM is ijk
          c_data_ptr = get_P_element(CMptr, i, j, k)
          call c_f_pointer(c_data_ptr, f_data_ptr)
          f_data_ptr = ESMF_ptr_3D(i, k, j)
        end do
      end do
    end do

    write(msgString, *) "P: ESMF_ptr_3D(i=100, k=10, j=200) = ", ESMF_ptr_3D(100, 10, 200)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

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

    do i = ids, ide-1
      do k = kds, kde-1
        do j = jds, jde-1
          ! WRF is ikj, CM is ijk
          c_data_ptr = get_U_element(CMptr, i, j, k)
          call c_f_pointer(c_data_ptr, f_data_ptr)
          f_data_ptr = ESMF_ptr_3D(i, k, j)
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

    do i = ids, ide-1
      do k = kds, kde-1
        do j = jds, jde-1
          ! WRF is ikj, CM is ijk
          c_data_ptr = get_V_element(CMptr, i, j, k)
          call c_f_pointer(c_data_ptr, f_data_ptr)
          f_data_ptr = ESMF_ptr_3D(i, k, j)
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

    do i = ids, ide-1
      do k = kds, kde-1
        do j = jds, jde-1
          ! WRF is ikj, CM is ijk
          c_data_ptr = get_W_element(CMptr, i, j, k)
          call c_f_pointer(c_data_ptr, f_data_ptr)
          f_data_ptr = ESMF_ptr_3D(i, k, j)
        end do
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

    do i = ids, ide-1
      do k = kds, kde-1
        do j = jds, jde-1
          ! WRF is ikj, CM is ijk
          c_data_ptr = get_QV_element(CMptr, i, j, k)
          call c_f_pointer(c_data_ptr, f_data_ptr)
          f_data_ptr = ESMF_ptr_3D(i, k, j)
        end do
      end do
    end do

    ! -----------------------------------------------

    ! Run the Contrail Manager
    call ContrailManager_run(CMptr, CM_startTime, CM_stopTime)

    ! -----------------------------------------------

    ! Exports

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

    do i = ids, ide-1
      do k = kds, kde-1
        do j = jds, jde-1
          ! WRF is ikj, CM is ijk
          c_data_ptr = get_deltaQV_element(CMptr, i, j, k)
          call c_f_pointer(c_data_ptr, f_data_ptr)
          f_data_ptr = ESMF_ptr_3D(i, k, j)
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

    do i = ids, ide-1
      do k = kds, kde-1
        do j = jds, jde-1
          ! WRF is ikj, CM is ijk
          c_data_ptr = get_deltaQI_element(CMptr, i, j, k)
          call c_f_pointer(c_data_ptr, f_data_ptr)
          f_data_ptr = ESMF_ptr_3D(i, k, j)
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

    do i = ids, ide-1
      do k = kds, kde-1
        do j = jds, jde-1
          ! WRF is ikj, CM is ijk
          c_data_ptr = get_deltaNI_element(CMptr, i, j, k)
          call c_f_pointer(c_data_ptr, f_data_ptr)
          f_data_ptr = ESMF_ptr_3D(i, k, j)
        end do
      end do
    end do

    ! -------------------- QIcon --------------------

    ! -------------------- NIcon --------------------

    ! -----------------------------------------------

  end subroutine Advance

  !-----------------------------------------------------------------------------

end module CM
