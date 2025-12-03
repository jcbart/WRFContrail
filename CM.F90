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
  integer(c_int), save :: ids, ide, jds, jde, kds, kde = 0

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

    ! Data dependency flags
    logical, save :: vars_initialised = .false.
    logical, save :: XLAT_satisfied = .false.
    logical, save :: XLONG_satisfied = .false.
    logical, save :: Z_satisfied = .false.
    logical, save :: U_satisfied = .false.
    logical, save :: V_satisfied = .false.
    logical, save :: W_satisfied = .false.
    logical, save :: QV_satisfied = .false.

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
      call ESMF_LogWrite("Z dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)

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

      call ESMF_LogWrite("Initialising Contrail Manager variables with dimensions:", ESMF_LOGMSG_INFO, rc=rc)
      write(msgString, '(A,I4,A,I4,A,I4)') "ids = ", ids, ", jds = ", jds, ", kds = ", kds
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      write(msgString, '(A,I4,A,I4,A,I4)') "ide = ", ide-1, ", jde = ", jde-1, ", kde = ", kde-1
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      call ESMF_LogWrite("Note: Contrail Manager does not use a staggered grid.", ESMF_LOGMSG_INFO, rc=rc)

      call init_CM_vars(CMptr, ids, ide-1, jds, jde-1, kds, kde-1)

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
      
      vars_initialised = .true.
      Z_satisfied = .true.
    else
      call ESMF_LogWrite("Z dependency not yet satisfied", ESMF_LOGMSG_INFO, rc=rc)
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
      call ESMF_LogWrite("XLAT dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)

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
    else
      call ESMF_LogWrite("XLAT dependency not yet satisfied", ESMF_LOGMSG_INFO, rc=rc)
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
      call ESMF_LogWrite("XLONG dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)

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
    else
      call ESMF_LogWrite("XLONG dependency not yet satisfied", ESMF_LOGMSG_INFO, rc=rc)
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
      call ESMF_LogWrite("U dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)

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
      call ESMF_LogWrite("V dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)

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
      call ESMF_LogWrite("W dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)

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
      call ESMF_LogWrite("QV dependency satisfied", ESMF_LOGMSG_INFO, rc=rc)

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
    else
      call ESMF_LogWrite("QV dependency not yet satisfied", ESMF_LOGMSG_INFO, rc=rc)
    end if

    ! -----------------------------------------------

    ! Check if all dependencies satisfied; if so, mark complete

    if (Z_satisfied .and. &
        XLAT_satisfied .and. &
        XLONG_satisfied .and. &
        U_satisfied .and. &
        V_satisfied .and. &
        W_satisfied .and. &
        QV_satisfied) then
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

    ! -----------------------------------------------

  end subroutine Advance

  !-----------------------------------------------------------------------------

end module CM
