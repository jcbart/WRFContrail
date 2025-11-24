! ContrailManager interfaces

module CM_interface

    use, intrinsic :: iso_c_binding

    type, public, bind(C) :: CMTime_F
        integer(c_int) :: yy
        integer(c_int) :: mm
        integer(c_int) :: dd
        integer(c_int) :: h
        integer(c_int) :: m
        integer(c_int) :: s
    end type CMTime_F

    interface
        function create_ContrailManager() bind(C, name='create_ContrailManager')
            import :: c_ptr
            type(c_ptr) :: create_ContrailManager
        end function create_ContrailManager
    end interface

    interface
        subroutine ContrailManager_init(CMptr) bind(C, name='ContrailManager_init_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
        end subroutine ContrailManager_init
    end interface

    interface
        subroutine ContrailManager_setStartTime(CMptr, startTime) bind(C, name='ContrailManager_setStartTime_extern')
            import :: c_ptr, CMTime_F
            type(c_ptr), intent(in), value :: CMptr
            type(CMTime_F), intent(in), value :: startTime
        end subroutine ContrailManager_setStartTime
    end interface

    interface
        subroutine ContrailManager_run(CMptr, startTime, stopTime) bind(C, name='ContrailManager_run_extern')
            import :: c_ptr, CMTime_F
            type(c_ptr), intent(in), value :: CMptr
            type(CMTime_F), intent(in), value :: startTime, stopTime
        end subroutine ContrailManager_run
    end interface


    ! Variable inits

    interface
        subroutine init_XLAT(CMptr, ids, ide, jds, jde) bind(C, name='init_XLAT_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: ids, ide, jds, jde
        end subroutine init_XLAT
    end interface

    interface
        subroutine init_XLONG(CMptr, ids, ide, jds, jde) bind(C, name='init_XLONG_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: ids, ide, jds, jde
        end subroutine init_XLONG
    end interface

    interface
        subroutine init_Z(CMptr, ids, ide, jds, jde, kds, kde) bind(C, name='init_Z_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: ids, ide, jds, jde, kds, kde
        end subroutine init_Z
    end interface


    ! Variable getters

    interface
        function get_XLAT_element(CMptr, i, j) bind(C, name='get_XLAT_element_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: i, j
            type(c_ptr) :: get_XLAT_element
        end function get_XLAT_element
    end interface

    interface
        function get_XLONG_element(CMptr, i, j) bind(C, name='get_XLONG_element_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: i, j
            type(c_ptr) :: get_XLONG_element
        end function get_XLONG_element
    end interface

    interface
        function get_Z_element(CMptr, i, j, k) bind(C, name='get_Z_element_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: i, j, k
            type(c_ptr) :: get_Z_element
        end function get_Z_element
    end interface

end module CM_interface