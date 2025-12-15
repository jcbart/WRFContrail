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
        subroutine ContrailManager_run(CMptr, startTime, stopTime) &
            bind(C, name='ContrailManager_run_extern')
            
            import :: c_ptr, CMTime_F
            type(c_ptr), intent(in), value :: CMptr
            type(CMTime_F), intent(in), value :: startTime, stopTime
        end subroutine ContrailManager_run
    end interface

    ! Projection setup

    interface
        subroutine init_projection(CMptr, proj_code, lat1, lon1, knowni, knownj, dx, stdlon, &
            truelat1, truelat2) bind(C, name='init_projection_extern')

            import :: c_ptr, c_int, c_float
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: proj_code
            real(c_float), intent(in), value :: lat1
            real(c_float), intent(in), value :: lon1
            real(c_float), intent(in), value :: knowni
            real(c_float), intent(in), value :: knownj
            real(c_float), intent(in), value :: dx
            real(c_float), intent(in), value :: stdlon
            real(c_float), intent(in), value :: truelat1
            real(c_float), intent(in), value :: truelat2
        end subroutine init_projection
    end interface


    ! Variable inits

    interface
        subroutine init_CM_vars(CMptr, ids, ide, jds, jde, kds, kde) &
            bind(C, name='init_vars_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: ids, ide, jds, jde, kds, kde
        end subroutine init_CM_vars
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

    interface
        function get_Z_AT_W_element(CMptr, i, j, k) bind(C, name='get_Z_AT_W_element_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: i, j, k
            type(c_ptr) :: get_Z_AT_W_element
        end function get_Z_AT_W_element
    end interface

    interface
        function get_DRYMASS_element(CMptr, i, j, k) bind(C, name='get_DRYMASS_element_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: i, j, k
            type(c_ptr) :: get_DRYMASS_element
        end function get_DRYMASS_element
    end interface

    interface
        function get_T_POT_element(CMptr, i, j, k) bind(C, name='get_T_POT_element_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: i, j, k
            type(c_ptr) :: get_T_POT_element
        end function get_T_POT_element
    end interface

    interface
        function get_P_element(CMptr, i, j, k) bind(C, name='get_P_element_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: i, j, k
            type(c_ptr) :: get_P_element
        end function get_P_element
    end interface

    interface
        function get_U_element(CMptr, i, j, k) bind(C, name='get_U_element_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: i, j, k
            type(c_ptr) :: get_U_element
        end function get_U_element
    end interface

    interface
        function get_V_element(CMptr, i, j, k) bind(C, name='get_V_element_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: i, j, k
            type(c_ptr) :: get_V_element
        end function get_V_element
    end interface

    interface
        function get_W_element(CMptr, i, j, k) bind(C, name='get_W_element_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: i, j, k
            type(c_ptr) :: get_W_element
        end function get_W_element
    end interface

    interface
        function get_QV_element(CMptr, i, j, k) bind(C, name='get_QV_element_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: i, j, k
            type(c_ptr) :: get_QV_element
        end function get_QV_element
    end interface

    interface
        function get_deltaQV_element(CMptr, i, j, k) bind(C, name='get_deltaQV_element_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: i, j, k
            type(c_ptr) :: get_deltaQV_element
        end function get_deltaQV_element
    end interface

    interface
        function get_deltaQI_element(CMptr, i, j, k) bind(C, name='get_deltaQI_element_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: i, j, k
            type(c_ptr) :: get_deltaQI_element
        end function get_deltaQI_element
    end interface

    interface
        function get_deltaNI_element(CMptr, i, j, k) bind(C, name='get_deltaNI_element_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: i, j, k
            type(c_ptr) :: get_deltaNI_element
        end function get_deltaNI_element
    end interface

    interface
        function get_QIcontrail_element(CMptr, i, j, k) bind(C, name='get_QIcontrail_element_extern')
            import :: c_ptr, c_int
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: i, j, k
            type(c_ptr) :: get_QIcontrail_element
        end function get_QIcontrail_element
    end interface

end module CM_interface