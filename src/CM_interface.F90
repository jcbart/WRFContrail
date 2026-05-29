! ContrailManager interfaces

module CM_interface

    use, intrinsic :: iso_c_binding

    implicit none

    ! Projection codes
    integer, parameter :: PROJ_LC = 1

    type, public, bind(C) :: CMTime_F
        integer(c_int) :: yy
        integer(c_int) :: mm
        integer(c_int) :: dd
        integer(c_int) :: h
        integer(c_int) :: m
        real(c_float) :: s
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

    ! Domain setup

    interface
        subroutine init_domainlc(CMptr, ids, ide, jds, jde, kds, kde, &
            lat1, lon1, knowni, knownj, dx, stdlon, truelat1, truelat2) &
            bind(C, name='init_domainlc_extern')

            import :: c_ptr, c_int, c_float
            type(c_ptr), intent(in), value :: CMptr
            integer(c_int), intent(in), value :: ids, ide, jds, jde, kds, kde
            real(c_float), intent(in), value :: lat1
            real(c_float), intent(in), value :: lon1
            real(c_float), intent(in), value :: knowni
            real(c_float), intent(in), value :: knownj
            real(c_float), intent(in), value :: dx
            real(c_float), intent(in), value :: stdlon
            real(c_float), intent(in), value :: truelat1
            real(c_float), intent(in), value :: truelat2
        end subroutine init_domainlc
    end interface


    ! Variable getters

    interface
        function get_XLAT(CMptr) bind(C, name='get_XLAT_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_XLAT
        end function get_XLAT
    end interface

    interface
        function get_XLONG(CMptr) bind(C, name='get_XLONG_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_XLONG
        end function get_XLONG
    end interface

    interface
        function get_Z(CMptr) bind(C, name='get_Z_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_Z
        end function get_Z
    end interface

    interface
        function get_Z_AT_W(CMptr) bind(C, name='get_Z_AT_W_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_Z_AT_W
        end function get_Z_AT_W
    end interface

    interface
        function get_DRYMASS(CMptr) bind(C, name='get_DRYMASS_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_DRYMASS
        end function get_DRYMASS
    end interface

    interface
        function get_T_POT(CMptr) bind(C, name='get_T_POT_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_T_POT
        end function get_T_POT
    end interface

    interface
        function get_deltaT_POT(CMptr) bind(C, name='get_deltaT_POT_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_deltaT_POT
        end function get_deltaT_POT
    end interface

    interface
        function get_T_POT_tend(CMptr) bind(C, name='get_T_POT_tend_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_T_POT_tend
        end function get_T_POT_tend
    end interface

    interface
        function get_P(CMptr) bind(C, name='get_P_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_P
        end function get_P
    end interface

    interface
        function get_U(CMptr) bind(C, name='get_U_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_U
        end function get_U
    end interface

    interface
        function get_V(CMptr) bind(C, name='get_V_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_V
        end function get_V
    end interface

    interface
        function get_W(CMptr) bind(C, name='get_W_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_W
        end function get_W
    end interface

    interface
        function get_TNSR(CMptr) bind(C, name='get_TNSR_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_TNSR
        end function get_TNSR
    end interface

    interface
        function get_OLR(CMptr) bind(C, name='get_OLR_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_OLR
        end function get_OLR
    end interface

    interface
        function get_QV(CMptr) bind(C, name='get_QV_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_QV
        end function get_QV
    end interface

    interface
        function get_deltaQV(CMptr) bind(C, name='get_deltaQV_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_deltaQV
        end function get_deltaQV
    end interface

    interface
        function get_QV_tend(CMptr) bind(C, name='get_QV_tend_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_QV_tend
        end function get_QV_tend
    end interface

    interface
        function get_QI(CMptr) bind(C, name='get_QI_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_QI
        end function get_QI
    end interface

    interface
        function get_deltaQI(CMptr) bind(C, name='get_deltaQI_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_deltaQI
        end function get_deltaQI
    end interface

    interface
        function get_QI_tend(CMptr) bind(C, name='get_QI_tend_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_QI_tend
        end function get_QI_tend
    end interface

    interface
        function get_deltaNI(CMptr) bind(C, name='get_deltaNI_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_deltaNI
        end function get_deltaNI
    end interface

    interface
        function get_NI_tend(CMptr) bind(C, name='get_NI_tend_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_NI_tend
        end function get_NI_tend
    end interface

    interface
        function get_QIcontrail(CMptr) bind(C, name='get_QIcontrail_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_QIcontrail
        end function get_QIcontrail
    end interface

    interface
        function get_REIcontrail(CMptr) bind(C, name='get_REIcontrail_extern')
            import :: c_ptr
            type(c_ptr), intent(in), value :: CMptr
            type(c_ptr) :: get_REIcontrail
        end function get_REIcontrail
    end interface

end module CM_interface