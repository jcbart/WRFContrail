set(WRF_SRC_ROOT_DIR "${WRF_DIR}")

if(NOT EXISTS "${WRF_DIR}/configure.wrf")
    message(FATAL_ERROR "File not found: ${WRF_DIR}/configure.wrf")
endif()

# Function to parse configure.wrf
function(get_configure_wrf_var LINE_LIST VAR_NAME OUTPUT_VAR)
    foreach(line IN LISTS ${LINE_LIST})
        if(line MATCHES "^[ \t]*${VAR_NAME}[ \t]*=[ \t]*(.*)")
            set(extracted_value "${CMAKE_MATCH_1}")
            # Remove extra internal spacing and leading/trailing whitespace
            string(STRIP "${extracted_value}" extracted_value)
            string(REGEX REPLACE "[ \t]+" " " extracted_value "${extracted_value}")
            # Replace $(...) with ${...}
            string(REGEX REPLACE "\\\$\\(([^)]+)\\)" "\${\\1}" extracted_value "${extracted_value}")
            string(CONFIGURE "${extracted_value}" extracted_value)
            separate_arguments(extracted_value)
            # Remove elements ending in .o
            #list(FILTER extracted_value EXCLUDE REGEX ".*\\.o$")
            set(${OUTPUT_VAR} "${extracted_value}" PARENT_SCOPE)
            return()
        endif()
    endforeach()
    message(FATAL_ERROR "Variable '${VAR_NAME}' not found in ${WRF_DIR}/configure.wrf")
endfunction()

file(READ "${WRF_DIR}/configure.wrf" FILE_CONTENTS)
# Replace any \ followed by newline with just a space
string(REGEX REPLACE "\\\\[ \t]*\n" " " collapsed_contents "${FILE_CONTENTS}")
# Turn into a CMake list of lines
string(REPLACE "\n" ";" CONFIGURE_WRF_LINE_LIST "${collapsed_contents}")

# Extract variables
get_configure_wrf_var(CONFIGURE_WRF_LINE_LIST "ESMF_IO_LIB" ESMF_IO_LIB)
get_configure_wrf_var(CONFIGURE_WRF_LINE_LIST "LIB_BUNDLED" LIB_BUNDLED)
get_configure_wrf_var(CONFIGURE_WRF_LINE_LIST "LIB_EXTERNAL" LIB_EXTERNAL)
get_configure_wrf_var(CONFIGURE_WRF_LINE_LIST "NETCDF4_DEP_LIB" NETCDF4_DEP_LIB)

set(LIB ${LIB_BUNDLED} ${LIB_EXTERNAL}  ${NETCDF4_DEP_LIB} -L${ESMFLIB} -lesmf)

set(WRF_INC
    "${WRF_DIR}/dyn_em"
    "${WRF_DIR}/main"
    "${WRF_DIR}/external/io_esmf"
    "${WRF_DIR}/external/io_netcdf"
    "${WRF_DIR}/external/io_int"
    "${WRF_DIR}/frame"
    "${WRF_DIR}/share"
    "${WRF_DIR}/phys"
    "${WRF_DIR}/chem"
    "${WRF_DIR}/inc"
    "${WRF_DIR}/wrftladj"
)

set(WRF_OBJECT_FILES "")
set(WRF_LIBRARIES "")

list(APPEND WRF_OBJECT_FILES "${WRF_DIR}/main/module_wrf_top.o")
list(APPEND WRF_LIBRARIES "${WRF_DIR}/main/libwrflib.a")

foreach(item ${LIB})
    if(item MATCHES "\\.o$")
        list(APPEND WRF_OBJECT_FILES ${item})
    else()
        list(APPEND WRF_LIBRARIES ${item})
    endif()
endforeach()

set(WRFIOFLAGS
    -fconvert=big-endian
    -frecord-marker=4
)