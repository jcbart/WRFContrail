cmake_minimum_required(VERSION 3.18)

if(NOT DEFINED ENV{ESMFMKFILE})
    message(FATAL_ERROR "Environment variable ESMFMKFILE is not set.")
endif()

if(NOT DEFINED ENV{ESMFLIB})
    message(FATAL_ERROR "Environment variable ESMFLIB is not set.")
endif()

set(ESMFLIB "$ENV{ESMFLIB}" CACHE FILEPATH "ESMF libary directory" FORCE)

if(NOT DEFINED WRF_DIR)
    if(DEFINED ENV{WRF_DIR})
        set(WRF_DIR "$ENV{WRF_DIR}" CACHE FILEPATH "WRF top directory" FORCE)
    else()
        message(FATAL_ERROR "WRF_DIR is not set. Pass -DWRF_DIR=/path/to/WRF or set WRF_DIR in environment.")
    endif()
endif()

if(NOT DEFINED CM_INSTALL)
    if(DEFINED ENV{CM_INSTALL})
        set(CM_INSTALL "$ENV{CM_INSTALL}" CACHE FILEPATH "Contrail Manager install directory" FORCE)
    else()
        message(FATAL_ERROR "CM_INSTALL is not set. Pass -DCM_INSTALL=/path/to/contrailmanager/install "
                "or set CM_INSTALL in environment.")
    endif()
endif()

set(CMAKE_PREFIX_PATH ${CM_INSTALL} ${CMAKE_PREFIX_PATH})
# Ideally remove below
set(contrailmanager_DIR ${CM_INSTALL}/lib64/cmake/contrailmanager/)

# Function to parse esmf.mk
function(get_esmf_var VAR_NAME OUTPUT_VAR)
    execute_process(
        COMMAND grep "^${VAR_NAME}=" $ENV{ESMFMKFILE}
        COMMAND cut -d= -f2-
        OUTPUT_VARIABLE RESULT
    )
    string(STRIP "${RESULT}" STRIPPED_RESULT)
    separate_arguments(STRIPPED_RESULT)
    set(${OUTPUT_VAR} "${STRIPPED_RESULT}" PARENT_SCOPE)
endfunction()

# Get the compiler and flags
get_esmf_var("ESMF_F90COMPILER" ESMF_F90COMPILER)
get_esmf_var("ESMF_F90COMPILEOPTS" ESMF_F90COMPILEOPTS)
get_esmf_var("ESMF_F90COMPILEPATHS" ESMF_F90COMPILEPATHS)
get_esmf_var("ESMF_F90COMPILEFREENOCPP" ESMF_F90COMPILEFREENOCPP)
get_esmf_var("ESMF_F90COMPILEFREECPP" ESMF_F90COMPILEFREECPP)
get_esmf_var("ESMF_F90COMPILECPPFLAGS" ESMF_F90COMPILECPPFLAGS)
get_esmf_var("ESMF_CXXCOMPILER" ESMF_CXXCOMPILER)
get_esmf_var("ESMF_CXXCOMPILEOPTS" ESMF_CXXCOMPILEOPTS)
get_esmf_var("ESMF_CXXCOMPILEPATHSLOCAL" ESMF_CXXCOMPILEPATHSLOCAL)
get_esmf_var("ESMF_CXXCOMPILEPATHS" ESMF_CXXCOMPILEPATHS)
get_esmf_var("ESMF_CXXCOMPILECPPFLAGS" ESMF_CXXCOMPILECPPFLAGS)
get_esmf_var("ESMF_F90LINKER" ESMF_F90LINKER)
get_esmf_var("ESMF_F90LINKOPTS" ESMF_F90LINKOPTS)
get_esmf_var("ESMF_F90LINKPATHS" ESMF_F90LINKPATHS)
get_esmf_var("ESMF_F90LINKRPATHS" ESMF_F90LINKRPATHS)
get_esmf_var("ESMF_F90ESMFLINKLIBS" ESMF_F90ESMFLINKLIBS)

set(CMAKE_Fortran_COMPILER "${ESMF_F90COMPILER}" CACHE FILEPATH "" FORCE)
set(CMAKE_CXX_COMPILER     "${ESMF_CXXCOMPILER}" CACHE FILEPATH "" FORCE)

project(WRFContrail LANGUAGES C CXX Fortran)

find_package(contrailmanager CONFIG REQUIRED)

# After ESMF 
include("${CMAKE_CURRENT_SOURCE_DIR}/wrflib.cmake")

set(SRC_DIR "${CMAKE_CURRENT_SOURCE_DIR}/src")

set(SOURCES
    "${SRC_DIR}/WRFContrail.F90"
    "${SRC_DIR}/driver.F90"
    "${SRC_DIR}/WRF.F90"
    "${SRC_DIR}/wrf_ESMFMod.F90"
    "${SRC_DIR}/CM.F90"
    "${SRC_DIR}/CM_interface.F90"
)

set(RUN_DIR "${CMAKE_CURRENT_BINARY_DIR}/run")

add_executable(WRFContrail ${SOURCES} ${WRF_OBJECT_FILES})

set_target_properties(WRFContrail PROPERTIES
    LINKER_LANGUAGE Fortran
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/run"
)

target_compile_options(WRFContrail PRIVATE
    ${ESMF_F90COMPILEOPTS}
    ${ESMF_F90COMPILEPATHS}
    ${WRFIOFLAGS}
)

target_include_directories(WRFContrail PRIVATE
    ${WRF_INC}
)

target_link_libraries(WRFContrail PRIVATE
    ${WRF_LIBRARIES}
    ${ESMF_F90ESMFLINKLIBS}
    contrailmanager::contrailmanager
)

target_link_options(WRFContrail PRIVATE
    ${ESMF_F90LINKOPTS}
    ${ESMF_F90LINKPATHS}
    ${ESMF_F90LINKRPATHS}
)

# Copy default input files if they do not already exist in build directory
set(WRFCONTRAIL_INPUT_FILES "WRFContrail.config")

foreach(FILE_NAME ${WRFCONTRAIL_INPUT_FILES})
    if(NOT EXISTS "${CMAKE_BINARY_DIR}/run/${FILE_NAME}")
        configure_file(
            "${CMAKE_CURRENT_SOURCE_DIR}/inputs/${FILE_NAME}.default"
            "${CMAKE_BINARY_DIR}/run/${FILE_NAME}"
            COPYONLY
        )
    endif()
endforeach()

# Copy Contrail Manager input files from install
file(GLOB CM_INPUT_FILES "${CM_INSTALL}/share/contrailmanager/*")

foreach(file ${CM_INPUT_FILES})
    get_filename_component(FILE_NAME ${file} NAME)
    if(NOT EXISTS "${CMAKE_BINARY_DIR}/run/${FILE_NAME}")
        file(COPY ${file} DESTINATION "${CMAKE_BINARY_DIR}/run")
    endif()
endforeach()