# Set WRF include and library paths
# Mostly copied from SKRIPS

# Fill and amend as required

WRF_SRC_ROOT_DIR=${WRF_DIR}

ESMF_IO_LIB     =    $(ESMF_F90LINKPATHS) $(ESMF_F90ESMFLINKLIBS) -L$(WRF_SRC_ROOT_DIR)/external/io_esmf -lwrfio_esmf -lmpi

LIB_BUNDLED     = \
					$(WRF_SRC_ROOT_DIR)/external/fftpack/fftpack5/libfftpack.a \
					$(WRF_SRC_ROOT_DIR)/external/io_grib1/libio_grib1.a \
					$(WRF_SRC_ROOT_DIR)/external/io_grib_share/libio_grib_share.a \
					$(WRF_SRC_ROOT_DIR)/external/io_int/libwrfio_int.a \
					$(ESMF_IO_LIB) \
					$(WRF_SRC_ROOT_DIR)/external/RSL_LITE/librsl_lite.a \
					$(WRF_SRC_ROOT_DIR)/frame/module_internal_header_util.o \
					$(WRF_SRC_ROOT_DIR)/frame/pack_utils.o

LIB_EXTERNAL    = # Fill

NETCDF4_DEP_LIB = # Fill

LIB             = $(LIB_BUNDLED) $(LIB_EXTERNAL)  $(NETCDF4_DEP_LIB) -L$(ESMFLIB) -lesmf

WRF_INC = \
   -I${WRF_DIR}/dyn_em \
   -I${WRF_DIR}/main \
   -I${WRF_DIR}/external/io_esmf \
   -I${WRF_DIR}/external/io_netcdf \
   -I${WRF_DIR}/external/io_int \
   -I${WRF_DIR}/frame \
   -I${WRF_DIR}/share \
   -I${WRF_DIR}/phys \
   -I${WRF_DIR}/chem \
   -I${WRF_DIR}/inc \
   -I${WRF_DIR}/wrftladj

WRF_LIB = \
	-Wl,--start-group \
	${WRF_DIR}/main/module_wrf_top.o \
  	${WRF_DIR}/main/libwrflib.a \
	$(LIB) \
	-Wl,--end-group