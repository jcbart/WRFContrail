# WRF-Contrail-Coupler

The **WRF-Contrail-Coupler** is a program for simulating contrail plumes using an online coupling with WRF using [ESMF NUOPC](https://earthsystemmodeling.org/nuopc/).

## Requirements

- [WRF](https://www.mmm.ucar.edu/models/wrf)
- [ContrailManager](https://github.com/jackbartlett20/ContrailManager)
- [ESMF NUOPC](https://earthsystemmodeling.org/nuopc/)

See each for their requirements.

## Installation

With ESMF installed on your system, there are three steps to creating the coupled model:

1. [Compile WRF](#compiling-wrf)
2. [Compile ContrailManager](#compiling-contrailmanager)
3. [Compile the coupled model](#compiling-the-coupled-model)

### Compiling WRF

If you are not familiar with WRF, use the [official tutorial](https://www2.mmm.ucar.edu/wrf/OnLineTutorial/index.php) to supplement the following.

Clone the ESMF branch of WRF from my [GitHub](https://github.com/jackbartlett20/WRF/tree/ESMF).

Your ESMF installation should have all of the library requirements for WRF already. Set `NETCDF` and `HDF5` in your environment and add `$(NETCDF)/bin` to your `PATH`.

Set the following paths in your environment:

- `ESMFLIB=/path/to/ESMF/lib`
- `ESMFINC=/path/to/ESMF/include`
- `ESMFMOD=/path/to/ESMF/mod`

Run `./configure` in WRF's top directory. You should see an additional "Configuring to use ESMF library to build WRF..." within the typical spiel as WRF recognises the environment paths. Configure for gcc. I imagine nesting will not work, so configure for basic nesting.

Make the following changes to `configure.wrf`:

1. Prepend the `ESMF_MOD_INC` line with `-I$(ESMFMOD)`.
2. Append the `LIB_EXTERNAL` line with `-Wl,--start-group $(ESMFLIB)/libesmf.a -Wl,--end-group`.
3. Append the `ESMF_IO_LIB` line with `-lmpi`.
4. Append the `LIB` line with `-L$(ESMFLIB) -lesmf`.

Most of these changes are the same as for the [SKRIPS](https://skrips.readthedocs.io/en/v2.0.1/wrf/install_WRF.html) model. You may need to adjust other configuration flags if WRF does not successfully compile. Pay close attention to `log.compile` and you should be able to work it out!

Compile WRF with `./compile`. **WRF-Contrail-Coupler** uses the libraries produced by compiling WRF to integrate WRF into the coupled model.


### Compiling ContrailManager



### Compiling the coupled model

Set `WRF_DIR` and `CM_DIR` in your environment to the top-level directory of each model.

In the `WRF-Contrail-Coupler` top-level directory, make a copy of `wrflib-bare.mk` named `wrflib.mk`.

Copy the contents of the lines `LIB_EXTERNAL` and `NETCDF4_DEP_LIB` in your `configure.wrf` file to the correct location in `wrflib.mk`.

Compile with `make`. The executable `CoupledModel` should be produced.

## Running the coupled model

If you have not run previously, copy or link the contents of `WRF/run` (except the executables) to `build`.

Use WPS and `real.exe` to prepare input and boundary data for WRF. Link `wrfbdy*` and `wrfinput*` files to `build`.

Set runtime configuration variables for the coupled model in `CoupledModel.config`. The `petList` variables tell the coupled model which Persistent Execution Threads (PETs; effectively processes) each model should use.

Run the coupled model with `mpirun -np X ./CoupledModel`, specifying the number of MPI processes to use with `X`. Ensure the `petList` settings in `CoupledModel.config` are consistent with the available processes.

## Acknowledgements

We are very grateful to ESMF for creating the coupling framework and the [nuopc-app-prototypes](https://github.com/esmf-org/nuopc-app-prototypes/) which were used to construct the coupled model.

The file `wrf_ESMFMod.F90` is a modified version of `main/wrf_ESMFMod.F` in WRF. Attribution for the code in this file should be directed towards the authors of WRF.

The [SKRIPS](https://github.com/iurnus/scripps_kaust_model) model was invaluable in determining how to compile WRF for ESMF coupling. We are very grateful to the authors.