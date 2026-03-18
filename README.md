# WRFContrail

**WRFContrail** is a program for simulating contrail plumes using an online coupling with WRF using [ESMF NUOPC](https://earthsystemmodeling.org/nuopc/).

## Requirements

- [WRF](https://www.mmm.ucar.edu/models/wrf)
- [ContrailManager](https://github.com/jcbart/ContrailManager)
- [ESMF NUOPC](https://earthsystemmodeling.org/nuopc/)

See each for their requirements.

Additionally:
- C++ compiler supporting C++20
- CMake
- Git

## Installation

With ESMF installed on your system, there are three steps to creating the coupled model:

1. [Compile WRF](#compiling-wrf)
2. [Installing ContrailManager](#installing-contrailmanager)
3. [Compile WRFContrail](#compiling-wrfcontrail)

### Compiling WRF

If you are not familiar with WRF, use the [official tutorial](https://www2.mmm.ucar.edu/wrf/OnLineTutorial/index.php) to supplement the following.

Clone the ESMF branch of this [WRF GitHub fork](https://github.com/jcbart/WRF/tree/ESMF).

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

Compile WRF with `./compile`. **WRFContrail** uses the libraries produced by compiling WRF to integrate WRF into the coupled model.


### Installing ContrailManager

See the [ContrailManager GitHub repo](https://github.com/jcbart/ContrailManager).

### Compiling WRFContrail

Two paths - `WRF_DIR` and `CM_INSTALL` - must either be set as variables in your environment or passed to the `cmake` command below (e.g. `-DCM_INSTALL=...`). These paths should be
- `WRF_DIR`: top level WRF directory; and
- `CM_INSTALL`: Contrail Manager install directory (containing `lib` and `share` directories etc).

The environment variable `ESMFMKFILE` must also be set to the `esmf.mk` file associated with your ESMF installation if it is not already.

To configure **WRFContrail** in a directory named `build`, run the followings commands from the top directory:
```bash
mkdir build
cd build
cmake ..
```

CMake will attempt to read variables from the `configure.wrf` file in your WRF directory. If this does not work, set the variables manually in the file `wrflib.cmake`.

If configuration is successful, run
```bash
cmake --build .
```
to compile. This will produce the executable `WRFContrail` in a directory named `run`.

## Running the coupled model

If you have not run previously, copy or link the contents of `WRF/run` (except the executables) to `run`. Ensure that CMake has automatically copied the Contrail Manager's input files.

Use WPS and `real.exe` to prepare input and boundary data for WRF. Link `wrfbdy*` and `wrfinput*` files to `build`.

Set runtime configuration variables for the coupled model in `WRFContrail.config`.

Run the coupled model with `mpirun -np X ./WRFContrail`, specifying the number of MPI ranks to use with `X`. WRFContrail will assign the 0th MPI rank to the Contrail Manager and the remainder to WRF.

## Acknowledgements

We are very grateful to ESMF for creating the coupling framework and the [nuopc-app-prototypes](https://github.com/esmf-org/nuopc-app-prototypes/) which were used to construct the coupled model.

The file `wrf_ESMFMod.F90` is a modified version of `main/wrf_ESMFMod.F` in WRF. Attribution for the code in this file should be directed towards the authors of WRF.

The [SKRIPS](https://github.com/iurnus/scripps_kaust_model) model was invaluable in determining how to compile WRF for ESMF coupling. We are very grateful to the authors.