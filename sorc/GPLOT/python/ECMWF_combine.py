#!/usr/bin/env python
"""
ECMWF_combine.py

Combines individual GRIB2 input files from ECMWF into a single NetCDF
input file for each respective forecast cycle and lead time.

Usage:
    python ECMWF_combine.py <CYCLE>

    CYCLE  -- forecast cycle string, e.g. 2019062600

Original NCL: sorc/GPLOT/ncl/ECMWF_combine.ncl
"""

import datetime
import glob
import os
import shutil
import subprocess
import sys


def main():
    print(f"MSG: ECMWF_combine.py started at {datetime.datetime.now()}")
    print("")

    # Read CYCLE as a command line argument
    if len(sys.argv) < 2:
        print("ERROR: CYCLE is undefined. Please provide it as a command line argument.")
        sys.exit(1)
    CYCLE = sys.argv[1]

    # Get important directories from the environment
    ODIR = os.getcwd()
    TMPDIR = os.path.join(ODIR, "TMP." + datetime.datetime.now().strftime("%f"))
    os.makedirs(TMPDIR, exist_ok=True)

    # Setup alternate CYCLE variable (append seconds)
    YMDHms = CYCLE + "0000"

    # Setup forecast hours for ECMWF data (hard-coded: 0-240h at 24h intervals)
    FHR = list(range(0, 241, 24))
    FHR_STR = ["an", "24h", "48h", "72h", "96h", "120h",
               "144h", "168h", "192h", "216h", "240h"]
    NFHR = len(FHR)

    # Variable name pairs: (short_name_in_filename, NCL-style_varname_in_output)
    VAR_STR = ["msl", "t_850hPa", "u_850hPa", "v_850hPa", "gh_500hPa"]
    VAR     = ["PRES_P0_L101_GLL0", "TMP_P0_L100_GLL0",
               "UGRD_P0_L100_GLL0", "VGRD_P0_L100_GLL0", "HGT_P0_L100_GLL0"]
    NVAR = len(VAR_STR)

    # Import I/O libraries (checked once outside the loop for clarity)
    try:
        import cfgrib
        import xarray as xr
    except ImportError:
        print("ERROR: cfgrib and xarray are required for GRIB2 reading.")
        shutil.rmtree(TMPDIR, ignore_errors=True)
        sys.exit(1)

    try:
        import netCDF4 as nc4
        import numpy as np
    except ImportError:
        print("ERROR: netCDF4 and numpy are required.")
        shutil.rmtree(TMPDIR, ignore_errors=True)
        sys.exit(1)

    # Loop over all forecast lead times
    for fff in range(NFHR):
        print(f"MSG: Working on this lead time --> {FHR_STR[fff]}")

        OFILE = None
        OFILE2 = None
        fout = None
        lat_0 = None
        lon_0 = None

        for vvv in range(NVAR):
            # Define the input file and check if it exists
            pattern = (f"{ODIR}/{CYCLE}/*C_ECMF_{YMDHms}_{FHR_STR[fff]}_"
                       f"{VAR_STR[vvv]}_global_0p5deg.grib2")
            matches = glob.glob(pattern)
            if not matches:
                continue
            IFILE = matches[0]

            # Create output NetCDF file on first variable found for this lead time
            if OFILE is None:
                OFILE  = (f"{TMPDIR}/ecmwf.{CYCLE}.global.0p5."
                          f"f{FHR[fff]:03d}.nc")
                OFILE2 = (f"{ODIR}/{CYCLE}/ecmwf.{CYCLE}.global.0p5."
                          f"f{FHR[fff]:03d}.nc")

                if os.path.exists(OFILE):
                    os.remove(OFILE)

                fout = nc4.Dataset(OFILE, "w")

            # Read the GRIB2 file
            try:
                ds = cfgrib.open_dataset(IFILE)
            except Exception as exc:
                print(f"WARNING: Could not read {IFILE}: {exc}")
                continue

            # Write lat/lon dimensions and coordinate variables once
            if lat_0 is None:
                lat_key = "latitude"  if "latitude"  in ds.coords else None
                lon_key = "longitude" if "longitude" in ds.coords else None
                if lat_key is None or lon_key is None:
                    print("WARNING: Could not locate lat/lon coordinates – skipping.")
                    ds.close()
                    continue

                lat_0 = ds.coords[lat_key].values.astype("float32")
                lon_0 = ds.coords[lon_key].values.astype("float32")
                nlat  = lat_0.size
                nlon  = lon_0.size

                fout.createDimension("lat_0", nlat)
                fout.createDimension("lon_0", nlon)

                lat_var             = fout.createVariable("lat_0", "f4", ("lat_0",))
                lat_var.units       = getattr(ds.coords[lat_key], "units", "degrees_north")
                lat_var.long_name   = "latitude"
                lat_var[:]          = lat_0

                lon_var             = fout.createVariable("lon_0", "f4", ("lon_0",))
                lon_var.units       = getattr(ds.coords[lon_key], "units", "degrees_east")
                lon_var.long_name   = "longitude"
                lon_var[:]          = lon_0

            # Read the main data variable (first data_var in the xarray dataset)
            data_vars = list(ds.data_vars)
            if not data_vars:
                ds.close()
                continue
            THIS_VAR = ds[data_vars[0]].values.astype("float32")

            # Define and write the variable using the NCL-style output name
            out_var = fout.createVariable(VAR[vvv], "f4", ("lat_0", "lon_0"),
                                          fill_value=9.96921e+36)
            # Copy attributes from the xarray variable
            for attr_name, attr_val in ds[data_vars[0]].attrs.items():
                try:
                    setattr(out_var, attr_name, attr_val)
                except Exception:
                    pass
            out_var[:, :] = THIS_VAR

            ds.close()

            # Remove the original linked GRIB2 file
            os.remove(IFILE)

        # Close the output NetCDF file
        if fout is not None:
            fout.close()

        # Move the temp file to the final destination (replace only if changed)
        if OFILE is not None and os.path.exists(OFILE):
            if os.path.exists(OFILE2):
                result = subprocess.run(["diff", OFILE, OFILE2],
                                        capture_output=True)
                if result.returncode != 0:   # files differ
                    shutil.move(OFILE, OFILE2)
                else:
                    os.remove(OFILE)
            else:
                shutil.move(OFILE, OFILE2)

    # Remove the temporary scratch directory
    shutil.rmtree(TMPDIR, ignore_errors=True)

    print("")
    print(f"MSG: ECMWF_combine.py completed at {datetime.datetime.now()}")


if __name__ == "__main__":
    main()
