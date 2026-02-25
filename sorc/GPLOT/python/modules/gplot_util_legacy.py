#!/usr/bin/env python
"""
gplot_util_legacy.py

Legacy utility functions for GPLOT. These functions have been superseded
by newer implementations in gplot_util.py. Do not import this module
unless you have a specific reason to use a legacy function.

Original NCL: sorc/GPLOT/ncl/GPLOT_util_legacy.ncl
Moved to legacy: December 18, 2019
"""

import warnings

warnings.warn(
    "gplot_util_legacy contains deprecated functions. "
    "Use gplot_util.find_var_name instead.",
    DeprecationWarning,
    stacklevel=2,
)

# ---------------------------------------------------------------------------
# Master list: generic variable name → category number
# ---------------------------------------------------------------------------
_MASTER_LIST = {
    "T":     "101",   # 3D Air Temperature
    "HGT":   "102",   # 3D Geopotential Height
    "U":     "103",   # 3D Zonal Wind
    "V":     "104",   # 3D Meridional Wind
    "UV":    "105",   # 3D Total Wind
    "TH":    "106",   # 3D Potential Temperature
    "RH":    "107",   # 3D Relative Humidity
    "SPH":   "108",   # 3D Specific Humidity
    "W":     "109",   # 3D Vertical Velocity
    "AVO":   "110",   # 3D Vorticity (absolute)
    "RVO":   "111",   # 3D Vorticity (relative)
    "DPT":   "112",   # 3D Dewpoint Temperature
    "U10":   "201",   # 10-m Zonal Wind
    "V10":   "202",   # 10-m Meridional Wind
    "UV10":  "203",   # 10-m Total Wind
    "T2":    "301",   # 2-m Air Temperature
    "DPT2":  "302",   # 2-m Dew Point Temperature
    "SPH2":  "303",   # 2-m Specific Humidity
    "RH2":   "304",   # 2-m Relative Humidity
    "MSLP":  "401",   # Mean Sea Level Pressure
    "LHFLX": "402",   # Surface Latent Heat Flux
    "SHFLX": "403",   # Surface Sensible Heat Flux
    "TSFC":  "404",   # Surface Temperature
    "SST":   "405",   # Sea Surface Temperature
    "PSFC":  "406",   # Surface Pressure
    "TTROP": "501",   # Tropopause Temperature
    "PRCP":  "601",   # Bulk Precipitation Rate
    "PRATE": "602",   # Instantaneous Precipitation Rate
    "TPRCP": "603",   # Total Precipitation
    "TPW":   "604",   # Total Precipitable Water
    "REFL":  "605",   # Composite Reflectivity
    "HLCY":  "606",   # Storm-Relative Helicity
    "CAPE":  "607",   # Convective Available Potential Energy
    "REFD":  "608",   # Reflectivity (200-100000 Pa)
    "LAND": "1001",   # Land-Sea Mask
}

# ---------------------------------------------------------------------------
# Per-model variable tables: category number → model-specific variable name
# Keys are model identifiers matched via equality or substring (see function).
# ---------------------------------------------------------------------------
_VAR_TABLES = {
    "GFS": {
        "101":  "TMP_P0_L100_GLL0",
        "102":  "HGT_P0_L100_GLL0",
        "103":  "UGRD_P0_L100_GLL0",
        "104":  "VGRD_P0_L100_GLL0",
        "105":  "UGRD_P0_L100_GLL0",
        "107":  "RH_P0_L100_GLL0",
        "108":  "SPFH_P0_L100_GLL0",
        "109":  "VVEL_P0_L100_GLL0",
        "110":  "ABSV_P0_L100_GLL0",
        "111":  "ABSV_P0_L100_GLL0",
        "112":  "DPT_P0_L100_GLL0",
        "201":  "UGRD_P0_L103_GLL0",
        "202":  "VGRD_P0_L103_GLL0",
        "203":  "UGRD_P0_L103_GLL0",
        "301":  "TMP_P0_L103_GLL0",
        "302":  "DPT_P0_L103_GLL0",
        "303":  "SPFH_P0_L103_GLL0",
        "304":  "RH_P0_L103_GLL0",
        "401":  "MSLET_P0_L101_GLL0",
        "402":  "LHTFL_P0_L1_GLL0",
        "403":  "SHTFL_P0_L1_GLL0",
        "404":  "TMP_P0_L1_GLL0",
        "405":  "WTMP_P0_L1_GLL0",
        "406":  "PRES_P0_L1_GLL0",
        "501":  "TMP_P0_L7_GLL0",
        "601":  "APCP_P8_L1_GLL0_acc",
        "602":  "PRATE_P0_L1_GLL0",
        "603":  "APCP_P8_L1_GLL0_acc",
        "604":  "PWAT_P0_L200_GLL0",
        "605":  "REFC_P0_L200_GLL0",
        "606":  "HLCY_P0_2L103_GLL0",
        "607":  "CAPE_P0_L1_GLL0",
        "1001": "LAND_P0_L1_GLL0",
    },
    "HWRF": {
        "101":  "TMP_P0_L100_GLL0",
        "102":  "HGT_P0_L100_GLL0",
        "103":  "UGRD_P0_L100_GLL0",
        "104":  "VGRD_P0_L100_GLL0",
        "105":  "UGRD_P0_L100_GLL0",
        "107":  "RH_P0_L100_GLL0",
        "108":  "SPFH_P0_L100_GLL0",
        "109":  "VVEL_P0_L100_GLL0",
        "110":  "ABSV_P0_L100_GLL0",
        "111":  "ABSV_P0_L100_GLL0",
        "112":  "DPT_P0_L100_GLL0",
        "201":  "UGRD_P0_L103_GLL0",
        "202":  "VGRD_P0_L103_GLL0",
        "203":  "UGRD_P0_L103_GLL0",
        "301":  "TMP_P0_L103_GLL0",
        "302":  "DPT_P0_L103_GLL0",
        "303":  "SPFH_P0_L103_GLL0",
        "304":  "RH_P0_L103_GLL0",
        "401":  "PRMSL_P0_L101_GLL0",
        "402":  "LHTFL_P0_L1_GLL0",
        "403":  "SHTFL_P0_L1_GLL0",
        "404":  "TMP_P0_L1_GLL0",
        "405":  "WTMP_P0_L1_GLL0",
        "406":  "PRES_P0_L1_GLL0",
        "501":  "TMP_P0_L7_GLL0",
        "601":  "APCP_P8_L1_GLL0_acc",
        "602":  "PRATE_P0_L1_GLL0",
        "603":  "APCP_P8_L1_GLL0_acc",
        "604":  "PWAT_P0_L200_GLL0",
        "605":  "REFC_P0_L200_GLL0",
        "606":  "HLCY_P0_2L103_GLL0",
        "607":  "CAPE_P0_L1_GLL0",
        "608":  "REFD_P0_L100_GLL0",
        "1001": "LAND_P0_L1_GLL0",
    },
    "HAFS": {
        "101":  "TMP_P0_L100_GLL0",
        "102":  "HGT_P0_L100_GLL0",
        "103":  "UGRD_P0_L100_GLL0",
        "104":  "VGRD_P0_L100_GLL0",
        "105":  "UGRD_P0_L100_GLL0",
        "107":  "RH_P0_L100_GLL0",
        "108":  "SPFH_P0_L100_GLL0",
        "109":  "VVEL_P0_L100_GLL0",
        "110":  "ABSV_P0_L100_GLL0",
        "111":  "ABSV_P0_L100_GLL0",
        "112":  "DPT_P0_L100_GLL0",
        "201":  "UGRD_P0_L103_GLL0",
        "202":  "VGRD_P0_L103_GLL0",
        "203":  "UGRD_P0_L103_GLL0",
        "301":  "TMP_P0_L103_GLL0",
        "302":  "DPT_P0_L103_GLL0",
        "303":  "SPFH_P0_L103_GLL0",
        "304":  "RH_P0_L103_GLL0",
        "401":  "MSLET_P0_L101_GLL0",
        "402":  "LHTFL_P0_L1_GLL0",
        "403":  "SHTFL_P0_L1_GLL0",
        "404":  "TMP_P0_L1_GLL0",
        "405":  "WTMP_P0_L1_GLL0",
        "406":  "PRES_P0_L1_GLL0",
        "501":  "TMP_P0_L7_GLL0",
        "601":  "APCP_P8_L1_GLL0_acc",
        "602":  "PRATE_P0_L1_GLL0",
        "603":  "APCP_P8_L1_GLL0_acc",
        "604":  "PWAT_P0_L200_GLL0",
        "605":  "REFC_P0_L200_GLL0",
        "606":  "HLCY_P0_2L103_GLL0",
        "607":  "CAPE_P0_L1_GLL0",
        "608":  "REFD_P0_L100_GLL0",
        "1001": "LAND_P0_L1_GLL0",
    },
    "ERAi": {
        "101": "TMP_P0_L100_GLL0",
        "102": "Z_GDS0_ISBL",
        "103": "UGRD_P0_L100_GLL0",
        "104": "VGRD_P0_L100_GLL0",
        "105": "UGRD_P0_L100_GLL0",
        "107": "RH_P0_L100_GLL0",
        "109": "VVEL_P0_L100_GLL0",
        "110": "ABSV_P0_L100_GLL0",
        "111": "ABSV_P0_L100_GLL0",
    },
    "H3HW": {
        "101": "TMP_GDS0_ISBL",
        "102": "HGT_GDS0_ISBL",
        "103": "U_GRD_GDS0_ISBL",
        "104": "V_GRD_GDS0_ISBL",
        "105": "U_GRD_GDS0_ISBL",
        "107": "R_H_GDS0_ISBL",
        "108": "SPF_H_GDS0_ISBL",
        "109": "V_VEL_GDS0_ISBL",
        "110": "ABS_V_GDS0_ISBL",
        "111": "ABS_V_GDS0_ISBL",
        "604": "P_WAT_GDS0_EATM",
        "605": "REFC_GDS0_EATM",
    },
    "HEDAS": {  # same table as H3HW
        "101": "TMP_GDS0_ISBL",
        "102": "HGT_GDS0_ISBL",
        "103": "U_GRD_GDS0_ISBL",
        "104": "V_GRD_GDS0_ISBL",
        "105": "U_GRD_GDS0_ISBL",
        "107": "R_H_GDS0_ISBL",
        "108": "SPF_H_GDS0_ISBL",
        "109": "V_VEL_GDS0_ISBL",
        "110": "ABS_V_GDS0_ISBL",
        "111": "ABS_V_GDS0_ISBL",
        "604": "P_WAT_GDS0_EATM",
        "605": "REFC_GDS0_EATM",
    },
    "GEFS": {
        "101":  "TMP_P1_L100_GLL0",
        "102":  "HGT_P1_L100_GLL0",
        "103":  "UGRD_P1_L100_GLL0",
        "104":  "VGRD_P1_L100_GLL0",
        "105":  "UGRD_P1_L100_GLL0",
        "107":  "RH_P1_L100_GLL0",
        "108":  "SPFH_P0_L100_GLL0",
        "110":  "ABSV_P0_L100_GLL0",
        "111":  "ABSV_P0_L100_GLL0",
        "112":  "DPT_P0_L100_GLL0",
        "201":  "UGRD_P1_L103_GLL0",
        "202":  "VGRD_P1_L103_GLL0",
        "203":  "UGRD_P0_L103_GLL0",
        "301":  "TMP_P1_L103_GLL0",
        "302":  "DPT_P0_L103_GLL0",
        "303":  "SPFH_P0_L103_GLL0",
        "304":  "RH_P1_L103_GLL0",
        "401":  "PRMSL_P1_L101_GLL0",
        "402":  "LHTFL_P11_L1_GLL0_avg",
        "403":  "SHTFL_P11_L1_GLL0_avg",
        "405":  "WTMP_P0_L1_GLL0",
        "406":  "PRES_P1_L1_GLL0",
        "407":  "HGT_P1_L1_GLL0",
        "501":  "TMP_P0_L7_GLL0",
        "502":  "ULWRF_P11_L8_GLL0_avg",
        "601":  "APCP_P8_L1_GLL0_acc",
        "602":  "PRATE_P0_L1_GLL0",
        "603":  "APCP_P8_L1_GLL0_acc",
        "604":  "PWAT_P1_L200_GLL0",
        "605":  "REFC_P0_L200_GLL0",
        "606":  "HLCY_P0_2L103_GLL0",
        "607":  "CAPE_P1_2L108_GLL0",
        "609":  "CIN_P1_2L108_GLL0",
        "701":  "VVEL_P1_L100_GLL0",
        "1001": "LAND_P0_L1_GLL0",
    },
    "ECMWF": {
        "101": "TMP_P0_L100_GLL0",
        "102": "HGT_P0_L100_GLL0",
        "103": "UGRD_P0_L100_GLL0",
        "104": "VGRD_P0_L100_GLL0",
        "105": "UGRD_P0_L100_GLL0",
        "401": "PRES_P0_L101_GLL0",
    },
}

# fvGFS_ATL uses level-suffixed names built at runtime (handled in the function)
_FVGFS_ATL_TABLE_STATIC = {
    "401":  "PRMSL",
    "406":  "PRESsfc",
    "604":  "PWAT",
    "605":  "REFC",
    "607":  "CAPEsfc",
    "201":  "UGRD10m",
    "202":  "VGRD10m",
    "203":  "UGRD10m",
    "301":  "TMP2m",
    "404":  "TMPsfc",
    "302":  "DPT2m",
    "303":  "SPFH2m",
    "602":  "PRATEsfc",
}
_FVGFS_ATL_TABLE_LEV = {   # these names have lev2 appended at runtime
    "101": "TMP",
    "102": "HGT",
    "104": "VGRD",
    "107": "RH",
    "109": "VVEL",
    "103": "UGRD",
    "105": "UGRD",
    "110": "UGRD",
    "111": "UGRD",
    "112": "DPT",
}


def _resolve_model_table(d_source, lev2):
    """Return the {cat_num: var_name} dict for the given model source."""
    # Exact matches first
    for key in ("GFS", "GFS_an", "GFS_fcst", "GFS_d03"):
        if d_source == key:
            return _VAR_TABLES["GFS"]
    if "HWRF" in d_source:
        return _VAR_TABLES["HWRF"]
    if "HAFS" in d_source:
        return _VAR_TABLES["HAFS"]
    if d_source == "ERAi":
        return _VAR_TABLES["ERAi"]
    if d_source in ("H3HW", "HEDAS"):
        return _VAR_TABLES[d_source]
    if "fvGFS_ATL" in d_source:
        table = dict(_FVGFS_ATL_TABLE_STATIC)
        for cat, base in _FVGFS_ATL_TABLE_LEV.items():
            table[cat] = base + lev2
        return table
    if d_source == "GEFS":
        return _VAR_TABLES["GEFS"]
    if d_source == "ECMWF":
        return _VAR_TABLES["ECMWF"]
    return None


def find_var_name(d_source, var, lev):
    """
    Map a generic GPLOT variable name to the model-specific NetCDF variable name.

    Parameters
    ----------
    d_source : str
        Model/data source identifier (e.g. 'GFS', 'HAFS', 'HWRF').
    var : str
        Generic variable name (e.g. 'T', 'U', 'MSLP').
    lev : str
        Level string (e.g. '850', '10', '2', '').

    Returns
    -------
    str or list of str
        Model-specific variable name(s), or 'missing' if not found.

    Raises
    ------
    ValueError
        If the generic variable name is not in the master list.
    """
    # Build lev2: truncated integer string used for fvGFS_ATL suffixes
    if lev and len(lev) <= 5:
        lev2 = str(int(lev))
    elif lev and len(lev) > 5:
        lev2 = str(int(lev[:4]))
    else:
        lev2 = ""

    # Surface levels append to the variable name
    old_var = var + lev if lev in ("10", "2") else var

    if old_var not in _MASTER_LIST:
        raise ValueError(f"findVarName: Variable '{old_var}' not recognized")

    cat_num = _MASTER_LIST[old_var]

    var_table = _resolve_model_table(d_source, lev2)
    if var_table is None:
        raise ValueError(f"findVarName: Data source '{d_source}' not recognized")

    if cat_num not in var_table:
        print(f"WARNING: findVarName: Variable '{old_var}' not found in table for {d_source}.")
        return "missing"

    new_var = var_table[cat_num]

    # Temp fix: append time-interval level suffix for PRCP variables (GFS/HAFS only)
    if d_source in ("GFS", "HAFS") and "PRCP" in var and "h" in lev:
        new_var = new_var + lev

    return new_var
