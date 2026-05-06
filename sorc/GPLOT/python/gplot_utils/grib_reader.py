"""
GRIB2 file reader for GPLOT.

Replaces the NCL addfile() + getVar2d()/getVar3d() workflow from
GPLOT_func.ncl using xarray + cfgrib. Provides:

  - Open GRIB2 files with automatic coordinate detection
  - Extract 2D/3D variables using Vtable lookup
  - Compute derived fields (total wind speed, shear, layer means)
  - Automatic unit conversions (m/s -> kt, Pa -> hPa, etc.)
"""

import os
import logging

import numpy as np
import xarray as xr

from . import constants as C
from .vtable import find_var_name

logger = logging.getLogger(__name__)

# Mapping from abstract GPLOT variable names to cfgrib variable names.
# cfgrib uses short names from GRIB2 metadata, not the NCL-style
# field names in the Vtable (e.g., 'mslet' not 'MSLET_P0_L101_GLL0').
# Keys: (abstract_var, level_hint) where level_hint helps distinguish
# surface vs. pressure-level variables.
_CFGRIB_VAR_MAP = {
    # Mean sea level pressure
    'MSLP': ['mslet', 'prmsl'],
    'PRMSL': ['prmsl', 'mslet'],

    # Pressure-level fields (3D on isobaricInhPa)
    'T': ['t'],
    'U': ['u'],
    'V': ['v'],
    'RH': ['r'],
    'HGT': ['gh'],
    'Q': ['q'],
    'W': ['w'],
    'RVO': ['vo', 'absv'],
    'AVO': ['absv'],

    # Surface / height-above-ground fields
    'U10': ['u10'],
    'V10': ['v10'],
    'T2': ['t2m', 't'],  # 2-m temp; HAFS GRIB2 only has surface 't'
    'DPT': ['d2m'],      # 2-m dewpoint (may be absent on HAFS)
    'SST': ['sst', 'tmpsfc'],

    # Surface fluxes (HAFS uses ishf for sensible, slhtf for latent)
    'LHFLX': ['slhtf', 'lhtfl'],
    'SHFLX': ['ishf', 'shtfl'],

    # Radiation fluxes (time-averaged surface fields; GRIB2 uses *avg*
    # stepType, so cfgrib typically reports the plain short names dlwrf,
    # ulwrf, dswrf, uswrf -- the legacy GrADS aliases dlwrfavesfc etc.
    # are kept in the candidate lists for completeness).
    'DLWRF': ['dlwrf', 'dlwrfavesfc', 'sdlwrf'],
    'ULWRF': ['ulwrf', 'ulwrfavesfc', 'sulwrf'],
    'DSWRF': ['dswrf', 'dswrfavesfc', 'sdswrf'],
    'USWRF': ['uswrf', 'uswrfavesfc', 'suswrf'],

    # Friction velocity u* (HAFS surface field)
    'FRICV': ['fricv'],

    # Planetary boundary-layer height
    'HPBL': ['hpbl'],

    # Omega (pressure vertical velocity, Pa/s)
    'OMEGA': ['w'],

    # Specific humidity (synonym of 'Q' for scripts that spell it out)
    'SPF': ['q'],

    # Precipitation
    'TPRCP': ['tp'],
    'PRCP': ['tp'],
    'PRATE': ['prate'],

    # Reflectivity. HAFS uses 'rare' (Radar Reflectivity, paramId 231066)
    # for the 3D pressure-level field rather than the standard 'refd'.
    # Put 'rare' before the 2D 'refc' so REFL resolves to a 3D dataset
    # whenever HAFS data are present.
    'REFL': ['rare', 'refd', 'refl', 'refc'],
    'REFD': ['rare', 'refd', 'refl', 'refc'],
    'REFC': ['refc'],

    # TPW
    'TPW': ['pwat'],

    # CAPE
    'CAPE': ['cape'],

    # Storm-relative helicity.  HAFS stores only the 0-3 km AGL layer
    # (heightAboveGroundLayer: 3000-0 m) under shortName='hlcy'.  Accept
    # both the generic abstract name and the explicit layer alias so
    # namelists can be self-documenting.
    'HLCY': ['hlcy'],
    'HLCY3000_0m': ['hlcy'],

    # Potential vorticity (may be absent on HAFS nests)
    'PV': ['pv'],

    # Simulated GOES-R ABI brightness temperatures.  These live in a
    # companion sat.f*.grb2 file, where every message has a generic
    # shortName of 'unknown' and differs only by (parameterCategory,
    # parameterNumber).  open_sat_file() loads each band under an
    # explicit name below so get_var_2d() can find it.
    # Parameter numbers come from NCEP local table 4.2-3-192.
    'SIMIR':       ['sbtagr13'],   # ABI Band 13 (10.3 um, clean IR window)
    'SBTAGR13toa': ['sbtagr13'],
    'SBTAGR8toa':  ['sbtagr8'],    # ABI Band 8  (6.2  um, upper-trop WV)
    'SBTAGR9toa':  ['sbtagr9'],    # ABI Band 9  (6.9  um, mid-trop   WV)
    'SBTAGR10toa': ['sbtagr10'],   # ABI Band 10 (7.34 um, lower-trop WV)
}

# NCEP local table 4.2-3-192 parameterNumber -> canonical variable name.
# The sat.f*.grb2 file only stores these under shortName='unknown', so
# open_sat_file() uses this map to rename each message's sole variable.
# HAFS sat files also contain SSMIS-F17 microwave channels at
# parameterNumber 62-65, but those are NOT IR brightness temperatures and
# should not be plotted with the IR colormap -- leave them unmapped.
_SAT_PARAM_MAP = {
    53: 'sbtagr8',    # ABI Band 8  (6.2  um, upper-trop WV)
    54: 'sbtagr9',    # ABI Band 9  (6.9  um, mid-trop   WV)
    55: 'sbtagr10',   # ABI Band 10 (7.34 um, lower-trop WV)
    58: 'sbtagr13',   # ABI Band 13 (10.3 um, clean IR window)
}

# NCEP local-table parameters that the system eccodes definitions don't
# decode (so cfgrib reports them as shortName='unknown' with paramId=0).
# Each entry says: "if you re-open the file with this filter, you'll get a
# single 'unknown' variable -- rename it to <canon_name>". Used as a
# targeted fallback inside open_grib2() so downstream lookups via
# _CFGRIB_VAR_MAP find the variable under its expected name.
#
# Format: tuple (discipline, parameterCategory, parameterNumber,
#                typeOfLevel) -> canonical short name.
_NCEP_LOCAL_PARAM_MAP = {
    # discipline 0, category 3 (Mass), parameterNumber 196 = HPBL
    # (Planetary Boundary Layer Height), reported on typeOfLevel='surface'
    # with stepType='instant'.
    (0, 3, 196, 'surface'): 'hpbl',
}

# Surface/near-ground variables that collide with pressure-level names.
# Looked up first with a 2D-preferring search so that e.g. T at level='2'
# resolves to the surface 't' rather than the 3D pressure-level 't'.
_CFGRIB_SURFACE_VARS = {
    ('U', '10'): ['u10'],
    ('V', '10'): ['v10'],
    ('T', '2'): ['t2m', 't'],
    ('DPT', '2'): ['d2m'],
    # 2-m specific humidity lives on heightAboveGround=2 in HAFS GRIB2
    # under shortName='sh2' (NOT 'q' -- that name is reserved for the 3D
    # pressure-level moisture array). List 'sh2' first so the lookup
    # never falls back to the 3D 'q' dataset on files where 'sh2' is
    # present.
    ('Q', '2'):   ['sh2', 'q'],
    ('SPF', '2'): ['sh2', 'q'],
    # 2-m RH -- HAFS writes it as 'r2' or plain 'r'.
    ('RH', '2'):  ['r2', 'r'],
}


def open_grib2(filepath, filter_by_keys=None):
    """
    Open a GRIB2 file with xarray + cfgrib.

    Opens multiple dataset groups by level type to handle the heterogeneous
    structure of GRIB2 files (pressure levels, surface, MSL, etc.).

    Parameters
    ----------
    filepath : str
        Path to the GRIB2 file.
    filter_by_keys : dict, optional
        cfgrib filter keys (e.g., {'typeOfLevel': 'isobaricInhPa'}).

    Returns
    -------
    list of xr.Dataset
        One dataset per GRIB2 message group.
    """
    if not os.path.isfile(filepath):
        raise FileNotFoundError(f"GRIB2 file not found: {filepath}")

    if filter_by_keys is not None:
        return [xr.open_dataset(
            filepath, engine='cfgrib',
            backend_kwargs={'filter_by_keys': filter_by_keys}
        )]

    # GRIB2 files contain multiple level types that must be opened separately.
    # Try common level type filters used in atmospheric model output.
    # heightAboveGround must be split by level value because a single
    # cfgrib-backed dataset can't merge variables that only exist at
    # specific levels (e.g. t2m/d2m/r2 at level=2 vs u10/v10 at level=10).
    # Without the level filter, cfgrib ends up returning only one of the
    # two groups and silently drops the other with errors='ignore'.
    _LEVEL_FILTERS = [
        {'typeOfLevel': 'isobaricInhPa'},
        {'typeOfLevel': 'heightAboveGround', 'level': 2},
        {'typeOfLevel': 'heightAboveGround', 'level': 10},
        {'typeOfLevel': 'heightAboveGround', 'level': 80},
        {'typeOfLevel': 'heightAboveGround', 'level': 100},
        {'typeOfLevel': 'meanSea'},
        # Surface and atmosphereSingleLayer often need stepType
        # disambiguation to avoid "multiple values for unique key" errors
        {'typeOfLevel': 'surface', 'stepType': 'instant'},
        {'typeOfLevel': 'surface', 'stepType': 'avg'},
        {'typeOfLevel': 'surface', 'stepType': 'accum'},
        {'typeOfLevel': 'atmosphereSingleLayer', 'stepType': 'instant'},
        {'typeOfLevel': 'atmosphereSingleLayer', 'stepType': 'avg'},
        # Storm-relative helicity (hlcy) lives on heightAboveGroundLayer
        # (3000-0 m AGL).  The same level-type also carries updraft-helicity
        # hourly maxima (UPHL 2-5 km, 0-3 km) with shortName='unknown' and
        # stepType='max' -- they share the generic 'unknown' label so we
        # must split on stepType or cfgrib collapses them into one.
        {'typeOfLevel': 'heightAboveGroundLayer', 'stepType': 'instant'},
        {'typeOfLevel': 'heightAboveGroundLayer', 'stepType': 'max'},
        {'typeOfLevel': 'depthBelowLandLayer'},
        {'typeOfLevel': 'tropopause'},
        {'typeOfLevel': 'maxWind'},
        {'typeOfLevel': 'isothermZero'},
        {'typeOfLevel': 'cloudTop'},
    ]

    datasets = []
    for filt in _LEVEL_FILTERS:
        try:
            ds = xr.open_dataset(
                filepath, engine='cfgrib',
                backend_kwargs={
                    'filter_by_keys': filt,
                    'errors': 'ignore',
                }
            )
            if len(ds.data_vars) > 0:
                datasets.append(ds)
        except Exception:
            continue

    # Fallback: try opening without any filter
    if not datasets:
        try:
            ds = xr.open_dataset(
                filepath, engine='cfgrib',
                backend_kwargs={'errors': 'ignore'}
            )
            if len(ds.data_vars) > 0:
                datasets.append(ds)
        except Exception:
            pass

    # Targeted re-open for NCEP local-table parameters that show up as
    # shortName='unknown' (paramId=0) because the local eccodes install
    # has no entry for them. We re-open with an explicit
    # (discipline, parameterCategory, parameterNumber, typeOfLevel)
    # filter and rename the lone 'unknown' variable to its canonical
    # short name so downstream _CFGRIB_VAR_MAP lookups find it.
    for (disc, cat, pnum, lev), canon in _NCEP_LOCAL_PARAM_MAP.items():
        try:
            ds_extra = xr.open_dataset(
                filepath, engine='cfgrib',
                backend_kwargs={
                    'filter_by_keys': {
                        'discipline': disc,
                        'parameterCategory': cat,
                        'parameterNumber': pnum,
                        'typeOfLevel': lev,
                    },
                    'errors': 'ignore',
                },
            )
        except Exception as e:
            logger.debug(
                f"open_grib2: NCEP local-param "
                f"({disc},{cat},{pnum},{lev}) skipped: {e}")
            continue
        if 'unknown' in ds_extra.data_vars:
            ds_extra = ds_extra.rename({'unknown': canon})
        if canon in ds_extra.data_vars:
            datasets.append(ds_extra)

    if not datasets:
        raise ValueError(f"No readable GRIB2 messages in {filepath}")

    return datasets


def open_sat_file(filepath):
    """
    Open an HAFS (or similar) satellite brightness-temperature GRIB2 file.

    The sat.f*.grb2 files store multiple IR/WV bands all under a single
    generic shortName of 'unknown' on typeOfLevel='nominalTop', so a
    plain open_grib2() call returns only one band (cfgrib drops the
    others with errors='ignore').  This helper opens each band
    explicitly by (parameterCategory=192, parameterNumber=N) and renames
    the lone variable to a canonical name (e.g. 'sbtagr13' for
    parameterNumber=58, the GOES-R ABI clean-IR window band).

    Parameters
    ----------
    filepath : str
        Path to the ``*.sat.f*.grb2`` file.

    Returns
    -------
    list of xr.Dataset
        One dataset per band that could be decoded. Each contains a
        single 2D brightness-temperature variable plus lat/lon coords.
        Empty list if the file exists but no bands decoded cleanly.
    """
    if not os.path.isfile(filepath):
        raise FileNotFoundError(f"Sat GRIB2 file not found: {filepath}")

    datasets = []
    for pnum, canon_name in _SAT_PARAM_MAP.items():
        try:
            ds = xr.open_dataset(
                filepath, engine='cfgrib',
                backend_kwargs={
                    'filter_by_keys': {
                        'typeOfLevel': 'nominalTop',
                        'parameterCategory': 192,
                        'parameterNumber': pnum,
                    },
                    'errors': 'ignore',
                },
            )
            if 'unknown' in ds.data_vars:
                ds = ds.rename({'unknown': canon_name})
            if len(ds.data_vars) > 0:
                datasets.append(ds)
        except Exception as e:
            logger.debug(f"open_sat_file: skipped paramNumber={pnum}: {e}")
            continue

    return datasets


def _find_dataset_with_var(datasets, var_name):
    """Find which dataset in a list contains a given variable name."""
    if isinstance(datasets, xr.Dataset):
        if var_name in datasets:
            return datasets
        return None

    for ds in datasets:
        if var_name in ds:
            return ds
    return None


# Dimension names considered purely horizontal (not vertical).
_HORIZ_DIM_NAMES = ('lat', 'latitude', 'lon', 'longitude',
                    'grid_yt', 'grid_xt', 'x', 'y')


def _find_dataset_with_2d_var(datasets, var_name):
    """Find a dataset where var_name exists and has NO vertical dim.

    Needed because pressure-level and surface GRIB2 messages can share a
    short name (both the 3D pressure-level temperature and the 2-m / surface
    temperature come through cfgrib as `t`). The plain finder returns
    whichever dataset was opened first, so surface lookups silently grab
    the 3D array. This variant prefers the dataset where the variable is
    already 2D, falling back to the first match if only 3D copies exist.
    """
    if isinstance(datasets, xr.Dataset):
        datasets_list = [datasets]
    else:
        datasets_list = datasets

    fallback = None
    for ds in datasets_list:
        if var_name not in ds:
            continue
        da = ds[var_name]
        non_horiz = [d for d in da.dims
                     if d.lower() not in _HORIZ_DIM_NAMES]
        # Any non-horizontal dim with size > 1 means it's still vertical.
        vertical_sized = [d for d in non_horiz if da.sizes[d] > 1]
        if not vertical_sized:
            return ds
        if fallback is None:
            fallback = ds
    return fallback


def _resolve_cfgrib_var(datasets, abstract_var, level=''):
    """
    Resolve an abstract GPLOT variable name to the actual cfgrib
    dataset and variable name.

    cfgrib uses short names from GRIB2 metadata (e.g., 'mslet', 'u', 't')
    rather than the NCL-style Vtable field names.

    Returns
    -------
    tuple
        (dataset, cfgrib_var_name) or (None, None) if not found.
    """
    # Surface/near-ground lookups (U/V+10, T+2, DPT+2) need a 2D-preferring
    # search to avoid grabbing the pressure-level copy of the same name.
    key = (abstract_var, level)
    if key in _CFGRIB_SURFACE_VARS:
        candidates = _CFGRIB_SURFACE_VARS[key]
        for name in candidates:
            ds = _find_dataset_with_2d_var(datasets, name)
            if ds is not None:
                return ds, name

    # Look up in the main mapping
    candidates = _CFGRIB_VAR_MAP.get(abstract_var, [])
    for name in candidates:
        ds = _find_dataset_with_var(datasets, name)
        if ds is not None:
            return ds, name

    # Fallback: try the abstract var name in lowercase
    ds = _find_dataset_with_var(datasets, abstract_var.lower())
    if ds is not None:
        return ds, abstract_var.lower()

    return None, None


def _detect_coord_names(ds):
    """
    Detect latitude, longitude, and level coordinate names in a dataset.

    Returns
    -------
    dict
        Keys: 'lat', 'lon', 'lev' (values are coord names or None).
    """
    coords = {}

    # Latitude
    for name in ds.coords:
        if name.lower() in ('latitude', 'lat', 'grid_yt'):
            coords['lat'] = name
            break
    else:
        for name in ds.dims:
            if 'lat' in name.lower():
                coords['lat'] = name
                break
        else:
            coords['lat'] = None

    # Longitude
    for name in ds.coords:
        if name.lower() in ('longitude', 'lon', 'grid_xt'):
            coords['lon'] = name
            break
    else:
        for name in ds.dims:
            if 'lon' in name.lower():
                coords['lon'] = name
                break
        else:
            coords['lon'] = None

    # Vertical level
    for name in ds.coords:
        if name.lower() in ('isobaricinhpa', 'isobaric', 'level', 'lev',
                            'pressureinhpa'):
            coords['lev'] = name
            break
    else:
        for name in ds.dims:
            if any(key in name.lower() for key in ('lev', 'isbl', 'isobar')):
                coords['lev'] = name
                break
        else:
            coords['lev'] = None

    return coords


def _convert_units(data, var, units=None):
    """
    Apply standard unit conversions matching NCL getVar2d behavior.

    Parameters
    ----------
    data : np.ndarray or xr.DataArray
        The variable data.
    var : str
        Abstract variable name (e.g., 'MSLP', 'UV', 'T').
    units : str, optional
        Current units string from the GRIB2 metadata.

    Returns
    -------
    tuple
        (converted_data, new_units_string)
    """
    if units is None:
        units = ''

    # Wind: m/s -> knots (cfgrib uses 'm s**-1')
    if var in ('UV', 'U', 'V', 'U10', 'V10', 'UV10') and \
       any(u in units for u in ('m s-1', 'm/s', 'm s**-1')):
        return data * C.ms2kts, 'kt'

    # Pressure: Pa -> hPa
    if var in ('MSLP', 'PRMSL') and 'Pa' in units and 'hPa' not in units:
        return data * 0.01, 'hPa'

    # Vorticity: s^-1 -> 10^-5 s^-1. cfgrib reports units as 's**-1'
    # (the earlier substring check for 's-1' missed that form, leaving
    # the data in SI and producing an all-white plot since SI RVO
    # values ~1e-4 fall below every 10^-5-scaled contour level). Match
    # all common inverse-second notations explicitly.
    if var in ('RVO', 'AVO') and units in (
            's-1', 's**-1', 's^-1', '1/s', 'second-1', 'second**-1'):
        return data * 1e5, '10^-5 s^-1'

    # Geopotential height: m -> dam
    if var == 'HGT' and units in ('m', 'gpm'):
        return data * 0.1, 'dam'

    # Precipitation: kg/m^2 -> inches (for TPRCP)
    if var == 'TPRCP' and 'kg' in units:
        return data / 25.4, 'in'

    # Simulated GOES-R ABI brightness temperatures: K -> degC.
    # GRIB records carry these values in Kelvin regardless of
    # whether a units string is present (HAFS sat files often write
    # an empty units attribute, which the colorbar then renders as
    # 'unknown'). Convert unconditionally so the colorbar reads
    # 'SIMIR (degC)' on a -100..+50 C scale matching the operational
    # Himawari/GOES IR convention.
    if var in ('SIMIR', 'SBTAGR13toa', 'SBTAGR8toa',
               'SBTAGR9toa', 'SBTAGR10toa'):
        return data - 273.15, 'degC'

    return data, units


def get_var_2d(datasets, dsource, var, level='', bounds=None,
               gplot_dir=None):
    """
    Extract a 2D variable from GRIB2 data.

    Replaces getVar2d() from GPLOT_func.ncl. Handles Vtable lookup,
    coordinate subsetting, and unit conversion.

    Parameters
    ----------
    datasets : xr.Dataset or list of xr.Dataset
        Opened GRIB2 dataset(s) from open_grib2().
    dsource : str
        Data source for Vtable lookup (e.g., 'HAFS', 'GFS').
    var : str
        Abstract variable name (e.g., 'MSLP', 'UV', 'RH').
    level : str, optional
        Level code (e.g., '850', '10', '').
    bounds : tuple, optional
        (lat_n, lat_s, lon_w, lon_e) for geographic subsetting.
    gplot_dir : str, optional
        GPLOT root directory.

    Returns
    -------
    dict
        Keys: 'data' (np.ndarray), 'lat' (np.ndarray), 'lon' (np.ndarray),
        'units' (str), 'var' (str), 'level' (str).
        Returns None if variable not found.
    """
    # Handle compound variables that need two components
    if var == 'UV':
        return _get_wind_speed_2d(datasets, dsource, level, bounds, gplot_dir)
    if var == 'SHDL':
        return _get_deep_layer_shear(datasets, dsource, bounds, gplot_dir)

    # Potential vorticity is not stored in the HAFS GRIB2 output; derive
    # it on demand from T/u/v on pressure levels. PV needs vertical
    # finite differences of theta, so we evaluate on a short vertical
    # stack bracketing the requested level and return the single-level
    # slice.
    if var == 'PV':
        return _compute_pv_at_level(datasets, dsource, level, bounds,
                                    gplot_dir)

    # 2-m dewpoint is typically archived directly (d2m); if it's missing,
    # fall back to deriving it from 2-m temperature and 2-m RH via metpy
    # so the plot still renders on outputs that omit d2m.
    if var == 'DPT':
        direct = _try_direct_dpt(datasets, level, bounds)
        if direct is not None:
            return direct
        return _derive_dpt_from_t_rh(datasets, level, bounds)

    # Composite reflectivity preference. The shared cfgrib registry for
    # REFL/REFD lists 'rare' (3D HAFS pressure-level reflectivity) first
    # so get_var_3d can find it. For the 2D / no-level case, however,
    # we want the model-native composite ('refc') whenever it exists --
    # that's the canonical column-max the model itself wrote, computed
    # at full vertical resolution rather than from the truncated
    # pressure-level archive. Only fall through to deriving the
    # composite from 'rare' if the file lacks 'refc'.
    if var in ('REFL', 'REFD') and not level:
        refc_ds = _find_dataset_with_var(datasets, 'refc')
        if refc_ds is not None:
            ds, grib_name = refc_ds, 'refc'
        else:
            ds, grib_name = _resolve_cfgrib_var(datasets, var, level)
    else:
        # Standard resolution path (specific level requested, or
        # non-reflectivity variable).
        ds, grib_name = _resolve_cfgrib_var(datasets, var, level)

    if ds is None:
        logger.warning(f"Variable '{var}' (level={level}) not found in GRIB2 data")
        return None

    da = ds[grib_name]
    coord_names = _detect_coord_names(ds)

    # Select pressure level if needed
    if coord_names['lev'] is not None and coord_names['lev'] in da.dims:
        lev_val = _parse_level_value(level, ds, coord_names['lev'])
        if lev_val is not None:
            da = da.sel({coord_names['lev']: lev_val}, method='nearest')

    # Fallback composite reflectivity: if 'refc' was unavailable above
    # we ended up with the 3D 'rare' (or 'refd'/'refl'), and -- since
    # the no-level case skipped the level-select step -- a level dim
    # with size > 1 survives. Column-max along that dim recovers a
    # composite reflectivity equivalent to what 'refc' would have been.
    # The 2D 'refc' path falls through this branch unchanged (no lev dim).
    if (coord_names['lev'] is not None and coord_names['lev'] in da.dims
            and da.sizes[coord_names['lev']] > 1
            and var in ('REFL', 'REFD')):
        da = da.max(dim=coord_names['lev'], skipna=True)

    # Squeeze extra dimensions (time, step, etc.)
    for dim in list(da.dims):
        if dim not in (coord_names['lat'], coord_names['lon']):
            if da.sizes[dim] == 1:
                da = da.squeeze(dim)

    # Get coordinates
    lat = da.coords[coord_names['lat']].values if coord_names['lat'] else None
    lon = da.coords[coord_names['lon']].values if coord_names['lon'] else None

    # Subset to bounds
    data = da.values
    if bounds is not None and lat is not None and lon is not None:
        data, lat, lon = _subset_to_bounds(data, lat, lon, bounds)

    # Unit conversion
    units = str(da.attrs.get('units', ''))
    data, units = _convert_units(data, var, units)

    return {
        'data': data,
        'lat': lat,
        'lon': lon,
        'units': units,
        'var': var,
        'level': level,
    }


def get_var_3d(datasets, dsource, var, lev_top, lev_bot, bounds=None,
               gplot_dir=None):
    """
    Extract a 3D variable (vertical slice) from GRIB2 data.

    Replaces getVar3d() from GPLOT_func.ncl.

    Parameters
    ----------
    datasets : xr.Dataset or list of xr.Dataset
        Opened GRIB2 dataset(s).
    dsource : str
        Data source for Vtable lookup.
    var : str
        Abstract variable name (e.g., 'T', 'U', 'RH').
    lev_top : float
        Top pressure level (hPa), e.g., 200.
    lev_bot : float
        Bottom pressure level (hPa), e.g., 850.
    bounds : tuple, optional
        (lat_n, lat_s, lon_w, lon_e) for geographic subsetting.
    gplot_dir : str, optional
        GPLOT root directory.

    Returns
    -------
    dict
        Keys: 'data' (3D np.ndarray [lev, lat, lon]), 'lat', 'lon',
        'lev' (np.ndarray of pressure levels), 'units', 'var'.
        Returns None if variable not found.
    """
    if var == 'UV':
        return _get_wind_speed_3d(datasets, dsource, lev_top, lev_bot,
                                  bounds, gplot_dir)

    # Resolve variable name via cfgrib mapping
    ds, grib_name = _resolve_cfgrib_var(datasets, var, str(int(lev_top)))
    if ds is None:
        logger.warning(f"Variable '{var}' (3D) not found in GRIB2 data")
        return None

    da = ds[grib_name]
    coord_names = _detect_coord_names(ds)

    if coord_names['lev'] is None or coord_names['lev'] not in da.dims:
        logger.warning(f"No vertical dimension for {grib_name}")
        return None

    # Select level range - detect ascending/descending level order
    lev_coord = da.coords[coord_names['lev']].values
    lev_min, lev_max = min(lev_top, lev_bot), max(lev_top, lev_bot)
    if len(lev_coord) > 1 and lev_coord[0] > lev_coord[-1]:
        # Descending levels (e.g., 1000, 975, ..., 200): slice high to low
        da = da.sel({coord_names['lev']: slice(lev_max, lev_min)})
    else:
        da = da.sel({coord_names['lev']: slice(lev_min, lev_max)})

    # Squeeze single-valued extra dimensions
    for dim in list(da.dims):
        if dim not in (coord_names['lev'], coord_names['lat'], coord_names['lon']):
            if da.sizes[dim] == 1:
                da = da.squeeze(dim)

    lat = da.coords[coord_names['lat']].values if coord_names['lat'] else None
    lon = da.coords[coord_names['lon']].values if coord_names['lon'] else None
    lev = da.coords[coord_names['lev']].values

    data = da.values
    if bounds is not None and lat is not None and lon is not None:
        data, lat, lon = _subset_to_bounds(data, lat, lon, bounds)

    units = str(da.attrs.get('units', ''))
    data, units = _convert_units(data, var, units)

    return {
        'data': data,
        'lat': lat,
        'lon': lon,
        'lev': lev,
        'units': units,
        'var': var,
    }


def get_layer_mean(datasets, dsource, var, lev_top, lev_bot, bounds=None,
                   gplot_dir=None):
    """
    Compute a pressure-weighted layer mean of a variable.

    Parameters
    ----------
    datasets, dsource, var, lev_top, lev_bot, bounds, gplot_dir
        Same as get_var_3d.

    Returns
    -------
    dict
        Same as get_var_2d (data is the layer mean).
    """
    result_3d = get_var_3d(datasets, dsource, var, lev_top, lev_bot,
                           bounds, gplot_dir)
    if result_3d is None:
        return None

    data_3d = result_3d['data']
    levs = result_3d['lev']

    if len(levs) < 2:
        return {
            'data': data_3d.squeeze(),
            'lat': result_3d['lat'],
            'lon': result_3d['lon'],
            'units': result_3d['units'],
            'var': var,
            'level': f'{int(lev_top)}-{int(lev_bot)}',
        }

    # Pressure-weighted mean
    dp = np.abs(np.diff(levs))
    weights = dp / dp.sum()

    # Average between adjacent levels, then weight
    mean_data = np.zeros_like(data_3d[0])
    for k in range(len(weights)):
        layer_avg = 0.5 * (data_3d[k] + data_3d[k + 1])
        mean_data += weights[k] * layer_avg

    return {
        'data': mean_data,
        'lat': result_3d['lat'],
        'lon': result_3d['lon'],
        'units': result_3d['units'],
        'var': var,
        'level': f'{int(lev_top)}-{int(lev_bot)}',
    }


def _get_wind_speed_2d(datasets, dsource, level, bounds, gplot_dir):
    """Compute total wind speed from U and V components."""
    ds_u, u_name = _resolve_cfgrib_var(datasets, 'U', level)
    ds_v, v_name = _resolve_cfgrib_var(datasets, 'V', level)

    if ds_u is None or ds_v is None:
        logger.warning(f"Cannot find U/V components for wind (level={level})")
        return None

    da_u = ds_u[u_name]
    da_v = ds_v[v_name]
    coord_names = _detect_coord_names(ds_u)

    # Select pressure level if 3D
    if coord_names['lev'] is not None and coord_names['lev'] in da_u.dims:
        lev_val = _parse_level_value(level, ds_u, coord_names['lev'])
        if lev_val is not None:
            da_u = da_u.sel({coord_names['lev']: lev_val}, method='nearest')
            da_v = da_v.sel({coord_names['lev']: lev_val}, method='nearest')

    # Squeeze extra dims
    for dim in list(da_u.dims):
        if dim not in (coord_names['lat'], coord_names['lon']):
            if da_u.sizes[dim] == 1:
                da_u = da_u.squeeze(dim)
                da_v = da_v.squeeze(dim)

    lat = da_u.coords[coord_names['lat']].values if coord_names['lat'] else None
    lon = da_u.coords[coord_names['lon']].values if coord_names['lon'] else None

    speed = np.sqrt(da_u.values ** 2 + da_v.values ** 2)

    if bounds is not None and lat is not None and lon is not None:
        speed, lat, lon = _subset_to_bounds(speed, lat, lon, bounds)

    units = str(da_u.attrs.get('units', ''))
    speed, units = _convert_units(speed, 'UV', units)

    return {
        'data': speed,
        'lat': lat,
        'lon': lon,
        'units': units,
        'var': 'UV',
        'level': level,
    }


def get_wind_components(datasets, dsource, level, bounds=None, gplot_dir=None):
    """
    Get U and V wind components (for vector/barb/streamline overlays).

    Returns
    -------
    dict
        Keys: 'u', 'v' (np.ndarray), 'lat', 'lon', 'units'.
        Returns None if not found.
    """
    ds_u, u_name = _resolve_cfgrib_var(datasets, 'U', level)
    ds_v, v_name = _resolve_cfgrib_var(datasets, 'V', level)

    if ds_u is None or ds_v is None:
        return None

    da_u = ds_u[u_name]
    da_v = ds_v[v_name]
    coord_names = _detect_coord_names(ds_u)

    if coord_names['lev'] is not None and coord_names['lev'] in da_u.dims:
        lev_val = _parse_level_value(level, ds_u, coord_names['lev'])
        if lev_val is not None:
            da_u = da_u.sel({coord_names['lev']: lev_val}, method='nearest')
            da_v = da_v.sel({coord_names['lev']: lev_val}, method='nearest')

    for dim in list(da_u.dims):
        if dim not in (coord_names['lat'], coord_names['lon']):
            if da_u.sizes[dim] == 1:
                da_u = da_u.squeeze(dim)
                da_v = da_v.squeeze(dim)

    lat = da_u.coords[coord_names['lat']].values if coord_names['lat'] else None
    lon = da_u.coords[coord_names['lon']].values if coord_names['lon'] else None

    u_data = da_u.values
    v_data = da_v.values

    if bounds is not None and lat is not None and lon is not None:
        u_data, lat, lon = _subset_to_bounds(u_data, lat, lon, bounds)
        v_data, _, _ = _subset_to_bounds(v_data, da_u.coords[coord_names['lat']].values,
                                         da_u.coords[coord_names['lon']].values, bounds)

    units = str(da_u.attrs.get('units', ''))
    u_data, units = _convert_units(u_data, 'U', units)
    v_data, _ = _convert_units(v_data, 'V', units)

    return {
        'u': u_data,
        'v': v_data,
        'lat': lat,
        'lon': lon,
        'units': units,
    }


def _get_wind_speed_3d(datasets, dsource, lev_top, lev_bot, bounds, gplot_dir):
    """Compute 3D total wind speed from U and V."""
    u_result = get_var_3d(datasets, dsource, 'U', lev_top, lev_bot, bounds, gplot_dir)
    v_result = get_var_3d(datasets, dsource, 'V', lev_top, lev_bot, bounds, gplot_dir)

    if u_result is None or v_result is None:
        return None

    speed = np.sqrt(u_result['data'] ** 2 + v_result['data'] ** 2)
    speed, units = _convert_units(speed, 'UV', u_result['units'])

    return {
        'data': speed,
        'lat': u_result['lat'],
        'lon': u_result['lon'],
        'lev': u_result['lev'],
        'units': units,
        'var': 'UV',
    }


def _compute_pv_at_level(datasets, dsource, level, bounds, gplot_dir):
    """
    Derive baroclinic potential vorticity at a requested pressure level
    using metpy.calc.potential_vorticity_baroclinic.

    HAFS GRIB2 output does not archive PV directly, so we compute it
    on-the-fly from T/u/v on pressure levels. PV requires a vertical
    theta gradient, so we pull a three-level stack bracketing the
    requested level (e.g. 175, 200, 225 hPa for a 200 hPa request),
    compute PV on that stack, and return the middle slice.

    The fields are passed to metpy as xarray DataArrays carrying CF-style
    coordinates ('latitude', 'longitude', 'isobaric') with units. This
    lets metpy auto-detect horizontal and vertical dims and broadcast
    the grid deltas internally; passing raw numpy/pint arrays triggers
    a 2D/3D dx-vs-u shape mismatch inside metpy's first_derivative.

    Returns PV in PVU (1 PVU = 1e-6 K m^2 kg^-1 s^-1) -- the standard
    presentation units for TC diagnostics (tropopause fold identification,
    dynamic tropopause maps, etc.).
    """
    try:
        import metpy.calc as mpcalc  # noqa: F401  (registers units)
    except ImportError:
        logger.warning("metpy not installed; cannot derive PV")
        return None

    try:
        target = float(level)
    except (TypeError, ValueError):
        target = 200.0

    # Locate the pressure-level dataset so we can pick bracketing levels
    # that actually exist in the GRIB2 file.
    ds, _ = _resolve_cfgrib_var(datasets, 'T', '500')
    if ds is None:
        logger.warning("No pressure-level dataset available for PV")
        return None
    coord_names = _detect_coord_names(ds)
    lev_coord = coord_names['lev']
    if lev_coord is None or lev_coord not in ds.dims:
        logger.warning("Pressure-level coord not found for PV")
        return None

    all_levs = ds.coords[lev_coord].values
    # HAFS stores pressure in hPa; other sources sometimes use Pa.
    in_pa = np.max(all_levs) > 1100
    target_native = target * 100.0 if in_pa else target

    # Three closest levels around the target, sorted ascending (hPa).
    order = np.argsort(np.abs(all_levs - target_native))
    bracket = sorted(all_levs[order[:3]])
    if len(bracket) < 2:
        logger.warning("Not enough pressure levels to derive PV")
        return None
    lev_top_hpa = (min(bracket) / 100.0) if in_pa else min(bracket)
    lev_bot_hpa = (max(bracket) / 100.0) if in_pa else max(bracket)

    t_res = get_var_3d(datasets, dsource, 'T', lev_top_hpa, lev_bot_hpa,
                       bounds, gplot_dir)
    u_res = get_var_3d(datasets, dsource, 'U', lev_top_hpa, lev_bot_hpa,
                       bounds, gplot_dir)
    v_res = get_var_3d(datasets, dsource, 'V', lev_top_hpa, lev_bot_hpa,
                       bounds, gplot_dir)
    if t_res is None or u_res is None or v_res is None:
        logger.warning("Missing T/u/v levels for PV derivation")
        return None

    levs_hpa = np.asarray(t_res['lev'], dtype=float)
    if np.max(levs_hpa) > 1100:
        levs_hpa = levs_hpa / 100.0

    t_data = t_res['data']
    # get_var_3d converted u/v to knots; metpy expects m/s.
    u_data = u_res['data'] / C.ms2kts
    v_data = v_res['data'] / C.ms2kts

    # Sort top-to-bottom (ascending pressure) so finite differences work.
    if levs_hpa[0] > levs_hpa[-1]:
        order_idx = np.argsort(levs_hpa)
        levs_hpa = levs_hpa[order_idx]
        t_data = t_data[order_idx]
        u_data = u_data[order_idx]
        v_data = v_data[order_idx]

    lat = t_res['lat']
    lon = t_res['lon']

    # Build xarray DataArrays with CF-style coords so metpy can
    # auto-detect horizontal/vertical axes and compute grid deltas.
    dims = ('isobaric', 'latitude', 'longitude')
    coords = {
        'isobaric': ('isobaric', levs_hpa, {'units': 'hPa'}),
        'latitude': ('latitude', lat, {'units': 'degrees_north'}),
        'longitude': ('longitude', lon, {'units': 'degrees_east'}),
    }

    def _mk_da(data, name, units_str):
        return xr.DataArray(
            data, dims=dims, coords=coords,
            attrs={'units': units_str}, name=name,
        ).metpy.quantify()

    t_da = _mk_da(t_data, 'air_temperature', 'K')
    u_da = _mk_da(u_data, 'u', 'm/s')
    v_da = _mk_da(v_data, 'v', 'm/s')
    p_da = xr.DataArray(
        levs_hpa, dims=('isobaric',),
        coords={'isobaric': ('isobaric', levs_hpa, {'units': 'hPa'})},
        attrs={'units': 'hPa'},
    ).metpy.quantify()

    try:
        theta = mpcalc.potential_temperature(p_da, t_da)
        pv = mpcalc.potential_vorticity_baroclinic(theta, p_da, u_da, v_da)
    except Exception as e:
        logger.warning(f"PV computation failed: {e}")
        return None

    # pv comes out in base SI units (K m^2 kg^-1 s^-1). Convert
    # numerically to PVU (1 PVU = 1e-6 K m^2 kg^-1 s^-1).
    pv_base = pv.metpy.dequantify().values
    pv_pvu = pv_base * 1e6

    k_target = int(np.argmin(np.abs(levs_hpa - target)))
    return {
        'data': pv_pvu[k_target],
        'lat': lat,
        'lon': lon,
        'units': 'PVU',
        'var': 'PV',
        'level': str(int(target)),
    }


def _try_direct_dpt(datasets, level, bounds):
    """Return d2m directly from GRIB2 if present (2-m dewpoint)."""
    ds = _find_dataset_with_2d_var(datasets, 'd2m')
    if ds is None:
        return None
    da = ds['d2m']
    coord_names = _detect_coord_names(ds)
    for dim in list(da.dims):
        if dim not in (coord_names['lat'], coord_names['lon']):
            if da.sizes[dim] == 1:
                da = da.squeeze(dim)
    lat = da.coords[coord_names['lat']].values
    lon = da.coords[coord_names['lon']].values
    data = da.values
    if bounds is not None:
        data, lat, lon = _subset_to_bounds(data, lat, lon, bounds)
    return {
        'data': data,
        'lat': lat,
        'lon': lon,
        'units': str(da.attrs.get('units', 'K')),
        'var': 'DPT',
        'level': level,
    }


def _derive_dpt_from_t_rh(datasets, level, bounds):
    """
    Derive 2-m dewpoint from 2-m temperature and 2-m RH via metpy.

    Used as a fallback when d2m isn't archived in the GRIB2 file.
    Requires t2m and r2 (2-m relative humidity in %).
    """
    try:
        import metpy.calc as mpcalc
        from metpy.units import units as mu
    except ImportError:
        logger.warning("metpy not installed; cannot derive dewpoint")
        return None

    ds_t = _find_dataset_with_2d_var(datasets, 't2m')
    ds_r = _find_dataset_with_2d_var(datasets, 'r2')
    if ds_t is None or ds_r is None:
        logger.warning("Cannot derive DPT: missing t2m or r2 in GRIB2")
        return None

    t = ds_t['t2m']
    r = ds_r['r2']
    coord_names = _detect_coord_names(ds_t)
    for dim in list(t.dims):
        if dim not in (coord_names['lat'], coord_names['lon']):
            if t.sizes[dim] == 1:
                t = t.squeeze(dim)
                r = r.squeeze(dim)

    t_arr = t.values * mu.K
    r_arr = (r.values / 100.0) * mu.dimensionless

    dpt = mpcalc.dewpoint_from_relative_humidity(t_arr, r_arr).to(mu.K)

    lat = t.coords[coord_names['lat']].values
    lon = t.coords[coord_names['lon']].values
    data = dpt.magnitude
    if bounds is not None:
        data, lat, lon = _subset_to_bounds(data, lat, lon, bounds)

    return {
        'data': data,
        'lat': lat,
        'lon': lon,
        'units': 'K',
        'var': 'DPT',
        'level': level,
    }


def _get_deep_layer_shear(datasets, dsource, bounds, gplot_dir):
    """
    Compute deep-layer wind shear (200-850 hPa vector difference).

    Shear = sqrt((U200-U850)^2 + (V200-V850)^2)
    """
    u200 = get_var_2d(datasets, dsource, 'U', '200', bounds, gplot_dir)
    u850 = get_var_2d(datasets, dsource, 'U', '850', bounds, gplot_dir)
    v200 = get_var_2d(datasets, dsource, 'V', '200', bounds, gplot_dir)
    v850 = get_var_2d(datasets, dsource, 'V', '850', bounds, gplot_dir)

    if any(x is None for x in [u200, u850, v200, v850]):
        logger.warning("Cannot compute deep-layer shear: missing U/V at 200/850")
        return None

    du = u200['data'] - u850['data']
    dv = v200['data'] - v850['data']
    shear = np.sqrt(du ** 2 + dv ** 2)

    return {
        'data': shear,
        'lat': u200['lat'],
        'lon': u200['lon'],
        'units': u200['units'],
        'var': 'SHDL',
        'level': '',
    }


def _parse_level_value(level_str, ds, lev_coord_name):
    """
    Parse a level string and return the numeric value for selection.

    Handles pressure levels in hPa (e.g., '850' -> 850.0) and
    accounts for whether the dataset levels are in hPa or Pa.
    """
    if not level_str or level_str in ('', 'N/A', '10', '2'):
        return None

    try:
        lev_val = float(level_str)
    except ValueError:
        return None

    # Check if dataset levels are in Pa (>1100 suggests Pa, not hPa)
    lev_coords = ds.coords[lev_coord_name].values
    if len(lev_coords) > 0 and np.max(lev_coords) > 1100:
        lev_val = lev_val * 100.0  # Convert hPa to Pa

    return lev_val


def _subset_to_bounds(data, lat, lon, bounds):
    """
    Subset data arrays to geographic bounds.

    Parameters
    ----------
    data : np.ndarray
        2D or 3D data array (..., lat, lon).
    lat : np.ndarray
        Latitude coordinate array.
    lon : np.ndarray
        Longitude coordinate array.
    bounds : tuple
        (lat_n, lat_s, lon_w, lon_e).

    Returns
    -------
    tuple
        (data_subset, lat_subset, lon_subset)
    """
    lat_n, lat_s, lon_w, lon_e = bounds

    # Detect and handle longitude convention mismatch
    # (bounds in -180..180 but data in 0..360, or vice versa)
    lon_min, lon_max = lon.min(), lon.max()
    if lon_min >= 0 and lon_max > 180:
        # Data is in 0..360 convention
        if lon_w < 0:
            lon_w = lon_w % 360
        if lon_e < 0:
            lon_e = lon_e % 360
    elif lon_max <= 180 and lon_min < 0:
        # Data is in -180..180 convention
        if lon_w > 180:
            lon_w = lon_w - 360
        if lon_e > 180:
            lon_e = lon_e - 360

    # Handle latitude (might be decreasing)
    if lat[0] > lat[-1]:  # decreasing lat
        lat_mask = (lat <= lat_n) & (lat >= lat_s)
    else:
        lat_mask = (lat >= lat_s) & (lat <= lat_n)

    # Handle longitude
    if lon_w < lon_e:
        lon_mask = (lon >= lon_w) & (lon <= lon_e)
    else:
        # Wrapping case (e.g., 170 to -170)
        lon_mask = (lon >= lon_w) | (lon <= lon_e)

    lat_sub = lat[lat_mask]
    lon_sub = lon[lon_mask]

    if data.ndim == 2:
        data_sub = data[np.ix_(lat_mask, lon_mask)]
    elif data.ndim == 3:
        data_sub = data[:, np.ix_(lat_mask, lon_mask)[0].ravel(), :]
        data_sub = data_sub[:, :, lon_mask]
    else:
        data_sub = data

    return data_sub, lat_sub, lon_sub


def get_grid_info(datasets, dsource, gplot_dir=None):
    """
    Get grid metadata from the first available variable in a GRIB2 file.

    Returns
    -------
    dict
        Keys: 'lat', 'lon' (full coordinate arrays), 'dx' (grid spacing
        in degrees), 'nx', 'ny' (grid dimensions).
    """
    if isinstance(datasets, xr.Dataset):
        ds = datasets
    else:
        ds = datasets[0]

    coord_names = _detect_coord_names(ds)

    lat = ds.coords[coord_names['lat']].values if coord_names['lat'] else None
    lon = ds.coords[coord_names['lon']].values if coord_names['lon'] else None

    dx = None
    if lat is not None and len(lat) > 1:
        dx = abs(float(lat[1] - lat[0]))

    return {
        'lat': lat,
        'lon': lon,
        'dx': dx,
        'ny': len(lat) if lat is not None else 0,
        'nx': len(lon) if lon is not None else 0,
    }
