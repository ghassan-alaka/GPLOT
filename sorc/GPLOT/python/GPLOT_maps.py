#!/usr/bin/env python3
"""
GPLOT Maps Module - Geographic contour maps from GRIB2 model output.

Replaces sorc/GPLOT/ncl/GPLOT_maps.ncl (~2500 lines).
Reads GRIB2 files, applies maps namelist plot recipes, and produces
layered geographic contour maps with filled contours, contour line
overlays, wind vectors/streamlines, H/L markers, and storm track overlays.

Usage:
    python GPLOT_maps.py --idate 2025102300 --sid 13L --domain d03 \
        --tier Tier1 --master-nml /path/to/namelist
"""

import argparse
import glob
import logging
import os
import re
import sys
from datetime import datetime, timedelta

import matplotlib
import xarray as xr
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import numpy as np
import cartopy.crs as ccrs
import cartopy.feature as cfeature
from scipy.ndimage import minimum_filter, maximum_filter

# Add the parent directory to sys.path for gplot_utils import
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from gplot_utils import constants as C
from gplot_utils.namelist import (read_master_namelist, read_maps_namelist,
                                   resolve_namelist_path)
from gplot_utils.atcf import read_atcf, read_bdeck, derive_longsid
from gplot_utils.grib_reader import (open_grib2, open_sat_file, get_var_2d,
                                      get_var_3d, get_layer_mean,
                                      get_wind_components, get_grid_info)
from gplot_utils.kurihara import vortex_filter
from gplot_utils.wn0_filter import remove_wavenumber0
from gplot_utils.colormaps import (get_colormap, get_contour_levels, get_norm,
                                    resample_cmap, build_discrete_cmap)
from gplot_utils.domains import (get_domain_bounds, is_storm_centered,
                                  is_storm_named_filename,
                                  get_nest_number)
from gplot_utils.plot_utils import (setup_map_axes, create_figure, add_titles,
                                     add_disclaimer, add_storm_marker,
                                     save_figure, update_plotted_file,
                                     read_spawn_file_list,
                                     sweep_orphan_pngs,
                                     get_plot_title, configure_cartopy)

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Filename token regexes (shared by find_grib_files and the nest-outline
# helpers). Match against dot-delimited filename components rather than as
# raw substrings so experiment names like "hfsb_multistorm" don't get
# mis-classified as a storm-nest file because they happen to contain
# "storm".
# ---------------------------------------------------------------------------
_NEST_TOKEN_RE = re.compile(
    r'(?:^|[._-])(storm\d*|nest\d*|moving|d03)(?:[._-]|$)',
    re.IGNORECASE,
)
_PARENT_TOKEN_RE = re.compile(
    r'(?:^|[._-])(parent|d01|hwrf)(?:[._-]|$)',
    re.IGNORECASE,
)


# ---------------------------------------------------------------------------
# Vortex-filter cache
# ---------------------------------------------------------------------------
# Vortex removal (wavenumber-0 subtraction, or -- as a fallback without
# a known TC center -- a Gaussian smoother) is the most expensive step in
# a steering-flow recipe. Each recipe reads U and V twice: once for the
# filled wind-speed field and again for the streamline overlay, both at
# the same (lev_top, lev_bot). We cache the filtered 3D cubes per
# (datasets_id, var, lev_top, lev_bot, bounds) so the second caller hits
# the cache rather than re-filtering. For the wn0 path U and V are
# coupled (the polar rotation mixes them), so both cubes are computed
# and cached together on the first call.
#
# The cache is cleared once per forecast hour (see `_clear_vortex_cache`)
# which bounds it at roughly 2 * 3 entries per fhr (U/V x SFDL/SFML/SFSL).
_VORTEX_CACHE = {}


def _write_status(status_file, value):
    """Write module status with a lockfile (spawn-compatible)."""
    status_lock = f"{status_file}.lock"
    os.system(f'lockfile -r-1 -l 180 "{status_lock}"')
    os.system(f'echo "{value}" > "{status_file}"')
    os.system(f'rm -f "{status_lock}"')


def _clear_vortex_cache():
    """Drop all cached filtered cubes. Called once per forecast hour."""
    _VORTEX_CACHE.clear()


def _vortex_cache_key(datasets, var, lev_top, lev_bot, bounds):
    """Build a hashable key for the vortex-filter cache."""
    return (id(datasets), var, int(lev_top), int(lev_bot),
            tuple(float(b) for b in bounds) if bounds else None)


def _get_filtered_3d(datasets, dsource, var, lev_top, lev_bot, bounds,
                     gplot_dir, tc_lat=None, tc_lon=None):
    """
    Return a vortex-filtered 3D cube for `var` over [lev_top, lev_bot].

    When ``tc_lat`` and ``tc_lon`` are both finite, the wavenumber-0
    filter is used: the U and V cubes are fetched and filtered together
    (they're coupled through the polar rotation) and both are cached.
    Otherwise the isotropic Gaussian smoother in ``kurihara.vortex_filter``
    is used as a scale-explicit fallback.

    Uses ``_VORTEX_CACHE`` so the second caller (e.g. the streamline
    overlay following the fill) hits the cache instead of re-filtering.
    """
    key = _vortex_cache_key(datasets, var, lev_top, lev_bot, bounds)
    cached = _VORTEX_CACHE.get(key)
    if cached is not None:
        return cached

    have_center = (tc_lat is not None and tc_lon is not None
                   and np.isfinite(tc_lat) and np.isfinite(tc_lon))

    if have_center and var in ('U', 'V'):
        # wavenumber-0 path: filter U and V together, cache both.
        u_cube = get_var_3d(datasets, dsource, 'U', lev_top, lev_bot,
                            bounds, gplot_dir)
        v_cube = get_var_3d(datasets, dsource, 'V', lev_top, lev_bot,
                            bounds, gplot_dir)
        if (u_cube is None or u_cube.get('data') is None
                or v_cube is None or v_cube.get('data') is None):
            return None

        lats = u_cube.get('lat')
        lons = u_cube.get('lon')
        if lats is None or lons is None:
            logger.warning(
                "wn0 filter: cube missing lat/lon coords; "
                "falling back to Gaussian")
        else:
            logger.debug(
                "_get_filtered_3d[wn0]: lev=[%s, %s] bounds=%s "
                "tc=(%.2f, %.2f) shape=%s",
                lev_top, lev_bot, bounds, tc_lat, tc_lon,
                u_cube['data'].shape,
            )
            u_filt, v_filt = remove_wavenumber0(
                u_cube['data'], v_cube['data'], lats, lons,
                tc_lat, tc_lon,
            )
            u_cube['data'] = u_filt
            v_cube['data'] = v_filt
            # Populate both cache entries so the sibling call is free.
            u_key = _vortex_cache_key(datasets, 'U', lev_top, lev_bot,
                                      bounds)
            v_key = _vortex_cache_key(datasets, 'V', lev_top, lev_bot,
                                      bounds)
            _VORTEX_CACHE[u_key] = u_cube
            _VORTEX_CACHE[v_key] = v_cube
            return u_cube if var == 'U' else v_cube

    # Fallback: Gaussian-on-scalar-field.
    cube = get_var_3d(datasets, dsource, var, lev_top, lev_bot, bounds,
                      gplot_dir)
    if cube is None or cube.get('data') is None:
        return None

    grid = get_grid_info(datasets, dsource, gplot_dir)
    dx_deg = grid.get('dx') if grid else None
    dx_km = float(dx_deg) * 111.0 if dx_deg else None

    cube['data'] = vortex_filter(cube['data'], dx_km=dx_km)
    _VORTEX_CACHE[key] = cube
    return cube


def _layer_mean_from_cube(cube_3d):
    """
    Pressure-weighted vertical mean of a 3D cube (lev, lat, lon).

    Mirrors the math in grib_reader.get_layer_mean so we can re-use an
    already-filtered cube without touching the dataset again.
    """
    data_3d = cube_3d['data']
    levs = cube_3d['lev']

    if data_3d.ndim == 2 or len(levs) < 2:
        return np.asarray(data_3d).squeeze()

    dp = np.abs(np.diff(levs))
    weights = dp / dp.sum()
    out = np.zeros_like(data_3d[0])
    for k in range(len(weights)):
        out += weights[k] * 0.5 * (data_3d[k] + data_3d[k + 1])
    return out


# ---------------------------------------------------------------------------
# Level code parsing
def _normalize_lon(lon):
    """Convert longitude from 0..360 to -180..180 if needed."""
    if lon is not None and np.any(lon > 180):
        return np.where(lon > 180, lon - 360, lon)
    return lon


# ---------------------------------------------------------------------------

def parse_level_code(level_str):
    """
    Parse a maps namelist level code into its components.

    Level code conventions:
      - Plain number ('850', '500')    -> single pressure level
      - '10', '2'                      -> height above ground
      - 'N/A' or empty                 -> surface / no level
      - '02000850d'                    -> difference: top=200, bot=850
      - '07000400a'                    -> layer average: bot=700, top=400
      - '02500850m'                    -> vortex-filtered layer mean
                                           (steering flow, Kurihara-smoothed)

    The 'm' suffix is treated as a pressure-weighted layer mean for the
    purposes of get_layer_mean / get_var_3d, but downstream callers use
    the returned 'suffix' key to decide whether to apply the Kurihara
    vortex-removal filter and swap to the SHDL colormap (see
    get_wind_field and make_map).

    Returns
    -------
    dict with keys:
        'type': 'single' | 'surface' | 'difference' | 'average' | 'none'
        'level': str (original level string)
        'lev_top': int (for difference/average)
        'lev_bot': int (for difference/average)
        'suffix': str or None -- 'd', 'a', 'm', or None for non-compound
    """
    if not level_str or level_str in ('N/A', 'n/a', ''):
        return {'type': 'none', 'level': '', 'lev_top': None,
                'lev_bot': None, 'suffix': None}

    # Check for compound level codes (e.g., 02000850d, 07000400a, 02500850m).
    # 'd' = difference (shear), 'a' = plain layer mean (e.g. RH layers),
    # 'm' = vortex-filtered layer mean (steering flow).
    m = re.match(r'^(\d{4})(\d{4})([dam])$', level_str)
    if m:
        top = int(m.group(1))
        bot = int(m.group(2))
        code = m.group(3)
        return {
            'type': 'difference' if code == 'd' else 'average',
            'level': level_str,
            'lev_top': top,
            'lev_bot': bot,
            'suffix': code,
        }

    # Height above ground
    if level_str in ('10', '2'):
        return {'type': 'surface', 'level': level_str,
                'lev_top': None, 'lev_bot': None, 'suffix': None}

    # Single pressure level
    try:
        int(level_str)
        return {'type': 'single', 'level': level_str,
                'lev_top': None, 'lev_bot': None, 'suffix': None}
    except ValueError:
        return {'type': 'none', 'level': level_str,
                'lev_top': None, 'lev_bot': None, 'suffix': None}


def get_field(datasets, dsource, var, level_str, bounds, gplot_dir,
              tc_lat=None, tc_lon=None):
    """
    Retrieve a 2D field from GRIB2, handling compound level codes.

    Parameters
    ----------
    datasets : list of xr.Dataset
    dsource : str
    var : str
        Abstract variable name.
    level_str : str
        Level code from maps namelist.
    bounds : tuple
    gplot_dir : str
    tc_lat, tc_lon : float, optional
        TC center position, used when ``level_str`` has the ``m`` suffix
        (vortex-filtered steering flow) to enable the wavenumber-0
        filter. When either is ``None`` the path falls back to the
        scale-explicit Gaussian smoother.

    Returns
    -------
    dict or None
        Same format as grib_reader.get_var_2d().
    """
    lc = parse_level_code(level_str)

    if lc['type'] == 'none':
        return get_var_2d(datasets, dsource, var, '', bounds, gplot_dir)

    if lc['type'] == 'surface' or lc['type'] == 'single':
        return get_var_2d(datasets, dsource, var, lc['level'], bounds,
                          gplot_dir)

    if lc['type'] == 'difference':
        # Vector difference of two levels (e.g., shear)
        if var == 'UV':
            # For UV shear, get_var_2d with 'SHDL' is already handled
            # but we need generic 2-level difference
            top = get_var_2d(datasets, dsource, var, str(lc['lev_top']),
                             bounds, gplot_dir)
            bot = get_var_2d(datasets, dsource, var, str(lc['lev_bot']),
                             bounds, gplot_dir)
            if top is None or bot is None:
                # Try SHDL fallback for deep-layer shear
                return get_var_2d(datasets, dsource, 'SHDL', '', bounds,
                                  gplot_dir)
            diff_data = np.sqrt((top['data'] ** 2 + bot['data'] ** 2))
            # Actually, for UV shear it should be magnitude of vector diff
            # Get U and V components at each level
            wind_top = get_wind_components(datasets, dsource,
                                           str(lc['lev_top']), bounds,
                                           gplot_dir)
            wind_bot = get_wind_components(datasets, dsource,
                                           str(lc['lev_bot']), bounds,
                                           gplot_dir)
            if wind_top is not None and wind_bot is not None:
                du = wind_top['u'] - wind_bot['u']
                dv = wind_top['v'] - wind_bot['v']
                diff_data = np.sqrt(du ** 2 + dv ** 2)
                return {
                    'data': diff_data,
                    'lat': wind_top['lat'],
                    'lon': wind_top['lon'],
                    'units': wind_top['units'],
                    'var': var,
                    'level': level_str,
                }
            return top  # fallback
        else:
            top = get_var_2d(datasets, dsource, var, str(lc['lev_top']),
                             bounds, gplot_dir)
            bot = get_var_2d(datasets, dsource, var, str(lc['lev_bot']),
                             bounds, gplot_dir)
            if top is None or bot is None:
                return None
            return {
                'data': top['data'] - bot['data'],
                'lat': top['lat'],
                'lon': top['lon'],
                'units': top['units'],
                'var': var,
                'level': level_str,
            }

    if lc['type'] == 'average':
        # 'm' suffix == vortex-filtered pressure-weighted layer mean
        # (steering flow). Runs the Kurihara smoother on the U and V 3D
        # cubes first, then derives the layer-mean wind speed from the
        # filtered cubes so both the fill and any streamline overlay see
        # the same environmental flow.
        if lc.get('suffix') == 'm' and var == 'UV':
            u_cube = _get_filtered_3d(datasets, dsource, 'U', lc['lev_top'],
                                      lc['lev_bot'], bounds, gplot_dir,
                                      tc_lat=tc_lat, tc_lon=tc_lon)
            v_cube = _get_filtered_3d(datasets, dsource, 'V', lc['lev_top'],
                                      lc['lev_bot'], bounds, gplot_dir,
                                      tc_lat=tc_lat, tc_lon=tc_lon)
            if u_cube is None or v_cube is None:
                logger.warning(
                    f"Vortex-filtered steering flow unavailable for "
                    f"{var} {level_str}; falling back to unfiltered mean")
                return get_layer_mean(datasets, dsource, var, lc['lev_top'],
                                      lc['lev_bot'], bounds, gplot_dir)
            u_mean = _layer_mean_from_cube(u_cube)
            v_mean = _layer_mean_from_cube(v_cube)
            # get_var_3d already converted m/s -> kt internally via
            # _convert_units, so u_cube/v_cube are in kt and no extra
            # scaling is required here.
            speed = np.sqrt(u_mean ** 2 + v_mean ** 2)
            return {
                'data': speed,
                'lat': u_cube['lat'],
                'lon': u_cube['lon'],
                'units': u_cube.get('units', 'kt'),
                'var': var,
                'level': level_str,
            }
        return get_layer_mean(datasets, dsource, var, lc['lev_top'],
                              lc['lev_bot'], bounds, gplot_dir)

    return None


def get_wind_field(datasets, dsource, level_str, bounds, gplot_dir,
                   tc_lat=None, tc_lon=None):
    """
    Retrieve wind U/V components, handling compound level codes.

    For compound levels (difference/average), returns the appropriate
    wind components.

    Parameters
    ----------
    tc_lat, tc_lon : float, optional
        TC center position; used for the wavenumber-0 steering-flow
        filter when ``level_str`` carries the ``m`` suffix. Falls back
        to a Gaussian smoother when either is ``None``.

    Returns
    -------
    dict or None
        Keys: 'u', 'v', 'lat', 'lon', 'units'.
    """
    lc = parse_level_code(level_str)

    if lc['type'] in ('none', 'surface', 'single'):
        return get_wind_components(datasets, dsource, lc['level'], bounds,
                                   gplot_dir)

    if lc['type'] == 'difference':
        top = get_wind_components(datasets, dsource, str(lc['lev_top']),
                                  bounds, gplot_dir)
        bot = get_wind_components(datasets, dsource, str(lc['lev_bot']),
                                  bounds, gplot_dir)
        if top is None or bot is None:
            return None
        return {
            'u': top['u'] - bot['u'],
            'v': top['v'] - bot['v'],
            'lat': top['lat'],
            'lon': top['lon'],
            'units': top['units'],
        }

    if lc['type'] == 'average':
        # Vortex-filtered steering-flow path: reuse the cached smoothed
        # U/V cubes so the streamline overlay in the same recipe hits the
        # cache populated by the fill call.
        if lc.get('suffix') == 'm':
            u_cube = _get_filtered_3d(datasets, dsource, 'U', lc['lev_top'],
                                      lc['lev_bot'], bounds, gplot_dir,
                                      tc_lat=tc_lat, tc_lon=tc_lon)
            v_cube = _get_filtered_3d(datasets, dsource, 'V', lc['lev_top'],
                                      lc['lev_bot'], bounds, gplot_dir,
                                      tc_lat=tc_lat, tc_lon=tc_lon)
            if u_cube is None or v_cube is None:
                logger.warning(
                    f"Vortex-filtered wind components unavailable for "
                    f"{level_str}; falling back to unfiltered mean")
            else:
                return {
                    'u': _layer_mean_from_cube(u_cube),
                    'v': _layer_mean_from_cube(v_cube),
                    'lat': u_cube['lat'],
                    'lon': u_cube['lon'],
                    'units': u_cube.get('units', 'kt'),
                }

        # Plain layer-average wind (no filtering). Compute component-by-
        # component pressure-weighted means.
        u_mean = get_layer_mean(datasets, dsource, 'U', lc['lev_top'],
                                lc['lev_bot'], bounds, gplot_dir)
        v_mean = get_layer_mean(datasets, dsource, 'V', lc['lev_top'],
                                lc['lev_bot'], bounds, gplot_dir)
        if u_mean is None or v_mean is None:
            return None
        return {
            'u': u_mean['data'],
            'v': v_mean['data'],
            'lat': u_mean['lat'],
            'lon': u_mean['lon'],
            'units': u_mean['units'],
        }

    return None


# ---------------------------------------------------------------------------
# StreamlineThin table
# ---------------------------------------------------------------------------

def load_streamline_thin(gplot_dir, dsource, domain):
    """
    Load the wind vector thinning factor from tbl/StreamlineThin.dat.

    Returns
    -------
    int
        Thinning skip factor (e.g., 10 means plot every 10th vector).
    """
    thin_path = os.path.join(gplot_dir, 'tbl', 'StreamlineThin.dat')
    default_thin = 4

    if not os.path.isfile(thin_path):
        return default_thin

    dsource_upper = dsource.upper()
    domain_lower = domain.lower()

    with open(thin_path, 'r') as f:
        for line in f:
            parts = line.strip().split()
            if len(parts) >= 3:
                src, dmn, val = parts[0], parts[1], parts[2]
                if src.upper() == dsource_upper and dmn.lower() == domain_lower:
                    try:
                        return int(val)
                    except ValueError:
                        pass

    # Try DEFAULT entry
    with open(thin_path, 'r') as f:
        for line in f:
            parts = line.strip().split()
            if len(parts) >= 3:
                src, dmn, val = parts[0], parts[1], parts[2]
                if src.upper() == 'DEFAULT' and dmn.lower() == domain_lower:
                    try:
                        return int(val)
                    except ValueError:
                        pass

    return default_thin


# ---------------------------------------------------------------------------
# H/L marker detection
# ---------------------------------------------------------------------------

def find_hl_markers(data, lat, lon, var='MSLP', min_dist_deg=5.0,
                    filter_deg=3.0, max_markers=6):
    """
    Find H (high) and L (low) pressure markers on a 2D field.

    Uses local min/max detection with a scipy filter whose window is
    sized in degrees (rather than grid points) so behaviour is stable
    across coarse parent grids and high-resolution storm nests.

    Parameters
    ----------
    data : np.ndarray
        2D data field.
    lat, lon : np.ndarray
        1D coordinate arrays.
    var : str
        Variable name (used to determine H vs L semantics).
    min_dist_deg : float
        Minimum distance between retained markers, in degrees.
    filter_deg : float
        Min/max filter window size, in degrees. Must be larger than
        the typical noise scale but smaller than the synoptic
        feature spacing.
    max_markers : int
        Cap on the total number of markers (H+L combined). Prevents
        clutter when the field has many weak extrema.

    Returns
    -------
    list of dict
        Each dict: {'type': 'H' or 'L', 'lat': float, 'lon': float,
                    'value': float}.
    """
    if data is None or lat is None or lon is None:
        return []

    # Grid resolution (deg per point), avoiding divide-by-zero
    dlat = abs(float(lat[1] - lat[0])) if len(lat) > 1 else 1.0
    dlon = abs(float(lon[1] - lon[0])) if len(lon) > 1 else 1.0
    dgrid = max(dlat, dlon, 1e-6)

    # Convert filter window from degrees to odd grid points
    filter_size = max(3, int(round(filter_deg / dgrid)))
    if filter_size % 2 == 0:
        filter_size += 1

    # Find local minima and maxima
    data_min = minimum_filter(data, size=filter_size)
    data_max = maximum_filter(data, size=filter_size)

    local_min_mask = (data == data_min) & np.isfinite(data)
    local_max_mask = (data == data_max) & np.isfinite(data)

    llon, llat = np.meshgrid(lon, lat)

    markers = []
    for i in np.argwhere(local_min_mask):
        markers.append({
            'type': 'L', 'lat': llat[i[0], i[1]],
            'lon': llon[i[0], i[1]], 'value': data[i[0], i[1]],
        })
    for i in np.argwhere(local_max_mask):
        markers.append({
            'type': 'H', 'lat': llat[i[0], i[1]],
            'lon': llon[i[0], i[1]], 'value': data[i[0], i[1]],
        })

    # Rank by "prominence": Ls by lowest value first, Hs by highest
    def rank_key(m):
        return m['value'] if m['type'] == 'L' else -m['value']
    markers.sort(key=rank_key)

    # Filter by minimum distance to avoid cluttered markers
    filtered = []
    for m in markers:
        if len(filtered) >= max_markers:
            break
        too_close = False
        for existing in filtered:
            dist = np.sqrt((m['lat'] - existing['lat']) ** 2 +
                           (m['lon'] - existing['lon']) ** 2)
            if dist < min_dist_deg:
                too_close = True
                break
        if not too_close:
            filtered.append(m)

    return filtered


# ---------------------------------------------------------------------------
# Plot composition
# ---------------------------------------------------------------------------

def _expected_ofile(recipe, longsid, fhr, idate, domain, odir):
    """
    Return the .gif path that ``draw_map`` would write for this recipe.

    Mirrors the filename-stem construction at the bottom of ``draw_map``
    (see the ``ofile_stem`` block there). Kept in lockstep with that
    block; if the naming convention ever changes, update both.

    Used by the FHR-loop on-disk gate to detect already-produced
    figures and avoid re-rendering them under ``--force``.
    """
    fname = recipe['FILE_NAME']
    if is_storm_named_filename(domain):
        stem = f"{longsid.lower()}.{fname}.{idate}.{domain}.f{fhr:03d}"
    else:
        stem = f"{fname}.{idate}.{domain}.f{fhr:03d}"
    return os.path.join(odir, stem + '.gif')


def draw_map(recipe, datasets, dsource, bounds, fhr, idate, expt,
             tc_lat, tc_lon, vmax, mslp_val, longsid, ensid,
             gplot_dir, odir, domain, thin_factor=4, atcf_df=None,
             nest_outlines=None):
    """
    Produce a single map plot from a maps namelist recipe.

    Parameters
    ----------
    recipe : dict
        Plot recipe from read_maps_namelist().
    datasets : list of xr.Dataset
        Opened GRIB2 datasets.
    dsource : str
        Data source name.
    bounds : tuple
        (lat_n, lat_s, lon_w, lon_e).
    fhr : int
        Forecast hour.
    idate : str
        Initialization date (YYYYMMDDHH).
    expt : str
        Experiment name.
    tc_lat, tc_lon : float or None
        TC center position at this forecast hour.
    vmax : int or None
        Max wind intensity (kt).
    mslp_val : int or None
        Min sea level pressure (hPa).
    longsid : str
        Long storm ID.
    ensid : str
        Ensemble member ID.
    gplot_dir : str
        GPLOT root directory.
    odir : str
        Output directory.
    domain : str
        Domain name.
    thin_factor : int
        Wind vector thinning factor.
    atcf_df : DataFrame or None
        ATCF track data for overlay.

    Returns
    -------
    str or None
        Path to the output file, or None if failed.
    """
    base_var = recipe['BASE_CN_FILL']
    base_lev = recipe['LEV1']
    filename = recipe['FILE_NAME']

    # --- 1. Get the base fill field ---
    base_field = get_field(datasets, dsource, base_var, base_lev, bounds,
                           gplot_dir, tc_lat=tc_lat, tc_lon=tc_lon)
    if base_field is None:
        logger.warning(f"Skipping {filename}: base field {base_var} "
                       f"(lev={base_lev}) not found")
        return None

    # Normalize longitude to -180..180 for cartopy PlateCarree
    if base_field['lon'] is not None and np.any(base_field['lon'] > 180):
        base_field['lon'] = np.where(base_field['lon'] > 180,
                                     base_field['lon'] - 360,
                                     base_field['lon'])

    # Derive map extent from the actual data coverage after subsetting.
    # The requested 'bounds' may be larger than the GRIB2 file's extent
    # (e.g., d03 box of 30deg vs. storm-nest file of ~20deg), which
    # would leave large empty areas in the map.  Intersect the requested
    # bounds with what's actually present to avoid this.
    lat_arr = base_field['lat']
    lon_arr = base_field['lon']
    req_latn, req_lats, req_lonw, req_lone = bounds
    if req_lonw > 180:
        req_lonw -= 360
    if req_lone > 180:
        req_lone -= 360
    if lat_arr is not None and len(lat_arr) > 0:
        data_latn = float(np.max(lat_arr))
        data_lats = float(np.min(lat_arr))
    else:
        data_latn, data_lats = req_latn, req_lats
    if lon_arr is not None and len(lon_arr) > 0:
        data_lone = float(np.max(lon_arr))
        data_lonw = float(np.min(lon_arr))
    else:
        data_lone, data_lonw = req_lone, req_lonw
    plot_bounds = (
        min(req_latn, data_latn),
        max(req_lats, data_lats),
        max(req_lonw, data_lonw),
        min(req_lone, data_lone),
    )

    # --- 2. Create the figure ---
    fig, ax = create_figure(figsize=(12, 9))
    setup_map_axes(ax, plot_bounds)

    # --- 3. Draw filled contours (base variable) ---
    # Determine which variable name to use for colormap/level lookup.
    # Both vector-difference shear (`d` suffix) and vortex-filtered
    # steering flow (`m` suffix) display a small residual wind magnitude
    # (typically 5-30 kt), so reuse the SHDL shear palette for both
    # rather than the full 10-137 kt UV Saffir-Simpson scale.
    cmap_var = base_var
    cmap_lev = base_lev
    lc = parse_level_code(base_lev)
    if lc['type'] == 'difference':
        cmap_var = 'SHDL' if base_var == 'UV' else base_var
        cmap_lev = ''
    elif (lc['type'] == 'average' and lc.get('suffix') == 'm'
          and base_var == 'UV'):
        cmap_var = 'SHDL'
        cmap_lev = ''

    cmap = get_colormap(cmap_var, cmap_lev, gplot_dir)
    levels = get_contour_levels(cmap_var, cmap_lev)

    if levels is not None and len(levels) > 0:
        # Build a discrete cmap sized for the interior bins, with
        # set_under/set_over populated from the source palette endpoints
        # so extend='both' uses the intended below-min / above-max colors
        # (e.g., tcwinds1.rgb: white under 10 kt, purple over 137 kt)
        # instead of reusing the first/last interior bin's color.
        #
        # The explicit BoundaryNorm is required: without it, contourf
        # auto-creates a continuous Normalize that maps data linearly
        # onto the 0..1 cmap range, causing matplotlib to sample our
        # discrete N-color ListedColormap at N+extend positions and
        # drop interior colors (the orange/dark-green disappearance
        # on the wind scale). With BoundaryNorm + set_under/set_over
        # each level bin gets its own dedicated color 1:1.
        cmap_final = build_discrete_cmap(cmap, len(levels) - 1, extend='both')
        norm_final = get_norm(levels)
        cf = ax.contourf(base_field['lon'], base_field['lat'],
                         base_field['data'], levels=levels,
                         cmap=cmap_final, norm=norm_final, extend='both',
                         transform=ccrs.PlateCarree())
    else:
        cf = ax.contourf(base_field['lon'], base_field['lat'],
                         base_field['data'], cmap=cmap, extend='both',
                         transform=ccrs.PlateCarree())

    # Colorbar
    cbar = fig.colorbar(cf, ax=ax, orientation='horizontal', pad=0.05,
                        shrink=0.8, aspect=40)
    cbar_label = f"{base_var}"
    if base_field['units']:
        cbar_label += f" ({base_field['units']})"
    cbar.set_label(cbar_label, fontsize=10)

    # Satellite variables use a 1-degC fill (smooth gradient on the
    # IR4 / WVCIMSS_r palettes) but should label only every 10 degC
    # so the colorbar stays readable. Pick ticks at multiples of 10
    # within the level range.
    _SAT_VARS = {'SIMIR', 'SBTAGR13toa',
                 'SIMWV_UPPER', 'SIMWV_MID',
                 'SBTAGR8toa', 'SBTAGR9toa', 'SBTAGR10toa'}
    if base_var in _SAT_VARS and levels is not None and len(levels) >= 2:
        lvmin, lvmax = float(levels[0]), float(levels[-1])
        # Round inward to nearest 10 so the displayed ticks are clean
        # multiples (-100, -90, ..., 50 etc.).
        import math as _math
        tick_lo = int(_math.ceil(lvmin / 10.0) * 10)
        tick_hi = int(_math.floor(lvmax / 10.0) * 10)
        ticks = list(range(tick_lo, tick_hi + 1, 10))
        if ticks:
            cbar.set_ticks(ticks)
        cbar.ax.tick_params(labelsize=8)
    # Label every level boundary when the scale has <=15 bins so
    # non-uniformly spaced breaks (e.g., Saffir-Simpson wind thresholds
    # 0/10/20/34/50/64/83/96/114/137) don't get silently dropped by
    # matplotlib's default locator. For denser scales (MSLP, RVO) the
    # default locator picks readable round-number ticks.
    elif levels is not None and 0 < len(levels) <= 15:
        cbar.set_ticks(list(levels))
        cbar.ax.tick_params(labelsize=8)

    # --- 3b. Self-overlay contour lines for HGT fills ---
    # Upper-level height plots are much easier to read when the fill
    # is accompanied by labeled contour lines of the same field, so
    # automatically overlay them every 3 dam (or ~6x the level step)
    # on top of the viridis/turbo fill.
    if base_var == 'HGT' and levels is not None and len(levels) > 2:
        step = max(1, int(round((levels[-1] - levels[0]) / 20)))
        hgt_line_levels = levels[::step]
        cs_self = ax.contour(base_field['lon'], base_field['lat'],
                             base_field['data'],
                             levels=hgt_line_levels,
                             colors='black', linewidths=0.6,
                             transform=ccrs.PlateCarree(), zorder=3)
        try:
            ax.clabel(cs_self, cs_self.levels[::2], fontsize=7,
                      fmt='%d', inline=True)
        except (IndexError, ValueError):
            pass

    # --- 4. Draw contour line overlay 1 ---
    _draw_contour_overlay(ax, datasets, dsource, recipe['OV_CN_LINE'],
                          recipe['LEV2'], bounds, gplot_dir,
                          color='black', linewidths=1.0,
                          tc_lat=tc_lat, tc_lon=tc_lon)

    # --- 5. Draw contour line overlay 2 ---
    _draw_contour_overlay(ax, datasets, dsource, recipe['OV_CN_LINE2'],
                          recipe['LEV3'], bounds, gplot_dir,
                          color='blue', linewidths=0.8,
                          tc_lat=tc_lat, tc_lon=tc_lon)

    # --- 6. Draw wind vector/barb overlay ---
    ov_wind = recipe['OV_VC_WIND']
    ov_wind_lev = recipe['LEV4']
    if ov_wind not in ('N/A', 'n/a', ''):
        _draw_wind_overlay(ax, datasets, dsource, ov_wind_lev, bounds,
                           gplot_dir, thin_factor,
                           tc_lat=tc_lat, tc_lon=tc_lon)

    # --- 7. Draw streamline overlay ---
    ov_stline = recipe['OV_STLINE']
    ov_stline_lev = recipe['LEV6']
    if ov_stline not in ('N/A', 'n/a', ''):
        _draw_streamline_overlay(ax, datasets, dsource, ov_stline_lev,
                                 bounds, gplot_dir, thin_factor,
                                 tc_lat=tc_lat, tc_lon=tc_lon)

    # --- 8. Draw TC low marker (from ATCF) ---
    # Every storm-centered plot benefits from a clear marker at the
    # TC center, so we draw the ATCF-driven 'L' + PMIN on all recipes
    # (not just those with OV_MAX_MIN=MSLP set in the maps namelist).
    # The old behavior used a scipy min/max filter on the full field,
    # which on a high-resolution storm nest produced dozens of
    # spurious H/L markers. The ATCF-based marker is always exactly
    # one low at the reported cyclone position.
    _draw_tc_low_marker(ax, tc_lat, tc_lon, mslp_val)

    # Optional moving-nest outlines (DRAW_NESTS=True in the master
    # namelist; only computed by main() for parent-style domains).
    # Each entry traces the actual defined-data footprint of a per-
    # storm d03/storm-nest GRIB2 file at this FHR, so multistorm
    # parent panels show every active nest at once.
    if nest_outlines:
        _draw_nest_outlines(ax, nest_outlines)

    # NOTE: the previous version overlaid the full ATCF track
    # polyline (past + future positions of the storm) on every
    # map panel, but that line obscures features near the TC core
    # on the storm-centric d03/hwrf panels. The 'L' low-pressure
    # marker drawn above already conveys the storm's instantaneous
    # position, so the track polyline is dropped. The
    # ``_draw_atcf_track`` helper is kept in the module for any
    # future caller that wants it back.

    # NOTE: the intensity-coded storm-marker dot used to be drawn
    # here, but it lives at the same lat/lon as the 'L' and was
    # visually redundant with the category info already encoded in
    # the title (VMAX=..) and in the wind-field colors. Dropped.

    # --- 11. Add titles ---
    # Storm-named domains (d03, hwrf) show one TC per panel, so the
    # right-side title carries longsid + VMAX/MSLP. Large-scale
    # domains (d01, atl, basin, ...) may carry multiple storms in a
    # single plot, so the title and filename are storm-agnostic.
    var_title = get_plot_title(filename)
    if is_storm_named_filename(domain):
        add_titles(ax, expt, var_title, fhr, idate, longsid=longsid,
                   vmax=vmax, mslp=mslp_val, ensid=ensid)
    else:
        add_titles(ax, expt, var_title, fhr, idate, longsid='',
                   vmax=None, mslp=None, ensid=ensid)
    add_disclaimer(ax, expt)

    # --- 12. Save ---
    # Filename pattern matches the legacy NCL/HRD convention so that
    # operational consumers and side-by-side comparisons line up.
    # Storm-named domains: <longsid>.<recipe>.<idate>.<domain>.f<fhr>
    #   e.g. maila30p.REFL_MSLP.2026040806.d03.f018.gif
    # Other domains: <recipe>.<idate>.<domain>.f<fhr>
    #   e.g. REFL_MSLP.2026040806.d01.f018.gif
    if is_storm_named_filename(domain):
        ofile_stem = os.path.join(
            odir,
            f"{longsid.lower()}.{filename}.{idate}.{domain}.f{fhr:03d}")
    else:
        ofile_stem = os.path.join(
            odir,
            f"{filename}.{idate}.{domain}.f{fhr:03d}")
    ofile = save_figure(fig, ofile_stem, do_trim=True, do_gif=True)
    logger.info(f"Saved: {ofile}")
    return ofile


def _draw_contour_overlay(ax, datasets, dsource, var, level_str, bounds,
                           gplot_dir, color='black', linewidths=1.0,
                           tc_lat=None, tc_lon=None):
    """Draw contour line overlay for a variable."""
    if var in ('N/A', 'n/a', ''):
        return

    field = get_field(datasets, dsource, var, level_str, bounds, gplot_dir,
                      tc_lat=tc_lat, tc_lon=tc_lon)
    if field is None:
        return

    field['lon'] = _normalize_lon(field['lon'])

    # Get contour levels for this variable
    lc = parse_level_code(level_str)
    levels = get_contour_levels(var, lc.get('level', ''))

    if var == 'MSLP':
        # MSLP contours: every 4 hPa
        if levels is None:
            levels = np.arange(900, 1060, 4)
        cs = ax.contour(field['lon'], field['lat'], field['data'],
                        levels=levels, colors=color, linewidths=linewidths,
                        transform=ccrs.PlateCarree())
        ax.clabel(cs, cs.levels[::2], fontsize=7, fmt='%d', inline=True)
    elif var == 'HGT':
        if levels is None:
            # HGT is in dam after unit conversion
            levels = np.arange(100, 1300, 3)
        else:
            # The level registry is tuned for filled-contour resolution
            # (1 dam spacing); for overlay lines that's unreadably
            # dense. Sub-sample so ~8 lines span the range.
            if len(levels) > 10:
                stride = max(1, len(levels) // 8)
                levels = np.asarray(levels)[::stride]
        cs = ax.contour(field['lon'], field['lat'], field['data'],
                        levels=levels, colors=color, linewidths=linewidths,
                        transform=ccrs.PlateCarree())
        try:
            # Label every other contour line to reduce clutter
            ax.clabel(cs, cs.levels[::2], fontsize=7, fmt='%.0f',
                      inline=True)
        except (IndexError, ValueError):
            pass
    else:
        if levels is not None:
            cs = ax.contour(field['lon'], field['lat'], field['data'],
                            levels=levels, colors=color,
                            linewidths=linewidths,
                            transform=ccrs.PlateCarree())
        else:
            cs = ax.contour(field['lon'], field['lat'], field['data'],
                            colors=color, linewidths=linewidths,
                            transform=ccrs.PlateCarree())
        try:
            ax.clabel(cs, cs.levels[::2], fontsize=7, inline=True)
        except (IndexError, ValueError):
            pass


def _compute_adaptive_skip(n_points, table_thin, target_n=30):
    """
    Pick a thinning skip that gives ~target_n barbs across the plot.

    The tbl/StreamlineThin.dat value was tuned for ~0.018 deg HWRF
    grids; on a 0.02 deg HAFS storm nest (500+ points per axis),
    applying that value directly produces an unreadably dense barb
    field. Use the larger of the table's skip and a grid-aware skip
    so small parent domains keep their table setting while high-res
    nests get thinned appropriately.
    """
    grid_skip = max(1, int(n_points / target_n))
    return max(int(table_thin), grid_skip)


def _draw_wind_overlay(ax, datasets, dsource, level_str, bounds, gplot_dir,
                        thin, tc_lat=None, tc_lon=None):
    """Draw wind barb overlay."""
    wind = get_wind_field(datasets, dsource, level_str, bounds, gplot_dir,
                          tc_lat=tc_lat, tc_lon=tc_lon)
    if wind is None:
        return

    wind['lon'] = _normalize_lon(wind['lon'])

    # Adaptive thinning: target ~25-30 barbs along the longer axis so
    # the field is readable on both coarse parent grids and high-res
    # storm nests (HAFS d03 is 0.02 deg => 500-800 points per axis).
    nx = len(wind['lon']) if wind['lon'] is not None else 1
    ny = len(wind['lat']) if wind['lat'] is not None else 1
    skip = _compute_adaptive_skip(max(nx, ny), thin, target_n=30)

    u_thin = wind['u'][::skip, ::skip]
    v_thin = wind['v'][::skip, ::skip]
    lat_thin = wind['lat'][::skip]
    lon_thin = wind['lon'][::skip]

    ax.barbs(lon_thin, lat_thin, u_thin, v_thin, length=5,
             linewidth=0.4, color='black',
             transform=ccrs.PlateCarree(), zorder=5)


def _draw_streamline_overlay(ax, datasets, dsource, level_str, bounds,
                              gplot_dir, thin, tc_lat=None, tc_lon=None):
    """Draw streamline overlay."""
    wind = get_wind_field(datasets, dsource, level_str, bounds, gplot_dir,
                          tc_lat=tc_lat, tc_lon=tc_lon)
    if wind is None:
        return

    wind['lon'] = _normalize_lon(wind['lon'])

    # Matplotlib's streamplot is expensive on dense grids; down-sample
    # first so it completes in reasonable time on the storm nest.
    nx = len(wind['lon']) if wind['lon'] is not None else 1
    ny = len(wind['lat']) if wind['lat'] is not None else 1
    skip = _compute_adaptive_skip(max(nx, ny), thin, target_n=60)
    u = wind['u'][::skip, ::skip]
    v = wind['v'][::skip, ::skip]
    lat = wind['lat'][::skip]
    lon = wind['lon'][::skip]

    try:
        ax.streamplot(lon, lat, u, v,
                      density=1.5, linewidth=0.5, color='black',
                      transform=ccrs.PlateCarree(), zorder=4)
    except Exception as e:
        logger.debug(f"Streamline overlay failed: {e}")


def _extract_storm_sid_from_filename(fn):
    """
    Derive a storm SID (e.g. '12L') from a HAFS-style nest GRIB2
    filename. Tries the filename prefix first (matches ``12l.YYYY...``)
    and falls back to the immediate parent directory name (HAFS
    multistorm puts each storm under a SID-named subdir of COMhafs).
    Returns the uppercased SID or None.
    """
    basename = os.path.basename(fn)
    m = re.match(r'^(\d{2}[a-z])\.', basename, re.IGNORECASE)
    if m:
        return m.group(1).upper()
    parent = os.path.basename(os.path.dirname(fn))
    if re.fullmatch(r'\d{2}[A-Za-z]', parent):
        return parent.upper()
    return None


def _lookup_storm_center(sid, idate, fhr, atcf_dirs, atcf_tag, mcode):
    """
    Look up a single storm's center position and MSLP at a given FHR
    by finding + reading its per-storm ATCF. Returns
    ``(lat, lon, mslp)`` or ``(None, None, None)`` on any failure.
    """
    if not sid or not atcf_dirs:
        return None, None, None
    try:
        atcf_file = find_atcf_file(atcf_dirs, idate, sid,
                                    atcf_tag=atcf_tag)
        if not atcf_file:
            return None, None, None
        atcf_df = read_atcf(atcf_file, model_id=mcode)
        if atcf_df is None or atcf_df.empty:
            atcf_df = read_atcf(atcf_file)
        if atcf_df is None or atcf_df.empty:
            return None, None, None
        row = atcf_df[atcf_df['fhr'] == fhr]
        if row.empty:
            return None, None, None
        return (float(row.iloc[0]['lat']),
                float(row.iloc[0]['lon']),
                float(row.iloc[0]['mslp']))
    except Exception as e:
        logger.debug(f"_lookup_storm_center failed for sid={sid}: {e}")
        return None, None, None


def _discover_nest_outlines(parent_grib_path, fhr, idate=None,
                             fhrfmt='%03d', is_mstorm=False,
                             atcf_dirs=None, atcf_tag=None, mcode=None):
    """
    For a parent-domain panel, find every per-storm nest GRIB2 file
    sitting alongside it for the same FHR and return the data needed
    to draw each nest's defined-region outline.

    The nest grid is typically rotated relative to lat/lon (or otherwise
    non-axis-aligned), so cfgrib presents it on a covering axis-aligned
    rectangle with NaN halo padding. ~44% of cells in a typical HAFS
    multistorm nest are halo NaN, which is why we trace the validity
    mask rather than drawing the bounding rectangle.

    Parameters
    ----------
    parent_grib_path : str
        Resolved path to the parent-domain GRIB2 we're plotting. The
        directory is searched for sibling nest files via
        ``_NEST_TOKEN_RE``.
    fhr : int
        Forecast hour, used to scope the glob to this specific FHR.
    idate : str, optional
        Cycle date (YYYYMMDDHH). When provided, included in the glob
        pattern to keep cross-cycle leftover GRIB2 files out of the
        results (relevant especially for the multistorm sibling sweep
        below).
    fhrfmt : str
        Format spec for the FHR (default ``'%03d'``).
    is_mstorm : bool
        When True, also search sibling subdirectories of the parent's
        directory. The HAFS multistorm workflow puts each storm's
        files in a per-storm subdir under ``COMhafs``, and each
        per-storm GPLOT invocation only sees its own subdir as
        ``IDIR`` (so the 00L "fake storm" pass would otherwise find
        zero nests on its d01 panel). Defaults to False for
        single-storm runs.

    Returns
    -------
    list of (str, np.ndarray, np.ndarray, np.ndarray)
        ``(label, lat_1d, lon_1d, valid_mask_2d)`` per nest. ``label``
        is the matched nest token from the filename (``storm1``,
        ``storm2``, ``d03``, ...). Empty list if no nest files are
        found or all opens fail.
    """
    if parent_grib_path is None:
        return []

    parent_dir = os.path.dirname(parent_grib_path)
    fhr_str = fhrfmt % fhr

    # Build the list of directories to search. Always include the
    # parent's own dir; in multistorm mode, also include every sibling
    # subdirectory (the per-storm subdirs under COMhafs).
    search_dirs = [parent_dir]
    if is_mstorm:
        root = os.path.dirname(parent_dir)
        if root and os.path.isdir(root):
            for entry in sorted(os.listdir(root)):
                sib = os.path.join(root, entry)
                if os.path.isdir(sib) and os.path.abspath(sib) != \
                        os.path.abspath(parent_dir):
                    search_dirs.append(sib)

    # Glob each search dir for GRIB2 files at this FHR. When idate is
    # supplied (operational path), require it in the filename so a
    # sibling dir holding a stale prior-cycle file doesn't pollute the
    # results.
    glob_pat = (f"*{idate}*f{fhr_str}*.grb2" if idate
                else f"*f{fhr_str}*.grb2")
    candidates = []
    for d in search_dirs:
        candidates.extend(sorted(glob.glob(os.path.join(d, glob_pat))))

    # De-dupe while preserving order (a sibling dir could be a symlink
    # back into parent_dir).
    seen = set()
    candidates = [c for c in candidates
                  if not (c in seen or seen.add(c))]

    # Filter to nest-token matches (storm1, storm2, d03, ...) and drop
    # companion *.sat.* satellite bundles.
    nest_files = [
        f for f in candidates
        if _NEST_TOKEN_RE.search(os.path.basename(f))
        and '.sat.' not in os.path.basename(f)
    ]

    # Diagnostic logging at WARNING level (visible without -v) for
    # operational multistorm runs. Single line per call summarizes the
    # full pipeline so a missing-overlay case is debuggable from the
    # spawn log alone.
    if is_mstorm:
        logger.warning(
            f"nest discovery [fhr={fhr:03d}, is_mstorm=True]: "
            f"walked {len(search_dirs)} dir(s), "
            f"{len(candidates)} candidate(s), "
            f"{len(nest_files)} after filter. "
            f"search_dirs={search_dirs} "
            f"nest_files={[os.path.basename(f) for f in nest_files]}")

    if not nest_files:
        return []

    # Try a small set of cheap 2D filters in order; the first one that
    # opens with data wins. NaN halo pattern is the same across fields
    # for a given nest, so the choice doesn't affect the resulting
    # mask shape.
    _MASK_FILTERS = [
        {'typeOfLevel': 'meanSea'},
        {'typeOfLevel': 'heightAboveGround', 'level': 2},
        {'typeOfLevel': 'surface', 'stepType': 'instant'},
    ]

    outlines = []
    for fn in nest_files:
        ds = None
        for filt in _MASK_FILTERS:
            try:
                cand = xr.open_dataset(
                    fn, engine='cfgrib',
                    backend_kwargs={'filter_by_keys': filt,
                                    'errors': 'ignore',
                                    'indexpath': ''})
                if cand.data_vars:
                    ds = cand
                    break
            except Exception:
                continue
        if ds is None:
            logger.warning(f"nest outline: no usable 2D field in {fn}")
            continue

        try:
            field = next(iter(ds.data_vars.values())).values
            lat = ds['latitude'].values
            lon = ds['longitude'].values
            mask = np.isfinite(field).astype(np.uint8)
            if int(mask.sum()) < 100:
                logger.warning(
                    f"nest outline: <100 valid points in {fn}; skipping")
                continue
            m = _NEST_TOKEN_RE.search(os.path.basename(fn))
            label = m.group(1).lower() if m else 'nest'
            # Per-storm position lookup so callers can drop an L
            # marker + PMIN inside each nest box. SID derived from
            # the GRIB filename prefix (or the parent dir name as a
            # fallback); position read from the storm's own ATCF.
            sid = _extract_storm_sid_from_filename(fn)
            c_lat, c_lon, c_mslp = _lookup_storm_center(
                sid, idate, fhr, atcf_dirs, atcf_tag, mcode)
            outlines.append((label, sid, lat, lon, mask,
                             c_lat, c_lon, c_mslp))
        except Exception as e:
            logger.warning(f"nest outline: could not read {fn}: {e}")
            continue

    return outlines


def _draw_nest_outlines(ax, nests, color='black', linestyle='--',
                        linewidth=1.5, label_storms=False,
                        draw_markers=True):
    """
    Overlay each nest's defined-region boundary on a cartopy axis as
    a dashed polyline at the 0.5 isoline of the validity mask.

    Cartopy auto-clips the contour when a nest has moved outside the
    parent panel's extent, so off-panel nests just don't draw. Any
    contour failure is logged and the nest skipped — the panel
    completes either way.

    Parameters
    ----------
    ax
        Matplotlib axes with a cartopy projection.
    nests : list of tuples
        Output of ``_discover_nest_outlines``. Each tuple is
        ``(label, sid, lat_1d, lon_1d, valid_mask_2d, c_lat, c_lon,
        c_mslp)``. The last three fields drive the in-nest L marker
        and are None when the storm's ATCF lookup failed.
    color, linestyle, linewidth
        Pass-throughs to ``ax.contour``. Defaults match the user's
        "dashed black" requested style.
    label_storms : bool
        If True, drop the nest's filename token (e.g. ``storm1``) as
        small text at the centroid of the defined region — useful for
        multistorm debugging.
    draw_markers : bool
        If True (default), draw an L marker + PMIN value at the
        ATCF-reported center of each nest. Mirrors the primary-storm
        marker style so the multistorm d01 panel shows every active
        storm's L. Silently skipped per-nest when the ATCF lookup
        returned None.
    """
    # White-halo path effect so the dashed outline reads on every
    # background -- dark IR cold tops, busy reflectivity, and light
    # surfaces all give the black line enough contrast via the halo.
    # Mirrors the convention already used by _draw_tc_low_marker for
    # the L marker. Halo width is chosen so the black line remains
    # the dominant visual element: just ~0.5pt of white on each side,
    # enough for contrast against dark fills without making the dashes
    # read as white framed by black. Lazy-import patheffects to avoid
    # touching it on runs that don't draw nests.
    import matplotlib.patheffects as pe
    halo_effect = [pe.withStroke(linewidth=linewidth + 1.5,
                                  foreground='white'),
                   pe.Normal()]

    for entry in nests:
        # Tolerate the legacy 4-tuple shape in case any caller hasn't
        # been migrated; the in-nest marker just doesn't draw there.
        if len(entry) == 4:
            label, lat, lon, mask = entry
            sid = c_lat = c_lon = c_mslp = None
        else:
            label, sid, lat, lon, mask, c_lat, c_lon, c_mslp = entry
        try:
            cs = ax.contour(lon, lat, mask, levels=[0.5],
                            colors=color, linestyles=linestyle,
                            linewidths=linewidth,
                            transform=ccrs.PlateCarree(), zorder=8)
            # Apply the halo to the contour's line collections.
            # Matplotlib >= 3.8 returns a ContourSet that's itself
            # collection-like; older versions expose `.collections`.
            try:
                for coll in cs.collections:
                    coll.set_path_effects(halo_effect)
            except AttributeError:
                cs.set_path_effects(halo_effect)
        except Exception as e:
            logger.warning(
                f"nest outline: contour failed for {label}: {e}")
            continue
        if draw_markers and c_lat is not None and c_lon is not None:
            _draw_tc_low_marker(ax, c_lat, c_lon, c_mslp)
        if label_storms:
            ys, xs = np.where(mask > 0)
            if ys.size:
                ax.text(float(lon[int(xs.mean())]),
                        float(lat[int(ys.mean())]),
                        sid or label,
                        transform=ccrs.PlateCarree(),
                        fontsize=8, color=color, zorder=9,
                        ha='center', va='center',
                        fontweight='bold')


def _draw_tc_low_marker(ax, tc_lat, tc_lon, mslp_val):
    """
    Draw a single 'L' marker at the ATCF-reported TC center.

    This replaces the old field-scan H/L detector which produced noisy
    output on high-resolution grids. The ATCF A-deck already gives us
    the authoritative center position and minimum sea-level pressure
    for the storm, so we use those values directly.

    A white path-effect outline keeps the red 'L' and PMIN readable
    on colorful backgrounds (e.g., reflectivity core colors).
    """
    if tc_lat is None or tc_lon is None:
        return

    import matplotlib.patheffects as pe
    outline = [pe.withStroke(linewidth=1.8, foreground='white')]

    ax.text(tc_lon, tc_lat, 'L', fontsize=11, fontweight='bold',
            color='red', ha='center', va='center',
            transform=ccrs.PlateCarree(), zorder=12,
            path_effects=outline)
    if mslp_val is not None and mslp_val > 0:
        ax.text(tc_lon, tc_lat - 0.5, f'{int(mslp_val)}',
                fontsize=8, fontweight='bold', color='red',
                ha='center', va='top',
                transform=ccrs.PlateCarree(), zorder=12,
                path_effects=[pe.withStroke(linewidth=1.5, foreground='white')])


def _draw_atcf_track(ax, atcf_df, current_fhr):
    """Draw ATCF forecast track line on the map."""
    if atcf_df is None or atcf_df.empty:
        return

    # Plot past track (up to current fhr)
    past = atcf_df[atcf_df['fhr'] <= current_fhr]
    if len(past) > 1:
        ax.plot(past['lon'].values, past['lat'].values,
                '-', color='black', linewidth=1.5,
                transform=ccrs.PlateCarree(), zorder=8)
        for _, row in past.iterrows():
            add_storm_marker(ax, row['lat'], row['lon'],
                             intensity=row.get('vmax', None), label='')

    # Plot future track (after current fhr)
    future = atcf_df[atcf_df['fhr'] > current_fhr]
    if len(future) > 0:
        # Connect current to future
        all_future = atcf_df[atcf_df['fhr'] >= current_fhr]
        if len(all_future) > 1:
            ax.plot(all_future['lon'].values, all_future['lat'].values,
                    '--', color='gray', linewidth=1.0,
                    transform=ccrs.PlateCarree(), zorder=7)


# ---------------------------------------------------------------------------
# GRIB2 file discovery
# ---------------------------------------------------------------------------

def find_grib_files(idir, idate, fhr, dsource, domain, itag='', ext='.grb2',
                    fhrfmt='%03d'):
    """
    Find GRIB2 input files for a given forecast hour.

    Searches for files matching common naming conventions used by
    HAFS, HWRF, GFS, etc.

    Parameters
    ----------
    idir : str
        Input directory.
    idate : str
        Initialization date (YYYYMMDDHH).
    fhr : int
        Forecast hour.
    dsource : str
        Data source name.
    domain : str
        Domain name.
    itag : str
        Input file tag/prefix.
    ext : str
        File extension.
    fhrfmt : str
        Format string for forecast hour.

    Returns
    -------
    str or None
        Path to GRIB2 file, or None if not found.
    """
    fhr_str = fhrfmt % fhr

    # Determine nest level
    nest = get_nest_number(domain)

    # Build search patterns
    patterns = []

    # HAFS-style: {idir}/{idate}/{itag}...f{fhr}.grb2
    if itag:
        patterns.append(os.path.join(idir, idate,
                                     f"{itag}*{fhr_str}*{ext}"))
        patterns.append(os.path.join(idir, idate,
                                     f"*{itag}*{fhr_str}*{ext}"))

    # In idate subdirectory
    patterns.append(os.path.join(idir, idate, f"*f{fhr_str}*{ext}"))
    patterns.append(os.path.join(idir, idate, f"*{fhr_str}*{ext}"))

    # Flat directory with idate in filename
    patterns.append(os.path.join(idir, f"*{idate}*f{fhr_str}*{ext}"))
    patterns.append(os.path.join(idir, f"*{idate}*.f{fhr_str}{ext}"))

    # Domain-token classification uses the module-level _NEST_TOKEN_RE /
    # _PARENT_TOKEN_RE compiled near the top of this file (shared with
    # the nest-outline helpers below).
    for pat in patterns:
        matches = sorted(glob.glob(pat))
        # Filter out non-atm files (e.g., .sat. files)
        matches = [m for m in matches if '.sat.' not in os.path.basename(m)]
        if matches:
            if nest == 3:
                nest_matches = [m for m in matches
                                if _NEST_TOKEN_RE.search(
                                    os.path.basename(m))]
                if nest_matches:
                    return nest_matches[0]
            else:
                # Parent domain: prefer files with an explicit parent/d01
                # token, falling back to anything that isn't a nested-grid
                # file.
                parent_matches = [m for m in matches
                                  if _PARENT_TOKEN_RE.search(
                                      os.path.basename(m))]
                if parent_matches:
                    return parent_matches[0]
                non_nest_matches = [m for m in matches
                                    if not _NEST_TOKEN_RE.search(
                                        os.path.basename(m))]
                if non_nest_matches:
                    return non_nest_matches[0]
            return matches[0]

    return None


# ---------------------------------------------------------------------------
# ATCF helpers
# ---------------------------------------------------------------------------

def get_tc_position(atcf_df, fhr):
    """
    Get TC position at a specific forecast hour from ATCF data.

    Returns
    -------
    tuple
        (lat, lon, vmax, mslp) or (None, None, None, None).
    """
    if atcf_df is None or atcf_df.empty:
        return None, None, None, None

    row = atcf_df[atcf_df['fhr'] == fhr]
    if row.empty:
        return None, None, None, None

    row = row.iloc[0]
    return (row['lat'], row['lon'],
            row.get('vmax', None), row.get('mslp', None))


def find_atcf_file(atcf_dir, idate, sid, atcf_tag=''):
    """
    Find the ATCF A-deck file for a given cycle and storm.

    When ``atcf_tag`` is supplied (from the namelist ATCF*_TAG), files
    whose basename contains the tag are preferred over siblings -- so
    e.g. the merged ``hfsb_multistorm.trak.atcfunix`` wins over
    ``.parent.trak.atcfunix`` or ``.storm2.trak.atcfunix``.  Per-fhr
    (``.f000``/``.f003``/...) and ``.all`` / ``.orig`` splits are
    excluded.

    Parameters
    ----------
    atcf_dir : str
        Directory to search.  May be a single path or a list/tuple of
        paths (searched in order); paths that don't exist are skipped.
    idate : str
    sid : str
    atcf_tag : str, optional

    Returns
    -------
    str or None
    """
    if isinstance(atcf_dir, (list, tuple)):
        dirs = [d for d in atcf_dir if d]
    else:
        dirs = [atcf_dir] if atcf_dir else []

    sid_lower = sid.lower()
    patterns = [
        f"*{sid_lower}*{idate}*trak*",
        f"*{sid_lower}*{idate}*atcf*",
        f"*{sid_lower}*{idate}*",
    ]

    def _rank(path):
        bn = os.path.basename(path)
        tag_match = 0 if (atcf_tag and atcf_tag in bn) else 1
        parent_penalty = 1 if '.parent.' in bn else 0
        return (tag_match, parent_penalty, bn)

    _FHR_RE = re.compile(r'\.f\d{3,4}$')

    for adir in dirs:
        if not adir or not os.path.isdir(adir):
            continue
        for pat in patterns:
            matches = glob.glob(os.path.join(adir, pat))
            matches = [m for m in matches
                       if not m.endswith(('.grb2', '.grb', '.idx',
                                          '.grib2', '.orig'))]
            matches = [m for m in matches
                       if not os.path.basename(m).endswith('.all')
                       and not _FHR_RE.search(os.path.basename(m))]
            if matches:
                matches.sort(key=_rank)
                return matches[0]

    return None


# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------

def parse_args():
    parser = argparse.ArgumentParser(
        description='GPLOT Maps Module - geographic contour maps')
    parser.add_argument('--idate', required=True,
                        help='Initialization date (YYYYMMDDHH)')
    parser.add_argument('--sid', required=True,
                        help='Storm ID (e.g., 13L)')
    parser.add_argument('--domain', default='d03',
                        help='Domain name (default: d03)')
    parser.add_argument('--tier', default='Tier1',
                        help='Tier level (default: Tier1)')
    parser.add_argument('--master-nml', default='namelist.master.default',
                        help='Path to master namelist')
    parser.add_argument('--force', action='store_true',
                        help='Force regeneration of all plots')
    parser.add_argument('--ensid', default='',
                        help='Ensemble member ID')
    parser.add_argument('--modelid', default='',
                        help='Model ID override')
    parser.add_argument('--idir', default=None,
                        help='Input directory override')
    parser.add_argument('--odir', default=None,
                        help='Output directory override')
    parser.add_argument('--atcf-dir', default=None,
                        help='ATCF directory override')
    parser.add_argument('-v', '--verbose', action='count', default=0,
                        help='Increase verbosity (-v or -vv)')
    return parser.parse_args()


def main():
    args = parse_args()

    # Set up logging
    log_level = logging.WARNING
    if args.verbose >= 2:
        log_level = logging.DEBUG
    elif args.verbose >= 1:
        log_level = logging.INFO
    logging.basicConfig(level=log_level,
                        format='%(asctime)s %(name)s %(levelname)s: %(message)s')

    idate = args.idate
    sid = args.sid.upper()
    domain = args.domain
    tier = args.tier

    logger.info(f"GPLOT Maps: idate={idate}, sid={sid}, domain={domain}, "
                f"tier={tier}")

    # ---- 1. Read master namelist ----
    nml = read_master_namelist(args.master_nml)
    configure_cartopy(nml.get('CARTOPY_DIR'))
    dsource = nml.get('DSOURCE', 'HAFS')
    expt = nml.get('EXPT', dsource)
    mcode = nml.get('MCODE', dsource[:4])
    idir = args.idir or nml.get('IDIR', '.')
    itag = nml.get('ITAG', '')
    if isinstance(itag, list):
        itag = itag[0] if itag else ''
    ext = nml.get('EXT', '.grb2')
    if isinstance(ext, list):
        ext = ext[0] if ext else '.grb2'
    fhrfmt_raw = nml.get('FMT_HR', 3)
    try:
        ndigits = int(fhrfmt_raw)
        fhrfmt = f'%0{ndigits}d'
    except (ValueError, TypeError):
        fhrfmt = '%03d'
    init_hr = nml.get('INIT_HR', 0)
    fnl_hr = nml.get('FNL_HR', 126)
    dt = nml.get('DT', 3)
    gplot_dir = nml.get('GPLOT_DIR', os.environ.get('GPLOT_DIR', '.'))

    # Output directory. Tier subdir dropped -- all tiers' figures
    # live under <domain>/ together (tier still selects the right
    # tier-specific namelist via resolve_namelist_path below).
    odir = args.odir or nml.get('ODIR', '.')
    odir_type = nml.get('ODIR_TYPE', 0)
    if odir_type == 1:
        odir_full = os.path.join(odir, domain)
    else:
        odir_full = os.path.join(odir, expt, idate, domain)
    os.makedirs(odir_full, exist_ok=True)
    # Match spawn_maps naming: only hwrf/d03 carry a storm tag.
    storm_tag = f'.{sid}' if is_storm_named_filename(domain) else ''
    status_file = os.path.join(odir_full, f'status.{domain}.{tier}{storm_tag}.log')
    _write_status(status_file, 'working')

    # ATCF directories.  Prefer ATCF2_DIR (higher-res / merged output
    # from the experiment) and fall back to ATCF1_DIR.  A --atcf-dir
    # CLI override takes precedence over both.
    if args.atcf_dir:
        atcf_dirs = [args.atcf_dir]
    else:
        atcf_dirs = [d for d in (nml.get('ATCF2_DIR', ''),
                                 nml.get('ATCF1_DIR', '')) if d]
    atcf_tag = nml.get('ATCF2_TAG', nml.get('ATCF1_TAG', ''))
    bdeck_dir = nml.get('BDECK_DIR', nml.get('BDECK2_DIR', ''))

    # ---- 2. Read maps namelist ----
    maps_nml_path = resolve_namelist_path(gplot_dir, 'maps', expt,
                                           domain, tier)
    if maps_nml_path is None:
        logger.error("Could not find maps namelist")
        _write_status(status_file, 'failed')
        sys.exit(1)

    logger.info(f"Maps namelist: {maps_nml_path}")
    recipes = read_maps_namelist(maps_nml_path)
    logger.info(f"Found {len(recipes)} enabled plot recipes")

    # ---- 3. Read ATCF data ----
    atcf_file = find_atcf_file(atcf_dirs, idate, sid, atcf_tag=atcf_tag)
    atcf_df = None
    if atcf_file:
        logger.info(f"ATCF file: {atcf_file}")
        atcf_df = read_atcf(atcf_file, model_id=mcode)
        if atcf_df is None or atcf_df.empty:
            logger.info(f"No rows for model={mcode}, trying without filter")
            atcf_df = read_atcf(atcf_file)
    else:
        logger.warning("No ATCF file found")

    # Parse storm info for long SID. Priority chain (matches the
    # rest of GPLOT): ATCF filename's '<name><sid>' prefix (legacy
    # NCL convention) -> B-deck column-28 storm_name -> A-deck
    # column-28 storm_name -> bare sid lowercase.
    snum = sid[:2] if len(sid) >= 3 else '00'
    basin = sid[2:] if len(sid) >= 3 else 'L'
    bdeck_df_for_name = None
    if bdeck_dir and os.path.isdir(bdeck_dir):
        _basin_map = {'l': 'al', 'e': 'ep', 'c': 'cp', 'w': 'wp',
                      's': 'sh', 'p': 'sh', 'a': 'io', 'b': 'io'}
        try:
            _basin1 = sid[2].lower()
            _basin2 = _basin_map.get(_basin1, '')
            _bdeck_path = os.path.join(bdeck_dir,
                                       f'b{_basin2}{snum}{idate[:4]}.dat')
            if os.path.isfile(_bdeck_path):
                bdeck_df_for_name = read_bdeck(_bdeck_path)
        except (IndexError, AttributeError):
            pass
    # Pass both b-deck and a-deck so derive_longsid can fall through
    # to the operational a-deck's per-cycle storm_name when the
    # b-deck has no row at idate (pre-genesis retrospective case).
    longsid = derive_longsid(atcf_file, sid, bdeck_df_for_name,
                             idate=idate, adeck_df=atcf_df)
    logger.info(f"LONGSID resolved to: {longsid}")

    # ---- 4. Get StreamlineThin factor ----
    thin_factor = load_streamline_thin(gplot_dir, dsource, domain)
    logger.info(f"Vector thinning factor: {thin_factor}")

    # DRAW_NESTS=True overlays per-storm moving-nest outlines on
    # parent-style domains (d01, atl, basin, ...). Auto-discovers
    # every storm/d03 GRIB2 file sitting next to the parent file at
    # each FHR, so multistorm runs draw multiple boxes on one panel
    # without spawn-side changes.
    #
    # IS_MSTORM=True widens the nest-file search across sibling
    # per-storm subdirs under COMhafs. The HAFS multistorm workflow
    # (exhafs_hrdgraphics.sh) runs one GPLOT invocation per storm,
    # each pointing at COMhafs/<STORMID>/ — so without the sibling
    # sweep the 00L "fake storm" pass sees zero nests on its d01
    # panel.
    draw_nests = bool(nml.get('DRAW_NESTS', False))
    is_mstorm  = bool(nml.get('IS_MSTORM', False))
    # Promoted to WARNING level: visible under the operational spawn's
    # default log config (batch_maps.sh runs python3 without -v, so the
    # logger.basicConfig WARNING default applies). One line per run
    # tells the operator at a glance whether DRAW_NESTS is in effect
    # and which mode is selected.
    # Gate on is_storm_centered (NEST=3 -> d03, alld03, core, storm),
    # NOT is_storm_named_filename. The HWRF "hwrf" outer domain is
    # storm-named for filename purposes (one TC per panel embeds
    # longsid) but is the PARENT containing d03, not the inner moving
    # nest -- so it should get nest overlays just like d01/atl/basin.
    if draw_nests and is_storm_centered(domain):
        logger.warning(f"DRAW_NESTS=True but domain={domain} is itself a "
                       f"moving nest (NEST=3); nest-outline overlay disabled.")
        draw_nests = False
    elif draw_nests:
        mode = "multistorm sibling-sweep" if is_mstorm else "single-dir"
        logger.warning(f"DRAW_NESTS=True ({mode}, IS_MSTORM={is_mstorm}): "
                       f"parent panel will overlay every per-storm nest "
                       f"outline discovered at each FHR.")
    else:
        logger.warning(f"DRAW_NESTS=False (or unset) for {domain}; "
                       f"no nest-outline overlay will be drawn.")

    # ---- 5. Build (fhr, grib_path) iteration list ----
    # Prefer the file list spawn_maps.sh prepared: it does the full
    # IDIR_OPTS directory-layout discovery (~30 variants spanning HAFS,
    # HWRF, HFSA, GFS, ECMWF, ensembles, etc.) that the in-Python
    # find_grib_files() cannot replicate. Polar/airsea already consume
    # these lists; bringing maps in line means HFSA and any other
    # spawn-supported layout works here without duplicating
    # directory-discovery logic in two places.
    #
    # Fallback to in-Python discovery when the spawn lists are absent
    # (e.g. running GPLOT_maps.py directly from the command line for dev
    # / smoke testing). That path preserves the legacy iter-by-FHR-range
    # behavior and is unchanged from before this refactor.
    iter_pairs = read_spawn_file_list(odir_full, domain, tier, sid)
    if iter_pairs is not None:
        logger.info(f"Using spawn-prepared file list: {len(iter_pairs)} FHRs")
    else:
        logger.info("No spawn file list found; falling back to "
                    "find_grib_files() per-FHR discovery")
        iter_pairs = []
        for fhr in range(init_hr, fnl_hr + 1, dt):
            gp = find_grib_files(idir, idate, fhr, dsource, domain,
                                  itag, ext, fhrfmt)
            if gp is not None:
                iter_pairs.append((fhr, gp))
        logger.info(f"find_grib_files() discovered {len(iter_pairs)} FHRs")

    n_plots = 0

    try:
        for fhr, grib_path_from_spawn in iter_pairs:
            logger.info(f"--- Processing FHR {fhr:03d} ---")

            # Get TC position for this forecast hour
            tc_lat, tc_lon, vmax, mslp_val = get_tc_position(atcf_df, fhr)

            # Compute domain bounds
            if is_storm_centered(domain):
                if tc_lat is None:
                    logger.warning(f"FHR {fhr:03d}: No TC position, skipping "
                                   "storm-centered domain")
                    continue
                bounds = get_domain_bounds(domain, tc_lat, tc_lon)
            else:
                bounds = get_domain_bounds(domain)
                # d01/hwrf parent domains return None from the registry because
                # their geographic extent varies per run (moving-nest parent grid
                # is recentered on the cyclone). Resolve them below from the
                # actual GRIB2 file after it's been opened.

            # PlottedFiles log path (used by both the fast-path on-disk gate
            # below and the per-recipe gate inside the recipe loop).
            plotted_log = os.path.join(
                odir_full,
                f'PlottedFiles.{domain}.{tier}{storm_tag}.log')

            # Fast-path on-disk gate. If every recipe's .gif is already on
            # disk, skip the entire FHR before the expensive GRIB2 open.
            # Runs regardless of --force, mirroring polar/airsea: the spawn
            # FORCE flip (triggered by recent ATCF mtime) wipes
            # PlottedFiles every loop while the model is running, and
            # without this gate every completed FHR re-renders. To
            # genuinely re-render, delete the .gif files.
            expected_ofiles = [_expected_ofile(r, longsid, fhr, idate,
                                                domain, odir_full)
                               for r in recipes]
            if expected_ofiles and all(os.path.isfile(p)
                                       for p in expected_ofiles):
                logger.info(f"FHR {fhr:03d}: all recipe figures on disk, "
                            f"skipping")
                # Re-mark in PlottedFiles so the spawn-level skip can
                # also short-circuit on the next loop.
                update_plotted_file(plotted_log, grib_path_from_spawn)
                continue

            # GRIB2 path already resolved upstream -- either pulled from
            # spawn's UnplottedFiles list (operational) or discovered via
            # the legacy find_grib_files() fallback (standalone dev).
            grib_path = grib_path_from_spawn
            if not os.path.isfile(grib_path):
                logger.warning(f"FHR {fhr:03d}: GRIB2 file missing on disk "
                               f"--> {grib_path}")
                continue

            logger.info(f"GRIB2 file: {grib_path}")

            # Check if already plotted (unless forced). Match the legacy
            # GPLOT polar/airsea naming convention so spawn_maps.sh and the
            # downstream scripts can find this file:
            # PlottedFiles.<DOMAIN>.<TIER>[.<SID>].log
            if not args.force and os.path.isfile(plotted_log):
                with open(plotted_log, 'r') as f:
                    plotted_content = f.read()
                if grib_path in plotted_content:
                    logger.info(f"FHR {fhr:03d}: Already plotted, skipping")
                    continue

            # Open GRIB2
            try:
                datasets = open_grib2(grib_path)
            except Exception as e:
                logger.error(f"FHR {fhr:03d}: Failed to open GRIB2: {e}")
                continue

            # HAFS ships simulated IR brightness temperatures in a separate
            # ``*.sat.f*.grb2`` file sitting next to the main atm file.  Try to
            # locate and open it; if present, append its datasets so recipes
            # like SIMIR can resolve via the same get_var_2d() path.
            sat_path = grib_path.replace('.atm.', '.sat.')
            if sat_path != grib_path and os.path.isfile(sat_path):
                try:
                    sat_datasets = open_sat_file(sat_path)
                    if sat_datasets:
                        datasets = list(datasets) + list(sat_datasets)
                        logger.info(f"Sat GRIB2 file: {sat_path} "
                                    f"({len(sat_datasets)} bands)")
                except Exception as e:
                    logger.warning(f"Failed to open sat file {sat_path}: {e}")

            # Resolve parent-domain bounds from the GRIB2 grid extent when the
            # domain registry didn't provide any. Use a tiny inset so cartopy
            # doesn't try to draw right at the edge.
            if bounds is None:
                grid = get_grid_info(datasets, dsource, gplot_dir)
                lat_arr = grid.get('lat')
                lon_arr = grid.get('lon')
                if lat_arr is None or lon_arr is None or len(lat_arr) == 0:
                    logger.warning(f"FHR {fhr:03d}: Cannot derive bounds from GRIB2")
                    continue
                # Convert 0..360 longitudes to -180..180 for cartopy-friendly bounds.
                lon_min = float(np.min(lon_arr))
                lon_max = float(np.max(lon_arr))
                if lon_max > 180:
                    lon_min = lon_min - 360 if lon_min > 180 else lon_min
                    lon_max = lon_max - 360 if lon_max > 180 else lon_max
                bounds = (
                    float(np.max(lat_arr)),
                    float(np.min(lat_arr)),
                    lon_min,
                    lon_max,
                )
                logger.info(f"Parent-domain bounds derived from GRIB2: {bounds}")

            # Reset the vortex-filter cache so we don't accidentally reuse a
            # smoothed cube from the previous forecast hour (the `datasets`
            # list id() also changes, but being explicit keeps memory bounded).
            _clear_vortex_cache()

            # Moving-nest outline overlay. Discovered once per FHR
            # (shared across every recipe's draw_map call so MSLP, REFD,
            # IR, etc. all show the same set of dashed boxes). Only
            # active on parent-style domains -- a d03/hwrf panel
            # showing its own outline would be redundant. For
            # multistorm runs the helper auto-discovers every per-storm
            # nest GRIB2 sitting next to the parent file, so a single
            # d01 panel can carry several nest outlines without any
            # changes to the spawn-side per-storm iteration.
            # See the matching gate-rationale comment near draw_nests
            # config read: is_storm_centered (NEST=3) is the correct
            # "this domain IS the moving nest" predicate; the HWRF
            # outer "hwrf" domain is the parent containing d03 and
            # gets the overlay.
            if draw_nests and not is_storm_centered(domain):
                nest_outlines = _discover_nest_outlines(
                    grib_path, fhr, idate=idate, fhrfmt=fhrfmt,
                    is_mstorm=is_mstorm,
                    atcf_dirs=atcf_dirs, atcf_tag=atcf_tag, mcode=mcode)
                # Bumped to WARNING so it's visible at the default log
                # level used by the operational spawn (batch_maps.sh
                # doesn't pass -v). Without this the only signal that
                # the nest overlay ran was its presence on the panel
                # itself, which is invisible when the helper returns 0.
                if nest_outlines:
                    logger.warning(f"FHR {fhr:03d}: drawing "
                                   f"{len(nest_outlines)} nest outline(s) "
                                   f"on {domain} panel")
                elif draw_nests:
                    logger.warning(f"FHR {fhr:03d}: DRAW_NESTS=True but "
                                   f"_discover_nest_outlines returned 0 "
                                   f"outline(s) for {domain}; see preceding "
                                   f"'nest discovery' WARNING for details.")
            else:
                nest_outlines = None

            # Loop over plot recipes. Track successes per-FHR so we can
            # gate the plotted-file marker on actually having produced
            # something -- a failed FHR (every recipe raised) must not be
            # marked plotted, otherwise the next run skips it and the
            # forecast hour is silently lost from output.
            #
            # Also track recipes whose .gif already existed on disk
            # (per-recipe on-disk gate). The fast-path above only fires
            # when *all* recipes are on disk; this handles the
            # partial-FHR case where some recipes succeeded last time
            # and others failed -- only the missing ones get re-rendered.
            n_recipe_plots = 0
            n_recipe_existing = 0
            for recipe in recipes:
                expected = _expected_ofile(recipe, longsid, fhr, idate,
                                           domain, odir_full)
                if os.path.isfile(expected):
                    logger.info(f"FHR {fhr:03d} {recipe['FILE_NAME']}: "
                                f"figure exists, skipping")
                    n_recipe_existing += 1
                    continue
                try:
                    ofile = draw_map(
                        recipe, datasets, dsource, bounds, fhr, idate, expt,
                        tc_lat, tc_lon, vmax, mslp_val, longsid, args.ensid,
                        gplot_dir, odir_full, domain, thin_factor, atcf_df,
                        nest_outlines=nest_outlines,
                    )
                    if ofile:
                        n_plots += 1
                        n_recipe_plots += 1
                except Exception as e:
                    logger.error(f"FHR {fhr:03d} {recipe['FILE_NAME']}: {e}",
                                 exc_info=True)

            # Mark this GRIB2 file as plotted if either (a) at least one
            # recipe was freshly produced, or (b) every recipe was either
            # freshly produced or already on disk -- i.e. the FHR is
            # fully satisfied. Failed FHRs (every recipe raised) are
            # still left unmarked so they retry next run.
            n_recipe_done = n_recipe_plots + n_recipe_existing
            if n_recipe_plots > 0 or n_recipe_done == len(recipes):
                update_plotted_file(plotted_log, grib_path)
            else:
                logger.warning(f"FHR {fhr:03d}: no plots produced; "
                               f"not marking GRIB2 as plotted")
    except Exception:
        _write_status(status_file, 'failed')
        raise

    logger.info(f"GPLOT Maps complete: {n_plots} plots generated")
    # Catch any orphan .png files left behind by transient ImageMagick
    # failures (NFS lag, etc.) and retry the conversion. If the retry
    # also fails, write status='incomplete' instead of 'complete' so
    # the workflow's status-check loop re-invokes us; the next spawn
    # iteration's on-disk fast-path will notice the missing .gif and
    # re-render that FHR, giving the convert another shot under
    # (typically) better disk conditions.
    sweep_result = sweep_orphan_pngs(odir_full)
    if sweep_result.get('still_failed', 0) > 0:
        logger.warning(
            f"GPLOT Maps: {sweep_result['still_failed']} PNG(s) still "
            f"unconverted after sweep; writing status='incomplete' to "
            f"trigger another spawn iteration.")
        _write_status(status_file, 'incomplete')
    else:
        _write_status(status_file, 'complete')


if __name__ == '__main__':
    main()
