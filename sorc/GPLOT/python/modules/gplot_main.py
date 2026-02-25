#!/usr/bin/env python
"""
gplot_main.py

Higher-level GPLOT functions: TC centre-finding and master namelist reading.

Usage:
    from modules import gplot_main as gm

Original NCL: sorc/GPLOT/ncl/GPLOT_main.ncl
"""

import datetime
import math
import os
import types

import numpy as np

try:
    import xarray as xr
    HAS_XARRAY = True
except ImportError:
    HAS_XARRAY = False

try:
    from scipy.ndimage import center_of_mass as _scipy_com
    HAS_SCIPY = True
except ImportError:
    HAS_SCIPY = False

try:
    from . import gplot_util as gu
    from . import gplot_func as gf
except ImportError:
    try:
        import gplot_util as gu
        import gplot_func as gf
    except ImportError:
        gu = None
        gf = None


# ===========================================================================
# Private helpers
# ===========================================================================

def _gc_dist_km(lat1, lon1, lat2, lon2):
    """Great-circle distance (km) between two points using the haversine formula."""
    R = 6371.
    phi1, phi2 = math.radians(lat1), math.radians(lat2)
    dphi  = math.radians(lat2 - lat1)
    dlam  = math.radians(lon2 - lon1)
    a = math.sin(dphi / 2.) ** 2 + math.cos(phi1) * math.cos(phi2) * math.sin(dlam / 2.) ** 2
    return R * 2. * math.asin(min(1., math.sqrt(a)))


def _centroid_idx(field2d, threshold, mode):
    """
    Find the (row, col) centroid index of *field2d* weighted by values that
    exceed *threshold* (mode=1) or fall below it (mode=-1).

    Returns (row_idx, col_idx) as 0-based integers, or (None, None) on failure.
    """
    if HAS_SCIPY:
        if mode == 1:
            mask = np.where(field2d >= threshold, np.abs(field2d), 0.)
        else:
            mask = np.where(field2d <= threshold, np.abs(field2d), 0.)
        if mask.sum() == 0:
            return None, None
        ycm, xcm = _scipy_com(mask)
        return int(round(ycm)), int(round(xcm))
    else:
        # Fall back to weighted mean of coordinates
        if mode == 1:
            idx = np.argwhere(field2d >= threshold)
            w   = np.abs(field2d[field2d >= threshold])
        else:
            idx = np.argwhere(field2d <= threshold)
            w   = np.abs(field2d[field2d <= threshold])
        if idx.size == 0:
            return None, None
        wtot = w.sum()
        row  = int(round((idx[:, 0] * w).sum() / wtot))
        col  = int(round((idx[:, 1] * w).sum() / wtot))
        return row, col


# ===========================================================================
# findCenter  →  find_center
# ===========================================================================

def find_center(ds, d_source, var, lev_ns, lat_arr, lonf_arr,
                flip_flag, bds, center_type, dim_names=None):
    """
    Locate the TC centre at each pressure level.

    Equivalent to NCL findCenter().  Uses filter121 smoothing then either
    the smoothed max/min position (type=0) or a centroid method (type=1).

    Parameters
    ----------
    ds          : xarray.Dataset
    d_source    : str, data source key
    var         : str, variable to use: 'AVO' | 'RVO' | 'HGT'
    lev_ns      : types.SimpleNamespace with:
                    values : 1-D float array of pressure levels
                    units  : str ('hPa' | 'Pa')
                    mf     : float, multiplier converting hPa → file units
    lat_arr     : 1-D float array of latitudes in the domain
    lonf_arr    : 1-D float array of full longitudes
    flip_flag   : bool, whether longitude was pivoted
    bds         : sequence [N, S, W, E]
    center_type : int, 0 = smoothed max/min, 1 = centroid
    dim_names   : list of str or None (auto-detect)

    Returns
    -------
    types.SimpleNamespace with:
        lat    : 1-D float array, centre latitude at each level
        lon    : 1-D float array, centre longitude at each level
        value  : 1-D float array, field value at centre
        lim    : 1-D float array, km/hPa coherence metric
        use    : 1-D bool array, True if level is part of same vortex
        levels : 1-D float array (input LEV values)
    Returns None on failure.
    """
    # --- Input validation ---
    allowed = ('AVO', 'RVO', 'HGT')
    if var not in allowed:
        print(f'ERROR: find_center: var must be one of {allowed}.')
        return None
    if center_type not in (0, 1):
        print('ERROR: find_center: center_type must be 0 or 1.')
        return None
    if not hasattr(lev_ns, 'units') or not hasattr(lev_ns, 'mf'):
        print("ERROR: find_center: lev_ns must have 'units' and 'mf' attributes.")
        return None

    if gu is None or gf is None:
        print('ERROR: find_center: gplot_util and gplot_func must be importable.')
        return None

    LEV = np.asarray(lev_ns.values, float)
    NZ  = LEV.size

    # --- Level code string (e.g. '08505000' for 850-500 hPa) ---
    lev_hpa = gu.level_convert(LEV.tolist(), lev_ns.units, 'integer', 'hPa')
    if NZ > 1:
        lev_code = (f'{int(min(lev_hpa)):04d}'
                    f'{int(max(lev_hpa)):04d}')
    else:
        lev_code = gu.level_convert([LEV[0]], lev_ns.units, 'string', 'hPa')
        lev_code = lev_code[0] if isinstance(lev_code, list) else str(lev_code)

    # --- Detect dimension names ---
    if dim_names is None:
        if HAS_XARRAY and hasattr(ds, 'dims'):
            dim_names = list(ds.dims)
        else:
            dim_names = []

    # --- Set mode parameters ---
    if var in ('AVO', 'RVO'):
        max_min = 'max'; b_mode = 1
        N = 71 if center_type == 0 else 25
    else:   # HGT
        max_min = 'min'; b_mode = -1
        N = 51 if center_type == 0 else 25

    # --- Read the field ---
    flags = {'flip': flip_flag, 'rm_vortex': False}
    if NZ > 1:
        result = gf.get_var3d(ds, d_source, [var, lev_code],
                              dim_names, bds, lev_ns.mf, flags,
                              lonf_arr, piv_lon=180.)
        if result is None:
            print('ERROR: find_center: get_var3d returned None.')
            return None
        V = result.data                 # shape (nlevs, nlat, nlon)
        lat_coord = result.lat
        lon_coord = result.lon
        # Ensure levels go from low to high (ascending pressure = high to low alt)
        flip_lev = False
        if NZ > 1 and LEV[0] > LEV[-1]:
            V   = V[::-1]
            LEV = LEV[::-1]
            flip_lev = True
    else:
        result = gf.get_var2d(ds, d_source, [var, lev_code],
                              dim_names, bds, lev_ns.mf, flags,
                              lonf_arr, piv_lon=180.)
        if result is None:
            print('ERROR: find_center: get_var2d returned None.')
            return None
        V = result.data[np.newaxis]     # shape (1, nlat, nlon)
        lat_coord = result.lat
        lon_coord = result.lon
        flip_lev = False

    # --- Restrict to ±2° around domain centre for smoothing ---
    lat_mid = 0.5 * (float(bds[0]) + float(bds[1]))
    lon_mid = 0.5 * (float(bds[2]) + float(bds[3]))
    lat_mask = (lat_coord >= lat_mid - 2.) & (lat_coord <= lat_mid + 2.)
    lon_mask = (lon_coord >= lon_mid - 2.) & (lon_coord <= lon_mid + 2.)
    V2    = V[:, lat_mask, :][:, :, lon_mask]
    lat2  = lat_coord[lat_mask]
    lon2  = lon_coord[lon_mask]
    ny2, nx2 = V2.shape[1], V2.shape[2]

    # --- Apply filter121 smoothing ---
    Vs = V2.copy()
    for k in range(NZ):
        Vs[k] = gu.filter121(V2[k], N, False)

    # --- Allocate output arrays ---
    cntr_lat   = np.full(NZ, np.nan)
    cntr_lon   = np.full(NZ, np.nan)
    cntr_val   = np.full(NZ, np.nan)
    cntr_lim   = np.zeros(NZ)
    cntr_use   = np.ones(NZ, bool)

    # --- Find centres ---
    for k in range(NZ):
        plane = Vs[k]

        if center_type == 0:
            if max_min == 'max':
                flat_idx = int(np.nanargmax(plane))
                cntr_val[k] = float(np.nanmax(plane))
            else:
                flat_idx = int(np.nanargmin(plane))
                cntr_val[k] = float(np.nanmin(plane))
            row, col = divmod(flat_idx, nx2)

        else:   # centroid
            if max_min == 'max':
                fmax = np.nanmax(plane); fmin = np.nanmin(plane)
                A    = fmax - 0.20 * abs(fmax - fmin)
                cntr_val[k] = float(np.nanmax(np.nanmax(Vs[k], axis=1), axis=0))
            else:
                fmax = np.nanmax(plane); fmin = np.nanmin(plane)
                A    = fmin + 0.20 * abs(fmax - fmin)
                cntr_val[k] = float(np.nanmin(np.nanmin(Vs[k], axis=1), axis=0))
            row, col = _centroid_idx(plane, A, b_mode)
            if row is None:
                continue

        if 0 <= row < ny2 and 0 <= col < nx2:
            cntr_lat[k] = float(lat2[row])
            cntr_lon[k] = float(lon2[col])

    # --- Check vertical coherence (≤ 1 km/hPa between adjacent levels) ---
    for k in range(NZ):
        if k == 0:
            cntr_use[k] = True
            cntr_lim[k] = 0.
        elif k == 1:
            if not np.isnan(cntr_lat[k]) and not np.isnan(cntr_lat[k - 1]):
                dist = _gc_dist_km(cntr_lat[k], cntr_lon[k],
                                    cntr_lat[k-1], cntr_lon[k-1])
                dlev = abs(LEV[k] - LEV[k-1]) / lev_ns.mf
                lim1 = dist / max(dlev, 1e-9)
            else:
                lim1 = 999.
            cntr_lim[k] = lim1
            cntr_use[k] = (lim1 <= 1.) and cntr_use[k - 1]
        else:
            if not np.isnan(cntr_lat[k]) and not np.isnan(cntr_lat[k - 1]):
                d1 = _gc_dist_km(cntr_lat[k], cntr_lon[k],
                                   cntr_lat[k-1], cntr_lon[k-1])
                lim1 = d1 / max(abs(LEV[k] - LEV[k-1]) / lev_ns.mf, 1e-9)
            else:
                lim1 = 999.
            if not np.isnan(cntr_lat[k]) and not np.isnan(cntr_lat[k - 2]):
                d2 = _gc_dist_km(cntr_lat[k], cntr_lon[k],
                                   cntr_lat[k-2], cntr_lon[k-2])
                lim2 = d2 / max(abs(LEV[k] - LEV[k-2]) / lev_ns.mf, 1e-9)
            else:
                lim2 = 999.
            cntr_lim[k] = lim1
            cntr_use[k] = ((lim1 <= 1. or lim2 <= 1.)
                           and cntr_use[k-1] and cntr_use[k-2])

    # --- Flip vertical back if needed ---
    if flip_lev:
        cntr_lat = cntr_lat[::-1]
        cntr_lon = cntr_lon[::-1]
        cntr_val = cntr_val[::-1]
        cntr_lim = cntr_lim[::-1]
        cntr_use = cntr_use[::-1]
        LEV      = LEV[::-1]

    return types.SimpleNamespace(
        lat=cntr_lat, lon=cntr_lon,
        value=cntr_val, lim=cntr_lim, use=cntr_use,
        levels=LEV)


# ===========================================================================
# Read_Master_Namelist  →  read_master_namelist
# ===========================================================================

def read_master_namelist(nfile):
    """
    Read the GPLOT master namelist file and return a SimpleNamespace
    containing all parsed configuration fields.

    Parameters
    ----------
    nfile : str, path to the namelist file

    Returns
    -------
    types.SimpleNamespace with all fields as Python-typed attributes.
    Returns None if the file cannot be read.
    """
    if not os.path.isfile(nfile):
        print(f'ERROR: read_master_namelist: file not found: {nfile}')
        return None

    with open(nfile) as fh:
        lines = [l.rstrip('\n') for l in fh]

    def _get(key, default, conv=str, fallback_keys=None):
        """Extract scalar value for *key* from namelist lines."""
        search_keys = [key] + (fallback_keys or [])
        for sk in search_keys:
            for line in lines:
                if f'{sk} =' in line:
                    parts = line.split('=', 1)
                    raw = parts[1].strip() if len(parts) == 2 else ''
                    if raw:
                        try:
                            return conv(raw)
                        except (ValueError, TypeError):
                            return default
        return default

    def _getbool(key, default, fallback_keys=None):
        raw = _get(key, None, str, fallback_keys)
        if raw is None:
            return default
        return raw.strip().lower() in ('true', '1', 'yes')

    def _getlist(key, default, fallback_keys=None):
        """Extract list value (space-separated RHS) for *key*."""
        search_keys = [key] + (fallback_keys or [])
        for sk in search_keys:
            for line in lines:
                if f'{sk} =' in line:
                    parts = line.split('=', 1)
                    if len(parts) == 2:
                        items = parts[1].strip().split()
                        return items if items else default
        return default

    nml = types.SimpleNamespace()

    # a. DOMAIN
    nml.DOMAIN = _get('DOMAIN', 'atl')
    # b. TIER
    nml.TIER = _get('TIER', 'Tier1')
    # c. DSOURCE
    nml.DSOURCE = _get('DSOURCE', 'HAFS')
    # d. MACHINE / SYS_ENV
    nml.MACHINE = _get('MACHINE', _get('SYS_ENV', 'JET'))
    nml.SYS_ENV = _get('SYS_ENV', 'JET')
    # e. IDATE
    nml.IDATE = _get('IDATE', datetime.datetime.now().strftime('%Y%m%d%H'))
    # f. SID
    nml.SID = _get('SID', 'NONE')
    # g. ENSID
    nml.ENSID = _get('ENSID', '')
    # h. MODELID (from MID key)
    nml.MODELID = _get('MID', '')
    # i. ATCF_REQD
    nml.ATCF_REQD = _getbool('ATCF_REQD', True)
    # j. EXPT
    nml.EXPT = _get('EXPT', 'HWRF_Forecast')
    # k. IDIR
    nml.IDIR = _get('IDIR', '.')
    # l. ITAG
    nml.ITAG = _get('ITAG', '')
    # m. EXT
    nml.EXT = _get('EXT', '')
    # n. ODIR
    nml.ODIR = _get('ODIR', '.')
    # o. ODIR_TYPE
    nml.ODIR_TYPE = _get('ODIR_TYPE', 0, int)
    # Derived ODIR subdirs
    nml.ODIR_ADECK = nml.ODIR.rstrip('/') + '/adeck/'
    # p. INIT_HR
    nml.INIT_HR = _get('INIT_HR', 0, lambda x: int(float(x)))
    # q. FNL_HR
    nml.FNL_HR = _get('FNL_HR', 126, lambda x: int(float(x)))
    # r. FMT_HR  (original stored as digit count, e.g. "3" → "%03d")
    _fmt_raw = _get('FMT_HR', '3')
    try:
        nml.FMT_HR = f'%0{int(_fmt_raw)}d'
    except ValueError:
        nml.FMT_HR = '%03d'
    # s. DT
    nml.DT = _get('DT', 3, lambda x: int(float(x)))
    # t. IS_MSTORM
    nml.IS_MSTORM = _getbool('IS_MSTORM', False)
    # u. DO_RMWHITE
    nml.DO_RMWHITE = _getbool('DO_RMWHITE', False)
    # v. DO_SRCLBL
    nml.DO_SRCLBL = _getbool('DO_SRCLBL', False)
    # w. PIV  (stored as MAP_PIV)
    nml.MAP_PIV = _get('PIV', 180, float)
    # x. DO_CONVERTGIF
    nml.DO_CONVERTGIF = _getbool('DO_CONVERTGIF', False)
    # y. NMAX_MAPS  (stored as MAP_NMAX)
    nml.MAP_NMAX = _get('NMAX_MAPS', 100, int)
    # z. DO_TITLES
    nml.DO_TITLES = _getbool('DO_TITLES', True)
    # aa. DO_DISCLAIMER
    nml.DO_DISCLAIMER = _getbool('DO_DISCLAIMER', True)

    # --- STATS-module options ---
    nml.ATCF1_DIR = _get('ATCF1_DIR', 'MISSING')
    nml.ATCF1_TAG = _get('ATCF1_TAG', 'MISSING')
    nml.ATCF2_DIR = _get('ATCF2_DIR', 'MISSING')
    nml.ATCF2_TAG = _get('ATCF2_TAG', 'MISSING')
    nml.ADECK_DIR = _get('ADECK_DIR', 'MISSING')
    nml.BDECK_DIR = _get('BDECK_DIR', _get('BDECK2_DIR', 'MISSING'))

    # MCODE and derived variants
    nml.MCODE  = _get('MCODE', 'HWRF')
    nml.MCODEV = 'GFSO' if nml.MCODE == 'AVNO' else nml.MCODE
    nml.MCODEI = _get('MCODEI', 'MISSING')
    nml.MCODEVI = 'GFSI' if nml.MCODEI == 'AVNI' else nml.MCODEI
    nml.MCODE12 = _get('MCODE12', 'MISSING')
    nml.MORIG   = _get('MORIG', nml.MCODE)

    # Model lists (track, intensity, pressure and their variants)
    nml.TRKmodels   = _getlist('TRKM',   ['MISSING'], ['TRKM00'])
    nml.TRKINTmodels = _getlist('TRKIM',  ['MISSING'])
    nml.INTmodels   = _getlist('INTM',   ['MISSING'], ['INTM00'])
    nml.PRSmodels   = _getlist('PRSM',   ['MISSING'], ['PRSM00'])
    nml.TRKmodelsI  = _getlist('TRKMI',  ['MISSING'], ['TRKMI00'])
    nml.INTmodelsI  = _getlist('INTMI',  ['MISSING'], ['INTMI00'])
    nml.TRKmodelsT  = _getlist('TRKMT',  ['MISSING'], ['TRKMT00'])
    nml.INTmodelsT  = _getlist('INTMT',  ['MISSING'], ['INTMT00'])
    nml.PRSmodelsT  = _getlist('PRSMT',  ['MISSING'], ['PRSMT00'])
    nml.etModels    = _getlist('ETM',    ['MISSING'])
    nml.eiModels    = _getlist('EIM',    ['MISSING'])
    nml.ltModels    = _getlist('LTM',    ['MISSING'])
    nml.liModels    = _getlist('LIM',    ['MISSING'])

    # LEAD_TIMES (space-separated integers)
    _lt_raw = _getlist('LEAD_TIMES', None)
    if _lt_raw:
        try:
            nml.LEAD_TIMES = [int(x) for x in _lt_raw]
        except ValueError:
            nml.LEAD_TIMES = list(range(0, 169, 12))
    else:
        nml.LEAD_TIMES = list(range(0, 169, 12))

    nml.nTrend      = _get('NTREND',       6,    int)
    nml.DO_INTERP   = _getbool('DO_INTERP',  False)
    nml.DO_MARKERS  = _getbool('DO_MARKERS', True)
    nml.MAX_FHR     = _get('MAX_FHR',       180,  int)
    nml.DO_FHRLABELS = _getbool('DO_FHRLABELS', True)

    # bf. DO_PDF  (overrides DO_CONVERTGIF and DO_RMWHITE)
    nml.DO_PDF = _getbool('DO_PDF', False)
    if nml.DO_PDF:
        nml.DO_CONVERTGIF = False
        nml.DO_RMWHITE    = False

    # bg. FORCE
    nml.FORCE = _getbool('FORCE', False)

    return nml
