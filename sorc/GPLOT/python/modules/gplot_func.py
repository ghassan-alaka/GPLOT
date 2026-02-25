#!/usr/bin/env python
"""
gplot_func.py

Higher-level functions for GPLOT: text annotations, ATCF reading,
coordinate dimension construction, and variable reading.

Usage:
    from modules import gplot_func as gf

Original NCL: sorc/GPLOT/ncl/GPLOT_func.ncl
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
    import matplotlib.pyplot as plt
    from matplotlib import patheffects as _pe
    HAS_MATPLOTLIB = True
except ImportError:
    HAS_MATPLOTLIB = False

try:
    from . import gplot_util as gu
except ImportError:
    try:
        import gplot_util as gu
    except ImportError:
        gu = None

# ---------------------------------------------------------------------------
# Module constants (mirrors NCL file-scope variables)
# ---------------------------------------------------------------------------
_G       = 9.80665
_PI      = math.pi
_D2R     = _PI / 180.
_OMEGA   = 0.0000727
_MS2KTS  = 1.94384449
_FVAL    = 9.96921e+36

# Half-widths (degrees) for TC-centred domain types 2-9
_TYPE_HALF = {2: 15., 3: 4., 5: 40., 6: 10., 7: 20., 8: 6., 9: 5.}

# ===========================================================================
# Private helpers
# ===========================================================================

def _just_kw(just):
    """Map NCL justification string to matplotlib ha kwarg."""
    j = (just or 'left').lower()
    ha = 'right' if j == 'right' else 'center' if j == 'center' else 'left'
    return {'ha': ha, 'va': 'center'}


def _lon_pivot(data_arr, lon_arr, piv=180.0):
    """
    Roll data and lon along the last axis so longitude starts at *piv*.
    Equivalent to NCL lonPivot(arr, piv).

    Parameters
    ----------
    data_arr : ndarray, shape (..., nlon)
    lon_arr  : 1-D ndarray, shape (nlon,)
    piv      : float

    Returns
    -------
    data_out, lon_out  (same shapes as inputs)
    """
    lon = np.asarray(lon_arr, dtype=float)
    idx = int(np.searchsorted(lon, piv, side='left'))
    return np.roll(data_arr, -idx, axis=-1), np.roll(lon, -idx)


def _parse_atcf_lat(s):
    """Parse ATCF lat string, e.g. '123N' → 12.3, '095S' → -9.5."""
    s = s.strip()
    if 'N' in s:
        return float(s.replace('N', '')) / 10.
    if 'S' in s:
        return -float(s.replace('S', '')) / 10.
    return float(s) / 10.


def _parse_atcf_lon(s):
    """Parse ATCF lon string, e.g. '2745W' → -274.5, '0900E' → 90.0."""
    s = s.strip()
    if 'W' in s:
        return -float(s.replace('W', '')) / 10.
    if 'E' in s:
        return float(s.replace('E', '')) / 10.
    return float(s) / 10.


def _col(lines, n_0based):
    """Return the n_0based-th comma-separated field from each line."""
    out = []
    for l in lines:
        parts = l.split(',')
        out.append(parts[n_0based].strip() if n_0based < len(parts) else '')
    return out


def _mwavg(V3d, lev_arr):
    """
    Pressure-weighted vertical average (replaces MWAVG Fortran external).
    V3d shape: (nlevs, nlat, nlon); lev_arr shape: (nlevs,).
    """
    lev = np.asarray(lev_arr, dtype=float)
    w   = lev / lev.sum()
    return np.sum(V3d * w[:, np.newaxis, np.newaxis], axis=0)


def _uv_to_vr(u, v, lat_arr, lon_arr):
    """
    Compute relative vorticity from (u, v) using centred finite differences.
    Equivalent to NCL uv2vr_cfd(u, v, lat, lon, 2).

    u, v     : 2-D (lat, lon) in m/s
    lat_arr  : 1-D degrees_north
    lon_arr  : 1-D degrees_east

    Returns vr (s^-1), same shape as u.
    """
    lat_r = np.deg2rad(lat_arr)
    lon_r = np.deg2rad(lon_arr)
    r = 6371000.
    cos_lat = np.cos(lat_r)
    dlat = np.gradient(lat_r)
    dlon_1d = np.gradient(lon_r)
    # dv/d(lon) / (r cos(lat))
    dvdlon = np.gradient(v, axis=1) / (r * cos_lat[:, np.newaxis] * dlon_1d[np.newaxis, :])
    # d(u cos(lat))/d(lat) / (r cos(lat))
    ucoslat = u * cos_lat[:, np.newaxis]
    ducoslat = np.gradient(ucoslat, axis=0) / (r * dlat[:, np.newaxis])
    return dvdlon - ducoslat


def _apply_unit_conv(V, var_name, units_in):
    """Apply standard unit conversions (mirrors NCL getVar2d/3d logic)."""
    u = units_in or ''
    if var_name in ('UV', 'U', 'V') and u in ('m s-1', 'm/s'):
        return _MS2KTS * V, 'kt'
    if var_name == 'HGT' and u in ('m', 'gpm'):
        return 0.1 * V, 'dam'
    if var_name in ('PRCP', 'PRATE') and u == 'kg m-2 s-1':
        return V * 3600., 'mm h-1'
    if var_name == 'TPRCP' and u == 'kg m-2':
        return V / 21.2, 'in'
    if var_name == 'MSLP' and u == 'Pa':
        return 0.01 * V, 'hPa'
    if var_name == 'PV' and u == 'K m2 kg-1 s-1':
        return 1.0e6 * V, '10^-6 K m2 kg-1 s-1'
    if var_name == 'SST' and u == 'K':
        return V - 273.14, 'oC'
    return V, u


# ===========================================================================
# Annotation / text functions
# ===========================================================================

def add_disclaimer(ax, expt, xloc, yloc, txhgt, just='left'):
    """
    Add disclaimer text to a matplotlib Axes.

    Parameters
    ----------
    ax    : matplotlib Axes
    expt  : str, experiment name
    xloc  : float, x in axes-fraction coordinates
    yloc  : float, y in axes-fraction coordinates
    txhgt : float, font size (points)
    just  : 'left' | 'center' | 'right'
    """
    if expt == 'fvGFS_ATL':
        msg = '*Experimental Product of HRD & GFDL*'
    else:
        msg = '*Experimental Product of NOAA/AOML/HRD*'
    ax.text(xloc, yloc, msg, transform=ax.transAxes,
            fontsize=txhgt, color='black', **_just_kw(just))


def add_graphic_title(ax, var, xloc, yloc, txhgt):
    """Add variable title (looked up via get_plot_title) to the Axes."""
    title = gu.get_plot_title(var) if gu else var
    ax.text(xloc, yloc, title, transform=ax.transAxes,
            fontsize=txhgt, color='black', ha='left', va='center')


def add_model_title(ax, expt, ensid, xloc, yloc, txhgt):
    """Add model/ensemble title to the Axes."""
    if gu:
        titles = gu.get_expt_info([expt], 'title')
        title = titles[0] if titles else expt
    else:
        title = expt
    if ensid:
        title = title + ' Mem:' + str(ensid)
    ax.text(xloc, yloc, title, transform=ax.transAxes,
            fontsize=txhgt, color='black', ha='left', va='center')


def add_mslp_label(ax, mslp, xloc, yloc, txhgt, just='left'):
    """Add MSLP annotation in brown text."""
    title = f'MSLP = {mslp:7.2f} mb'
    ax.text(xloc, yloc, title, transform=ax.transAxes,
            fontsize=txhgt, color='brown', **_just_kw(just))


def add_sid_label(ax, sid, xloc, yloc, txhgt):
    """Add storm-ID label with white outline and grey fill."""
    kw = dict(transform=ax.transAxes, fontsize=txhgt,
               ha='center', va='center')
    if HAS_MATPLOTLIB:
        ax.text(xloc, yloc, sid, color='grey',
                path_effects=[_pe.withStroke(linewidth=4, foreground='white')],
                **kw)
    else:
        ax.text(xloc, yloc, sid, color='grey', **kw)


def add_storm_marker(ax, intensity, sid_num, xloc, yloc, txhgt):
    """
    Add TC intensity marker symbol to the Axes.

    Intensity thresholds (kt): >=64 → hurricane 'H', 34-63 → storm 'S',
    invest (sid_num > 50) → 'X', else → 'L'.
    """
    if intensity >= 64.:
        label = 'H'
    elif intensity >= 34.:
        label = 'S'
    elif sid_num > 50:
        label = 'X'
    else:
        label = 'L'
    kw = dict(transform=ax.transAxes, ha='center', va='center')
    # Black outline
    ax.text(xloc, yloc, label, fontsize=txhgt * 1.25, color='black',
            fontweight='bold', **kw)
    # Red fill
    ax.text(xloc, yloc, label, fontsize=txhgt, color='red', **kw)


def add_storm_title(ax, title, xloc, yloc, txhgt, just='left'):
    """
    Add multi-storm title, wrapping into up to 3 rows of ≤4 storms each.
    """
    kw = _just_kw(just)
    parts = title.split(',')
    n = len(parts)
    if n <= 4:
        ax.text(xloc, yloc, title, transform=ax.transAxes,
                fontsize=txhgt, color='black', **kw)
    elif n <= 8:
        r1 = ','.join(parts[:4])
        r2 = ','.join(parts[4:])
        ax.text(xloc, yloc,        r1, transform=ax.transAxes, fontsize=txhgt, color='black', **kw)
        ax.text(xloc, yloc + 0.010, r2, transform=ax.transAxes, fontsize=txhgt, color='black', **kw)
    else:
        r1 = ','.join(parts[:4])
        r2 = ','.join(parts[4:8])
        r3 = ','.join(parts[8:])
        ax.text(xloc, yloc,        r1, transform=ax.transAxes, fontsize=txhgt, color='black', **kw)
        ax.text(xloc, yloc + 0.010, r2, transform=ax.transAxes, fontsize=txhgt, color='black', **kw)
        ax.text(xloc, yloc + 0.020, r3, transform=ax.transAxes, fontsize=txhgt, color='black', **kw)


def add_time_title(ax, title, xloc, yloc, txhgt, just='left'):
    """Add time-string annotation to the Axes."""
    ax.text(xloc, yloc, title, transform=ax.transAxes,
            fontsize=txhgt, color='black', **_just_kw(just))


def add_vmax_label(ax, vmax, xloc, yloc, txhgt, just='left'):
    """Add VMAX annotation in brown text."""
    title = f'VMAX = {vmax:7.2f} kt'
    ax.text(xloc, yloc, title, transform=ax.transAxes,
            fontsize=txhgt, color='brown', **_just_kw(just))


# ===========================================================================
# ATCF reading functions
# ===========================================================================

def adeck_read(atcf_file, imodel, info):
    """
    Read an ATCF A-deck file and return filtered track data.

    Parameters
    ----------
    atcf_file : str, path to A-deck
    imodel    : str, model filter ('' → all)
    info      : list of str, [IDATE[, SNUM[, SBAS]]]

    Returns
    -------
    types.SimpleNamespace with lat, lon, lead, vmax, pmin (float arrays),
    N (int), models, all_init_dates (lists), init_date, SID (str).
    Returns None on failure.
    """
    if not os.path.isfile(atcf_file):
        print(f'ERROR: adeck_read: file not found: {atcf_file}')
        return None
    with open(atcf_file) as fh:
        lines = [l.rstrip('\n') for l in fh if l.strip()]
    if not lines:
        print('ERROR: adeck_read: file is empty.')
        return None

    idate = info[0] if len(info) >= 1 else ''
    snum  = info[1] if len(info) >= 2 else ''
    sbas  = info[2] if len(info) >= 3 else ''

    # Read key columns (1-based in NCL → 0-based here)
    wind_rad   = [int(x) if x.strip().lstrip('-').isdigit() else 99
                  for x in _col(lines, 11)]   # col 12
    all_dates  = _col(lines, 2)               # col  3
    all_models = _col(lines, 4)               # col  5
    all_storms = _col(lines, 1)               # col  2
    all_basins = _col(lines, 0)               # col  1

    # Build boolean masks
    mask_wr  = [w <= 34 for w in wind_rad]
    mask_mdl = [imodel in m for m in all_models] if imodel else mask_wr[:]
    if idate:
        mask_dt = [d == idate for d in all_dates]
        idate2  = idate
    else:
        mask_dt = mask_wr[:]
        idate2  = sorted({d for d, m in zip(all_dates, mask_wr) if m},
                         key=lambda x: x)[0] if any(mask_wr) else ''
    mask_sn = [s == snum  for s in all_storms] if snum else mask_wr[:]
    mask_bs = [b == sbas  for b in all_basins] if sbas else mask_wr[:]

    vals = [i for i in range(len(lines))
            if mask_wr[i] and mask_mdl[i] and mask_dt[i]
            and mask_sn[i] and mask_bs[i]]
    if not vals:
        print('ERROR: adeck_read: no valid entries found.')
        return types.SimpleNamespace(
            lat=np.array([]), lon=np.array([]),
            lead=np.array([]), vmax=np.array([]), pmin=np.array([]),
            N=0, models=[], all_init_dates=[], init_date=idate2, SID='')

    lat  = np.array([_parse_atcf_lat(_col(lines, 6)[i])  for i in vals], float)
    lon  = np.array([_parse_atcf_lon(_col(lines, 7)[i])  for i in vals], float)
    lead = np.array([float(_col(lines, 5)[i])             for i in vals], float)
    vmax = np.array([float(_col(lines, 8)[i])             for i in vals], float)
    pmin = np.array([float(_col(lines, 9)[i])             for i in vals], float)

    # Pivot longitude
    if lon.max() >= 0:
        piv_lon = lon.max() - 180.
        lon = np.where(lon <= piv_lon, lon + 360., lon)
    else:
        piv_lon = lon[0] + 180.
        lon = np.where(lon >= piv_lon, lon - 360., lon)

    sid = _col(lines, 0)[vals[0]] + _col(lines, 1)[vals[0]]
    return types.SimpleNamespace(
        lat=lat, lon=lon, lead=lead, vmax=vmax, pmin=pmin,
        N=len(vals),
        models=[_col(lines, 4)[i]  for i in vals],
        all_init_dates=[_col(lines, 2)[i] for i in vals],
        init_date=idate2,
        SID=sid.replace(' ', ''),
    )


def atcf_read(atcf_file, cycle, mid):
    """
    Read a generic ATCF file, auto-detecting the wind-radii column.

    Parameters
    ----------
    atcf_file : str
    cycle     : str, forecast cycle filter (YYYYMMDDHH; '' → all)
    mid       : str, model ID filter ('' → all)

    Returns
    -------
    types.SimpleNamespace with lat, lon, lead, intensity (float arrays),
    N (int), SID (str).  Returns None on failure.
    """
    if not os.path.isfile(atcf_file):
        print(f'ERROR: atcf_read: file not found: {atcf_file}')
        return None
    with open(atcf_file) as fh:
        lines = [l.rstrip('\n') for l in fh if l.strip()]
    if not lines:
        return None

    # Find column that contains 'NEQ'; wind-radii col is one before it
    wind_col = 11   # default (0-based) = col 12
    for line in lines:
        parts = line.split(',')
        for ci, val in enumerate(parts):
            if val.strip() == 'NEQ':
                wind_col = ci - 1
                break
        else:
            continue
        break

    def f(line, idx):
        parts = line.split(',')
        return parts[idx].strip() if idx < len(parts) else ''

    def safe_int(s):
        s = s.strip().lstrip('-')
        return int(s) if s.isdigit() else -1

    wind_vals = [safe_int(f(l, wind_col))         for l in lines]
    mask_wr   = [w in (34, 0)                      for w in wind_vals]
    mask_mdl  = [f(l, wind_col - 7) == mid         for l in lines] if mid   else mask_wr[:]
    mask_cyc  = [f(l, wind_col - 9) == cycle       for l in lines] if cycle else mask_wr[:]

    vals = [i for i in range(len(lines))
            if mask_wr[i] and mask_mdl[i] and mask_cyc[i]]
    if not vals:
        print('ERROR: atcf_read: no valid entries found.')
        return None

    lat  = np.array([_parse_atcf_lat(f(lines[i], wind_col - 5)) for i in vals], float)
    lon  = np.array([_parse_atcf_lon(f(lines[i], wind_col - 4)) for i in vals], float)
    lead = np.array([float(f(lines[i], wind_col - 6) or '0')    for i in vals], float)
    inty = np.array([float(f(lines[i], wind_col - 3) or '0')    for i in vals], float)
    sid  = f(lines[vals[0]], 0) + f(lines[vals[0]], 1)
    return types.SimpleNamespace(
        lat=lat, lon=lon, lead=lead, intensity=inty,
        N=len(vals), SID=sid.replace(' ', ''))


def bdeck_read(best_file, idate):
    """
    Read an ATCF B-deck (best-track) file.

    Parameters
    ----------
    best_file : str
    idate     : str, YYYYMMDDHH initialisation date; 'none' → use first date

    Returns
    -------
    types.SimpleNamespace with lat, lon, lead, vmax, pmin (float arrays),
    valid_times (list), init_date (str), type_ (list), N (int).
    Returns None on failure.
    """
    if not os.path.isfile(best_file):
        print(f'ERROR: bdeck_read: file not found: {best_file}')
        return None
    with open(best_file) as fh:
        lines = [l.rstrip('\n') for l in fh if l.strip()]

    wind_rad  = [int(x) if x.strip().lstrip('-').isdigit() else 99
                 for x in _col(lines, 11)]
    vals      = [i for i, w in enumerate(wind_rad) if w <= 34]
    if not vals:
        print('ERROR: bdeck_read: no valid entries found.')
        return None

    all_dates = [_col(lines, 2)[i] for i in vals]
    if idate == 'none':
        idate = all_dates[0]

    def _dt(s):
        return datetime.datetime(int(s[:4]), int(s[4:6]), int(s[6:8]), int(s[8:10]))

    try:
        t0 = _dt(idate)
    except Exception:
        t0 = _dt(all_dates[0])
    lead = np.array([(_dt(d) - t0).total_seconds() / 3600. for d in all_dates], float)

    lat  = np.array([_parse_atcf_lat(_col(lines, 6)[i])  for i in vals], float)
    lon  = np.array([_parse_atcf_lon(_col(lines, 7)[i])  for i in vals], float)
    vmax = np.array([float(_col(lines, 8)[i] or '0')      for i in vals], float)
    pmin = np.array([float(_col(lines, 9)[i] or '0')      for i in vals], float)
    type_ = [_col(lines, 10)[i] for i in vals]

    # Pivot longitude
    if len(lon) and lon[0] >= 0:
        piv_lon = lon[0] - 180.
        lon = np.where(lon <= piv_lon, lon + 360., lon)
    elif len(lon):
        piv_lon = lon[0] + 180.
        lon = np.where(lon >= piv_lon, lon - 360., lon)

    return types.SimpleNamespace(
        lat=lat, lon=lon, lead=lead, vmax=vmax, pmin=pmin,
        valid_times=all_dates, init_date=idate, type_=type_,
        N=len(vals))


# ===========================================================================
# Dimension / coordinate helper functions
# ===========================================================================

def get_dim_lat(ds, dim, boco, tclats, tclons, i, type_):
    """
    Build a latitude axis for the requested domain type.

    Parameters
    ----------
    ds      : xarray.Dataset (or dict with array values)
    dim     : str, name of the latitude dimension
    boco    : sequence [N, S, W, E] (float)
    tclats  : 1-D array of TC latitudes
    tclons  : 1-D array of TC longitudes
    i       : int, current time/track index
    type_   : int, domain type 0-9 (see GPLOT_func.ncl for definitions)

    Returns
    -------
    types.SimpleNamespace(values, boco, latf) or None if no overlap.
    """
    if HAS_XARRAY and hasattr(ds, 'coords'):
        latf = ds.coords[dim].values.astype(float)
    else:
        latf = np.asarray(ds[dim], dtype=float)

    bds = list(float(b) for b in boco[:4])   # [N, S, W, E]

    if type_ == 1:
        lat = latf.copy()
        bds[0] = float(lat.max())
        bds[1] = float(lat.min())

    elif type_ == 4:
        tca = np.asarray(tclats, float)
        tco = np.asarray(tclons, float)
        tca = tca[~np.isnan(tca)]
        tco = tco[~np.isnan(tco)]
        if tca.size == 0 or tco.size == 0:
            print('ERROR: get_dim_lat: no TC positions for TYPE 4.')
            return None
        rng_lat = abs((tca.max() + 5.) - (tca.min() - 5.))
        rng_lon = abs((tco.max() + 5.) - (tco.min() - 5.))
        bds[0] = tca.max() + 5.
        bds[1] = tca.min() - 5.
        if rng_lon > 0 and rng_lat / rng_lon < 0.5:
            extra = 0.5 * abs(0.5 * rng_lon - rng_lat)
            bds[0] += extra
            bds[1] -= extra
        print(f'MSG: New Ratio = {abs(bds[0]-bds[1]) / max(rng_lon, 1e-9):.4f}')
        lat = None

    elif type_ in _TYPE_HALF:
        half = _TYPE_HALF[type_]
        tidx = 0 if type_ == 5 else int(i)
        tc_lat = float(np.asarray(tclats).flat[tidx])
        if math.isnan(tc_lat):
            print(f'ERROR: get_dim_lat: TC lat missing for TYPE {type_}.')
            return None
        bds[0] = tc_lat + half
        bds[1] = tc_lat - half
        lat = None

    else:   # TYPE 0 — use bds as provided
        lat = None

    # Domain overlap check
    if not all(b == 0. for b in bds[:2]):
        if latf.max() < bds[1] or latf.min() > bds[0]:
            return None   # No overlap

    if lat is None:
        lo = max(bds[1], float(latf.min()))
        hi = min(bds[0], float(latf.max()))
        mask = (latf >= lo) & (latf <= hi)
        lat = latf[mask]
        if lat.size == 0:
            return None

    return types.SimpleNamespace(values=lat, boco=bds, latf=latf,
                                 units='degrees_north', long_name='Latitude')


def get_dim_lon(ds, dim, boco, tclons, tclats, i, type_,
                piv_lon=180., do_pivot=None):
    """
    Build a longitude axis with optional pivot for the requested domain type.

    Parameters
    ----------
    ds       : xarray.Dataset
    dim      : str, name of the longitude dimension
    boco     : sequence [N, S, W, E]
    tclons   : 1-D array of TC longitudes
    tclats   : 1-D array of TC latitudes
    i        : int, current time/track index
    type_    : int, domain type 0-9
    piv_lon  : float, pivot longitude (default 180)
    do_pivot : bool or None (None → auto-detect)

    Returns
    -------
    types.SimpleNamespace(values, lonf, boco, flip_flag) or None.
    """
    if HAS_XARRAY and hasattr(ds, 'coords'):
        lon_raw = ds.coords[dim].values.astype(float)
    else:
        lon_raw = np.asarray(ds[dim], dtype=float)

    bds = list(float(b) for b in boco[:4])

    if do_pivot is None:
        do_pivot = True
    if piv_lon > lon_raw.max() or piv_lon < lon_raw.min():
        do_pivot = False

    flip_flag = False
    tco = np.asarray(tclons, float)
    all_pts = list(tco[~np.isnan(tco)]) + [bds[2], bds[3]]

    if lon_raw.min() >= 0. and lon_raw.max() <= 360.:
        if do_pivot:
            lonf, _ = _lon_pivot(lon_raw, lon_raw, piv_lon)
            lonf = np.where(lonf >= piv_lon, lonf - 360., lonf)
            if piv_lon >= 0 and len(all_pts) and np.nanmax(all_pts) > piv_lon:
                lonf = lonf + 360.
            flip_flag = True
        else:
            lonf = lon_raw.copy()
    elif lon_raw.max() > 360.:
        lonf = lon_raw - 360.
    elif lon_raw.min() > 180.:
        lonf = lon_raw - 360.
    else:
        if do_pivot:
            lonf, _ = _lon_pivot(lon_raw, lon_raw, piv_lon)
            if piv_lon >= 0 and len(all_pts) and np.nanmax(all_pts) > piv_lon:
                lonf = lonf + 360.
            flip_flag = True
        else:
            lonf = lon_raw.copy()

    # Overlap check before type-specific adjustment
    if not all(b == 0. for b in bds[2:4]):
        if lonf.max() < bds[2] or lonf.min() > bds[3]:
            return None

    if type_ == 1:
        bds[3] = float(lonf.max())
        bds[2] = float(lonf.min())

    elif type_ == 4:
        tco_ok = tco[~np.isnan(tco)]
        tca_ok = np.asarray(tclats, float)
        tca_ok = tca_ok[~np.isnan(tca_ok)]
        if tco_ok.size == 0 or tca_ok.size == 0:
            print('ERROR: get_dim_lon: no TC positions for TYPE 4.')
            return None
        rng_lat = abs((tca_ok.max() + 5.) - (tca_ok.min() - 5.))
        rng_lon = abs((tco_ok.max() + 5.) - (tco_ok.min() - 5.))
        bds[3] = tco_ok.max() + 5.
        bds[2] = tco_ok.min() - 5.
        if rng_lon > 0 and rng_lat / rng_lon > 0.5:
            extra = 0.5 * abs(2. * rng_lat - rng_lon)
            bds[3] += extra
            bds[2] -= extra
        print(f'MSG: New Ratio = {rng_lat / max(abs(bds[3]-bds[2]), 1e-9):.4f}')

    elif type_ in _TYPE_HALF:
        half = _TYPE_HALF[type_]
        tidx = 0 if type_ == 5 else int(i)
        tc_lon = float(np.asarray(tclons).flat[tidx])
        if math.isnan(tc_lon):
            print(f'ERROR: get_dim_lon: TC lon missing for TYPE {type_}.')
            return None
        bds[3] = tc_lon + half
        bds[2] = tc_lon - half

    # Shift lonf into the same range as bds
    if type_ in (2, 3, 5, 6, 7, 8):
        if bds[3] < lonf.min():
            lonf = lonf - 360.
        elif bds[2] > lonf.max():
            lonf = lonf + 360.

    # Final overlap check
    if not all(b == 0. for b in bds[2:4]):
        if lonf.max() < bds[2] or lonf.min() > bds[3]:
            return None

    lo = max(bds[2], float(lonf.min()))
    hi = min(bds[3], float(lonf.max()))
    mask = (lonf >= lo) & (lonf <= hi)
    lon = lonf[mask]

    # Preserve orientation
    if lonf.size > 1 and lon.size > 1:
        if (lonf[1] - lonf[0]) * (lon[1] - lon[0]) < 0:
            lon = lon[::-1]

    if lon.size == 0:
        return None

    return types.SimpleNamespace(values=lon, lonf=lonf, boco=bds,
                                 flip_flag=flip_flag,
                                 units='degrees_east', long_name='Longitude')


def get_uniq_ind(lst):
    """
    Return the index of the first occurrence of each unique element.

    e.g. [x, x, a, a, b] → [0, 2, 4]
    """
    seen, result = [], []
    for idx, val in enumerate(lst):
        s = str(val)
        if s not in seen:
            seen.append(s)
            result.append(idx)
    return result

# ===========================================================================
# I/O helpers
# ===========================================================================

def get_input_file(ifiles, n_indices, var):
    """
    Return the first xarray.Dataset (from ifiles[n_indices]) that
    contains *var*.  Returns None if not found.

    Parameters
    ----------
    ifiles    : list of str
    n_indices : list-like of int (indices into ifiles to try)
    var       : str, variable name
    """
    if not HAS_XARRAY:
        print('ERROR: get_input_file: xarray is required.')
        return None
    for idx in n_indices:
        path = ifiles[idx]
        if not os.path.isfile(path):
            continue
        try:
            ds = xr.open_dataset(path)
            if var in ds.data_vars or var in ds.coords:
                return ds
            ds.close()
        except Exception as exc:
            print(f'WARNING: get_input_file: cannot open {path}: {exc}')
    return None


def get_topo(lat, lon, flip_flag=False):
    """
    Load NCARG topography and bilinear-interpolate to *lat* × *lon*.

    Uses $NCARG_ROOT/topo/all10/topo0.1.nc.

    Returns 2-D float array (nlat × nlon) or None if file not found.
    """
    if not HAS_XARRAY:
        print('ERROR: get_topo: xarray is required.')
        return None
    ncarg_root = os.environ.get('NCARG_ROOT', '')
    topo_file  = os.path.join(ncarg_root, 'topo', 'all10', 'topo0.1.nc')
    if not os.path.isfile(topo_file):
        print(f'ERROR: get_topo: topography file not found: {topo_file}')
        return None
    lat0 = np.asarray(lat, float)
    lon0 = np.asarray(lon, float)
    ds   = xr.open_dataset(topo_file)
    topo = ds['TOPO']
    sub  = topo.sel(lat=slice(float(lat0.min()), float(lat0.max())),
                    lon=slice(float(lon0.min()), float(lon0.max())))
    out  = sub.interp(lat=lat0, lon=lon0, method='linear').values.astype(float)
    ds.close()
    return out


# ===========================================================================
# Variable reading functions
# ===========================================================================

def get_var2d(ds, d_source, var_info, dim_names, i_bds, mf,
              flags, lon_arr, piv_lon=180.):
    """
    Read a 2-D field from *ds* with optional vortex removal and unit
    conversion.

    Parameters
    ----------
    ds        : xarray.Dataset
    d_source  : str, data source key (e.g. 'HWRF', 'GFS')
    var_info  : list of str [var_name, level_str[, time_index_str]]
                level_str length:
                  1-4 chars  → single level
                  9 chars    → 'LLLLULLLf' multi-level op (f=d/a/m)
    dim_names : list of str, dimension names present in the dataset
    i_bds     : sequence [N, S, W, E]
    mf        : float, level multiplier (1 → hPa files, 100 → Pa files)
    flags     : dict with 'flip' (bool), 'rm_vortex' (bool)
    lon_arr   : 1-D array; if len==1 and value==0 use the full file lon
    piv_lon   : float, pivot longitude for flip (default 180)

    Returns
    -------
    types.SimpleNamespace(data, lat, lon, units, flag1, flag2)
    or None on failure.
    """
    if not HAS_XARRAY:
        print('ERROR: get_var2d: xarray is required.')
        return None

    flip_flag = bool(flags.get('flip', False))
    rm_vortex = bool(flags.get('rm_vortex', False))
    lat_n = float(i_bds[0]); lat_s = float(i_bds[1])
    lon_w = float(i_bds[2]); lon_e = float(i_bds[3])
    var_name = var_info[0]
    lev_str  = var_info[1] if len(var_info) > 1 else ''
    t_idx    = int(var_info[2]) if len(var_info) > 2 else 0

    # Detect dimension names
    lat_dim = next((d for d in dim_names
                    if 'lat' in d.lower() or 'grid_yt' in d.lower()), None)
    lon_dim = next((d for d in dim_names
                    if 'lon' in d.lower() or 'grid_xt' in d.lower()), None)
    lev_dim = next((d for d in dim_names
                    if any(k in d for k in ('ISBL','HGT','lev','lv'))), None)
    tim_dim = next((d for d in dim_names if 'time' in d.lower()), None)

    if lat_dim is None or lon_dim is None:
        print('ERROR: get_var2d: lat/lon dimensions not found.')
        return None

    # Resolution-based N for vortex removal
    if rm_vortex and lat_dim in ds.coords:
        lv = ds.coords[lat_dim].values
        dx = abs(float(lv[1]) - float(lv[0])) if len(lv) > 1 else 0.5
        N  = 60 if dx < 0.05 else 45 if dx < 0.15 else 30 if dx < 0.30 else 15
    else:
        N = 15

    def _read(vname, lev=None, do_lat_sl=True):
        """Slice ds[vname] at lev (file units) and lat bounds."""
        if vname not in ds.data_vars:
            return None, ''
        da = ds[vname]
        if tim_dim is not None and tim_dim in da.dims:
            da = da.isel({tim_dim: t_idx})
        if lev is not None and lev_dim is not None and lev_dim in da.dims:
            da = da.sel({lev_dim: mf * lev}, method='nearest')
        if do_lat_sl and lat_dim in da.dims:
            # xarray slice direction-aware: sort first
            da_sorted = da.sortby(lat_dim)
            da = da_sorted.sel({lat_dim: slice(lat_s, lat_n)})
        return da.values.astype(float), da.attrs.get('units', '')

    def _vname(v, l=''):
        return gu.find_var_name(d_source, v, str(int(float(l))) if l else '') if gu else v

    units = ''
    arr   = None

    # --- Multi-level case ---
    if len(lev_str) > 4:
        if len(lev_str) != 9:
            print(f'ERROR: get_var2d: level string must be 9 chars: {lev_str}')
            return None
        lev1, lev2, lev_flag = float(lev_str[:4]), float(lev_str[4:8]), lev_str[8]
        NP = 25.

        if lev_flag == 'd':   # Difference lev1 - lev2
            if var_name == 'UV':
                U1, u = _read(_vname('U', lev1), lev1)
                U2, _ = _read(_vname('U', lev2), lev2)
                V1, _ = _read(_vname('V', lev1), lev1)
                V2, _ = _read(_vname('V', lev2), lev2)
                if any(x is None for x in (U1, U2, V1, V2)):
                    return None
                arr = np.sqrt((U1-U2)**2 + (V1-V2)**2); units = u or 'm s-1'
            else:
                A1, units = _read(_vname(var_name, lev1), lev1)
                A2, _     = _read(_vname(var_name, lev2), lev2)
                if A1 is None or A2 is None: return None
                arr = A1 - A2

        elif lev_flag == 'a':   # Layer average
            levs = np.arange(lev1, lev2 + NP / 2., NP)
            stk_u, stk_v, stk_s = [], [], []
            for l in levs:
                if var_name == 'UV':
                    a, u = _read(_vname('U', l), l)
                    b, _ = _read(_vname('V', l), l)
                    if a is not None and b is not None:
                        stk_u.append(a); stk_v.append(b); units = u
                else:
                    a, u = _read(_vname(var_name, l), l)
                    if a is not None:
                        stk_s.append(a); units = u
            if var_name == 'UV':
                if not stk_u: return None
                Um = np.mean(np.stack(stk_u, 0), 0)
                Vm = np.mean(np.stack(stk_v, 0), 0)
                arr = np.sqrt(Um**2 + Vm**2)
            else:
                if not stk_s: return None
                arr = np.mean(np.stack(stk_s, 0), 0)

        elif lev_flag == 'm':   # Mass-weighted average
            levs = np.arange(lev1, lev2 + NP / 2., NP)
            stk, lev_wts = [], []
            stk_v_mw = []
            for l in levs:
                if var_name == 'UV':
                    a, u = _read(_vname('U', l), l)
                    b, _ = _read(_vname('V', l), l)
                    if a is not None and b is not None:
                        stk.append(a); stk_v_mw.append(b)
                        lev_wts.append(l); units = u
                else:
                    a, u = _read(_vname(var_name, l), l)
                    if a is not None:
                        stk.append(a); lev_wts.append(l); units = u
            if not stk: return None
            V3d = np.stack(stk, 0)
            arr = _mwavg(V3d, lev_wts)
            if var_name == 'UV' and stk_v_mw:
                Vm = _mwavg(np.stack(stk_v_mw, 0), lev_wts)
                arr = np.sqrt(arr**2 + Vm**2)
        else:
            print(f'ERROR: get_var2d: unknown level flag "{lev_flag}".')
            return None

    # --- Single-level case ---
    else:
        lev_val = float(lev_str) if lev_str else None
        lat_sl  = True

        if var_name == 'UV':
            U, u  = _read(_vname('U', lev_str), lev_val, lat_sl)
            Vv, _ = _read(_vname('V', lev_str), lev_val, lat_sl)
            if U is None or Vv is None: return None
            arr   = np.sqrt(U**2 + Vv**2); units = u or 'm s-1'

        elif var_name in ('RVO', 'AVO'):
            ncn = _vname(var_name, lev_str)
            if ncn in ds.data_vars:
                arr, units = _read(ncn, lev_val, lat_sl)
            else:
                U, _ = _read(_vname('U', lev_str), lev_val, lat_sl)
                Vv, _ = _read(_vname('V', lev_str), lev_val, lat_sl)
                if U is None or Vv is None: return None
                lat_vals = ds.coords[lat_dim].sortby(lat_dim).sel(
                    {lat_dim: slice(lat_s, lat_n)}).values.astype(float)
                lon_vals = ds.coords[lon_dim].values.astype(float)
                arr   = _uv_to_vr(U, Vv, lat_vals, lon_vals); units = 's-1'
                if var_name == 'AVO':
                    fc  = 2. * _OMEGA * np.sin(_D2R * lat_vals)
                    arr = arr + fc[:, np.newaxis]

        elif var_name == 'PV':
            lev_f   = float(lev_str) if lev_str else 500.
            avn     = _vname('AVO', lev_str)
            if avn in ds.data_vars:
                AVO, _ = _read(avn, lev_f, lat_sl)
            else:
                U, _  = _read(_vname('U', lev_str), lev_f, lat_sl)
                Vv, _ = _read(_vname('V', lev_str), lev_f, lat_sl)
                if U is None or Vv is None: return None
                lat_vals = ds.coords[lat_dim].sortby(lat_dim).sel(
                    {lat_dim: slice(lat_s, lat_n)}).values.astype(float)
                lon_vals = ds.coords[lon_dim].values.astype(float)
                AVO  = _uv_to_vr(U, Vv, lat_vals, lon_vals)
                fc   = 2. * _OMEGA * np.sin(_D2R * lat_vals)
                AVO  = AVO + fc[:, np.newaxis]
            # Upper/lower levels for dTheta/dp
            if lev_dim and lev_dim in ds.coords:
                lev_c  = ds.coords[lev_dim].values * mf
                vidx   = int(np.argmin(np.abs(lev_c - mf * lev_f)))
                lev_up = lev_c[vidx - 1] if vidx > 0 else lev_c[vidx]
                lev_dn = lev_c[vidx + 1] if vidx < len(lev_c)-1 else lev_c[vidx]
            else:
                lev_up = mf * lev_f * 0.9; lev_dn = mf * lev_f * 1.1
            lev_diff = lev_up - lev_dn
            lev_ref  = ds.coords[lev_dim].values.max() * mf if lev_dim else mf * lev_f
            tn       = _vname('T', lev_str)
            ThU, _   = _read(tn, lev_up / mf, lat_sl)
            ThD, _   = _read(tn, lev_dn / mf, lat_sl)
            if ThU is None or ThD is None: return None
            ThU = ThU * (lev_ref / max(abs(lev_up), 1e-6)) ** 0.286
            ThD = ThD * (lev_ref / max(abs(lev_dn), 1e-6)) ** 0.286
            arr   = -1. * _G * AVO * (ThU - ThD) / max(abs(lev_diff), 1e-6)
            units = 'K m2 kg-1 s-1'

        else:
            ncn = _vname(var_name, lev_str)
            arr, units = _read(ncn, lev_val, lat_sl)
            if arr is None: return None

    # Squeeze extra dimensions
    if arr is None: return None
    while arr.ndim > 2:
        arr = arr[0]

    # Longitude pivot/flip
    lon_full = ds.coords[lon_dim].values.astype(float)
    if flip_flag:
        arr, lon_full = _lon_pivot(arr, lon_full, piv_lon)

    # Vortex removal
    if rm_vortex and gu is not None:
        arr = gu.hbfilter(arr[np.newaxis], N)[0]

    # Subset to lon bounds
    lon_a = np.asarray(lon_arr, float)
    if lon_a.size > 1 or (lon_a.size == 1 and lon_a[0] != 0):
        mask    = (lon_full >= lon_w) & (lon_full <= lon_e)
        arr     = arr[:, mask]
        lon_out = lon_full[mask]
    else:
        lon_out = lon_full

    lat_out = ds.coords[lat_dim].sortby(lat_dim).sel(
        {lat_dim: slice(lat_s, lat_n)}).values.astype(float)

    # RVO special case: file may contain AVO; subtract planetary vorticity
    if var_name == 'RVO' and units == 's-1':
        avn = _vname('AVO', lev_str)
        if avn in ds.data_vars:
            fc  = 2. * _OMEGA * np.sin(_D2R * lat_out)
            arr = 1e5 * (arr - fc[:, np.newaxis])
            units = '10^-5 s-1'

    # Standard unit conversions
    arr, units = _apply_unit_conv(arr, var_name, units)
    # vorticity scale (if not already converted above)
    if var_name in ('RVO', 'AVO') and units == 's-1':
        arr *= 1e5; units = '10^-5 s-1'

    return types.SimpleNamespace(
        data=arr, lat=lat_out, lon=lon_out,
        units=units, flag1=flip_flag, flag2=rm_vortex)


def get_var3d(ds, d_source, var_info, dim_names, i_bds, mf,
              flags, lon_arr, piv_lon=180.):
    """
    Read a 3-D (lev × lat × lon) field.

    var_info[1] must be exactly 8 chars: 'LLLLUlll' (lev1=0:3, lev2=4:7).

    Returns
    -------
    types.SimpleNamespace(data, lat, lon, lev, units, flag1, flag2)
    or None on failure.
    """
    if not HAS_XARRAY:
        print('ERROR: get_var3d: xarray is required.')
        return None
    if len(var_info) < 2 or len(var_info[1]) != 8:
        print('ERROR: get_var3d: level string must be exactly 8 chars.')
        return None

    lev1 = float(var_info[1][:4])
    lev2 = float(var_info[1][4:8])
    var_name  = var_info[0]
    flip_flag = bool(flags.get('flip', False))
    rm_vortex = bool(flags.get('rm_vortex', False))
    lat_n = float(i_bds[0]); lat_s = float(i_bds[1])
    lon_w = float(i_bds[2]); lon_e = float(i_bds[3])

    lat_dim = next((d for d in dim_names if 'lat' in d.lower()), None)
    lon_dim = next((d for d in dim_names if 'lon' in d.lower()), None)
    lev_dim = next((d for d in dim_names
                    if any(k in d for k in ('ISBL','HGT','lev','lv'))), None)

    def _vname(v, l=''):
        return gu.find_var_name(d_source, v, str(int(l)) if l else '') if gu else v

    def _read3(vname):
        if vname not in ds.data_vars:
            return None, ''
        da = ds[vname]
        if lev_dim and lev_dim in da.dims:
            da = da.sel({lev_dim: slice(mf * lev1, mf * lev2)})
        if lat_dim and lat_dim in da.dims:
            da = da.sortby(lat_dim).sel({lat_dim: slice(lat_s, lat_n)})
        return da.values.astype(float), da.attrs.get('units', '')

    if var_name == 'UV':
        U,   units = _read3(_vname('U', lev1))
        Vv,  _     = _read3(_vname('V', lev1))
        if U is None or Vv is None: return None
        arr = np.sqrt(U**2 + Vv**2); units = units or 'm s-1'
    else:
        arr, units = _read3(_vname(var_name, lev1))
        if arr is None: return None

    lon_full = ds.coords[lon_dim].values.astype(float) if lon_dim else np.array([0.])
    if flip_flag and arr.ndim >= 3:
        arr, lon_full = _lon_pivot(arr, lon_full, piv_lon)

    if rm_vortex and gu is not None:
        arr = gu.hbfilter(arr, 1)

    lon_a = np.asarray(lon_arr, float)
    if lon_a.size > 1 or (lon_a.size == 1 and lon_a[0] != 0):
        mask    = (lon_full >= lon_w) & (lon_full <= lon_e)
        arr     = arr[:, :, mask]
        lon_out = lon_full[mask]
    else:
        lon_out = lon_full

    lat_out = (ds.coords[lat_dim].sortby(lat_dim).sel(
                   {lat_dim: slice(lat_s, lat_n)}).values.astype(float)
               if lat_dim else np.array([0.]))
    lev_out = (ds.coords[lev_dim].sel(
                   {lev_dim: slice(mf * lev1, mf * lev2)}).values.astype(float)
               if lev_dim else np.array([lev1]))

    arr, units = _apply_unit_conv(arr, var_name, units)
    if var_name == 'RVO' and units == 's-1':
        fc  = 2. * _OMEGA * np.sin(_D2R * lat_out)
        arr = 1e5 * (arr - fc[np.newaxis, :, np.newaxis])
        units = '10^-5 s-1'

    return types.SimpleNamespace(data=arr, lat=lat_out, lon=lon_out,
                                 lev=lev_out, units=units,
                                 flag1=flip_flag, flag2=rm_vortex)


def get_var_xc(ds, d_source, var_info, dim_names, i_bds, mf,
               flags, lon_arr, piv_lon=180.):
    """
    Read a vertical cross-section (lev × spatial slice).

    var_info[1] is a 9-char string 'LLLLULLLf':
      lev1 = chars 0-3, lev2 = chars 4-7,
      f = 'x' → zonal XC (fix lat to domain midpoint, full lon)
      f = 'y' → meridional XC (full lat, average over lon range)

    Returns
    -------
    types.SimpleNamespace(data, lev, lon|lat, units, flag1, flag2)
    or None on failure.
    """
    if not HAS_XARRAY:
        print('ERROR: get_var_xc: xarray is required.')
        return None
    if len(var_info) < 2 or len(var_info[1]) != 9:
        print('ERROR: get_var_xc: level string must be 9 chars.')
        return None

    lev1     = mf * float(var_info[1][:4])
    lev2     = mf * float(var_info[1][4:8])
    lev_flag = var_info[1][8]
    var_name  = var_info[0]
    flip_flag = bool(flags.get('flip', False))
    lat_n = float(i_bds[0]); lat_s = float(i_bds[1])
    lon_w = float(i_bds[2]); lon_e = float(i_bds[3])
    lat_mid = 0.5 * (lat_n + lat_s)

    lat_dim = next((d for d in dim_names if 'lat' in d.lower()), None)
    lon_dim = next((d for d in dim_names if 'lon' in d.lower()), None)
    lev_dim = next((d for d in dim_names
                    if any(k in d for k in ('ISBL','HGT','lev','lv'))), None)

    def _vname(v, l=''):
        return gu.find_var_name(d_source, v, str(int(l)) if l else '') if gu else v

    lev1_raw = float(var_info[1][:4])   # in file units (before mf)

    def _readxc(vname, lat_sel):
        if vname not in ds.data_vars:
            return None, ''
        da = ds[vname]
        if lev_dim and lev_dim in da.dims:
            da = da.sel({lev_dim: slice(lev1, lev2)})
        if lat_sel == 'mid':
            da = da.sel({lat_dim: lat_mid}, method='nearest')
        elif lat_sel == 'range':
            da = da.sortby(lat_dim).sel({lat_dim: slice(lat_s, lat_n)})
        return da.values.astype(float), da.attrs.get('units', '')

    if lev_flag == 'x':   # Zonal cross-section (lev × lon)
        if var_name == 'UV':
            U, _  = _readxc(_vname('U', lev1_raw), 'mid')
            Vv, _ = _readxc(_vname('V', lev1_raw), 'mid')
            if U is None or Vv is None: return None
            arr = _MS2KTS * np.sqrt(U**2 + Vv**2); units = 'kts'
        else:
            arr, units = _readxc(_vname(var_name, lev1_raw), 'mid')
            if arr is None: return None

        lon_full = ds.coords[lon_dim].values.astype(float) if lon_dim else np.array([0.])
        if flip_flag:
            arr, lon_full = _lon_pivot(arr, lon_full, piv_lon)
        mask    = (lon_full >= lon_w) & (lon_full <= lon_e)
        arr     = arr[:, mask] if arr.ndim == 2 else arr[mask]
        lon_out = lon_full[mask]
        lev_out = (ds.coords[lev_dim].sel({lev_dim: slice(lev1, lev2)}).values.astype(float)
                   if lev_dim else np.array([lev1]))
        arr, units = _apply_unit_conv(arr, var_name, units)
        return types.SimpleNamespace(data=arr, lev=lev_out, lon=lon_out,
                                     units=units, flag1=flip_flag, flag2=False)

    elif lev_flag == 'y':   # Meridional cross-section (lev × lat, averaged over lon)
        if var_name == 'UV':
            U, _  = _readxc(_vname('U', lev1_raw), 'range')
            Vv, _ = _readxc(_vname('V', lev1_raw), 'range')
            if U is None or Vv is None: return None
            arr = _MS2KTS * np.sqrt(U**2 + Vv**2); units = 'kts'
        else:
            arr, units = _readxc(_vname(var_name, lev1_raw), 'range')
            if arr is None: return None

        lon_full = ds.coords[lon_dim].values.astype(float) if lon_dim else np.array([0.])
        if flip_flag:
            arr, lon_full = _lon_pivot(arr, lon_full, piv_lon)
        lon_mask = (lon_full >= lon_w) & (lon_full <= lon_e)
        # arr shape: (lev, lat, lon) → average over lon → (lev, lat)
        if arr.ndim == 3:
            arr = arr[:, :, lon_mask].mean(axis=-1)
        elif arr.ndim == 2:
            arr = arr[:, lon_mask].mean(axis=-1)
        lat_out = (ds.coords[lat_dim].sortby(lat_dim).sel(
                       {lat_dim: slice(lat_s, lat_n)}).values.astype(float)
                   if lat_dim else np.array([lat_mid]))
        lev_out = (ds.coords[lev_dim].sel({lev_dim: slice(lev1, lev2)}).values.astype(float)
                   if lev_dim else np.array([lev1]))
        arr, units = _apply_unit_conv(arr, var_name, units)
        return types.SimpleNamespace(data=arr, lev=lev_out, lat=lat_out,
                                     units=units, flag1=flip_flag, flag2=False)

    else:
        print(f"ERROR: get_var_xc: unknown lev_flag '{lev_flag}' (expect 'x' or 'y').")
        return None


# ===========================================================================
# Namelist reader
# ===========================================================================

def nml_read(nml, var):
    """
    Extract a variable's value from GPLOT master namelist lines.

    Parameters
    ----------
    nml : list of str (lines of the namelist file)
    var : str, variable name to look up

    Returns
    -------
    str value (stripped), or '' if not found.
    """
    search = var + ' ='
    for line in nml:
        if search in line:
            parts = line.split('=', 1)
            return parts[1].strip() if len(parts) == 2 else ''
    return ''
