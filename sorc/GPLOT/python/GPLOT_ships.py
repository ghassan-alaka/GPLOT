#!/usr/bin/env python3
"""
GPLOT Ships Module - SHIPS Large-Scale Diagnostic computation and plotting.

Replaces sorc/GPLOT/ncl/GPLOT_ships.ncl (~3000 lines).
Reads GRIB2 model output and ATCF track files, computes vortex-centric
SHIPS diagnostics, writes .DAT files, and produces trend/hodograph/TCCEN plots.

Usage:
    python GPLOT_ships.py --idate 2025102300 --sid 13L --master-nml /path/to/namelist

Diagnostics computed:
  Shear:     SHRD, SHTD, SHRS, SHTS, SHDC, SDDC
  Pressure:  MSLP, PENV
  Wind:      VMAX, IKE34, IKE50, IKE64, U200, U20C, V20C
  Humidity:  RHLO, RHMD, RHHI, R000
  Other:     Z850, D200, DIVC, T000, CAPE, HLCY
  Maps:      TCCEN (center fixes), TCHODO (hodograph)
"""

import argparse
import glob
import logging
import os
import re
import sys
from datetime import datetime, timedelta

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.ticker as mticker
import numpy as np

try:
    import cartopy.crs as ccrs
    import cartopy.feature as cfeature
    HAS_CARTOPY = True
except ImportError:
    HAS_CARTOPY = False

# Add the parent directory to sys.path for gplot_utils import
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from gplot_utils.namelist import read_master_namelist, read_ships_namelist
from gplot_utils.atcf import (read_atcf, read_bdeck, derive_longsid,
                              atcf_from_listfile, walk_files_depth_limited)
from gplot_utils import ensemble as ens_utils
from gplot_utils.grib_reader import (open_grib2, get_var_2d, get_var_3d,
                                      get_layer_mean, get_wind_components,
                                      get_grid_info)
from gplot_utils.coord_transform import (sph2cart, sph2cart_3d,
                                          make_cartesian_grid,
                                          annular_mean, circular_mean,
                                          compute_wind_shear)
from gplot_utils.plot_utils import (save_figure, configure_cartopy,
                                    update_plotted_file,
                                    read_spawn_file_list,
                                    sweep_orphan_pngs)
from gplot_utils import constants as C

logger = logging.getLogger('__main__')


def _write_status(status_file, value):
    """Write module status with a lockfile (spawn-compatible)."""
    status_lock = f"{status_file}.lock"
    os.system(f'lockfile -r-1 -l 180 "{status_lock}"')
    os.system(f'echo "{value}" > "{status_file}"')
    os.system(f'rm -f "{status_lock}"')


# ============================================================
# DAT file I/O
# ============================================================

def _dat_path(odir, longsid, fname, idate):
    """Build path to a .DAT output file.

    ``longsid`` is the long storm identifier (e.g. ``melissa13l`` for
    a named storm, or just the bare sid like ``13l`` for an unnamed
    one). Files are namespaced by cycle (``idate``), so a storm
    getting named mid-run only causes one cycle's worth of scalar
    diagnostics to be recomputed -- existing per-cycle DATs under
    the bare-sid name are simply orphaned and can be removed.
    """
    return os.path.join(odir, f"{longsid.lower()}.{fname}.{idate}.ships.dat")


def _read_existing_dat(path, ncols=2):
    """Read existing DAT file into dict keyed by (fhr,) or (fhr, lev)."""
    data = {}
    if not os.path.isfile(path):
        return data
    with open(path, 'r') as f:
        for line in f:
            parts = line.split()
            if len(parts) < ncols:
                continue
            try:
                if ncols == 2:
                    key = int(parts[0])
                    data[key] = float(parts[1])
                elif ncols == 5:
                    key = (int(parts[0]), int(float(parts[1])))
                    data[key] = [float(p) for p in parts[2:]]
                elif ncols == 4:
                    key = (int(parts[0]), int(float(parts[1])))
                    data[key] = [float(p) for p in parts[2:]]
            except (ValueError, IndexError):
                continue
    return data


def _write_dat_2col(path, data_dict):
    """Write 2-column DAT file (FHR, VALUE), sorted by FHR."""
    with open(path, 'w') as f:
        for fhr in sorted(data_dict.keys()):
            f.write(f"{fhr:6d} {data_dict[fhr]:12.4f}\n")


def _write_dat_multicol(path, data_dict, ncols):
    """Write multi-column DAT file, sorted by keys."""
    with open(path, 'w') as f:
        for key in sorted(data_dict.keys()):
            vals = data_dict[key]
            if ncols == 5:
                fhr, lev = key
                f.write(f"{fhr:6d} {lev:6d} {vals[0]:12.4f} {vals[1]:12.4f} {vals[2]:6d}\n")
            elif ncols == 4:
                fhr, lev = key
                f.write(f"{fhr:6d} {lev:6d} {vals[0]:12.4f} {vals[1]:12.4f}\n")


# ============================================================
# Diagnostic computation functions
# ============================================================

def compute_shear(datasets, dsource, tc_lat, tc_lon, lev_top, lev_bot,
                  r_inner=200, r_outer=800):
    """
    Compute vertical wind shear magnitude and heading.

    Parameters
    ----------
    lev_top, lev_bot : int
        Top/bottom pressure levels in hPa (e.g., 200, 850).

    Returns (shear_mag_kts, shear_heading_deg) or (nan, nan).
    """
    u_top = get_var_2d(datasets, dsource, 'U', str(lev_top))
    u_bot = get_var_2d(datasets, dsource, 'U', str(lev_bot))
    v_top = get_var_2d(datasets, dsource, 'V', str(lev_top))
    v_bot = get_var_2d(datasets, dsource, 'V', str(lev_bot))

    if any(x is None for x in [u_top, u_bot, v_top, v_bot]):
        return np.nan, np.nan

    # Shear = top - bottom
    du = u_top['data'] - u_bot['data']
    dv = v_top['data'] - v_bot['data']
    lat = u_top['lat']
    lon = u_top['lon']

    # Ensure tc_lon is in the data convention
    tc_lon_data = _match_lon_convention(tc_lon, lon)

    x_km, y_km = make_cartesian_grid(radius_km=r_outer, spacing_km=10)
    du_cart = sph2cart(du, lat, lon, tc_lat, tc_lon_data, x_km, y_km)
    dv_cart = sph2cart(dv, lat, lon, tc_lat, tc_lon_data, x_km, y_km)

    shear_mag, shear_dir = compute_wind_shear(du_cart, dv_cart, x_km, y_km,
                                               r_inner, r_outer)
    return shear_mag, shear_dir


def compute_mslp_penv(datasets, dsource, tc_lat, tc_lon):
    """Compute minimum SLP and environmental SLP (200-800km average)."""
    result = get_var_2d(datasets, dsource, 'MSLP')
    if result is None:
        return np.nan, np.nan

    data = result['data']
    lat, lon = result['lat'], result['lon']

    mslp_min = np.nanmin(data)

    # PENV: average in 200-800 km annulus
    tc_lon_data = _match_lon_convention(tc_lon, lon)
    x_km, y_km = make_cartesian_grid(radius_km=800, spacing_km=10)
    mslp_cart = sph2cart(data, lat, lon, tc_lat, tc_lon_data, x_km, y_km)
    penv = annular_mean(mslp_cart, x_km, y_km, 200, 800)

    return mslp_min, penv


def compute_vmax_ike(datasets, dsource, tc_lat, tc_lon):
    """Compute VMAX and IKE34/50/64 from 10m wind."""
    result = get_var_2d(datasets, dsource, 'UV', '10')
    if result is None:
        return np.nan, 0.0, 0.0, 0.0

    data = result['data']  # in knots after unit conversion
    lat, lon = result['lat'], result['lon']
    vmax = np.nanmax(data)

    # IKE needs Cartesian grid, larger radius
    tc_lon_data = _match_lon_convention(tc_lon, lon)
    x_km, y_km = make_cartesian_grid(radius_km=1000, spacing_km=10)
    v_cart = sph2cart(data, lat, lon, tc_lat, tc_lon_data, x_km, y_km)

    # IKE constants
    dx_m = 10000.0  # 10 km in meters
    vol = dx_m * dx_m * 10.0  # 10m layer depth
    rho = 1.225  # kg/m^3

    ike34, ike50, ike64 = 0.0, 0.0, 0.0
    v_ms = v_cart / C.ms2kts  # convert back to m/s

    for threshold, label in [(34, 'ike34'), (50, 'ike50'), (64, 'ike64')]:
        mask = np.isfinite(v_cart) & (v_cart >= threshold)
        if np.any(mask):
            ke = 0.5 * rho * (v_ms[mask] ** 2) * vol
            val = np.sum(ke) / 1e12
        else:
            val = 0.0
        if label == 'ike34':
            ike34 = val
        elif label == 'ike50':
            ike50 = val
        else:
            ike64 = val

    return vmax, ike34, ike50, ike64


def compute_annular_scalar(datasets, dsource, var, level, tc_lat, tc_lon,
                           r_inner=200, r_outer=800, radius_km=800,
                           scale=1.0, offset=0.0, clamp=None):
    """
    Generic annular-averaged scalar diagnostic.

    Used for U200, U20C, V20C, RHLO, RHMD, RHHI, R000, T000, Z850, CAPE, HLCY.
    """
    # Handle layer-mean variables
    layer_vars = {
        'RHLO': ('RH', 700, 850),
        'RHMD': ('RH', 500, 700),
        'RHHI': ('RH', 300, 500),
    }

    if var in layer_vars:
        grib_var, ltop, lbot = layer_vars[var]
        result = get_layer_mean(datasets, dsource, grib_var, ltop, lbot)
    else:
        result = get_var_2d(datasets, dsource, var, level)

    if result is None:
        return np.nan

    data = result['data']
    lat, lon = result['lat'], result['lon']

    # Apply offset (e.g., K->C for temperature)
    if offset != 0:
        data = data + offset
    # Clamp (e.g., RH to 0-100)
    if clamp is not None:
        data = np.clip(data, clamp[0], clamp[1])

    tc_lon_data = _match_lon_convention(tc_lon, lon)
    x_km, y_km = make_cartesian_grid(radius_km=radius_km, spacing_km=10)
    data_cart = sph2cart(data, lat, lon, tc_lat, tc_lon_data, x_km, y_km)

    val = annular_mean(data_cart, x_km, y_km, r_inner, r_outer)
    return val * scale


def compute_divergence(datasets, dsource, tc_lat, tc_lon, r_outer=1000,
                       center_lat=None, center_lon=None):
    """
    Compute 200 hPa divergence averaged within r_outer km.

    If center_lat/center_lon provided, uses that instead of ATCF center.
    """
    wind = get_wind_components(datasets, dsource, '200')
    if wind is None:
        return np.nan

    u = wind['u']
    v = wind['v']
    lat = wind['lat']
    lon = wind['lon']

    # Compute divergence on lat/lon grid: du/dx + dv/dy
    # Use centered differences (matching NCL uv2dv_cfd with flag=2)
    dlat = np.gradient(lat)
    dlon = np.gradient(lon)

    # dy in meters
    dy = np.abs(dlat) * C.d2r * C.r_earth
    # dx in meters (varies with latitude)
    cos_lat = np.cos(np.radians(lat))

    # du/dx
    dudx = np.zeros_like(u)
    for j in range(len(lat)):
        dx_j = dlon * C.d2r * C.r_earth * cos_lat[j]
        dudx[j, :] = np.gradient(u[j, :]) / dx_j

    # dv/dy
    dvdy = np.zeros_like(v)
    for i in range(len(lon)):
        dvdy[:, i] = np.gradient(v[:, i]) / dy

    div = dudx + dvdy

    clat = center_lat if center_lat is not None else tc_lat
    clon = center_lon if center_lon is not None else tc_lon
    clon_data = _match_lon_convention(clon, lon)

    x_km, y_km = make_cartesian_grid(radius_km=r_outer, spacing_km=10)
    div_cart = sph2cart(div, lat, lon, clat, clon_data, x_km, y_km)

    val = circular_mean(div_cart, x_km, y_km, r_outer)
    # Scale to 10^-7 s^-1 and undo knot conversion in wind
    # Wind was converted to knots, so divergence is in knot-related units
    # Need to convert back: wind in m/s for proper divergence
    # Actually, get_wind_components converts to knots, so we need to undo that
    # The divergence units are then (kt / m) which needs conversion
    # D200 = 1e7 * div_ms / ms2kts  -- but our div is already in kt/m units
    # So D200 = 1e7 * val / ms2kts^2 ... this is complex.
    # Simpler: the NCL does divergence on raw (m/s) winds, then scales by 1e7/ms2kts
    # Our winds are in kt, so div is in kt/m. Convert: kt/m * (1/ms2kts) = (m/s)/m = 1/s
    # Then scale by 1e7
    val_scaled = 1e7 * val / C.ms2kts
    return val_scaled


def _filter121_2d(arr, n_iter):
    """
    Apply a 1-2-1 smoothing filter (NCL FILTER121 equivalent) to a 2D
    field. Each iteration smooths along axis 0, then axis 1, on the
    interior; edges are left unchanged. NaNs are filled with the field
    mean before filtering so they don't propagate.
    """
    out = np.asarray(arr, dtype=float).copy()
    if not np.all(np.isfinite(out)):
        fill = np.nanmean(out)
        out = np.where(np.isfinite(out), out, fill)
    for _ in range(int(n_iter)):
        tmp = out.copy()
        tmp[1:-1, :] = 0.25 * (out[:-2, :] + 2.0 * out[1:-1, :] + out[2:, :])
        out[:, 1:-1] = 0.25 * (tmp[:, :-2] + 2.0 * tmp[:, 1:-1] + tmp[:, 2:])
        out[0, :] = tmp[0, :]
        out[-1, :] = tmp[-1, :]
    return out


def _centroid_min(field):
    """
    Value-weighted centroid of the lower-tail of a 2D field. Replicates
    NCL findCenter type=1 with b=-1 (HGT min mode):
    threshold A = min + 0.20 * (max - min); centroid is the
    (A - field)-weighted mean of grid indices over points where
    field <= A (so the deepest part of the trough dominates).

    Returns (i_centroid, j_centroid) as float indices into field, or
    (nan, nan) if the field has no usable values.
    """
    if not np.isfinite(field).any():
        return np.nan, np.nan
    fmin = float(np.nanmin(field))
    fmax = float(np.nanmax(field))
    if fmax == fmin:
        return np.nan, np.nan
    A = fmin + 0.20 * (fmax - fmin)
    weights = np.maximum(A - field, 0.0)
    weights = np.nan_to_num(weights, nan=0.0)
    total = float(weights.sum())
    if total <= 0 or not np.isfinite(total):
        return np.nan, np.nan
    i_idx, j_idx = np.indices(field.shape)
    i_c = float((i_idx * weights).sum() / total)
    j_c = float((j_idx * weights).sum() / total)
    return i_c, j_c


def _haversine_km(lat1, lon1, lat2, lon2):
    """Great-circle distance in km between two points (degrees)."""
    rlat1, rlat2 = np.radians(lat1), np.radians(lat2)
    dlat = np.radians(lat2 - lat1)
    dlon = np.radians(lon2 - lon1)
    a = np.sin(dlat / 2) ** 2 + np.cos(rlat1) * np.cos(rlat2) * np.sin(dlon / 2) ** 2
    return 2.0 * 6371.0 * np.arcsin(np.sqrt(a))


def find_center_at_level(datasets, dsource, level, tc_lat, tc_lon, lat, lon):
    """
    Find the TC center at a given pressure level via the geopotential-
    height centroid algorithm ported from NCL findCenter type=1:
    1) crop to ~5 degrees around the ATCF position;
    2) smooth with a 1-2-1 filter, 25 iterations;
    3) value-weighted centroid of the lower 20% of the smoothed field.

    Returns (center_lat, center_lon, hgt_value, found_flag).
    hgt_value is min(smoothed) at the center for downstream "lowest
    level" marker selection. found_flag is 1 if a center could be
    computed (vortex-continuity check happens later in compute_tccen).
    """
    result = get_var_2d(datasets, dsource, 'HGT', str(level))
    if result is None:
        return np.nan, np.nan, np.nan, 0

    data = result['data']
    rlat = result['lat']
    rlon = result['lon']

    tc_lon_data = _match_lon_convention(tc_lon, rlon)
    lat_mask = (rlat >= tc_lat - 5) & (rlat <= tc_lat + 5)
    lon_mask = (rlon >= tc_lon_data - 5) & (rlon <= tc_lon_data + 5)

    if not np.any(lat_mask) or not np.any(lon_mask):
        return np.nan, np.nan, np.nan, 0

    sub = data[np.ix_(lat_mask, lon_mask)]
    sub_lat = rlat[lat_mask]
    sub_lon = rlon[lon_mask]

    if sub.size == 0 or np.all(np.isnan(sub)):
        return np.nan, np.nan, np.nan, 0

    smoothed = _filter121_2d(sub, n_iter=25)
    i_c, j_c = _centroid_min(smoothed)
    if not (np.isfinite(i_c) and np.isfinite(j_c)):
        return np.nan, np.nan, np.nan, 0

    clat = float(np.interp(i_c, np.arange(len(sub_lat)), sub_lat))
    clon = float(np.interp(j_c, np.arange(len(sub_lon)), sub_lon))
    if clon > 180:
        clon -= 360

    hgt_val = float(np.nanmin(smoothed))
    return clat, clon, hgt_val, 1


def compute_tccen(datasets, dsource, tc_lat, tc_lon, levels=None):
    """
    Compute TC centers at multiple pressure levels via the smoothed
    HGT centroid, then apply NCL's vortex-continuity check: a level's
    center is "in the vortex" iff it lies within 1 km per 1 hPa of the
    level immediately below it (or, for k>=2, also valid against k-2).
    Continuity propagates: a level fails if any of its anchor levels
    below failed.

    Returns dict: level -> (lat, lon, hgt_value, use_flag).
    """
    if levels is None:
        # 25 hPa increments from 200 to 1000, matching the NCL
        # coordinate-subscript LEV(:{200:1000}). Models that don't
        # provide every 25-hPa level will have those entries return
        # None from get_var_2d and be filtered out downstream.
        levels = list(range(200, 1001, 25))

    grid = get_grid_info(datasets, dsource)
    lat = grid['lat']
    lon = grid['lon']

    # Process surface->top so the continuity chain anchors at the
    # near-surface vortex (matches NCL: levels are flipped if
    # max(LEV) != LEV(0) before the use-flag pass).
    levs_asc = sorted(levels, reverse=True)

    raw = {}
    for lev in levs_asc:
        clat, clon, hgt, found = find_center_at_level(
            datasets, dsource, lev, tc_lat, tc_lon, lat, lon)
        raw[lev] = (clat, clon, hgt, found)
        if found:
            logger.debug(f"  TCCEN L={lev}: ({clat:.2f}, {clon:.2f})")

    use = {}
    for k, lev in enumerate(levs_asc):
        clat, clon, hgt, found = raw[lev]
        if not found or not np.isfinite(clat):
            use[lev] = False
            continue
        if k == 0:
            use[lev] = True
            continue
        prev_lev = levs_asc[k - 1]
        pclat, pclon, _phgt, pfound = raw[prev_lev]
        if not pfound or not use.get(prev_lev, False):
            use[lev] = False
            continue
        d1 = _haversine_km(clat, clon, pclat, pclon)
        lim1 = d1 / max(abs(lev - prev_lev), 1e-6)  # km per hPa
        ok = lim1 <= 1.0
        if not ok and k >= 2:
            prev2_lev = levs_asc[k - 2]
            p2clat, p2clon, _p2hgt, p2found = raw[prev2_lev]
            if p2found and use.get(prev2_lev, False):
                d2 = _haversine_km(clat, clon, p2clat, p2clon)
                lim2 = d2 / max(abs(lev - prev2_lev), 1e-6)
                ok = lim2 <= 1.0
        use[lev] = bool(ok)

    centers = {}
    for lev in levels:
        clat, clon, hgt, _ = raw[lev]
        centers[lev] = (clat, clon, hgt, 1 if use.get(lev, False) else 0)
    return centers


def compute_hodograph(datasets, dsource, tc_lat, tc_lon, centers=None,
                      levels=None):
    """
    Compute wind hodograph (200-800km annular mean U,V at each level).

    Returns dict: level -> (wind_mag_kts, wind_dir_deg)
    """
    if levels is None:
        # 25 hPa increments to match compute_tccen / NCL convention.
        levels = list(range(200, 1001, 25))

    hodo = {}
    for lev in levels:
        # Use the TCCEN center for this level if available, else ATCF
        if centers and lev in centers and np.isfinite(centers[lev][0]):
            clat = centers[lev][0]
            clon = centers[lev][1]
        else:
            clat = tc_lat
            clon = tc_lon

        u_result = get_var_2d(datasets, dsource, 'U', str(lev))
        v_result = get_var_2d(datasets, dsource, 'V', str(lev))

        if u_result is None or v_result is None:
            continue

        lat = u_result['lat']
        lon = u_result['lon']
        clon_data = _match_lon_convention(clon, lon)

        x_km, y_km = make_cartesian_grid(radius_km=800, spacing_km=10)
        u_cart = sph2cart(u_result['data'], lat, lon, clat, clon_data,
                         x_km, y_km)
        v_cart = sph2cart(v_result['data'], lat, lon, clat, clon_data,
                         x_km, y_km)

        u_mean = annular_mean(u_cart, x_km, y_km, 200, 800)
        v_mean = annular_mean(v_cart, x_km, y_km, 200, 800)

        if np.isfinite(u_mean) and np.isfinite(v_mean):
            mag = np.sqrt(u_mean**2 + v_mean**2)
            dirn = np.degrees(np.arctan2(u_mean, v_mean)) % 360
            hodo[lev] = (mag, dirn)

    return hodo


# ============================================================
# Plotting functions
# ============================================================

def plot_trend(dat_files, fname, odir, storm, idate, ylabel='',
               do_gif=False):
    """
    Plot time-series trend for a diagnostic showing multiple forecast cycles.

    Parameters
    ----------
    dat_files : list of (idate_str, data_dict)
        Each is (cycle_idate, {fhr: value}) for up to 6 cycles.
    fname : str
        Diagnostic name (e.g., 'SHRD').
    """
    if not dat_files:
        return

    fig, ax = plt.subplots(figsize=(10, 6))

    # Color palette for multiple cycles
    colors = ['#FF0000', '#FF8800', '#00BB00', '#0000FF', '#8800FF', '#888888']

    max_fhr = 0
    for i, (cycle_id, data) in enumerate(dat_files):
        if not data:
            continue
        fhrs = sorted(data.keys())
        vals = [data[f] for f in fhrs]
        color = colors[i % len(colors)]

        # Offset for earlier cycles: each prior cycle shifts left by 6h
        offset = -6 * i
        x = [f + offset for f in fhrs]

        ax.plot(x, vals, '-o', color=color, markersize=3, linewidth=1.2,
                label=cycle_id)
        if fhrs:
            max_fhr = max(max_fhr, max(fhrs))

    ax.axvline(0, color='black', linewidth=0.8, linestyle='-')
    ax.set_xlabel('Forecast Time [h]')
    ax.set_ylabel(ylabel if ylabel else fname)
    ax.set_title(f'{fname} - {storm} - Init: {idate}')
    ax.legend(fontsize=8, loc='best')
    ax.grid(True, alpha=0.3)

    # Set x-axis ticks every 12 hours
    xticks = np.arange(-24, max_fhr + 12, 12)
    ax.set_xticks(xticks)

    out_base = os.path.join(odir, f"{storm.lower()}.{fname}.{idate}.ships")
    save_figure(fig, out_base, do_gif=do_gif)
    plt.close(fig)
    return out_base + '.png'


def _draw_overlay_box(ax, corner, label, mag_kt, dir_deg, color):
    """
    Render a small information box anchored to one of the four axes
    corners showing a labeled directional arrow with magnitude. Used
    on TCCEN plots for the deep-shear, shallow-shear, and storm-motion
    overlays. Box position is in axes coords so it stays put under any
    map projection or zoom level.

    corner : one of {'tl', 'tr', 'bl', 'br'}.
    """
    if mag_kt is None or not np.isfinite(mag_kt) or mag_kt <= 0:
        return
    if dir_deg is None or not np.isfinite(dir_deg):
        return
    pad = 0.015
    w, h = 0.18, 0.13
    if corner == 'tl':
        x0, y0 = pad, 1.0 - pad - h
    elif corner == 'tr':
        x0, y0 = 1.0 - pad - w, 1.0 - pad - h
    elif corner == 'bl':
        x0, y0 = pad, pad
    else:  # 'br'
        x0, y0 = 1.0 - pad - w, pad

    rect = mpatches.Rectangle(
        (x0, y0), w, h, transform=ax.transAxes,
        facecolor='white', edgecolor='black', linewidth=0.7,
        alpha=0.92, zorder=20)
    ax.add_patch(rect)
    ax.text(x0 + w / 2, y0 + h - 0.012, label,
            transform=ax.transAxes, ha='center', va='top',
            fontsize=9, fontweight='bold', zorder=21)
    cx, cy = x0 + w / 2, y0 + h / 2
    # Arrow length scales linearly with magnitude (0 kt -> minimum
    # nub, 40 kt -> the full box width). Capped so a 60 kt SHRD or
    # fast-mover motion vector still fits inside the box.
    arrow_min = 0.020
    arrow_max = 0.075
    mag_ref = 40.0  # kt at which the arrow reaches arrow_max
    arrow_len = arrow_min + (arrow_max - arrow_min) * min(mag_kt / mag_ref, 1.0)
    rad = np.radians(dir_deg)
    dx = arrow_len * np.sin(rad)
    dy = arrow_len * np.cos(rad)
    ax.annotate(
        '', xy=(cx + dx / 2, cy + dy / 2),
        xytext=(cx - dx / 2, cy - dy / 2),
        xycoords=ax.transAxes,
        arrowprops=dict(arrowstyle='->', color=color, lw=2.0),
        zorder=21)
    ax.text(x0 + w / 2, y0 + 0.012, f'{mag_kt:.1f} kt',
            transform=ax.transAxes, ha='center', va='bottom',
            fontsize=9, zorder=21)


def _draw_level_legend(fig, levels, cmap, norm,
                       x=0.88, y_top=0.90, y_bot=0.12):
    """
    Draw the right-side "Level [hPa]" column: a vertical stack of
    pressure-level labels colored by the same rainbow_r colormap used
    for the per-level markers. Positioned in figure coordinates so it
    sits in the right margin reserved by subplots_adjust.
    """
    fig.text(x, y_top + 0.04, 'Level [hPa]', fontsize=12,
             fontweight='bold', ha='left', va='top')
    levs_top_down = sorted(levels, reverse=False)
    n = len(levs_top_down)
    if n == 0:
        return
    ys = np.linspace(y_top, y_bot, n)
    for lev, y in zip(levs_top_down, ys):
        fig.text(x, y, str(lev), fontsize=9, color=cmap(norm(lev)),
                 ha='left', va='center', fontweight='bold')


def plot_tccen(centers, tc_lat, tc_lon, fhr, storm, idate, odir,
               shrd_mag=None, shrd_dir=None, shrs_mag=None, shrs_dir=None,
               motion_spd=None, motion_dir=None, do_gif=False, zoom=False):
    """
    Plot TC center fixes at multiple levels on a map.

    Two extents:
    - Default: ±5 degrees around the ATCF position.
    - zoom=True: tight box scaled to the spread of the per-level
      center fixes (mirrors NCL TCCEN_zoom: half-width =
      max(1.10 * lon_spread, 1.10 * lat_spread), 0.5 deg fallback).

    Markers follow NCL's three-class convention: open circle = valid
    vortex (continuity check passed), star = level with the deepest
    HGT among valid centers, 'x' = found but failed continuity.
    Three corner overlays show 850-200 hPa shear (top-right), 850-500
    hPa shear (bottom-right), and storm motion (top-left). The right
    margin carries a colored "Level [hPa]" legend.
    """
    if not HAS_CARTOPY:
        logger.warning("Cartopy not available; skipping TCCEN plot")
        return None

    fig = plt.figure(figsize=(11, 10))
    # Reserve the right ~14% of the figure for the level legend.
    fig.subplots_adjust(left=0.08, right=0.84, top=0.92, bottom=0.08)
    ax = fig.add_subplot(1, 1, 1, projection=ccrs.PlateCarree())

    all_levs = sorted(centers.keys(), reverse=True)
    if not all_levs:
        plt.close(fig)
        return None

    # Determine map extent
    if zoom:
        lats = [centers[L][0] for L in all_levs if np.isfinite(centers[L][0])]
        lons = [centers[L][1] for L in all_levs if np.isfinite(centers[L][1])]
        if not lats:
            plt.close(fig)
            return None
        lat_spread = max(lats) - min(lats)
        lon_spread = max(lons) - min(lons)
        edge = max(1.10 * lon_spread, 1.10 * lat_spread)
        if edge == 0:
            edge = 0.5
        cx = float(np.mean(lons))
        cy = float(np.mean(lats))
        ax.set_extent([cx - edge, cx + edge, cy - edge, cy + edge],
                      crs=ccrs.PlateCarree())
    else:
        ax.set_extent([tc_lon - 5, tc_lon + 5, tc_lat - 5, tc_lat + 5],
                      crs=ccrs.PlateCarree())

    ax.add_feature(cfeature.COASTLINE.with_scale('50m'), linewidth=0.8)
    ax.add_feature(cfeature.BORDERS.with_scale('50m'), linewidth=0.5)
    gl = ax.gridlines(draw_labels=True, linewidth=0.3, alpha=0.5)
    gl.top_labels = False
    gl.right_labels = False  # right margin reserved for level legend

    cmap = plt.cm.rainbow_r
    norm = plt.Normalize(vmin=min(all_levs), vmax=max(all_levs))

    # Identify the "lowest HGT" level among valid vortex levels for the
    # star marker (matches NCL's CTR@value == min(valid) test).
    valid_vals = [(L, centers[L][2]) for L in all_levs
                  if centers[L][3] == 1 and np.isfinite(centers[L][2])]
    lowest_lev = (min(valid_vals, key=lambda t: t[1])[0]
                  if valid_vals else None)

    for lev in all_levs:
        clat, clon, _hgt, flag = centers[lev]
        if not np.isfinite(clat) or not np.isfinite(clon):
            continue
        color = cmap(norm(lev))
        if flag != 1:
            ax.plot(clon, clat, marker='x', color=color, markersize=9,
                    markeredgewidth=2.0, linestyle='',
                    transform=ccrs.PlateCarree(), zorder=5)
        elif lev == lowest_lev:
            ax.plot(clon, clat, marker='*', color=color, markersize=14,
                    markeredgecolor=color, markeredgewidth=1.2,
                    linestyle='', transform=ccrs.PlateCarree(), zorder=6)
        else:
            ax.plot(clon, clat, marker='o', markerfacecolor='none',
                    markeredgecolor=color, markeredgewidth=1.6,
                    markersize=9, linestyle='',
                    transform=ccrs.PlateCarree(), zorder=5)

    # Plot ATCF position
    ax.plot(tc_lon, tc_lat, '+', color='black', markersize=12,
            markeredgewidth=1.5,
            transform=ccrs.PlateCarree(), zorder=10)

    # Marker-class legend (matches NCL: Lowest / Vortex / Non-Vtx?)
    # plus the ATCF center marker drawn just below.
    legend_handles = [
        plt.Line2D([0], [0], marker='*', color='gray', markerfacecolor='gray',
                   markersize=12, linestyle='', label='Lowest'),
        plt.Line2D([0], [0], marker='o', color='gray', markerfacecolor='none',
                   markeredgewidth=1.6, markersize=9, linestyle='',
                   label='Vortex'),
        plt.Line2D([0], [0], marker='x', color='gray',
                   markersize=9, markeredgewidth=2.0, linestyle='',
                   label='Non-Vtx?'),
        plt.Line2D([0], [0], marker='+', color='black',
                   markersize=12, markeredgewidth=1.5, linestyle='',
                   label='ATCF center'),
    ]
    ax.legend(handles=legend_handles, loc='lower left', fontsize=9,
              framealpha=0.85)

    # Corner overlays: motion (top-left), 850-200 SHR (top-right),
    # 850-500 SHR (bottom-right). Bottom-left is the marker legend.
    _draw_overlay_box(ax, 'tr', '850-200 SHR', shrd_mag, shrd_dir, 'blue')
    _draw_overlay_box(ax, 'br', '850-500 SHR', shrs_mag, shrs_dir, 'green')
    _draw_overlay_box(ax, 'tl', 'Motion', motion_spd, motion_dir, 'firebrick')

    # Title
    valid_dt = datetime.strptime(idate, '%Y%m%d%H') + timedelta(hours=fhr)
    title_suffix = ' (zoom)' if zoom else ''
    ax.set_title(f'Center Fixes [geopotential height centroid]'
                 f'{title_suffix} - {storm}\n'
                 f'Init: {idate}  FHR: {fhr:03d}  '
                 f'Valid: {valid_dt.strftime("%Y%m%d%H")}',
                 fontsize=11)

    # Right-side colored level legend
    _draw_level_legend(fig, all_levs, cmap, norm)

    fname = 'TCCEN_zoom' if zoom else 'TCCEN'
    out_base = os.path.join(odir,
                            f"{storm.lower()}.{fname}.{idate}.ships.f{fhr:03d}")
    save_figure(fig, out_base, do_gif=do_gif)
    plt.close(fig)
    return out_base + '.png'


def plot_hodograph(hodo, fhr, storm, idate, odir,
                   motion_spd=None, motion_dir=None, do_gif=False,
                   zoom=False):
    """
    Plot wind hodograph showing environmental wind at each pressure level.
    """
    fig, ax = plt.subplots(figsize=(8, 8))

    # Concentric circles
    max_wind = 60 if not zoom else None
    circle_radii = [15, 30, 45, 60]

    if zoom and hodo:
        max_val = max(mag for mag, _ in hodo.values()) if hodo else 30
        # Pick scale from standard values
        for sc in [5, 10, 20, 30, 50, 80]:
            if sc > max_val * 1.2:
                max_wind = sc
                break
        else:
            max_wind = 80
        circle_radii = np.linspace(max_wind / 4, max_wind,
                                    4).astype(int).tolist()

    if max_wind is None:
        max_wind = 60

    for r in circle_radii:
        theta = np.linspace(0, 2 * np.pi, 100)
        ax.plot(r * np.cos(theta), r * np.sin(theta), 'k-',
                linewidth=0.5, alpha=0.3)
        ax.text(r + 0.5, 0.5, f'{r}', fontsize=7, alpha=0.5)

    # Axes
    ax.axhline(0, color='k', linewidth=0.5, alpha=0.3)
    ax.axvline(0, color='k', linewidth=0.5, alpha=0.3)
    ax.text(max_wind + 1, 0, 'E', fontsize=10, ha='left', va='center')
    ax.text(-max_wind - 1, 0, 'W', fontsize=10, ha='right', va='center')
    ax.text(0, max_wind + 1, 'N', fontsize=10, ha='center', va='bottom')
    ax.text(0, -max_wind - 1, 'S', fontsize=10, ha='center', va='top')

    # Plot hodograph points
    levs_sorted = sorted(hodo.keys(), reverse=True)
    cmap = plt.cm.rainbow_r
    norm = plt.Normalize(vmin=min(hodo.keys()) if hodo else 200,
                         vmax=max(hodo.keys()) if hodo else 1000)

    xs, ys = [], []
    for lev in levs_sorted:
        mag, dirn = hodo[lev]
        # Convert to Cartesian: x=east, y=north
        x = mag * np.sin(np.radians(dirn))
        y = mag * np.cos(np.radians(dirn))
        xs.append(x)
        ys.append(y)
        color = cmap(norm(lev))
        ax.plot(x, y, 'o', color=color, markersize=8, zorder=5)
        ax.text(x + 0.5, y + 0.5, str(lev), fontsize=6, color=color, zorder=5)

    # Connect with lines
    if len(xs) > 1:
        ax.plot(xs, ys, '-', color='gray', linewidth=0.8, alpha=0.6, zorder=4)

    # Motion vector
    if motion_spd is not None and np.isfinite(motion_spd):
        mx = motion_spd * np.sin(np.radians(motion_dir))
        my = motion_spd * np.cos(np.radians(motion_dir))
        ax.annotate('', xy=(mx, my), xytext=(0, 0),
                    arrowprops=dict(arrowstyle='->', color='firebrick', lw=2))
        ax.text(mx, my, 'MOT', fontsize=7, color='firebrick')

    ax.set_xlim(-max_wind * 1.1, max_wind * 1.1)
    ax.set_ylim(-max_wind * 1.1, max_wind * 1.1)
    ax.set_aspect('equal')
    ax.set_xlabel('U [kt]')
    ax.set_ylabel('V [kt]')

    valid_dt = datetime.strptime(idate, '%Y%m%d%H') + timedelta(hours=fhr)
    suffix = '_zoom' if zoom else ''
    ax.set_title(f'Wind Hodograph{" (zoom)" if zoom else ""} - {storm}\n'
                 f'Init: {idate}  FHR: {fhr:03d}  '
                 f'Valid: {valid_dt.strftime("%Y%m%d%H")}',
                 fontsize=11)

    fname = f"TCHODO{suffix}"
    out_base = os.path.join(
        odir, f"{storm.lower()}.{fname}.{idate}.ships.f{fhr:03d}")
    save_figure(fig, out_base, do_gif=do_gif)
    plt.close(fig)
    return out_base + '.png'


# ============================================================
# Utility helpers
# ============================================================

def _match_lon_convention(lon_val, lon_array):
    """Ensure a single lon value matches the convention of the lon array."""
    if lon_array.min() >= 0 and lon_array.max() > 180:
        # Data in 0..360
        if lon_val < 0:
            return lon_val + 360
    elif lon_array.max() <= 180:
        # Data in -180..180
        if lon_val > 180:
            return lon_val - 360
    return lon_val


def compute_storm_motion(atcf_df, fhr):
    """
    Compute storm motion from ATCF 6-hour displacement.

    Returns (speed_kts, heading_deg) or (nan, nan).
    """
    df = atcf_df
    row_now = df[df['fhr'] == fhr]
    row_prev = df[df['fhr'] == fhr - 6]

    if row_now.empty or row_prev.empty:
        return np.nan, np.nan

    lat1 = row_prev.iloc[0]['lat']
    lon1 = row_prev.iloc[0]['lon']
    lat2 = row_now.iloc[0]['lat']
    lon2 = row_now.iloc[0]['lon']

    # Great-circle approximate distance
    dlat = np.radians(lat2 - lat1)
    dlon = np.radians(lon2 - lon1)
    a = (np.sin(dlat / 2) ** 2 +
         np.cos(np.radians(lat1)) * np.cos(np.radians(lat2)) *
         np.sin(dlon / 2) ** 2)
    dist_km = 2 * C.r_earth / 1000.0 * np.arcsin(np.sqrt(a))

    # Speed in knots (6 hours = 21600 seconds)
    speed_kts = C.ms2kts * dist_km * 1000.0 / 21600.0

    # Heading (meteorological convention: where the storm is heading)
    heading = np.degrees(np.arctan2(
        lon2 - lon1, lat2 - lat1)) % 360

    return speed_kts, heading


# Regexes used to distinguish the moving storm-nest GRIB2 (`.storm2.`,
# `.nest.`, `.d03.`, `.moving.`) from the parent (`.parent.`, `.d01.`)
# in file globs.  The tokens must sit between dot/underscore/hyphen
# separators so substrings of unrelated words (e.g. "multistorm")
# don't false-match.
_NEST_TOKEN_RE = re.compile(
    r'(?:^|[._-])(storm\d*|nest\d*|moving|d03)(?:[._-]|$)',
    re.IGNORECASE,
)
_PARENT_TOKEN_RE = re.compile(
    r'(?:^|[._-])(parent|d01|hwrf)(?:[._-]|$)',
    re.IGNORECASE,
)


def find_grib_files(idir, itag, ext, idate, fhr_fmt, init_hr, fnl_hr, dt,
                    prefer_nest=True):
    """
    Build list of (fhr, filepath) for available GRIB2 files.

    SHIPS diagnostics are storm-centric (shear annulus averages,
    vortex-centered fields, IKE integrals, etc.), so when multiple
    per-fhr GRIB2 files exist -- e.g. HAFS multistorm runs with both
    `.parent.atm.*.grb2` and `.storm2.atm.*.grb2` side by side -- we
    select the higher-resolution storm nest rather than the parent.
    The ``prefer_nest`` flag controls this; set False to take whatever
    ``sorted(glob)`` returns (legacy behavior).

    The ``.sat.`` files (simulated IR / microwave) are excluded here --
    they are picked up separately by ``open_sat_file()`` when needed.
    """
    files = []
    fhr = init_hr
    while fhr <= fnl_hr:
        fhr_str = fhr_fmt % fhr
        # Try common patterns
        patterns = [
            os.path.join(idir, f"{itag}*{fhr_str}*{ext}"),
            os.path.join(idir, f"*f{fhr_str}*{ext}"),
            os.path.join(idir, f"*{fhr_str}*{ext}"),
        ]
        found = False
        for pat in patterns:
            matches = sorted(glob.glob(pat))
            # Drop the companion ``*.sat.*`` satellite-IR bundle; it
            # has no atmospheric fields we need here, and mixing the
            # two would grab the wrong one alphabetically.
            matches = [m for m in matches
                       if '.sat.' not in os.path.basename(m)]
            if not matches:
                continue

            if prefer_nest:
                nest_matches = [m for m in matches
                                if _NEST_TOKEN_RE.search(
                                    os.path.basename(m))]
                if nest_matches:
                    files.append((fhr, nest_matches[0]))
                    found = True
                    break
                # No storm nest at this fhr - fall back to whatever
                # non-parent candidate exists.  This keeps runs where
                # only a single parent file is produced (non-multistorm
                # HAFS, legacy HWRF) working.
                non_parent = [m for m in matches
                              if not _PARENT_TOKEN_RE.search(
                                  os.path.basename(m))]
                chosen = non_parent[0] if non_parent else matches[0]
            else:
                chosen = matches[0]

            files.append((fhr, chosen))
            found = True
            break
        if not found:
            logger.debug(f"No GRIB2 file for fhr={fhr}")
        fhr += dt
    return files


def find_atcf_file(atcf_dir, atcf_tag, idate, sid):
    """Search for ATCF file matching storm and cycle.

    ``atcf_dir`` may be a single path or a list/tuple of paths to try
    in order -- typically [ATCF2_DIR, ATCF1_DIR] so the experiment's
    real track directory wins over a placeholder.  Files whose basename
    contains ``atcf_tag`` are preferred over siblings, and ``.parent.``
    variants are ranked below nest-merged ones.  Per-fhr splits
    (``.f000``, ``.f003``, ...) and ``.all`` / ``.orig`` are excluded.
    """
    if isinstance(atcf_dir, (list, tuple)):
        dirs = [d for d in atcf_dir if d]
    else:
        dirs = [atcf_dir] if atcf_dir else []
    if not dirs:
        return None

    basin = sid[-1].lower() if sid else ''
    storm_num = sid[:-1] if sid else ''

    pattern_tmpls = [
        f"{atcf_tag}*{sid.lower()}*{idate}*atcf*",
        f"*{sid.lower()}*{idate}*trak*atcf*",
        f"*{sid.lower()}*{idate}*atcf*",
        f"*{storm_num}{basin}*{idate}*atcf*",
    ]
    broader_tmpls = [
        f"*{sid.lower()}*{idate}*",
        f"*{storm_num}{basin}*{idate[:4]}*",
    ]

    def _rank(path):
        bn = os.path.basename(path)
        tag_match = 0 if (atcf_tag and atcf_tag in bn) else 1
        parent_penalty = 1 if '.parent.' in bn else 0
        return (tag_match, parent_penalty, bn)

    _FHR_RE = re.compile(r'\.f\d{3,4}$')

    def _filter(matches):
        return [m for m in matches
                if not m.endswith(('.grb2', '.grb', '.idx',
                                   '.grib2', '.orig', '.nc'))
                and not os.path.basename(m).endswith('.all')
                and not _FHR_RE.search(os.path.basename(m))]

    for adir in dirs:
        if not os.path.isdir(adir):
            continue
        for tmpl in pattern_tmpls:
            matches = _filter(glob.glob(os.path.join(adir, tmpl)))
            if matches:
                matches.sort(key=_rank)
                return matches[0]
        for tmpl in broader_tmpls:
            matches = _filter(glob.glob(os.path.join(adir, tmpl)))
            if matches:
                matches.sort(key=_rank)
                return matches[0]

    # Bounded recursive fallback: the flat globs above are non-recursive, so a
    # track nested under e.g. com/<cycle>/<storm>/ is missed when ATCF*_DIR
    # points higher. Walk up to 4 levels below each dir (depth-capped so a big
    # ATCF*_DIR can't trigger an unbounded walk) and match basename on
    # sid + idate + 'atcf'.
    sid_lc = sid.lower()
    walked = [full for full, bn in walk_files_depth_limited(dirs, max_depth=4)
              if sid_lc in bn.lower() and idate in bn
              and 'atcf' in bn.lower()]
    walked = _filter(walked)
    if walked:
        walked.sort(key=_rank)
        return walked[0]

    return None


# ============================================================
# Main
# ============================================================

def main():
    parser = argparse.ArgumentParser(
        description='GPLOT Ships Module - SHIPS diagnostics and plots')
    parser.add_argument('--idate', required=True, help='Init date (YYYYMMDDHH)')
    parser.add_argument('--sid', required=True, help='Storm ID (e.g., 13L)')
    parser.add_argument('--domain', default='ships', help='Domain name')
    parser.add_argument('--tier', default='Tier1', help='Graphic tier')
    parser.add_argument('--master-nml', required=True, help='Master namelist path')
    parser.add_argument('--force', action='store_true', help='Force reprocessing')
    parser.add_argument('--ensid', default='', help='Ensemble ID')
    parser.add_argument('--modelid', default='', help='Model ID override')
    parser.add_argument('--idir', default='', help='Input directory override')
    parser.add_argument('--odir', default='', help='Output directory override')
    parser.add_argument('--atcf-dir', default='', help='ATCF directory override')
    parser.add_argument('-v', '--verbose', action='count', default=0,
                        help='Increase verbosity')

    args = parser.parse_args()

    # Set up logging
    log_level = logging.DEBUG if args.verbose > 0 else logging.INFO
    logging.basicConfig(
        level=log_level,
        format='%(asctime)s %(name)s %(levelname)s: %(message)s')

    idate = args.idate
    sid = args.sid
    domain = args.domain
    tier = args.tier
    ensid = ens_utils.normalize_ensid(getattr(args, 'ensid', ''))

    # ---- Read master namelist ----
    nml = read_master_namelist(args.master_nml)
    configure_cartopy(nml.get('CARTOPY_DIR'))
    gplot_dir = nml.get('GPLOT_DIR', os.environ.get('GPLOT_DIR', ''))
    if not gplot_dir:
        gplot_dir = os.path.abspath(
            os.path.join(os.path.dirname(__file__), '..', '..', '..'))

    dsource = nml.get('DSOURCE', 'HAFS')
    expt = nml.get('EXPT', '')
    idir = args.idir or nml.get('IDIR', '')
    odir = args.odir or nml.get('ODIR', '')
    itag = nml.get('ITAG', '')
    ext = nml.get('EXT', '.grb2')
    init_hr = int(nml.get('INIT_HR', 0))
    fnl_hr = int(nml.get('FNL_HR', 126))
    fhrfmt_raw = nml.get('FMT_HR', 3)
    dt = int(nml.get('DT', 3))
    do_gif = str(nml.get('DO_CONVERTGIF', 'False')).lower() == 'true'
    modelid = args.modelid or nml.get('MODELID', '')

    # ATCF directory/tag: prefer the merged multistorm (ATCF2) over parent
    # track (ATCF1), mirroring the selection logic in GPLOT_maps.py.
    if args.atcf_dir:
        atcf_dirs = [args.atcf_dir]
    else:
        atcf_dirs = [d for d in (nml.get('ATCF2_DIR', ''),
                                 nml.get('ATCF1_DIR', '')) if d]
    atcf_tag = nml.get('ATCF2_TAG', '') or nml.get('ATCF1_TAG', '')

    # Handle list values from namelist
    if isinstance(ext, list):
        ext = ext[0] if ext else '.grb2'
    if isinstance(itag, list):
        itag = itag[0] if itag else ''
    if isinstance(idir, list):
        idir = idir[0] if idir else ''

    # Build forecast hour format string
    try:
        ndigits = int(fhrfmt_raw)
        fhrfmt = f'%0{ndigits}d'
    except (ValueError, TypeError):
        fhrfmt = '%03d'

    # ---- Build output directory ----
    # Convention shared with polar/maps: ODIR_TYPE=1 means the caller
    # supplied an experiment/cycle-specific ODIR (just append the
    # module/domain); ODIR_TYPE=0 (default) means ODIR is the GPOUT
    # root and we append <expt>/<idate>/<domain> ourselves. The legacy
    # ships ODIR_TYPE=1 path inserted idate/sid.upper() into an
    # already-specific ODIR (non-standard) and the ODIR_TYPE=0 branch
    # was missing the experiment/date prefix entirely (which produced
    # a spurious GPOUT/ships/Tier1/ directory). Tier subdir dropped --
    # all tiers' figures live under <domain>/ together (tier still
    # selects the right namelist via resolve_namelist_path).
    odir_type = int(nml.get('ODIR_TYPE', 0))
    # Ensemble member sub-directory ('' for deterministic -> unchanged path).
    ens_seg = ens_utils.member_segment(ensid)
    if odir_type == 1:
        odir_ships = os.path.join(odir, ens_seg, domain)
    else:
        odir_ships = os.path.join(odir, expt, idate, ens_seg, domain)
    os.makedirs(odir_ships, exist_ok=True)
    status_file = os.path.join(odir_ships, f'status.{domain}.{tier}.{sid}.log')
    _write_status(status_file, 'working')

    # Plotted-file tracking (matches the polar/airsea/maps convention
    # and the spawn_ships.sh expectation of
    # ${ODIR_FULL}/PlottedFiles.${DMN}.${TR}${STORMTAG}.log). Per-FHR
    # GRIB2 paths are appended on successful FHR processing; on
    # subsequent runs (without --force) any FHR whose full path is
    # already in this log is skipped. Coexists with the in-memory
    # dat_store skip below -- either skip mechanism wins.
    plotted_log = os.path.join(
        odir_ships,
        f'PlottedFiles.{domain}.{tier}.{sid}.log')

    logger.info(f"GPLOT Ships starting: {sid} {idate}")
    logger.info(f"  DSOURCE={dsource} EXPT={expt}")
    logger.info(f"  IDIR={idir}")
    logger.info(f"  ODIR={odir_ships}")

    # ---- Read ships namelist ----
    ships_nml_name = nml.get('SHIPS_NML', f'namelist.ships.default')
    ships_nml_path = os.path.join(gplot_dir, 'parm', ships_nml_name)
    if not os.path.isfile(ships_nml_path):
        # Try without path
        ships_nml_path = os.path.join(gplot_dir, 'parm',
                                       'namelist.ships.default')

    recipes = read_ships_namelist(ships_nml_path)
    active_diags = {r['FILE_NAME']: r for r in recipes
                    if r.get('DATA_ON', 'F').upper() == 'T'}
    plot_diags = {r['FILE_NAME']: r for r in recipes
                  if r.get('PLOT_ON', 'F').upper() == 'T'}

    logger.info(f"  Active diagnostics: {list(active_diags.keys())}")

    # ---- Find ATCF file ----
    atcf_file = find_atcf_file(atcf_dirs, atcf_tag, idate, sid)
    if atcf_file is None:
        # find_atcf_file globs ATCF*_DIR non-recursively; if those namelist
        # dirs sit above the actual file (e.g. ATCF under com/<cycle>/<storm>/)
        # it misses it. Fall back to the spawn's recursively-resolved path in
        # ATCF_FILES.dat -- the same source polar/airsea use.
        fallback = atcf_from_listfile(odir_ships, sid)
        if fallback is not None:
            logger.warning(f"find_atcf_file found nothing under {atcf_dirs}; "
                           f"using ATCF_FILES.dat fallback -> {fallback}")
            atcf_file = fallback
    if atcf_file is None:
        logger.error("No ATCF file found; SHIPS requires ATCF data")
        _write_status(status_file, 'failed')
        return 1

    logger.info(f"  ATCF: {atcf_file}")
    atcf_df = read_atcf(atcf_file)

    # Ensemble member ATCFs are 00L-named multi-storm; keep only this storm.
    if atcf_df is not None and ensid and len(sid) >= 3:
        atcf_df = ens_utils.filter_atcf_df(atcf_df, sid[2], sid[:2])
    if atcf_df.empty:
        logger.error("ATCF file is empty")
        _write_status(status_file, 'failed')
        return 1

    # Resolve LONGSID for plot titles + figure filenames. Priority:
    # ATCF filename's '<name><sid>' prefix (legacy NCL convention) ->
    # B-deck column-28 storm_name -> A-deck column-28 storm_name ->
    # bare sid. DAT filenames continue to use raw sid for stability
    # (existing DAT files on disk would otherwise be orphaned when the
    # storm gets named mid-cycle).
    bdeck_df_for_name = None
    bdeck_dir = (nml.get('BDECK_DIR') or '').strip()
    if bdeck_dir and os.path.isdir(bdeck_dir):
        _basin_map = {'l': 'al', 'e': 'ep', 'c': 'cp', 'w': 'wp',
                      's': 'sh', 'p': 'sh', 'a': 'io', 'b': 'io'}
        try:
            _basin1 = sid[2].lower()
            _basin2 = _basin_map.get(_basin1, '')
            _snum = sid[:2]
            _bdeck_path = os.path.join(bdeck_dir,
                                       f'b{_basin2}{_snum}{idate[:4]}.dat')
            if os.path.isfile(_bdeck_path):
                bdeck_df_for_name = read_bdeck(_bdeck_path)
        except (IndexError, AttributeError):
            pass
    # Pass both b-deck and a-deck so derive_longsid can fall through
    # to the operational a-deck's per-cycle storm_name when the
    # b-deck has no row at idate (e.g., retrospective at pre-genesis
    # cycles where the post-season b-deck doesn't go back that far).
    longsid = derive_longsid(atcf_file, sid, bdeck_df_for_name,
                             idate=idate, adeck_df=atcf_df, ensid=ensid)
    logger.info(f"  LONGSID: {longsid}")

    # Filter to this cycle
    if 'init_date' in atcf_df.columns:
        cycle_mask = atcf_df['init_date'] == idate
        if cycle_mask.any():
            atcf_df = atcf_df[cycle_mask].copy()
    # Filter to model if specified
    if modelid and 'model' in atcf_df.columns:
        model_mask = atcf_df['model'].str.upper() == modelid.upper()
        if model_mask.any():
            atcf_df = atcf_df[model_mask].copy()

    # ---- Find GRIB2 files ----
    # Prefer the file list spawn_ships.sh prepared: it does the full
    # IDIR_OPTS directory-layout discovery (HAFS, HWRF, HFSA, GFS,
    # ECMWF, ensembles, ...) that the in-Python find_grib_files()
    # cannot replicate. Polar/airsea already consume these lists; this
    # brings ships in line so HFSA + any other spawn-supported layout
    # works without duplicating directory-discovery logic.
    #
    # Fallback to in-Python discovery when the spawn lists are absent
    # (e.g. running GPLOT_ships.py directly for dev / smoke testing).
    grib_files = read_spawn_file_list(odir_ships, domain, tier, sid)
    if grib_files is not None:
        logger.info(f"  Using spawn-prepared file list: "
                    f"{len(grib_files)} FHRs")
    else:
        logger.info("  No spawn file list found; falling back to "
                    "find_grib_files() discovery")
        grib_files = find_grib_files(idir, itag, ext, idate, fhrfmt,
                                      init_hr, fnl_hr, dt)
    if not grib_files:
        logger.error(f"No GRIB2 files found in {idir}")
        _write_status(status_file, 'failed')
        return 1

    logger.info(f"  Found {len(grib_files)} GRIB2 files")

    # ---- Initialize DAT storage ----
    # Load existing DAT files to preserve previous data
    dat_store = {}
    scalar_diags = ['SHRD', 'SHTD', 'SHRS', 'SHTS', 'SHDC', 'SDDC',
                    'MSLP', 'PENV', 'VMAX', 'IKE34', 'IKE50', 'IKE64',
                    'U200', 'U20C', 'V20C', 'RHLO', 'RHMD', 'RHHI',
                    'R000', 'Z850', 'D200', 'DIVC', 'T000', 'CAPE', 'HLCY']

    for diag in scalar_diags:
        if diag in active_diags:
            path = _dat_path(odir_ships, longsid, diag, idate)
            dat_store[diag] = _read_existing_dat(path, ncols=2)

    tccen_store = {}
    tchodo_store = {}
    if 'TCCEN' in active_diags:
        path = _dat_path(odir_ships, longsid, 'TCCEN', idate)
        tccen_store = _read_existing_dat(path, ncols=5)
    if 'TCHODO' in active_diags:
        path = _dat_path(odir_ships, longsid, 'TCHODO', idate)
        tchodo_store = _read_existing_dat(path, ncols=4)

    # ---- Main forecast hour loop ----
    n_processed = 0
    generated_plots = []

    for fhr, grib_path in grib_files:
        # Check ATCF availability for this hour
        atcf_row = atcf_df[atcf_df['fhr'] == fhr]
        if atcf_row.empty:
            logger.debug(f"No ATCF entry for fhr={fhr}, skipping")
            continue

        tc_lat = atcf_row.iloc[0]['lat']
        tc_lon = atcf_row.iloc[0]['lon']

        # On-disk figure gate. If any ships figure for this FHR exists,
        # treat the FHR as done -- regardless of --force. Mirrors the
        # polar/airsea behavior so the spawn FORCE flip (triggered by
        # recent ATCF mtime while the model is running) doesn't cause
        # already-finished FHRs to re-render every loop. dat_store /
        # tccen_store / tchodo_store are preloaded from existing DAT
        # files above, so end-of-run DAT writers stay consistent --
        # values for skipped FHRs come from disk and get rewritten
        # unchanged. To genuinely re-render, delete the figure files.
        fhr_glob = os.path.join(
            odir_ships,
            f"{longsid.lower()}.*.{idate}.ships.f{fhr:03d}.*")
        if glob.glob(fhr_glob):
            logger.debug(f"FHR {fhr:03d}: ships figure(s) on disk, "
                         f"skipping")
            continue

        # Skip if already done and not forcing.
        # Two independent skip checks; either may fire:
        #   (a) PlottedFiles.<dmn>.<tier>.<sid>.log lists this GRIB
        #       basename (the polar/airsea/maps convention).
        #   (b) Every active scalar diagnostic already has a value
        #       for this FHR in the in-memory dat_store (the legacy
        #       ships behavior, preserved for backward compatibility).
        if not args.force:
            if os.path.isfile(plotted_log):
                with open(plotted_log, 'r') as f:
                    if grib_path in f.read():
                        logger.debug(f"FHR {fhr:03d} already in "
                                     f"PlottedFiles, skipping")
                        continue
            all_done = True
            for diag in active_diags:
                if diag in scalar_diags and fhr not in dat_store.get(diag, {}):
                    all_done = False
                    break
            if all_done:
                logger.debug(f"FHR {fhr:03d} already processed, skipping")
                continue

        logger.info(f"Processing FHR {fhr:03d}: {grib_path}")
        logger.info(f"  TC position: {tc_lat:.2f}N, {tc_lon:.2f}E")

        try:
            datasets = open_grib2(grib_path)
        except Exception as e:
            logger.error(f"Failed to open {grib_path}: {e}")
            continue

        # Storm motion
        motion_spd, motion_dir = compute_storm_motion(atcf_df, fhr)

        # ---- Compute each diagnostic ----

        # SHRD / SHTD (deep-layer shear, 200-800km)
        if 'SHRD' in active_diags or 'SHTD' in active_diags:
            shrd, shtd = compute_shear(datasets, dsource, tc_lat, tc_lon,
                                        200, 850, 200, 800)
            if 'SHRD' in active_diags:
                dat_store.setdefault('SHRD', {})[fhr] = shrd
            if 'SHTD' in active_diags:
                dat_store.setdefault('SHTD', {})[fhr] = shtd
            logger.debug(f"  SHRD={shrd:.1f} kt, SHTD={shtd:.1f} deg")

        # SHRS / SHTS (shallow-layer shear)
        if 'SHRS' in active_diags or 'SHTS' in active_diags:
            shrs, shts = compute_shear(datasets, dsource, tc_lat, tc_lon,
                                        500, 850, 200, 800)
            if 'SHRS' in active_diags:
                dat_store.setdefault('SHRS', {})[fhr] = shrs
            if 'SHTS' in active_diags:
                dat_store.setdefault('SHTS', {})[fhr] = shts

        # TCCEN - must come before SHDC/DIVC which use 850 hPa center
        centers = None
        if 'TCCEN' in active_diags:
            centers = compute_tccen(datasets, dsource, tc_lat, tc_lon)
            for lev, (clat, clon, _hgt, flag) in centers.items():
                if np.isfinite(clat):
                    tccen_store[(fhr, lev)] = [clat, clon, flag]

            # Plot TCCEN for each forecast hour (full + zoom; matches
            # the legacy NCL behavior of producing both panels).
            if 'TCCEN' in plot_diags:
                shrd_val = dat_store.get('SHRD', {}).get(fhr, np.nan)
                shtd_val = dat_store.get('SHTD', {}).get(fhr, np.nan)
                shrs_val = dat_store.get('SHRS', {}).get(fhr, np.nan)
                shts_val = dat_store.get('SHTS', {}).get(fhr, np.nan)
                p = plot_tccen(centers, tc_lat, tc_lon, fhr, longsid, idate,
                               odir_ships, shrd_val, shtd_val, shrs_val,
                               shts_val, motion_spd, motion_dir,
                               do_gif)
                if p:
                    generated_plots.append(p)
                p = plot_tccen(centers, tc_lat, tc_lon, fhr, longsid, idate,
                               odir_ships, shrd_val, shtd_val, shrs_val,
                               shts_val, motion_spd, motion_dir,
                               do_gif, zoom=True)
                if p:
                    generated_plots.append(p)

        # SHDC / SDDC (deep shear centered on 850 hPa vortex)
        if 'SHDC' in active_diags:
            lat850, lon850 = tc_lat, tc_lon
            if centers and 850 in centers and np.isfinite(centers[850][0]):
                lat850, lon850 = centers[850][0], centers[850][1]
            shdc, sddc = compute_shear(datasets, dsource, lat850, lon850,
                                        200, 850, 0, 500)
            dat_store.setdefault('SHDC', {})[fhr] = shdc
            dat_store.setdefault('SDDC', {})[fhr] = sddc

        # MSLP / PENV
        if 'MSLP' in active_diags or 'PENV' in active_diags:
            mslp, penv = compute_mslp_penv(datasets, dsource, tc_lat, tc_lon)
            if 'MSLP' in active_diags:
                dat_store.setdefault('MSLP', {})[fhr] = mslp
            if 'PENV' in active_diags:
                dat_store.setdefault('PENV', {})[fhr] = penv

        # VMAX / IKE
        if any(d in active_diags for d in ['VMAX', 'IKE34', 'IKE50', 'IKE64']):
            vmax, ike34, ike50, ike64 = compute_vmax_ike(
                datasets, dsource, tc_lat, tc_lon)
            if 'VMAX' in active_diags:
                dat_store.setdefault('VMAX', {})[fhr] = vmax
            if 'IKE34' in active_diags:
                dat_store.setdefault('IKE34', {})[fhr] = ike34
            if 'IKE50' in active_diags:
                dat_store.setdefault('IKE50', {})[fhr] = ike50
            if 'IKE64' in active_diags:
                dat_store.setdefault('IKE64', {})[fhr] = ike64

        # U200 (200 hPa zonal wind, 200-800km)
        if 'U200' in active_diags:
            val = compute_annular_scalar(datasets, dsource, 'U', '200',
                                          tc_lat, tc_lon, 200, 800)
            dat_store.setdefault('U200', {})[fhr] = val

        # U20C (200 hPa zonal wind, 0-500km)
        if 'U20C' in active_diags:
            val = compute_annular_scalar(datasets, dsource, 'U', '200',
                                          tc_lat, tc_lon, 0, 500,
                                          radius_km=500)
            dat_store.setdefault('U20C', {})[fhr] = val

        # V20C (200 hPa meridional wind, 0-500km)
        if 'V20C' in active_diags:
            val = compute_annular_scalar(datasets, dsource, 'V', '200',
                                          tc_lat, tc_lon, 0, 500,
                                          radius_km=500)
            dat_store.setdefault('V20C', {})[fhr] = val

        # RHLO (850-700 hPa RH, 200-800km)
        if 'RHLO' in active_diags:
            val = compute_annular_scalar(datasets, dsource, 'RHLO', '',
                                          tc_lat, tc_lon, 200, 800,
                                          clamp=(0, 100))
            dat_store.setdefault('RHLO', {})[fhr] = val

        # RHMD (700-500 hPa RH)
        if 'RHMD' in active_diags:
            val = compute_annular_scalar(datasets, dsource, 'RHMD', '',
                                          tc_lat, tc_lon, 200, 800,
                                          clamp=(0, 100))
            dat_store.setdefault('RHMD', {})[fhr] = val

        # RHHI (500-300 hPa RH)
        if 'RHHI' in active_diags:
            val = compute_annular_scalar(datasets, dsource, 'RHHI', '',
                                          tc_lat, tc_lon, 200, 800,
                                          clamp=(0, 100))
            dat_store.setdefault('RHHI', {})[fhr] = val

        # R000 (1000 hPa RH, 200-800km)
        if 'R000' in active_diags:
            val = compute_annular_scalar(datasets, dsource, 'RH', '1000',
                                          tc_lat, tc_lon, 200, 800,
                                          clamp=(0, 100))
            dat_store.setdefault('R000', {})[fhr] = val

        # Z850 (850 hPa vorticity, 0-1000km, scaled x100)
        if 'Z850' in active_diags:
            val = compute_annular_scalar(datasets, dsource, 'RVO', '850',
                                          tc_lat, tc_lon, 0, 1000,
                                          radius_km=1000, scale=100.0)
            dat_store.setdefault('Z850', {})[fhr] = val

        # D200 (200 hPa divergence, 0-1000km)
        if 'D200' in active_diags:
            val = compute_divergence(datasets, dsource, tc_lat, tc_lon, 1000)
            dat_store.setdefault('D200', {})[fhr] = val

        # DIVC (200 hPa divergence at 850 hPa center)
        if 'DIVC' in active_diags:
            lat850, lon850 = tc_lat, tc_lon
            if centers and 850 in centers and np.isfinite(centers[850][0]):
                lat850, lon850 = centers[850][0], centers[850][1]
            val = compute_divergence(datasets, dsource, tc_lat, tc_lon, 1000,
                                      center_lat=lat850, center_lon=lon850)
            dat_store.setdefault('DIVC', {})[fhr] = val

        # T000 (1000 hPa temperature, 200-800km, K->C)
        if 'T000' in active_diags:
            val = compute_annular_scalar(datasets, dsource, 'T', '1000',
                                          tc_lat, tc_lon, 200, 800,
                                          offset=-273.15)
            dat_store.setdefault('T000', {})[fhr] = val

        # CAPE (0-200km)
        if 'CAPE' in active_diags:
            val = compute_annular_scalar(datasets, dsource, 'CAPE', '',
                                          tc_lat, tc_lon, 0, 200,
                                          radius_km=200)
            dat_store.setdefault('CAPE', {})[fhr] = val

        # HLCY (0-200km)
        if 'HLCY' in active_diags:
            val = compute_annular_scalar(datasets, dsource, 'HLCY', '',
                                          tc_lat, tc_lon, 0, 200,
                                          radius_km=200)
            dat_store.setdefault('HLCY', {})[fhr] = val

        # TCHODO (wind hodograph)
        if 'TCHODO' in active_diags:
            hodo = compute_hodograph(datasets, dsource, tc_lat, tc_lon,
                                      centers)
            for lev, (mag, dirn) in hodo.items():
                tchodo_store[(fhr, lev)] = [mag, dirn]

            # Plot hodograph for each forecast hour
            if 'TCHODO' in plot_diags:
                p = plot_hodograph(hodo, fhr, longsid, idate, odir_ships,
                                    motion_spd, motion_dir, do_gif)
                if p:
                    generated_plots.append(p)
                p = plot_hodograph(hodo, fhr, longsid, idate, odir_ships,
                                    motion_spd, motion_dir, do_gif,
                                    zoom=True)
                if p:
                    generated_plots.append(p)

        n_processed += 1

        # FHR completed successfully (we got past every diagnostic
        # without raising and at least one diag wrote into dat_store).
        # Mark in PlottedFiles.<dmn>.<tier>.<sid>.log so subsequent
        # runs without --force skip this GRIB cleanly.
        update_plotted_file(plotted_log, grib_path)

    # ---- Write all DAT files ----
    for diag in scalar_diags:
        if diag in dat_store and dat_store[diag]:
            path = _dat_path(odir_ships, longsid, diag, idate)
            _write_dat_2col(path, dat_store[diag])
            logger.info(f"  Wrote {path} ({len(dat_store[diag])} entries)")

    if tccen_store:
        path = _dat_path(odir_ships, longsid, 'TCCEN', idate)
        _write_dat_multicol(path, tccen_store, ncols=5)
        logger.info(f"  Wrote {path}")

    if tchodo_store:
        path = _dat_path(odir_ships, longsid, 'TCHODO', idate)
        _write_dat_multicol(path, tchodo_store, ncols=4)
        logger.info(f"  Wrote {path}")

    # ---- Trend plots (at the end) ----
    for diag in scalar_diags:
        if diag in plot_diags and diag in dat_store and dat_store[diag]:
            # For now, just plot the current cycle
            # TODO: Load prior cycles for multi-cycle overlay
            trend_data = [(idate, dat_store[diag])]
            ylabel = _diag_ylabel(diag)
            p = plot_trend(trend_data, diag, odir_ships, longsid, idate,
                           ylabel=ylabel, do_gif=do_gif)
            if p:
                generated_plots.append(p)

    logger.info(f"GPLOT Ships complete: {n_processed} forecast hours processed, "
                f"{len(generated_plots)} plots generated")
    # Retry-convert any orphan .png left behind by transient ImageMagick
    # failures. If the retry also fails, write status='incomplete' so
    # the workflow re-invokes us and the on-disk fast-path picks up
    # the missing .gif FHRs.
    sweep_result = sweep_orphan_pngs(odir_ships)
    if sweep_result.get('still_failed', 0) > 0:
        logger.warning(
            f"GPLOT Ships: {sweep_result['still_failed']} PNG(s) still "
            f"unconverted after sweep; writing status='incomplete'.")
        _write_status(status_file, 'incomplete')
    else:
        _write_status(status_file, 'complete')
    return 0


def _diag_ylabel(diag):
    """Return Y-axis label for a given diagnostic."""
    labels = {
        'SHRD': 'Deep Shear [kt]',
        'SHTD': 'Deep Shear Heading [deg]',
        'SHRS': 'Shallow Shear [kt]',
        'SHTS': 'Shallow Shear Heading [deg]',
        'SHDC': 'Deep Shear (Vortex Center) [kt]',
        'MSLP': 'Min SLP [hPa]',
        'PENV': 'Env Pressure [hPa]',
        'VMAX': 'Max Wind [kt]',
        'IKE34': 'IKE (34kt) [TJ]',
        'IKE50': 'IKE (50kt) [TJ]',
        'IKE64': 'IKE (64kt) [TJ]',
        'U200': '200hPa Zonal Wind [kt]',
        'U20C': '200hPa Zonal Wind 0-500km [kt]',
        'V20C': '200hPa Merid Wind 0-500km [kt]',
        'RHLO': 'RH 850-700hPa [%]',
        'RHMD': 'RH 700-500hPa [%]',
        'RHHI': 'RH 500-300hPa [%]',
        'R000': 'RH 1000hPa [%]',
        'Z850': '850hPa Vorticity [10^-5 s^-1]',
        'D200': '200hPa Divergence [10^-7 s^-1]',
        'DIVC': '200hPa Divergence (Vortex Ctr) [10^-7 s^-1]',
        'T000': '1000hPa Temperature [C]',
        'CAPE': 'CAPE [J/kg]',
        'HLCY': 'Helicity [J/kg]',
    }
    return labels.get(diag, diag)


if __name__ == '__main__':
    sys.exit(main() or 0)
