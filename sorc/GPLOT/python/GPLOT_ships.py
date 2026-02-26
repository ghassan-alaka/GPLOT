#!/usr/bin/env python3
"""
GPLOT_ships.py

Python translation of GPLOT_ships.ncl (3072 lines).

Extracts SHIPS (Statistical Hurricane Intensity Prediction Scheme)
diagnostics from model output, writes .DAT files, and produces
graphical products (TCCEN maps, TCHODO hodographs, trend time-series).

Original NCL: GPLOT/sorc/GPLOT/ncl/GPLOT_ships.ncl
Translated:   GPLOT/sorc/GPLOT/python/GPLOT_ships.py

Authors: Translated by GPLOT Python migration (2024)
         Original NCL: Ghassan Alaka Jr., Mu-Chieh Ko, Lewis J. Gramer
"""

# =======================================================================
# PART I – IMPORTS & CONSTANTS
# =======================================================================
import os
import sys
import math
import glob
import time
import fcntl
import logging
import datetime
import argparse
import subprocess
import numpy as np

try:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    import matplotlib.ticker as mticker
    import matplotlib.colors as mcolors
    import matplotlib.cm as cm
    from matplotlib.patches import FancyArrowPatch
    HAS_MATPLOTLIB = True
except ImportError:
    HAS_MATPLOTLIB = False

try:
    import cartopy.crs as ccrs
    import cartopy.feature as cfeature
    HAS_CARTOPY = True
except ImportError:
    HAS_CARTOPY = False

try:
    from scipy.interpolate import RegularGridInterpolator
    HAS_SCIPY = True
except ImportError:
    HAS_SCIPY = False

try:
    import netCDF4 as nc4
    HAS_NC4 = True
except ImportError:
    HAS_NC4 = False

_SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(_SCRIPT_DIR, "modules"))

try:
    import gplot_util as gu
    import gplot_func as gf
    import gplot_main as gm
except ImportError as _e:
    sys.exit(f"ERROR: Cannot import GPLOT modules: {_e}")

# Physical / unit constants
MS2KTS  = 1.94384        # m/s → knots
KTS2MS  = 1.0 / MS2KTS
RHO_AIR = 1.15           # air density [kg m-3]
RE_KM   = 6371.0         # Earth radius [km]
DEG2RAD = math.pi / 180.0
RAD2DEG = 180.0 / math.pi
FILL    = 1.0e20

LOG = logging.getLogger("GPLOT_ships")

# =======================================================================
# PART II – SPH2CART REPLACEMENT (haversine-based Cartesian resampling)
# =======================================================================

def _cart_grid(cart_km: float = 1200.0, dx_km: float = 20.0):
    """Build symmetric 1-D X and Y arrays (km) for the Cartesian grid."""
    n = int(cart_km / dx_km)
    x = np.linspace(-cart_km, cart_km, 2 * n + 1)
    y = np.linspace(-cart_km, cart_km, 2 * n + 1)
    return x, y


def sph2cart(field, lat_1d, lon_1d, ctr_lat, ctr_lon,
             cart_km: float = 1200.0, dx_km: float = 20.0,
             fill_val: float = FILL):
    """
    Resample *field* (lat×lon or lev×lat×lon) from geographic coords to a
    Cartesian grid centred on (ctr_lat, ctr_lon).

    Returns
    -------
    cart   : ndarray  (ny, nx) or (nlev, ny, nx)
    x_km   : ndarray  (nx,)
    y_km   : ndarray  (ny,)
    """
    if not HAS_SCIPY:
        raise RuntimeError("scipy required for sph2cart (RegularGridInterpolator)")

    field  = np.asarray(field,  dtype=float)
    lat_1d = np.asarray(lat_1d, dtype=float)
    lon_1d = np.asarray(lon_1d, dtype=float)

    # Ensure lons are monotonically ordered and consistent with ctr_lon
    lon_1d = lon_1d.copy()
    if ctr_lon > 0 and np.any(lon_1d < 0):
        lon_1d[lon_1d < 0] += 360.0
    elif ctr_lon < 0 and np.any(lon_1d > 180):
        lon_1d[lon_1d > 180] -= 360.0

    # Ensure lat_1d is ascending for RegularGridInterpolator
    lat_flip = lat_1d[0] > lat_1d[-1]
    if lat_flip:
        lat_1d = lat_1d[::-1]
        field   = field[..., ::-1, :] if field.ndim >= 2 else field

    x_km, y_km = _cart_grid(cart_km, dx_km)
    ny, nx = len(y_km), len(x_km)

    coslat = max(math.cos(math.radians(ctr_lat)), 1e-6)
    xx, yy = np.meshgrid(x_km, y_km)        # (ny, nx)
    geo_lat = ctr_lat + yy / RE_KM * RAD2DEG
    geo_lon = ctr_lon + (xx / RE_KM * RAD2DEG) / coslat

    # Validity mask: inside model domain
    lat_min, lat_max = float(lat_1d[0]),  float(lat_1d[-1])
    lon_min, lon_max = float(lon_1d[0]),  float(lon_1d[-1])
    if lon_min > lon_max:
        lon_min, lon_max = lon_max, lon_min
    valid = ((geo_lat >= lat_min) & (geo_lat <= lat_max) &
             (geo_lon >= lon_min) & (geo_lon <= lon_max))

    def _interp2d(fld2d):
        interp = RegularGridInterpolator(
            (lat_1d, lon_1d), fld2d,
            method="linear", bounds_error=False, fill_value=fill_val)
        pts = np.column_stack([geo_lat.ravel(), geo_lon.ravel()])
        out = interp(pts).reshape(ny, nx)
        out[~valid] = fill_val
        return out

    if field.ndim == 2:
        cart = _interp2d(field)
    elif field.ndim == 3:
        cart = np.full((field.shape[0], ny, nx), fill_val)
        for k in range(field.shape[0]):
            cart[k] = _interp2d(field[k])
    else:
        raise ValueError(f"sph2cart: expected 2-D or 3-D field, got {field.shape}")

    return cart, x_km, y_km


def _dist_grid(x_km, y_km):
    """Return 2-D distance array (km) from the Cartesian centre."""
    xx, yy = np.meshgrid(x_km, y_km)
    return np.sqrt(xx**2 + yy**2)


def annular_avg(cart2d, x_km, y_km, r_inner, r_outer, fill_val=FILL):
    """Mean of *cart2d* over annulus [r_inner, r_outer] km; returns fill_val if empty."""
    dist = _dist_grid(x_km, y_km)
    mask = (dist >= r_inner) & (dist <= r_outer)
    vals = cart2d[mask]
    valid = vals[np.abs(vals) < np.abs(fill_val) * 0.9]
    return float(np.nanmean(valid)) if valid.size > 0 else fill_val


def annular_avg_uv(cU, cV, x_km, y_km, r_inner, r_outer, fill_val=FILL):
    """Mean U and V over the annulus."""
    ubar = annular_avg(cU, x_km, y_km, r_inner, r_outer, fill_val)
    vbar = annular_avg(cV, x_km, y_km, r_inner, r_outer, fill_val)
    return ubar, vbar


# =======================================================================
# PART III – DIAGNOSTIC COMPUTATION HELPERS
# =======================================================================

def compute_shear(U, V, lat, lon, ctr_lat, ctr_lon, lev,
                  lev_top, lev_bot, r_inner=200.0, r_outer=800.0,
                  fill_val=FILL, dx_km=20.0):
    """
    Compute 850–200 hPa (or lev_bot–lev_top) wind shear magnitude and
    direction over an annulus [r_inner, r_outer] km from TC centre.

    Returns (shear_mag_kts, shear_dir_deg)
    """
    lev = np.asarray(lev, dtype=float)
    itop = np.argmin(np.abs(lev - lev_top))
    ibot = np.argmin(np.abs(lev - lev_bot))

    Utop_cart, x_km, y_km = sph2cart(U[itop], lat, lon, ctr_lat, ctr_lon,
                                       dx_km=dx_km, fill_val=fill_val)
    Vtop_cart, _,    _    = sph2cart(V[itop], lat, lon, ctr_lat, ctr_lon,
                                       dx_km=dx_km, fill_val=fill_val)
    Ubot_cart, _,    _    = sph2cart(U[ibot], lat, lon, ctr_lat, ctr_lon,
                                       dx_km=dx_km, fill_val=fill_val)
    Vbot_cart, _,    _    = sph2cart(V[ibot], lat, lon, ctr_lat, ctr_lon,
                                       dx_km=dx_km, fill_val=fill_val)

    dU = Utop_cart - Ubot_cart
    dV = Vtop_cart - Vbot_cart

    dUbar, dVbar = annular_avg_uv(dU, dV, x_km, y_km, r_inner, r_outer, fill_val)
    if abs(dUbar) >= abs(fill_val) * 0.9 or abs(dVbar) >= abs(fill_val) * 0.9:
        return fill_val, fill_val

    mag_ms  = math.sqrt(dUbar**2 + dVbar**2)
    mag_kts = mag_ms * MS2KTS
    # Meteorological convention: direction FROM which shear vector points
    dir_deg = (math.degrees(math.atan2(dUbar, dVbar)) + 180.0) % 360.0
    return mag_kts, dir_deg


def compute_deep_shear(U, V, lat, lon, ctr_lat_850, ctr_lon_850, lev,
                       r_inner=0.0, r_outer=500.0,
                       fill_val=FILL, dx_km=20.0):
    """
    Deep-layer (850–200 hPa) shear with vortex removed, centred at 850 hPa
    vortex position.  Returns (SHDC_kts, SDDC_deg).
    """
    return compute_shear(U, V, lat, lon, ctr_lat_850, ctr_lon_850, lev,
                         200.0, 850.0, r_inner, r_outer, fill_val, dx_km)


def compute_ike(UV10, lat, lon, threshold_kts, fill_val=FILL):
    """
    Integrated Kinetic Energy [TJ] for 10-m wind speed >= threshold_kts.
    IKE = 0.5 * rho * V^2 * Vol  summed over grid cells meeting threshold.
    VOL = dx * dy * dz  where dz≈1 m for surface layer.
    """
    UV10 = np.asarray(UV10, dtype=float)
    lat  = np.asarray(lat,  dtype=float)
    lon  = np.asarray(lon,  dtype=float)

    # Wind speed in knots
    spd_kts = UV10 * MS2KTS
    mask = (spd_kts >= threshold_kts) & (np.abs(UV10) < np.abs(fill_val) * 0.9)

    if not np.any(mask):
        return 0.0

    # Grid cell area using cosine(lat) weighting
    dlat = np.abs(np.gradient(lat))        # deg
    dlon = np.abs(np.gradient(lon))        # deg
    dlat_m = dlat * DEG2RAD * RE_KM * 1000.0
    dlon_m = dlon * DEG2RAD * RE_KM * 1000.0
    dlon2d, dlat2d = np.meshgrid(dlon_m, dlat_m)
    coslat2d = np.cos(np.radians(lat[:, np.newaxis]))
    area_m2 = dlat2d * dlon2d * coslat2d   # m²

    IKE_J  = 0.5 * RHO_AIR * (UV10**2) * area_m2 * 1.0  # dz=1 m
    IKE_TJ = np.sum(IKE_J[mask]) / 1.0e12
    return float(IKE_TJ)


def compute_divergence(U, V, lat, lon):
    """
    Finite-difference horizontal divergence on a lat/lon grid.
    Returns div [s^-1], same shape as U.
    """
    lat  = np.asarray(lat,  dtype=float)
    lon  = np.asarray(lon,  dtype=float)
    U    = np.asarray(U,    dtype=float)
    V    = np.asarray(V,    dtype=float)

    dlat = np.gradient(lat)      # deg
    dlon = np.gradient(lon)      # deg
    dlat_m = dlat * DEG2RAD * RE_KM * 1000.0
    dlon_m = dlon * DEG2RAD * RE_KM * 1000.0

    dlon2d, dlat2d = np.meshgrid(dlon_m, dlat_m)
    coslat2d = np.cos(np.radians(lat[:, np.newaxis]))

    dUdx = np.gradient(U, axis=-1) / (dlon2d * coslat2d)
    dVdy = np.gradient(V, axis=-2) / dlat2d
    return dUdx + dVdy


def compute_relative_vorticity(U, V, lat, lon):
    """Finite-difference relative vorticity [s^-1]."""
    lat  = np.asarray(lat,  dtype=float)
    lon  = np.asarray(lon,  dtype=float)
    U    = np.asarray(U,    dtype=float)
    V    = np.asarray(V,    dtype=float)

    dlat = np.gradient(lat)
    dlon = np.gradient(lon)
    dlat_m = dlat * DEG2RAD * RE_KM * 1000.0
    dlon_m = dlon * DEG2RAD * RE_KM * 1000.0

    dlon2d, dlat2d = np.meshgrid(dlon_m, dlat_m)
    coslat2d = np.cos(np.radians(lat[:, np.newaxis]))

    dVdx = np.gradient(V, axis=-1) / (dlon2d * coslat2d)
    dUdy = np.gradient(U, axis=-2) / dlat2d
    return dVdx - dUdy


def layer_avg_rh(RH_lev, lev_1d, lev_top, lev_bot):
    """Pressure-weighted vertical average of RH between lev_bot and lev_top."""
    lev_1d  = np.asarray(lev_1d, dtype=float)
    RH_lev  = np.asarray(RH_lev, dtype=float)     # (nlev, nlat, nlon)

    mask = (lev_1d >= lev_top) & (lev_1d <= lev_bot)
    if not np.any(mask):
        return np.full(RH_lev.shape[1:], FILL)

    p_sel  = lev_1d[mask]
    rh_sel = RH_lev[mask]                           # (nsel, nlat, nlon)
    dp     = np.abs(np.gradient(p_sel))
    weights = dp[:, np.newaxis, np.newaxis]
    return np.sum(rh_sel * weights, axis=0) / np.sum(dp)


# =======================================================================
# PART IV – FILE I/O HELPERS
# =======================================================================

def _dat_file_path(odir, longsid, fname, idate, domain):
    return os.path.join(odir, f"{longsid.lower()}.{fname}.{idate}.{domain}.dat")


def _write_dat_value(dat_path, fhr, value, write_mode):
    """Write a scalar (fhr, value) pair to a .DAT file."""
    mode = "w" if write_mode == "W" else "a"
    with open(dat_path, mode) as fh:
        fh.write(f"{int(fhr):4d}  {value:.4f}\n")


def _write_tccen_row(dat_path, fhr, lev, lat, lon, use_flag, write_mode):
    """Write one TCCEN row: fhr lev lat lon use_flag."""
    mode = "w" if write_mode == "W" else "a"
    with open(dat_path, mode) as fh:
        fh.write(f"{int(fhr):4d}  {float(lev):6.1f}  {lat:.4f}  {lon:.4f}  {int(use_flag):1d}\n")


def _write_tchodo_row(dat_path, fhr, lev, mag, dirn, write_mode):
    """Write one TCHODO row: fhr lev wind_mag wind_dir."""
    mode = "w" if write_mode == "W" else "a"
    with open(dat_path, mode) as fh:
        fh.write(f"{int(fhr):4d}  {float(lev):6.1f}  {mag:.4f}  {dirn:.2f}\n")


def _read_dat(dat_path):
    """Read a 2-column (or more) .DAT file; return list of rows as float lists."""
    rows = []
    if not os.path.isfile(dat_path):
        return rows
    with open(dat_path) as fh:
        for line in fh:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            try:
                rows.append([float(x) for x in line.split()])
            except ValueError:
                pass
    return rows


# =======================================================================
# PART V – LOCK FILE HELPERS  (mirrors GPLOT_maps.py)
# =======================================================================

def _lock(lock_path, timeout=180):
    t0 = time.time()
    while True:
        try:
            fd = os.open(lock_path, os.O_CREAT | os.O_EXCL | os.O_WRONLY)
            os.close(fd)
            return True
        except FileExistsError:
            if time.time() - t0 > timeout:
                return False
            time.sleep(2)


def _unlock(lock_path):
    try:
        os.remove(lock_path)
    except OSError:
        pass


# =======================================================================
# PART VI – STATUS FILE HELPER
# =======================================================================

def _update_status(status_file, lock_path, sid_tag, tier, new_entries):
    """Append new_entries (list of strings) to the status/plotted-files log."""
    if not _lock(lock_path, timeout=180):
        LOG.warning("Could not acquire lock for status file %s", status_file)
        return
    try:
        with open(status_file, "a") as fh:
            for line in new_entries:
                fh.write(line + "\n")
    finally:
        _unlock(lock_path)


# =======================================================================
# PART VII – POST-PROCESSING (trim / GIF conversion)
# =======================================================================

def _post_process(ofile, do_rmwhite=True, do_gif=False):
    """Trim white borders and optionally convert to GIF."""
    if do_rmwhite and ofile.endswith(".png"):
        subprocess.run(["convert", "-trim", ofile, ofile],
                       check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    if do_gif:
        gif_path = ofile.replace(".png", ".gif")
        subprocess.run(["convert", ofile, gif_path],
                       check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        return gif_path
    return ofile


# =======================================================================
# PART VIII – GRAPHICS: TCCEN MAP
# =======================================================================

# Level colours matching NCL GPLOT_ships.ncl convention
_LEV_COLORS = {
    1000: "black",
    925:  "saddlebrown",
    850:  "red",
    700:  "magenta",
    500:  "blue",
    400:  "cyan",
    300:  "limegreen",
    250:  "lime",
    200:  "gold",
    150:  "orange",
    100:  "grey",
}
_LEV_LABEL = list(sorted(_LEV_COLORS.keys(), reverse=True))  # 1000→100


def _arrow_uv(mag_kts, dir_deg, scale=1.0):
    """Convert shear magnitude (kts) and meteorological direction → (du, dv)."""
    # Dir is direction FROM which the wind blows; vector points opposite
    rad = math.radians(dir_deg)
    u = -mag_kts * math.sin(rad) * scale
    v = -mag_kts * math.cos(rad) * scale
    return u, v


def plot_tccen(dat_path, ofile, fhr, ctr_lat, ctr_lon,
               shrd, shtd, shrs, shts, motion_spd, motion_dir,
               boco, domain_name, idate_str, sid, model,
               zoom=False, gplot_dir="", do_rmwhite=True, do_gif=False):
    """
    Produce a TCCEN map: coloured level markers for TC centre fixes,
    plus SHRD/SHRS shear arrows and storm motion vector.

    Parameters
    ----------
    dat_path    : path to TCCEN .DAT file (5 columns: fhr lev lat lon use)
    ofile       : output image path (.png)
    fhr         : forecast hour (for selecting rows)
    ctr_lat/lon : nominal storm centre (for arrow reference point)
    shrd/shtd   : 850–200 hPa shear magnitude [kts] / direction [deg]
    shrs/shts   : 850–500 hPa shear magnitude [kts] / direction [deg]
    motion_spd  : 6-h storm motion [kts]
    motion_dir  : storm motion bearing [deg]
    boco        : [lat_s, lat_n, lon_w, lon_e]
    zoom        : if True, auto-scale the domain around the TC centre fixes
    """
    if not (HAS_MATPLOTLIB and HAS_CARTOPY):
        LOG.warning("matplotlib/cartopy not available; skipping TCCEN plot")
        return

    rows = _read_dat(dat_path)
    # Filter to current fhr
    fhr_rows = [r for r in rows if int(r[0]) == int(fhr)]
    if not fhr_rows:
        return

    fig = plt.figure(figsize=(10, 8))
    proj = ccrs.PlateCarree()
    ax   = fig.add_subplot(1, 1, 1, projection=proj)

    if zoom:
        lats = [r[2] for r in fhr_rows]
        lons = [r[3] for r in fhr_rows]
        pad  = 5.0
        ax.set_extent([min(lons)-pad, max(lons)+pad,
                       min(lats)-pad, max(lats)+pad], crs=proj)
    else:
        ax.set_extent([boco[2], boco[3], boco[0], boco[1]], crs=proj)

    ax.add_feature(cfeature.COASTLINE, linewidth=0.5)
    ax.add_feature(cfeature.BORDERS,   linewidth=0.3)
    ax.add_feature(cfeature.LAND,      facecolor="lightgrey", alpha=0.5)
    gl = ax.gridlines(draw_labels=True, linewidth=0.3, linestyle="--", alpha=0.5)
    gl.top_labels   = False
    gl.right_labels = False

    # Plot centre fixes
    for r in fhr_rows:
        _, lev, lat, lon, use = r[0], r[1], r[2], r[3], r[4]
        clr = _LEV_COLORS.get(int(lev), "grey")
        mstyle = "o" if int(use) else "x"
        ax.plot(lon, lat, marker=mstyle, color=clr, markersize=8,
                transform=proj, zorder=5, label=f"{int(lev)} hPa")

    # Shear arrows from nominal centre
    arrow_scale = 0.05   # degrees per knot
    if shrd < FILL * 0.9:
        du, dv = _arrow_uv(shrd, shtd, arrow_scale)
        ax.annotate("", xy=(ctr_lon + du, ctr_lat + dv),
                    xytext=(ctr_lon, ctr_lat),
                    arrowprops=dict(arrowstyle="->", color="red", lw=2),
                    transform=proj, zorder=6)
        ax.text(ctr_lon + du * 1.1, ctr_lat + dv * 1.1,
                f"SHRD={shrd:.1f}kt", color="red", fontsize=7, transform=proj)
    if shrs < FILL * 0.9:
        du, dv = _arrow_uv(shrs, shts, arrow_scale * 0.7)
        ax.annotate("", xy=(ctr_lon + du, ctr_lat + dv),
                    xytext=(ctr_lon, ctr_lat),
                    arrowprops=dict(arrowstyle="->", color="blue", lw=2),
                    transform=proj, zorder=6)
        ax.text(ctr_lon + du * 1.1, ctr_lat + dv * 1.1,
                f"SHRS={shrs:.1f}kt", color="blue", fontsize=7, transform=proj)

    # Storm motion vector
    if motion_spd < FILL * 0.9:
        du, dv = _arrow_uv(motion_spd, (motion_dir + 180.0) % 360.0, arrow_scale)
        ax.annotate("", xy=(ctr_lon + du, ctr_lat + dv),
                    xytext=(ctr_lon, ctr_lat),
                    arrowprops=dict(arrowstyle="->", color="black", lw=1.5,
                                   linestyle="dashed"),
                    transform=proj, zorder=6)

    # De-duplicate legend
    handles, labels = ax.get_legend_handles_labels()
    by_label = dict(zip(labels, handles))
    ax.legend(by_label.values(), by_label.keys(),
              fontsize=6, loc="upper right", framealpha=0.7)

    zoom_tag = "_zoom" if zoom else ""
    title = (f"{model} {sid}  FHR={int(fhr):03d}  Init: {idate_str}\n"
             f"TC Centre Fixes by Level")
    ax.set_title(title, fontsize=9)

    plt.tight_layout()
    plt.savefig(ofile, dpi=120, bbox_inches="tight")
    plt.close(fig)
    _post_process(ofile, do_rmwhite, do_gif)
    LOG.info("Wrote TCCEN plot: %s", ofile)


# =======================================================================
# PART IX – GRAPHICS: TCHODO HODOGRAPH
# =======================================================================

def plot_tchodo(dat_path, ofile, fhr, motion_spd, motion_dir,
                idate_str, sid, model,
                zoom=False, do_rmwhite=True, do_gif=False):
    """
    Produce a hodograph (polar plot) from TCHODO .DAT data.

    Columns in .DAT: fhr lev wind_mag wind_dir
    """
    if not HAS_MATPLOTLIB:
        LOG.warning("matplotlib not available; skipping TCHODO plot")
        return

    rows = _read_dat(dat_path)
    fhr_rows = [r for r in rows if int(r[0]) == int(fhr)]
    if not fhr_rows:
        return

    # Sort by level (descending = surface first for typical hodograph)
    fhr_rows.sort(key=lambda r: -r[1])

    fig, ax = plt.subplots(1, 1, figsize=(8, 8))
    ax.set_aspect("equal")

    # Auto-scale or fixed axis
    mags = [r[2] for r in fhr_rows]
    max_mag = max(mags) if mags else 50.0
    if zoom:
        ax_lim = max_mag * 1.2
    else:
        ax_lim = max(50.0, max_mag * 1.2)

    ax.set_xlim(-ax_lim, ax_lim)
    ax.set_ylim(-ax_lim, ax_lim)

    # Polar grid rings
    for r_ring in np.arange(10, ax_lim + 10, 10):
        circle = plt.Circle((0, 0), r_ring, color="grey",
                             fill=False, linewidth=0.4, linestyle="--")
        ax.add_patch(circle)
        ax.text(r_ring * 0.707, r_ring * 0.707, f"{r_ring:.0f}kt",
                fontsize=6, color="grey")

    # Polar axes lines
    for ang in range(0, 360, 30):
        rad = math.radians(ang)
        ax.plot([0, ax_lim * math.sin(rad)],
                [0, ax_lim * math.cos(rad)],
                color="grey", linewidth=0.3, linestyle=":")

    ax.axhline(0, color="grey", linewidth=0.5)
    ax.axvline(0, color="grey", linewidth=0.5)

    # Plot hodograph points (met convention: wind blows FROM dir)
    us, vs, levs = [], [], []
    for r in fhr_rows:
        _, lev, mag, dirn = r[0], r[1], r[2], r[3]
        rad = math.radians(dirn)
        u = -mag * math.sin(rad)   # wind component East
        v = -mag * math.cos(rad)   # wind component North
        us.append(u)
        vs.append(v)
        levs.append(lev)
        clr = _LEV_COLORS.get(int(lev), "grey")
        ax.plot(u, v, "o", color=clr, markersize=8, zorder=5,
                label=f"{int(lev)} hPa")

    # Connect points with coloured segments
    for i in range(len(us) - 1):
        lev_mid = (levs[i] + levs[i + 1]) / 2.0
        clr = _LEV_COLORS.get(int(levs[i]), "grey")
        ax.plot([us[i], us[i+1]], [vs[i], vs[i+1]], "-", color=clr,
                linewidth=1.5, zorder=4)

    # Storm motion vector
    if motion_spd < FILL * 0.9:
        rad = math.radians((motion_dir + 180.0) % 360.0)  # direction OF motion
        mu = motion_spd * math.sin(rad)
        mv = motion_spd * math.cos(rad)
        ax.annotate("", xy=(mu, mv), xytext=(0, 0),
                    arrowprops=dict(arrowstyle="->", color="black", lw=2))
        ax.text(mu * 1.05, mv * 1.05, f"Motion\n{motion_spd:.1f}kt",
                fontsize=7, color="black")

    handles, labels = ax.get_legend_handles_labels()
    by_label = dict(zip(labels, handles))
    ax.legend(by_label.values(), by_label.keys(),
              fontsize=7, loc="upper right", framealpha=0.7)

    title = (f"{model} {sid}  FHR={int(fhr):03d}  Init: {idate_str}\n"
             f"Near-Storm Hodograph (200–800 km annulus)")
    ax.set_title(title, fontsize=9)
    ax.set_xlabel("U (kt)  →East")
    ax.set_ylabel("V (kt)  →North")

    plt.tight_layout()
    plt.savefig(ofile, dpi=120, bbox_inches="tight")
    plt.close(fig)
    _post_process(ofile, do_rmwhite, do_gif)
    LOG.info("Wrote TCHODO hodograph: %s", ofile)


# =======================================================================
# PART X – GRAPHICS: TREND TIME-SERIES
# =======================================================================

_TREND_COLORS = [
    "black", "red", "blue", "green", "magenta", "orange",
    "cyan", "saddlebrown", "grey", "gold",
]


def _int_mjr_grid(vmin, vmax, n_ticks=6):
    """
    Choose a 'nice' major gridline interval so that ~n_ticks intervals
    span [vmin, vmax].  Mirrors NCL IntMjrGrd logic.
    """
    rng = vmax - vmin
    if rng <= 0:
        return 1.0
    raw = rng / n_ticks
    for base in [1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000]:
        if raw <= base:
            return float(base)
    return raw


def plot_trend(var_name, unit_label, dat_paths_with_dates,
               ofile, sid, model,
               do_rmwhite=True, do_gif=False):
    """
    Overlay trend time-series for nTrend previous forecast cycles.

    Parameters
    ----------
    var_name              : e.g. "SHRD"
    unit_label            : e.g. "kts"
    dat_paths_with_dates  : list of (dat_path, date_label) newest-first
    ofile                 : output image path (.png)
    """
    if not HAS_MATPLOTLIB:
        return

    fig, ax = plt.subplots(figsize=(10, 6))
    ax.set_xlabel("Forecast Hour", fontsize=10)
    ax.set_ylabel(f"{var_name} [{unit_label}]", fontsize=10)

    all_vals = []
    plotted  = 0
    for idx, (dp, dlabel) in enumerate(dat_paths_with_dates):
        rows = _read_dat(dp)
        if not rows:
            continue
        fhrs = [r[0] for r in rows]
        vals = [r[1] for r in rows]
        clr  = _TREND_COLORS[idx % len(_TREND_COLORS)]
        lw   = 2.0 if idx == 0 else 1.0
        ax.plot(fhrs, vals, "-o", color=clr, linewidth=lw, markersize=4,
                label=dlabel, zorder=5 - idx)
        all_vals.extend(vals)
        plotted += 1

    if plotted == 0:
        plt.close(fig)
        return

    # Y-axis range
    valid_vals = [v for v in all_vals if abs(v) < FILL * 0.9]
    if valid_vals:
        vmin = min(valid_vals)
        vmax = max(valid_vals)
        pad  = (vmax - vmin) * 0.1 if vmax != vmin else abs(vmax) * 0.1 + 1.0
        vmin -= pad
        vmax += pad
        intv = _int_mjr_grid(vmin, vmax)
        ax.set_ylim(vmin, vmax)
        ax.yaxis.set_major_locator(mticker.MultipleLocator(intv))

    ax.axvline(x=0, color="grey", linewidth=0.8, linestyle="--")
    ax.grid(True, linestyle="--", alpha=0.4)
    ax.legend(fontsize=8, loc="best", framealpha=0.7)
    ax.set_title(f"{model} {sid} – {var_name} Trend", fontsize=10)

    plt.tight_layout()
    plt.savefig(ofile, dpi=120, bbox_inches="tight")
    plt.close(fig)
    _post_process(ofile, do_rmwhite, do_gif)
    LOG.info("Wrote trend plot: %s", ofile)


# =======================================================================
# PART XI – VARIABLE METADATA TABLE
# =======================================================================

# Each entry: (fname, unit_label, r_inner, r_outer, description)
_VAR_META = {
    "SHRD": ("SHRD", "kts",    200.0,  800.0, "850-200 hPa shear magnitude"),
    "SHTD": ("SHTD", "deg",    200.0,  800.0, "850-200 hPa shear direction"),
    "SHRS": ("SHRS", "kts",    200.0,  800.0, "850-500 hPa shear magnitude"),
    "SHTS": ("SHTS", "deg",    200.0,  800.0, "850-500 hPa shear direction"),
    "SHDC": ("SHDC", "kts",      0.0,  500.0, "850-200 hPa deep shear (vortex removed)"),
    "SDDC": ("SDDC", "deg",      0.0,  500.0, "850-200 hPa deep shear direction (vortex removed)"),
    "MSLP": ("MSLP", "hPa",     None,  None,  "Minimum MSLP"),
    "PENV": ("PENV", "hPa",    200.0,  800.0, "Environmental surface pressure"),
    "VMAX": ("VMAX", "kts",     None,  None,  "Maximum 10-m wind speed"),
    "IKE34":("IKE34","TJ",      None,  None,  "Integrated Kinetic Energy >= 34 kt"),
    "IKE50":("IKE50","TJ",      None,  None,  "Integrated Kinetic Energy >= 50 kt"),
    "IKE64":("IKE64","TJ",      None,  None,  "Integrated Kinetic Energy >= 64 kt"),
    "U200": ("U200", "m/s",    200.0,  800.0, "200 hPa zonal wind (200-800 km)"),
    "U20C": ("U20C", "m/s",      0.0,  500.0, "200 hPa zonal wind (0-500 km)"),
    "V20C": ("V20C", "m/s",      0.0,  500.0, "200 hPa meridional wind (0-500 km)"),
    "RHLO": ("RHLO", "%",      200.0,  800.0, "850-700 hPa layer-mean RH"),
    "RHMD": ("RHMD", "%",      200.0,  800.0, "700-500 hPa layer-mean RH"),
    "RHHI": ("RHHI", "%",      200.0,  800.0, "500-300 hPa layer-mean RH"),
    "R000": ("R000", "%",      200.0,  800.0, "1000 hPa RH"),
    "Z850": ("Z850", "1e-7/s", 0.0,  1000.0,  "850 hPa relative vorticity"),
    "D200": ("D200", "1e-7/s", 0.0,  1000.0,  "200 hPa divergence"),
    "DIVC": ("DIVC", "1e-7/s", 0.0,  1000.0,  "200 hPa divergence (× kts conversion)"),
    "T000": ("T000", "°C",     200.0,  800.0,  "1000 hPa temperature"),
    "CAPE": ("CAPE", "J/kg",     0.0,  200.0,  "CAPE"),
    "HLCY": ("HLCY", "m2/s2",   0.0,  200.0,  "Storm-relative helicity"),
    "TCCEN":("TCCEN","",        None,  None,   "TC centre fixes by level"),
    "TCHODO":("TCHODO","",      200.0, 800.0,  "Near-storm hodograph"),
}


# =======================================================================
# PART XII – MAIN DRIVER
# =======================================================================

def main():
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s  %(levelname)s  %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
    )

    print("MSG: GPLOT_ships.py started at", datetime.datetime.now())
    print("")
    print("MSG: Welcome to GPLOT, Ships Module.")
    print("MSG: GPLOT is the Graphical Post-processed Locus for Output for Tropical cyclones.")
    print("MSG: The Ships Module produces SHIPS diagnostic .DAT files and graphical products.")
    print("")

    # -------------------------------------------------------------------
    # PART I – CONSTANTS / SETTINGS
    # -------------------------------------------------------------------
    nTrend    = 6           # number of previous forecast cycles to overlay on trend plots
    DOMAIN    = "ships"     # domain is always "ships" for this script
    MSEC_SLEEP = 0          # sleep between plots (ms) – not used in Python

    # -------------------------------------------------------------------
    # PART II – ENVIRONMENT VARIABLES
    # -------------------------------------------------------------------
    GPLOT_DIR   = os.environ.get("GPLOT_DIR", "")
    IDATE       = os.environ.get("IDATE", "")
    SID         = os.environ.get("SID", "")
    DSOURCE     = os.environ.get("DSOURCE", "")
    EXPT        = os.environ.get("EXPT", "")
    TIER        = os.environ.get("TIER", "1")
    ODIR        = os.environ.get("ODIR", "")
    IDIR        = os.environ.get("IDIR", "")
    DO_CONVERTGIF = os.environ.get("DO_CONVERTGIF", "NO").upper() == "YES"
    DO_RMWHITE    = os.environ.get("DO_RMWHITE",    "YES").upper() == "YES"
    MASTER_NML_IN = os.environ.get("MASTER_NML_IN", "")

    for required, name in [(GPLOT_DIR, "GPLOT_DIR"), (IDATE, "IDATE"),
                           (SID, "SID"), (DSOURCE, "DSOURCE")]:
        if not required:
            sys.exit(f"ERROR: Environment variable {name} is not set.")

    LOG.info("GPLOT_DIR  = %s", GPLOT_DIR)
    LOG.info("IDATE      = %s", IDATE)
    LOG.info("SID        = %s", SID)
    LOG.info("DSOURCE    = %s", DSOURCE)
    LOG.info("EXPT       = %s", EXPT)
    LOG.info("TIER       = %s", TIER)
    LOG.info("ODIR       = %s", ODIR)
    LOG.info("IDIR       = %s", IDIR)

    # -------------------------------------------------------------------
    # PART III – MASTER NAMELIST
    # -------------------------------------------------------------------
    if MASTER_NML_IN and os.path.isfile(MASTER_NML_IN):
        nml_path = MASTER_NML_IN
    else:
        nml_path = os.path.join(GPLOT_DIR, "parm", "namelist.input.default")

    try:
        NML = gm.read_master_namelist(nml_path)
    except Exception as ex:
        LOG.warning("Could not read master namelist (%s): %s", nml_path, ex)
        NML = {}

    MODEL      = NML.get("MODEL",      DSOURCE)
    LONGSID    = NML.get("LONGSID",    SID.lower())
    SID_TAG    = NML.get("SID_TAG",    SID)
    BOCO_STR   = NML.get("BOCO",       "")
    SC_DOMAIN  = NML.get("SC_DOMAIN",  "YES").upper() == "YES"
    ADECK_DIR  = NML.get("ADECK_DIR",  "")
    ADECK_FILE = NML.get("ADECK_FILE", "")

    # Parse domain bounds
    boco = [0.0, 50.0, -100.0, -20.0]
    if BOCO_STR:
        try:
            parts = BOCO_STR.split(",")
            boco = [float(p) for p in parts[:4]]
        except Exception:
            pass

    # -------------------------------------------------------------------
    # PART IV – OUTPUT DIRECTORY
    # -------------------------------------------------------------------
    if not ODIR:
        ODIR = os.path.join(GPLOT_DIR, "output", EXPT, IDATE, DOMAIN)
    os.makedirs(ODIR, exist_ok=True)
    LOG.info("ODIR = %s", ODIR)

    # -------------------------------------------------------------------
    # PART V – GRAPHICS NAMELIST (DATA_ON / PLOT_ON per variable)
    # -------------------------------------------------------------------
    var_nml_path = os.path.join(GPLOT_DIR, "parm",
                                f"namelist.{DOMAIN}.{DSOURCE}")
    if not os.path.isfile(var_nml_path):
        var_nml_path = os.path.join(GPLOT_DIR, "parm",
                                    f"namelist.{DOMAIN}")
    if not os.path.isfile(var_nml_path):
        var_nml_path = os.path.join(GPLOT_DIR, "parm",
                                    f"namelist.{DOMAIN}.default")

    DATA_ON_vars = list(_VAR_META.keys())   # default: all vars on
    PLOT_ON_vars = list(_VAR_META.keys())

    if os.path.isfile(var_nml_path):
        DATA_ON_vars.clear()
        PLOT_ON_vars.clear()
        with open(var_nml_path) as fh:
            for line in fh:
                line = line.strip()
                if not line or line.startswith("#") or line.startswith("!"):
                    continue
                parts = line.split()
                if len(parts) >= 1:
                    vname = parts[0]
                    if vname not in _VAR_META:
                        continue
                    data_on = True
                    plot_on = True
                    if len(parts) >= 2:
                        data_on = parts[1].upper() in ("YES", "1", "TRUE")
                    if len(parts) >= 3:
                        plot_on = parts[2].upper() in ("YES", "1", "TRUE")
                    if data_on:
                        DATA_ON_vars.append(vname)
                    if plot_on:
                        PLOT_ON_vars.append(vname)
        LOG.info("Loaded graphics namelist: %s", var_nml_path)
    else:
        LOG.info("No graphics namelist found; defaulting all vars ON")

    # -------------------------------------------------------------------
    # PART VI – INPUT FILE LIST
    # -------------------------------------------------------------------
    search_pattern = os.path.join(IDIR, f"*.{IDATE}.*{SID}*")
    input_files = sorted(glob.glob(search_pattern))
    if not input_files:
        # Fallback: flat IDIR
        search_pattern = os.path.join(IDIR, f"*{IDATE}*")
        input_files = sorted(glob.glob(search_pattern))

    if not input_files:
        LOG.error("No input files found matching pattern: %s", search_pattern)
        sys.exit(1)

    LOG.info("Found %d input file(s)", len(input_files))

    # -------------------------------------------------------------------
    # PART VII – ATCF READING
    # -------------------------------------------------------------------
    atcf_path = ""
    if ADECK_FILE and os.path.isfile(ADECK_FILE):
        atcf_path = ADECK_FILE
    elif ADECK_DIR:
        basin = SID[:2].lower() if len(SID) >= 2 else "al"
        atcf_path = os.path.join(ADECK_DIR, f"a{basin}{SID[2:]}.dat")

    tc_fhrs   = []
    tc_lats   = []
    tc_lons   = []
    tc_vmax   = []
    tc_motion_spd = []
    tc_motion_dir = []

    if atcf_path and os.path.isfile(atcf_path):
        try:
            atcf_data = gf.adeck_read(atcf_path, SID, IDATE, DSOURCE)
            if atcf_data is not None:
                for row in atcf_data:
                    try:
                        tc_fhrs.append(float(row.get("fhr", 0)))
                        tc_lats.append(float(row.get("lat", 0)))
                        tc_lons.append(float(row.get("lon", 0)))
                        tc_vmax.append(float(row.get("vmax", 0)))
                    except (KeyError, ValueError):
                        pass
        except Exception as ex:
            LOG.warning("ATCF read failed: %s", ex)

    # Compute 6-h storm motion from ATCF positions
    tc_motion_spd = [FILL] * len(tc_fhrs)
    tc_motion_dir = [FILL] * len(tc_fhrs)
    for i in range(1, len(tc_fhrs)):
        dt_h = tc_fhrs[i] - tc_fhrs[i-1]
        if dt_h <= 0:
            continue
        dlat = tc_lats[i] - tc_lats[i-1]
        dlon = tc_lons[i] - tc_lons[i-1]
        # Approximate distance in nm
        dist_km = math.sqrt((dlat * RE_KM * DEG2RAD)**2 +
                            (dlon * RE_KM * DEG2RAD *
                             math.cos(math.radians(tc_lats[i-1])))**2)
        dist_nm = dist_km * 0.539957
        spd_kts = dist_nm / dt_h
        bearing = math.degrees(math.atan2(dlon, dlat)) % 360.0
        tc_motion_spd[i] = spd_kts
        tc_motion_dir[i] = bearing

    # -------------------------------------------------------------------
    # PART VIII – PLOTTED FILES / STATUS TRACKING
    # -------------------------------------------------------------------
    plotted_files_log = os.path.join(
        ODIR, f"PlottedFiles.ships.{TIER}.{SID_TAG}.log")
    lock_path = plotted_files_log + ".lock"
    plotted_set = set()
    if os.path.isfile(plotted_files_log):
        with open(plotted_files_log) as fh:
            for line in fh:
                plotted_set.add(line.strip())

    new_status_entries = []

    # -------------------------------------------------------------------
    # PART IX – MAIN FILE LOOP
    # -------------------------------------------------------------------
    for ifile in input_files:
        LOG.info("Processing file: %s", ifile)

        # Open dataset
        try:
            if HAS_NC4:
                ds = nc4.Dataset(ifile, "r")
            else:
                LOG.error("netCDF4 not available; cannot open %s", ifile)
                continue
        except Exception as ex:
            LOG.warning("Cannot open %s: %s", ifile, ex)
            continue

        # Read dimensions
        try:
            lat_var  = gf.get_var2d(ds, DSOURCE, "lat",  None) if hasattr(gf, "get_var2d") else None
            lon_var  = gf.get_var2d(ds, DSOURCE, "lon",  None) if hasattr(gf, "get_var2d") else None
            lev_var  = gf.get_var2d(ds, DSOURCE, "lev",  None) if hasattr(gf, "get_var2d") else None
        except Exception:
            lat_var = lon_var = lev_var = None

        # Fallback dimension reading
        lat = None
        lon = None
        lev = None
        for dname in ["latitude", "lat", "XLAT"]:
            if dname in ds.variables:
                lat = np.asarray(ds.variables[dname][:], dtype=float).squeeze()
                if lat.ndim > 1:
                    lat = lat[:, 0]
                break
        for dname in ["longitude", "lon", "XLONG"]:
            if dname in ds.variables:
                lon = np.asarray(ds.variables[dname][:], dtype=float).squeeze()
                if lon.ndim > 1:
                    lon = lon[0, :]
                break
        for dname in ["level", "lev", "pressure", "isobaric", "isobaricInhPa"]:
            if dname in ds.variables:
                lev = np.asarray(ds.variables[dname][:], dtype=float).squeeze()
                break

        if lat is None or lon is None:
            LOG.warning("Cannot find lat/lon in %s; skipping", ifile)
            ds.close()
            continue

        # Determine forecast hours from filename or time variable
        fhr_list = []
        try:
            fname_base = os.path.basename(ifile)
            parts = fname_base.split(".")
            for p in parts:
                if p.startswith("f") and p[1:].isdigit():
                    fhr_list = [float(p[1:])]
                    break
            if not fhr_list:
                # Try to get time from ATCF
                if tc_fhrs:
                    fhr_list = tc_fhrs
                else:
                    fhr_list = [0.0]
        except Exception:
            fhr_list = [0.0]

        # Determine FHR from file (single-time files)
        for FHR in fhr_list:

            # Find TC centre at this FHR
            ctr_lat = boco[0] + (boco[1] - boco[0]) / 2.0
            ctr_lon = boco[2] + (boco[3] - boco[2]) / 2.0
            motion_spd = FILL
            motion_dir = FILL

            if tc_fhrs:
                fhr_idx = None
                for ii, f in enumerate(tc_fhrs):
                    if abs(f - FHR) < 1.0:
                        fhr_idx = ii
                        break
                if fhr_idx is not None:
                    ctr_lat    = tc_lats[fhr_idx]
                    ctr_lon    = tc_lons[fhr_idx]
                    motion_spd = tc_motion_spd[fhr_idx]
                    motion_dir = tc_motion_dir[fhr_idx]

            LOG.info("FHR=%03d  TC ctr=(%.2f, %.2f)", int(FHR), ctr_lat, ctr_lon)

            # -----------------------------------------------------------
            # VARIABLE LOOP (ppp loop in NCL)
            # -----------------------------------------------------------
            computed = {}   # cache results for this FHR

            for VNAME in DATA_ON_vars:
                vmeta = _VAR_META.get(VNAME)
                if vmeta is None:
                    continue
                FNAME, unit_lbl, r_inner, r_outer, _ = vmeta

                dat_path = _dat_file_path(ODIR, LONGSID, FNAME, IDATE, DOMAIN)
                # Determine write mode
                write_mode = "W" if not os.path.isfile(dat_path) else "A"

                # ------- SHRD / SHTD --------------------------------
                if VNAME == "SHRD":
                    if lev is None:
                        continue
                    try:
                        U3d = np.asarray(
                            ds.variables[gu.find_var_name(ds, "U", DSOURCE)][:],
                            dtype=float).squeeze()
                        V3d = np.asarray(
                            ds.variables[gu.find_var_name(ds, "V", DSOURCE)][:],
                            dtype=float).squeeze()
                        shrd, shtd = compute_shear(U3d, V3d, lat, lon,
                                                   ctr_lat, ctr_lon, lev,
                                                   200.0, 850.0, 200.0, 800.0)
                        computed["SHRD"] = shrd
                        computed["SHTD"] = shtd
                        _write_dat_value(dat_path, FHR, shrd, write_mode)
                        shtd_path = _dat_file_path(ODIR, LONGSID, "SHTD", IDATE, DOMAIN)
                        _write_dat_value(shtd_path, FHR, shtd,
                                         "W" if not os.path.isfile(shtd_path) else "A")
                    except Exception as ex:
                        LOG.warning("SHRD/SHTD failed at FHR=%d: %s", int(FHR), ex)

                # ------- SHRS / SHTS --------------------------------
                elif VNAME == "SHRS":
                    if lev is None:
                        continue
                    try:
                        U3d = np.asarray(
                            ds.variables[gu.find_var_name(ds, "U", DSOURCE)][:],
                            dtype=float).squeeze()
                        V3d = np.asarray(
                            ds.variables[gu.find_var_name(ds, "V", DSOURCE)][:],
                            dtype=float).squeeze()
                        shrs, shts = compute_shear(U3d, V3d, lat, lon,
                                                   ctr_lat, ctr_lon, lev,
                                                   500.0, 850.0, 200.0, 800.0)
                        computed["SHRS"] = shrs
                        computed["SHTS"] = shts
                        _write_dat_value(dat_path, FHR, shrs, write_mode)
                        shts_path = _dat_file_path(ODIR, LONGSID, "SHTS", IDATE, DOMAIN)
                        _write_dat_value(shts_path, FHR, shts,
                                         "W" if not os.path.isfile(shts_path) else "A")
                    except Exception as ex:
                        LOG.warning("SHRS/SHTS failed at FHR=%d: %s", int(FHR), ex)

                # ------- SHDC / SDDC (vortex-removed deep shear) ----
                elif VNAME == "SHDC":
                    if lev is None:
                        continue
                    # Use 850 hPa vortex centre from TCCEN.dat if available
                    tccen_path = _dat_file_path(ODIR, LONGSID, "TCCEN", IDATE, DOMAIN)
                    ctr_lat_850 = ctr_lat
                    ctr_lon_850 = ctr_lon
                    if os.path.isfile(tccen_path):
                        rows = _read_dat(tccen_path)
                        for r in rows:
                            if int(r[0]) == int(FHR) and abs(r[1] - 850.0) < 1.0:
                                ctr_lat_850 = r[2]
                                ctr_lon_850 = r[3]
                                break
                    try:
                        U3d = np.asarray(
                            ds.variables[gu.find_var_name(ds, "U", DSOURCE)][:],
                            dtype=float).squeeze()
                        V3d = np.asarray(
                            ds.variables[gu.find_var_name(ds, "V", DSOURCE)][:],
                            dtype=float).squeeze()
                        shdc, sddc = compute_deep_shear(
                            U3d, V3d, lat, lon, ctr_lat_850, ctr_lon_850, lev)
                        computed["SHDC"] = shdc
                        computed["SDDC"] = sddc
                        _write_dat_value(dat_path, FHR, shdc, write_mode)
                        sddc_path = _dat_file_path(ODIR, LONGSID, "SDDC", IDATE, DOMAIN)
                        _write_dat_value(sddc_path, FHR, sddc,
                                         "W" if not os.path.isfile(sddc_path) else "A")
                    except Exception as ex:
                        LOG.warning("SHDC/SDDC failed at FHR=%d: %s", int(FHR), ex)

                # ------- MSLP ---------------------------------------
                elif VNAME == "MSLP":
                    try:
                        vn = gu.find_var_name(ds, "MSLP", DSOURCE)
                        mslp = np.asarray(ds.variables[vn][:], dtype=float).squeeze()
                        mslp_val = float(np.nanmin(mslp))
                        # Convert Pa → hPa if needed
                        if mslp_val > 2000:
                            mslp_val /= 100.0
                        computed["MSLP"] = mslp_val
                        _write_dat_value(dat_path, FHR, mslp_val, write_mode)
                    except Exception as ex:
                        LOG.warning("MSLP failed at FHR=%d: %s", int(FHR), ex)

                # ------- PENV (environmental MSLP) -----------------
                elif VNAME == "PENV":
                    try:
                        vn = gu.find_var_name(ds, "MSLP", DSOURCE)
                        mslp = np.asarray(ds.variables[vn][:], dtype=float).squeeze()
                        if np.nanmax(mslp) > 2000:
                            mslp = mslp / 100.0
                        cart, x_km, y_km = sph2cart(mslp, lat, lon,
                                                     ctr_lat, ctr_lon)
                        penv = annular_avg(cart, x_km, y_km, 200.0, 800.0)
                        computed["PENV"] = penv
                        _write_dat_value(dat_path, FHR, penv, write_mode)
                    except Exception as ex:
                        LOG.warning("PENV failed at FHR=%d: %s", int(FHR), ex)

                # ------- VMAX (maximum 10-m wind) -------------------
                elif VNAME == "VMAX":
                    try:
                        vn = gu.find_var_name(ds, "WSPD10", DSOURCE)
                        wspd = np.asarray(ds.variables[vn][:], dtype=float).squeeze()
                        vmax_ms  = float(np.nanmax(wspd))
                        vmax_kts = vmax_ms * MS2KTS
                        computed["VMAX"] = vmax_kts
                        _write_dat_value(dat_path, FHR, vmax_kts, write_mode)
                    except Exception as ex:
                        LOG.warning("VMAX failed at FHR=%d: %s", int(FHR), ex)

                # ------- IKE34 / IKE50 / IKE64 ----------------------
                elif VNAME in ("IKE34", "IKE50", "IKE64"):
                    thresh_map = {"IKE34": 34.0, "IKE50": 50.0, "IKE64": 64.0}
                    thresh = thresh_map[VNAME]
                    try:
                        vn = gu.find_var_name(ds, "WSPD10", DSOURCE)
                        wspd = np.asarray(ds.variables[vn][:], dtype=float).squeeze()
                        ike = compute_ike(wspd, lat, lon, thresh)
                        computed[VNAME] = ike
                        _write_dat_value(dat_path, FHR, ike, write_mode)
                    except Exception as ex:
                        LOG.warning("%s failed at FHR=%d: %s", VNAME, int(FHR), ex)

                # ------- U200 (200 hPa U, 200-800 km) ---------------
                elif VNAME == "U200":
                    if lev is None:
                        continue
                    try:
                        vn = gu.find_var_name(ds, "U", DSOURCE)
                        U3d = np.asarray(ds.variables[vn][:], dtype=float).squeeze()
                        i200 = np.argmin(np.abs(lev - 200.0))
                        cart, x_km, y_km = sph2cart(U3d[i200], lat, lon,
                                                     ctr_lat, ctr_lon)
                        u200 = annular_avg(cart, x_km, y_km, 200.0, 800.0)
                        computed["U200"] = u200
                        _write_dat_value(dat_path, FHR, u200, write_mode)
                    except Exception as ex:
                        LOG.warning("U200 failed at FHR=%d: %s", int(FHR), ex)

                # ------- U20C (200 hPa U, 0-500 km) -----------------
                elif VNAME == "U20C":
                    if lev is None:
                        continue
                    try:
                        vn = gu.find_var_name(ds, "U", DSOURCE)
                        U3d = np.asarray(ds.variables[vn][:], dtype=float).squeeze()
                        i200 = np.argmin(np.abs(lev - 200.0))
                        cart, x_km, y_km = sph2cart(U3d[i200], lat, lon,
                                                     ctr_lat, ctr_lon)
                        u20c = annular_avg(cart, x_km, y_km, 0.0, 500.0)
                        computed["U20C"] = u20c
                        _write_dat_value(dat_path, FHR, u20c, write_mode)
                    except Exception as ex:
                        LOG.warning("U20C failed at FHR=%d: %s", int(FHR), ex)

                # ------- V20C (200 hPa V, 0-500 km) -----------------
                elif VNAME == "V20C":
                    if lev is None:
                        continue
                    try:
                        vn = gu.find_var_name(ds, "V", DSOURCE)
                        V3d = np.asarray(ds.variables[vn][:], dtype=float).squeeze()
                        i200 = np.argmin(np.abs(lev - 200.0))
                        cart, x_km, y_km = sph2cart(V3d[i200], lat, lon,
                                                     ctr_lat, ctr_lon)
                        v20c = annular_avg(cart, x_km, y_km, 0.0, 500.0)
                        computed["V20C"] = v20c
                        _write_dat_value(dat_path, FHR, v20c, write_mode)
                    except Exception as ex:
                        LOG.warning("V20C failed at FHR=%d: %s", int(FHR), ex)

                # ------- RHLO / RHMD / RHHI -------------------------
                elif VNAME in ("RHLO", "RHMD", "RHHI"):
                    if lev is None:
                        continue
                    layer_map = {
                        "RHLO": (700.0, 850.0),
                        "RHMD": (500.0, 700.0),
                        "RHHI": (300.0, 500.0),
                    }
                    ptop, pbot = layer_map[VNAME]
                    try:
                        vn = gu.find_var_name(ds, "RH", DSOURCE)
                        RH3d = np.asarray(ds.variables[vn][:], dtype=float).squeeze()
                        rh_layer = layer_avg_rh(RH3d, lev, ptop, pbot)
                        cart, x_km, y_km = sph2cart(rh_layer, lat, lon,
                                                     ctr_lat, ctr_lon)
                        rh_val = annular_avg(cart, x_km, y_km, 200.0, 800.0)
                        # Convert fraction → percent if needed
                        if rh_val <= 1.5:
                            rh_val *= 100.0
                        computed[VNAME] = rh_val
                        _write_dat_value(dat_path, FHR, rh_val, write_mode)
                    except Exception as ex:
                        LOG.warning("%s failed at FHR=%d: %s", VNAME, int(FHR), ex)

                # ------- R000 (1000 hPa RH, 200-800 km) ------------
                elif VNAME == "R000":
                    if lev is None:
                        continue
                    try:
                        vn   = gu.find_var_name(ds, "RH", DSOURCE)
                        RH3d = np.asarray(ds.variables[vn][:], dtype=float).squeeze()
                        i1000 = np.argmin(np.abs(lev - 1000.0))
                        cart, x_km, y_km = sph2cart(RH3d[i1000], lat, lon,
                                                     ctr_lat, ctr_lon)
                        r000 = annular_avg(cart, x_km, y_km, 200.0, 800.0)
                        if r000 <= 1.5:
                            r000 *= 100.0
                        computed["R000"] = r000
                        _write_dat_value(dat_path, FHR, r000, write_mode)
                    except Exception as ex:
                        LOG.warning("R000 failed at FHR=%d: %s", int(FHR), ex)

                # ------- Z850 (850 hPa relative vorticity) ----------
                elif VNAME == "Z850":
                    if lev is None:
                        continue
                    try:
                        vn_u = gu.find_var_name(ds, "U", DSOURCE)
                        vn_v = gu.find_var_name(ds, "V", DSOURCE)
                        U3d  = np.asarray(ds.variables[vn_u][:], dtype=float).squeeze()
                        V3d  = np.asarray(ds.variables[vn_v][:], dtype=float).squeeze()
                        i850 = np.argmin(np.abs(lev - 850.0))
                        rvo  = compute_relative_vorticity(U3d[i850], V3d[i850], lat, lon)
                        cart, x_km, y_km = sph2cart(rvo, lat, lon,
                                                     ctr_lat, ctr_lon)
                        z850 = annular_avg(cart, x_km, y_km, 0.0, 1000.0)
                        z850 *= 1.0e7   # → × 10^-7 s^-1
                        computed["Z850"] = z850
                        _write_dat_value(dat_path, FHR, z850, write_mode)
                    except Exception as ex:
                        LOG.warning("Z850 failed at FHR=%d: %s", int(FHR), ex)

                # ------- D200 / DIVC (200 hPa divergence) -----------
                elif VNAME in ("D200", "DIVC"):
                    if lev is None:
                        continue
                    try:
                        vn_u = gu.find_var_name(ds, "U", DSOURCE)
                        vn_v = gu.find_var_name(ds, "V", DSOURCE)
                        U3d  = np.asarray(ds.variables[vn_u][:], dtype=float).squeeze()
                        V3d  = np.asarray(ds.variables[vn_v][:], dtype=float).squeeze()
                        i200 = np.argmin(np.abs(lev - 200.0))
                        div  = compute_divergence(U3d[i200], V3d[i200], lat, lon)
                        cart, x_km, y_km = sph2cart(div, lat, lon,
                                                     ctr_lat, ctr_lon)
                        d200_raw = annular_avg(cart, x_km, y_km, 0.0, 1000.0)
                        d200 = d200_raw * 1.0e7   # → × 10^-7 s^-1
                        if VNAME == "DIVC":
                            d200 /= MS2KTS           # additional unit conversion
                        computed[VNAME] = d200
                        _write_dat_value(dat_path, FHR, d200, write_mode)
                    except Exception as ex:
                        LOG.warning("%s failed at FHR=%d: %s", VNAME, int(FHR), ex)

                # ------- T000 (1000 hPa temperature, 200-800 km) ---
                elif VNAME == "T000":
                    if lev is None:
                        continue
                    try:
                        vn    = gu.find_var_name(ds, "T", DSOURCE)
                        T3d   = np.asarray(ds.variables[vn][:], dtype=float).squeeze()
                        i1000 = np.argmin(np.abs(lev - 1000.0))
                        T2d   = T3d[i1000]
                        # K → °C if needed
                        if np.nanmean(T2d) > 200:
                            T2d = T2d - 273.15
                        cart, x_km, y_km = sph2cart(T2d, lat, lon,
                                                     ctr_lat, ctr_lon)
                        t000 = annular_avg(cart, x_km, y_km, 200.0, 800.0)
                        computed["T000"] = t000
                        _write_dat_value(dat_path, FHR, t000, write_mode)
                    except Exception as ex:
                        LOG.warning("T000 failed at FHR=%d: %s", int(FHR), ex)

                # ------- CAPE (0-200 km annulus) --------------------
                elif VNAME == "CAPE":
                    try:
                        vn   = gu.find_var_name(ds, "CAPE", DSOURCE)
                        cape = np.asarray(ds.variables[vn][:], dtype=float).squeeze()
                        cart, x_km, y_km = sph2cart(cape, lat, lon,
                                                     ctr_lat, ctr_lon)
                        cape_val = annular_avg(cart, x_km, y_km, 0.0, 200.0)
                        computed["CAPE"] = cape_val
                        _write_dat_value(dat_path, FHR, cape_val, write_mode)
                    except Exception as ex:
                        LOG.warning("CAPE failed at FHR=%d: %s", int(FHR), ex)

                # ------- HLCY (storm-relative helicity, 0-200 km) --
                elif VNAME == "HLCY":
                    try:
                        vn   = gu.find_var_name(ds, "HLCY", DSOURCE)
                        hlcy = np.asarray(ds.variables[vn][:], dtype=float).squeeze()
                        cart, x_km, y_km = sph2cart(hlcy, lat, lon,
                                                     ctr_lat, ctr_lon)
                        hlcy_val = annular_avg(cart, x_km, y_km, 0.0, 200.0)
                        computed["HLCY"] = hlcy_val
                        _write_dat_value(dat_path, FHR, hlcy_val, write_mode)
                    except Exception as ex:
                        LOG.warning("HLCY failed at FHR=%d: %s", int(FHR), ex)

                # ------- TCCEN (TC centre fixes per level) ----------
                elif VNAME == "TCCEN":
                    tccen_lev_list = [1000, 925, 850, 700, 500, 400, 300, 250, 200, 150, 100]
                    if lev is not None:
                        tccen_lev_list = [l for l in tccen_lev_list
                                          if any(abs(lev - l) < 5)]
                    for ilev, plev in enumerate(tccen_lev_list):
                        try:
                            ctr_l, ctr_n = gm.find_center(
                                ds, DSOURCE, "HGT", plev, lat, lon,
                                boco, ctr_lat, ctr_lon)
                        except Exception:
                            ctr_l, ctr_n = ctr_lat, ctr_lon
                        use_flag = 1
                        wm = "W" if (not os.path.isfile(dat_path) and ilev == 0) else "A"
                        _write_tccen_row(dat_path, FHR, plev, ctr_l, ctr_n, use_flag, wm)

                    # Graphics – TCCEN map (if PLOT_ON)
                    if VNAME in PLOT_ON_vars:
                        shrd = computed.get("SHRD", FILL)
                        shtd = computed.get("SHTD", FILL)
                        shrs = computed.get("SHRS", FILL)
                        shts = computed.get("SHTS", FILL)

                        for zoom_flag in (False, True):
                            zoom_tag = "_zoom" if zoom_flag else ""
                            ofile = os.path.join(
                                ODIR,
                                f"{LONGSID.lower()}.tccen.{IDATE}.f{int(FHR):03d}"
                                f".{DSOURCE.lower()}{zoom_tag}.png")
                            if ofile not in plotted_set:
                                plot_tccen(
                                    dat_path, ofile, FHR,
                                    ctr_lat, ctr_lon,
                                    shrd, shtd, shrs, shts,
                                    motion_spd, motion_dir,
                                    boco, DOMAIN, IDATE, SID, MODEL,
                                    zoom=zoom_flag,
                                    do_rmwhite=DO_RMWHITE,
                                    do_gif=DO_CONVERTGIF)
                                if os.path.isfile(ofile):
                                    new_status_entries.append(ofile)
                                    plotted_set.add(ofile)

                # ------- TCHODO (near-storm hodograph) --------------
                elif VNAME == "TCHODO":
                    if lev is None:
                        continue
                    hodo_lev_list = [1000, 925, 850, 700, 500, 400, 300, 250, 200]
                    tccen_path = _dat_file_path(ODIR, LONGSID, "TCCEN", IDATE, DOMAIN)

                    for ilev, plev in enumerate(hodo_lev_list):
                        if not any(abs(lev - plev) < 5):
                            continue
                        # Get level-specific TC centre from TCCEN.dat
                        ctr_l, ctr_n = ctr_lat, ctr_lon
                        if os.path.isfile(tccen_path):
                            rows = _read_dat(tccen_path)
                            for r in rows:
                                if int(r[0]) == int(FHR) and abs(r[1] - plev) < 5:
                                    ctr_l, ctr_n = r[2], r[3]
                                    break
                        try:
                            vn_u = gu.find_var_name(ds, "U", DSOURCE)
                            vn_v = gu.find_var_name(ds, "V", DSOURCE)
                            U3d  = np.asarray(ds.variables[vn_u][:], dtype=float).squeeze()
                            V3d  = np.asarray(ds.variables[vn_v][:], dtype=float).squeeze()
                            ilvl = np.argmin(np.abs(lev - plev))
                            cU, x_km, y_km = sph2cart(U3d[ilvl], lat, lon,
                                                       ctr_l, ctr_n)
                            cV, _,     _   = sph2cart(V3d[ilvl], lat, lon,
                                                       ctr_l, ctr_n)
                            ubar, vbar = annular_avg_uv(cU, cV, x_km, y_km,
                                                        200.0, 800.0)
                            mag  = math.sqrt(ubar**2 + vbar**2) * MS2KTS
                            dirn = (math.degrees(math.atan2(ubar, vbar)) + 180.0) % 360.0
                            wm   = "W" if (not os.path.isfile(dat_path) and ilev == 0) else "A"
                            _write_tchodo_row(dat_path, FHR, plev, mag, dirn, wm)
                        except Exception as ex:
                            LOG.warning("TCHODO lev=%d failed at FHR=%d: %s",
                                        plev, int(FHR), ex)

                    # Graphics – TCHODO hodograph (if PLOT_ON)
                    if VNAME in PLOT_ON_vars:
                        for zoom_flag in (False, True):
                            zoom_tag = "_zoom" if zoom_flag else ""
                            ofile = os.path.join(
                                ODIR,
                                f"{LONGSID.lower()}.tchodo.{IDATE}.f{int(FHR):03d}"
                                f".{DSOURCE.lower()}{zoom_tag}.png")
                            if ofile not in plotted_set:
                                plot_tchodo(
                                    dat_path, ofile, FHR,
                                    motion_spd, motion_dir,
                                    IDATE, SID, MODEL,
                                    zoom=zoom_flag,
                                    do_rmwhite=DO_RMWHITE,
                                    do_gif=DO_CONVERTGIF)
                                if os.path.isfile(ofile):
                                    new_status_entries.append(ofile)
                                    plotted_set.add(ofile)

            # end variable loop
        # end FHR loop

        ds.close()
    # end file loop

    # -------------------------------------------------------------------
    # PART X – TREND PLOTS  (produced after all files processed)
    # -------------------------------------------------------------------
    # Only for scalar variables (not TCCEN / TCHODO)
    trend_vars = [v for v in PLOT_ON_vars
                  if v not in ("TCCEN", "TCHODO", "SHTD", "SHTS", "SDDC")]

    for VNAME in trend_vars:
        vmeta = _VAR_META.get(VNAME)
        if vmeta is None:
            continue
        FNAME, unit_lbl = vmeta[0], vmeta[1]

        # Collect last nTrend DAT files (6-hourly back)
        dat_paths_with_dates = []
        try:
            idate_dt = datetime.datetime.strptime(IDATE, "%Y%m%d%H")
        except Exception:
            idate_dt = None

        current_path = _dat_file_path(ODIR, LONGSID, FNAME, IDATE, DOMAIN)
        if os.path.isfile(current_path):
            dat_paths_with_dates.append((current_path, IDATE))

        if idate_dt is not None:
            for n in range(1, nTrend):
                prev_dt    = idate_dt - datetime.timedelta(hours=6 * n)
                prev_idate = prev_dt.strftime("%Y%m%d%H")
                prev_odir  = ODIR.replace(IDATE, prev_idate)
                prev_path  = _dat_file_path(prev_odir, LONGSID, FNAME,
                                             prev_idate, DOMAIN)
                if os.path.isfile(prev_path):
                    dat_paths_with_dates.append((prev_path, prev_idate))

        if len(dat_paths_with_dates) < 1:
            continue

        ofile = os.path.join(
            ODIR,
            f"{LONGSID.lower()}.{FNAME.lower()}.{IDATE}.trend.{DSOURCE.lower()}.png")

        if ofile not in plotted_set:
            plot_trend(VNAME, unit_lbl, dat_paths_with_dates, ofile,
                       SID, MODEL,
                       do_rmwhite=DO_RMWHITE, do_gif=DO_CONVERTGIF)
            if os.path.isfile(ofile):
                new_status_entries.append(ofile)
                plotted_set.add(ofile)

    # -------------------------------------------------------------------
    # PART XI – UPDATE STATUS FILE
    # -------------------------------------------------------------------
    if new_status_entries:
        _update_status(plotted_files_log, lock_path,
                       SID_TAG, TIER, new_status_entries)

    # -------------------------------------------------------------------
    # PART XII – FINAL SUMMARY PRINT
    # -------------------------------------------------------------------
    n_new = len(new_status_entries)
    print("")
    print(f"MSG: GPLOT_ships.py completed at {datetime.datetime.now()}")
    print(f"MSG: Produced {n_new} new output file(s) this run.")
    if n_new > 0:
        for f in new_status_entries[:10]:
            print(f"  {f}")
        if n_new > 10:
            print(f"  ... and {n_new - 10} more")
    print("")


# =======================================================================
# ENTRY POINT
# =======================================================================
if __name__ == "__main__":
    main()
