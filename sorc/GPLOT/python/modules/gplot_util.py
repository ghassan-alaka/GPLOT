#!/usr/bin/env python
"""gplot_util.py - Core utility library for GPLOT.

Lower-level functions for GPLOT with no dependencies on other GPLOT modules.
Converted from NCL to Python (original: sorc/GPLOT/ncl/GPLOT_util.ncl).

Written by:    Ghassan Alaka, Jr.
Converted:     2026-02-25
Original NCL:  sorc/GPLOT/ncl/GPLOT_util.ncl

Function list (alphabetical):
  all_masters, arrow, basin_codes, calc_theta, change_time_fmt,
  chk_cmd_inputs, circle_ll, define_cmap_fill, define_cmap_name,
  define_levels, filter121, find_var_name, gen_plot_res, get_auto_dir,
  get_dmn_bds, get_dmn_info, get_expt_info, get_file_tag, get_file_tag1,
  get_file_tag2, get_file_tag3, get_i_dir, get_invest_sid, get_lat_lon_lbl,
  get_lat_lon_lbl2, get_model_info, get_model_info2, get_plot_title,
  get_stm_thin, hbfilter, is_str_subset2, level_convert, load_constants,
  print_max_min, remove_duplicates, stat_plot_res, string_out,
  test_plot_2d_map, test_plot_map, test_plot_contour
"""

import os
import math
import datetime
from types import SimpleNamespace

import numpy as np

try:
    import matplotlib.pyplot as plt
    import matplotlib.cm as mpl_cm
    import matplotlib.colors as mcolors
    HAS_MATPLOTLIB = True
except ImportError:
    HAS_MATPLOTLIB = False

try:
    from scipy.ndimage import convolve1d
    HAS_SCIPY = True
except ImportError:
    HAS_SCIPY = False

try:
    import cartopy.crs as ccrs
    import cartopy.feature as cfeature
    HAS_CARTOPY = True
except ImportError:
    HAS_CARTOPY = False

# ============================================================
# load_constants
# ============================================================

def load_constants():
    """Return a SimpleNamespace of universal physical constants.

    Equivalent to NCL load_constants().
    """
    c = SimpleNamespace()
    c.g      = 9.80665              # Gravitational Acceleration [m s-2]
    c.G      = 6.67e-11             # Universal Gravitational Constant [N m-2 kg-2]
    c.pi     = math.acos(-1.0)      # Pi [n/a]
    c.c      = 2.998e+08            # Speed of Light [m s-1]
    c.R      = 287.058              # Specific Gas Constant for Dry Air [J K-1 kg-1]
    c.c_p    = 1004.0               # Specific Heat at Constant Pressure [J K-1 kg-1]
    c.c_v    = 717.0                # Specific Heat at Constant Volume [J K-1 kg-1]
    c.dalr   = c.g / c.c_p          # Dry Adiabatic Lapse Rate [K m-1]
    c.d2r    = c.pi / 180.0         # Degrees-to-Radians [rad deg-1]
    c.r2d    = 180.0 / c.pi         # Radians-to-Degrees [deg rad-1]
    c.r      = 6.371e+06            # Earth Radius [m]
    c.omega  = 7.27e-05             # Earth Angular Rotation Rate [rad s-1]
    c.kappa  = c.R / c.c_p          # Poisson Constant [n/a]
    c.sigma  = 5.67e-08             # Stefan-Boltzmann Constant [J s-1 m-2 K-4]
    c.rho_0  = 1.25                 # Air Density at Sea Level [kg m-3]
    c.ms2kts = 1.94384449           # m/s to knots [m s-1 kt-1]
    c.fval   = 9.96921e+36          # Missing Value Float #1
    c.fval2  = -2147483647          # Missing Value Integer
    c.fval3  = 1.0e+20              # Missing Value Float #2
    c.fval4  = "missing"            # Missing Value String
    c.tunits = "hours since 1970-01-01 00:00:00"
    return c


# ============================================================
# all_masters
# ============================================================

def all_masters(TYPE):
    """Return master lists. Currently only TYPE='basin' is supported.

    Returns a list of (long_name, XX_code, X_code) tuples.
    """
    if TYPE != "basin":
        raise ValueError(f"all_masters: Master list type '{TYPE}' not recognized.")
    return [
        ("Atlantic",    "AL", "L"),
        ("East Pacific", "EP", "E"),
        ("West Pacific", "WP", "W"),
    ]


# ============================================================
# arrow
# ============================================================

def arrow(ax, xpts, ypts, xlim=None, ylim=None, **kwargs):
    """Draw a line with an arrowhead on a matplotlib Axes.

    Equivalent to NCL arrow(wks, plt, xpts, ypts, res).

    Parameters
    ----------
    ax     : matplotlib Axes
    xpts   : sequence of 2 x-coordinates [start, end]
    ypts   : sequence of 2 y-coordinates [start, end]
    xlim   : (xmin, xmax) or None to use ax limits
    ylim   : (ymin, ymax) or None to use ax limits
    **kwargs : passed to ax.plot (e.g. color, linewidth)
    """
    ax.plot(xpts, ypts, **kwargs)
    if xlim is None:
        xlim = ax.get_xlim()
    if ylim is None:
        ylim = ax.get_ylim()
    x0, x1 = xlim
    y0, y1 = ylim

    asp   = 0.4
    fhead = 0.3
    xp1n  = (xpts[0] - x0) / (x1 - x0)
    xp2n  = (xpts[1] - x0) / (x1 - x0)
    yp1n  = (ypts[0] - y0) / (y1 - y0)
    yp2n  = (ypts[1] - y0) / (y1 - y0)

    theta = math.atan2(yp2n - yp1n, xp2n - xp1n)
    if theta < 0 and ypts[1] > ypts[0]:
        theta = math.pi + theta
    elif theta > 0 and ypts[1] < ypts[0]:
        theta = math.pi + theta

    ahead = math.sqrt((xp2n - xp1n)**2 + (yp2n - yp1n)**2) * fhead
    phi   = math.atan(asp)

    for sign in (+1, -1):
        lam = theta + sign * phi
        xa  = x0 + (xp2n - ahead * math.cos(lam)) * (x1 - x0)
        ya  = y0 + (yp2n - ahead * math.sin(lam)) * (y1 - y0)
        ax.plot([xa, xpts[1]], [ya, ypts[1]], **kwargs)

    return ax


# ============================================================
# basin_codes
# ============================================================

def basin_codes(IN, TYPE):
    """Map a basin identifier between long_name, XX (2-letter), X (1-letter) forms.

    Parameters
    ----------
    IN   : str, input basin identifier
    TYPE : str, one of 'long_name', 'XX', 'X'

    Returns
    -------
    str
    """
    valid_types = ("long_name", "XX", "X")
    if TYPE not in valid_types:
        raise ValueError(
            f"basin_codes: Type '{TYPE}' not recognized. "
            f"Recognized: {valid_types}"
        )
    type_idx = valid_types.index(TYPE)
    in_idx   = 2 if len(IN) == 1 else (1 if len(IN) == 2 else 0)

    for row in all_masters("basin"):
        if row[in_idx] == IN:
            return row[type_idx]
    raise ValueError(
        f"basin_codes: Input '{IN}' not recognized. "
        "Recognized basins: Atlantic, East Pacific, West Pacific"
    )


# ============================================================
# calc_theta
# ============================================================

def calc_theta(T, P, Ps):
    """Calculate potential temperature.

    Parameters
    ----------
    T  : array-like, temperature (any shape; first dim must match P)
    P  : 1-D array-like, pressure levels (same units as Ps)
    Ps : float, reference (surface) pressure
    """
    C  = load_constants()
    T  = np.asarray(T, dtype=float)
    P  = np.asarray(P, dtype=float)
    if T.shape[0] != P.shape[0]:
        raise ValueError("calc_theta: Vertical dimension does not match.")
    P2 = P.reshape((P.shape[0],) + (1,) * (T.ndim - 1))
    return T * (float(Ps) / P2) ** C.kappa


# ============================================================
# change_time_fmt  (+ private helpers)
# ============================================================

_SUPPORTED_FMTS = (
    "MM/DD/YYYY (HH:mm)",
    "YYYYMMDDHH",
    "YYYYMMDD_HH",
    "YYYYMMDD",
    "YYYYMMDD_HHmmss",
    "YYYY-MM-DD_HH",
    "HHz Month DD YYYY",
    "HHz DOW, Month DD YYYY",
    "DD/HH",
)
_MONTH_ABBR = ("Jan","Feb","Mar","Apr","May","Jun",
               "Jul","Aug","Sep","Oct","Nov","Dec")
_DOW_ABBR   = ("Sun","Mon","Tue","Wed","Thu","Fri","Sat")


def _parse_dt(s, fmt):
    """Parse a time string into (YYYY, MM, DD, HH, mm, ss) ints."""
    if fmt == "MM/DD/YYYY (HH:mm)":
        return int(s[6:10]), int(s[0:2]), int(s[3:5]), int(s[12:14]), int(s[15:17]), 0
    elif fmt == "YYYYMMDDHH":
        return int(s[0:4]), int(s[4:6]), int(s[6:8]), int(s[8:10]), 0, 0
    elif fmt == "YYYYMMDD_HH":
        return int(s[0:4]), int(s[4:6]), int(s[6:8]), int(s[9:11]), 0, 0
    elif fmt == "YYYYMMDD":
        return int(s[0:4]), int(s[4:6]), int(s[6:8]), 0, 0, 0
    elif fmt == "YYYYMMDD_HHmmss":
        return int(s[0:4]), int(s[4:6]), int(s[6:8]), int(s[9:11]), int(s[11:13]), int(s[13:15])
    elif fmt == "YYYY-MM-DD_HH":
        return int(s[0:4]), int(s[5:7]), int(s[8:10]), int(s[11:13]), 0, 0
    else:
        raise ValueError(f"_parse_dt: format '{fmt}' not parseable.")


def _fmt_dt(YYYY, MM, DD, HH, mm, ss, fmt):
    """Format (YYYY, MM, DD, HH, mm, ss) → string using fmt."""
    Y = f"{YYYY:04d}"; Mo = f"{MM:02d}"; D = f"{DD:02d}"
    H = f"{HH:02d}";  Mi = f"{mm:02d}"; S = f"{ss:02d}"
    if fmt == "MM/DD/YYYY (HH:mm)":        return f"{Mo}/{D}/{Y} ({H}:{Mi})"
    elif fmt == "YYYYMMDDHH":              return f"{Y}{Mo}{D}{H}"
    elif fmt == "YYYYMMDD_HH":            return f"{Y}{Mo}{D}_{H}"
    elif fmt == "YYYYMMDD":               return f"{Y}{Mo}{D}"
    elif fmt == "YYYYMMDD_HHmmss":        return f"{Y}{Mo}{D}_{H}{Mi}{S}"
    elif fmt == "YYYY-MM-DD_HH":          return f"{Y}-{Mo}-{D}_{H}"
    elif fmt == "HHz Month DD YYYY":      return f"{H}z {_MONTH_ABBR[MM-1]} {D} {Y}"
    elif fmt == "HHz DOW, Month DD YYYY":
        # NCL day_of_week: Sun=0; Python weekday: Mon=0 → Sun=6
        ncl_dow = (datetime.date(YYYY, MM, DD).weekday() + 1) % 7
        return f"{H}z {_DOW_ABBR[ncl_dow]}, {_MONTH_ABBR[MM-1]} {D} {Y}"
    elif fmt == "DD/HH":                  return f"{D}/{H}"
    else:
        raise ValueError(f"_fmt_dt: format '{fmt}' not supported.")


def change_time_fmt(old_strs, old_fmt, new_fmt, f_hr):
    """Convert time strings between supported formats, optionally adding forecast hours.

    Parameters
    ----------
    old_strs : str or list of str
    old_fmt  : one of _SUPPORTED_FMTS
    new_fmt  : one of _SUPPORTED_FMTS
    f_hr     : float or list of floats (hours to add; 0 = no change)

    Returns
    -------
    str or list of str
    """
    scalar = isinstance(old_strs, str)
    if scalar:
        old_strs = [old_strs]
    if isinstance(f_hr, (int, float)):
        f_hr = [float(f_hr)] * len(old_strs)
    if old_fmt not in _SUPPORTED_FMTS:
        raise ValueError(f"change_time_fmt: oldFormat '{old_fmt}' not supported.")
    if new_fmt not in _SUPPORTED_FMTS:
        raise ValueError(f"change_time_fmt: newFormat '{new_fmt}' not supported.")
    if len(f_hr) not in (1, len(old_strs)):
        raise ValueError("change_time_fmt: f_hr length must match old_strs or be 1.")

    if old_fmt == new_fmt and all(h == 0 for h in f_hr):
        return old_strs[0] if scalar else list(old_strs)

    result = []
    for i, s in enumerate(old_strs):
        YYYY, MM, DD, HH, mm, ss = _parse_dt(s, old_fmt)
        fh = f_hr[i] if len(f_hr) > 1 else f_hr[0]
        if fh != 0:
            dt = datetime.datetime(YYYY, MM, DD, HH, mm, ss) + datetime.timedelta(hours=fh)
            YYYY, MM, DD, HH, mm, ss = (dt.year, dt.month, dt.day,
                                         dt.hour, dt.minute, dt.second)
        result.append(_fmt_dt(YYYY, MM, DD, HH, mm, ss, new_fmt))
    return result[0] if scalar else result


# ============================================================
# chk_cmd_inputs
# ============================================================

def chk_cmd_inputs(vars2chk, ns):
    """Validate command-line inputs in a SimpleNamespace, setting defaults.

    Parameters
    ----------
    vars2chk : list of str, variable names to check
    ns       : SimpleNamespace, mutated in-place with defaults
    """
    for v in vars2chk:
        if v == "nameBds":
            if not getattr(ns, "nameBds", None):
                print("MSG:  'nameBds' is not defined.")
                print("MSG:  Assuming nameBds='basin'.")
                ns.nameBds = "basin"
        elif v == "tier":
            if not getattr(ns, "tier", None):
                print("MSG:  'tier' is not defined.")
                print("MSG:  Assuming tier='Tier1'.")
                ns.tier = "Tier1"
        elif v == "dSource":
            if not getattr(ns, "dSource", None):
                print("MSG:  'dSource' is not defined.")
                print("MSG:  Assuming dSource='HWRF'.")
                ns.dSource = "HWRF"
        elif v == "iDate":
            if not getattr(ns, "iDate", None):
                ns.iDate = ""
        elif v == "IDtag":
            if not getattr(ns, "IDtag", None):
                ns.IDtag = ""


# ============================================================
# circle_ll
# ============================================================

def circle_ll(ax, x0, y0, r, scale=1.0, rotation=0.0, **kwargs):
    """Draw a circle (or rotated ellipse) centered at (x0, y0) on ax.

    Parameters
    ----------
    ax       : matplotlib Axes
    x0, y0   : center in data coordinates
    r        : radius in x-direction (data units)
    scale    : y/x ratio for ellipse (default 1 = circle)
    rotation : rotation angle in degrees, CCW positive
    **kwargs : passed to ax.plot
    """
    xlim = ax.get_xlim(); ylim = ax.get_ylim()
    xmin, xmax = xlim;    ymin, ymax = ylim
    fig  = ax.get_figure()
    bbox = ax.get_position()
    vpw  = bbox.width  * fig.get_figwidth()
    vph  = bbox.height * fig.get_figheight()

    rx   = r / (xmax - xmin)
    ry   = rx * vpw / vph

    th   = np.linspace(0, 2 * math.pi, 361)
    rr   = math.radians(rotation)
    xp1  = np.cos(th);           yp1 = np.sin(th) * scale
    xp2  = xp1 * math.cos(rr) - yp1 * math.sin(rr)
    yp2  = xp1 * math.sin(rr) + yp1 * math.cos(rr)
    xpts = rx * xp2 * (xmax - xmin) + x0
    ypts = ry * yp2 * (ymax - ymin) + y0
    ax.plot(xpts, ypts, **kwargs)


# ============================================================
# define_cmap_fill
# ============================================================

def define_cmap_fill(var, p_lev, colormap_name):
    """Load a matplotlib colormap, optionally reversing/slicing for specific variables.

    Note: Custom NCL colormaps (e.g. 'tcwinds1', 'tprcp2', 'mslp2') must be
    registered in matplotlib separately before use.

    Parameters
    ----------
    var           : str, variable name (e.g. 'RVO', 'TPRCP')
    p_lev         : str, pressure level string
    colormap_name : str, colormap name

    Returns
    -------
    matplotlib colormap object
    """
    if not HAS_MATPLOTLIB:
        raise ImportError("matplotlib is required for define_cmap_fill.")
    cmap = mpl_cm.get_cmap(colormap_name)
    if var == "RVO" and p_lev in ("850", "700", "500", "200"):
        n = cmap.N
        colors = cmap(np.linspace(0, 1, n))
        colors = colors[8: n - 9][::-1]
        colors[0] = [0., 0., 0., 0.]
        cmap = mcolors.LinearSegmentedColormap.from_list("custom_rvo", colors)
    return cmap


# ============================================================
# define_cmap_name
# ============================================================

def define_cmap_name(var, lev):
    """Return the colormap name string for a given variable and level.

    Parameters
    ----------
    var : str, variable name
    lev : str, level string (may contain 'diff' for difference plots)

    Returns
    -------
    str, colormap name
    """
    cmap = "BlAqGrYeOrReVi200"  # default

    if var == "RVO":
        if lev in ("850", "700", "500", "200", "02001000"):
            cmap = "matlab_hot"
    elif var == "UV":
        if lev in ("02000850d", "05000850d", "07000850d"):
            cmap = "shear"
        elif lev in ("02500850m", "05000850m", "02500500m", "07000850m"):
            cmap = "WhiteBlueGreenYellowRed"
        elif lev in ("850", "10", "02001000", "02001000y", "02001000x"):
            cmap = "tcwinds1"
        elif lev == "750":
            cmap = "TDRwind"
    elif var == "RH":
        cmap = "MPL_BrBG"
    elif var in ("PRCP", "PRATE"):
        cmap = "precip3_16lev"
    elif var == "TPRCP":
        cmap = "tprcp2"
    elif var == "TPW":
        cmap = "MPL_BrBG"
    elif var == "HGT":
        cmap = "BlueWhiteOrangeRed" if "diff" in lev else "BlAqGrYeOrRe"
    elif var == "CAPE":
        cmap = "WhiteBlueGreenYellowRed"
    elif var == "MSLP":
        cmap = "mslp2"
    elif var == "T":
        cmap = "ncl_default"
    elif var == "SFLO":
        cmap = "WhViBlGrYeOrRe"
    elif var == "VWS":
        cmap = "WhViBlGrYeOrRe"
    elif var == "REFL":
        cmap = "dbz"
    elif var == "REFD":
        cmap = "REFD"
    elif var == "PV":
        cmap = "BlueWhiteOrangeRed"

    # Override for diff plots
    if "diff" in lev:
        cmap = "BlueWhiteOrangeRed"

    return cmap


# ============================================================
# define_levels
# ============================================================

def define_levels(var, lev):
    """Return contour level array for a given variable and level string.

    Uses numpy.linspace in place of NCL fspan.

    Parameters
    ----------
    var : str, variable name
    lev : str, level string

    Returns
    -------
    numpy.ndarray of contour levels, or None if var/lev not recognized
    """
    levels = None

    if var == "RVO" and lev in ("850", "700", "500", "200"):
        levels = np.linspace(2., 70., 35)

    elif var == "U":
        levels = np.linspace(-10, 10, 21) if "diff" in lev else np.linspace(-40., 40., 21)

    elif var == "UV":
        if lev in ("02000850d", "05000850d", "07000850d"):
            levels = np.linspace(5.0, 50.0, 19)
        elif lev == "02500850m":
            levels = np.array([5.,7.5,10.,12.5,15.,17.5,20.,22.5,25.,27.5,
                               30.,35.,40.,45.,50.,55.,60.,70.,80.,90.])
        elif lev == "05000850m":
            levels = np.array([5.,7.5,10.,12.5,15.,17.5,20.,22.5,25.,27.5,
                               30.,35.,40.,45.,50.,55.,60.])
        elif lev == "07000850m":
            levels = np.array([5.,7.5,10.,12.5,15.,17.5,20.,22.5,25.,27.5,30.,35.,40.])
        elif lev == "02500500m":
            levels = np.array([5.,7.5,10.,12.5,15.,17.5,20.,22.5,25.,27.5,
                               30.,35.,40.,45.,50.,55.,60.,70.,80.,90.])
        elif lev == "750":
            levels = np.array([7.,10.,13.,16.,19.,22.,25.,28.,31.,34.,36.,38.,40.,42.,
                               44.,46.,48.,50.,52.,54.,56.,58.,60.,62.,64.,
                               69.333,74.666,80.,85.333,90.666,96.,100.666,105.333,110.,
                               115.,120.,125.,130.,135.,140.,145.,150.,155.])
        elif lev in ("10","850","0850","02001000","02001000y","02001000x"):
            levels = np.array([10.,20.,34.,50.,64.,83.,96.,114.,137.])

    elif var == "RH":
        levels = np.linspace(5., 95., 19)

    elif var in ("PRCP", "PRATE"):
        levels = np.array([0.1,0.25,0.5,1.,1.5,2.,3.,4.,5.,7.5,10.,15.,20.,25.,30.,40.])

    elif var == "TPRCP":
        levels = np.array([0.01,0.10,0.25,0.50,0.75,1.00,1.25,1.50,1.75,2.00,2.50,3.00,
                           4.00,5.00,7.00,10.00,15.00,20.00,25.00,30.00,35.00,40.00,50.00,60.00])

    elif var == "TPW":
        levels = np.linspace(5., 65., 31)

    elif var == "HGT":
        if lev == "02500500d":
            levels = np.linspace(490., 510., 21)
        elif lev in ("500", "0500"):
            levels = np.concatenate([np.linspace(468., 540., 13), np.linspace(542., 600., 30)])
        elif lev == "200":
            levels = np.linspace(1170., 1260., 31)
        elif "diff" in lev:
            levels = np.linspace(-0.1, 0.1, 21)

    elif var == "REFL":
        levels = np.linspace(5., 75., 15)

    elif var == "REFD":
        levels = np.linspace(2., 78., 39)

    elif var == "HLCY":
        levels = np.linspace(100., 600., 11)

    elif var == "CAPE":
        levels = np.linspace(500., 4500., 17)

    elif var == "MSLP":
        levels = np.linspace(940., 1040., 26)

    elif var == "T":
        if lev == "2":
            levels = np.linspace(280., 320., 21)
        elif lev == "850":
            levels = np.linspace(270., 310., 21)

    elif var == "SFLO":
        levels = np.linspace(10., 60., 11)

    elif var == "DPT":
        if lev in ("2", "850"):
            levels = np.linspace(270., 310., 21)

    elif var == "SST":
        levels = np.linspace(18., 32., 15)

    elif var == "LHFLX":
        levels = np.linspace(-50., 800., 35)

    elif var == "SHFLX":
        levels = np.linspace(-50., 300., 11)

    elif var == "PV":
        if lev == "200":
            levels = np.linspace(-5., 5., 41)
        elif lev == "200diff":
            levels = np.linspace(-1., 1., 21)

    return levels


# ============================================================
# filter121
# ============================================================

def _wgt_runave(V, wgts, axis, cyclic):
    """Apply weighted running average [0.25, 0.5, 0.25] along axis."""
    if HAS_SCIPY:
        mode = "wrap" if cyclic else "nearest"
        return convolve1d(V, wgts, axis=axis, mode=mode)
    # Manual fallback
    V = np.asarray(V, dtype=float).copy()
    slc_m = [slice(None)] * V.ndim
    slc_l = [slice(None)] * V.ndim
    slc_r = [slice(None)] * V.ndim
    slc_m[axis] = slice(1, -1)
    slc_l[axis] = slice(None, -2)
    slc_r[axis] = slice(2, None)
    V[tuple(slc_m)] = (wgts[0] * V[tuple(slc_l)] +
                       wgts[1] * V[tuple(slc_m)] +
                       wgts[2] * V[tuple(slc_r)])
    return V


def filter121(V, N, cyclic):
    """Apply a 1-2-1 smoother to array V.

    Filters in the x-direction (last axis), then y-direction (second-to-last).
    N is the number of passes (currently one pass per call, matching NCL do lll=1,1).

    Parameters
    ----------
    V      : numpy array, 2-D to 4-D
    N      : int, number of passes (kept for interface compatibility)
    cyclic : bool, if True the x-direction is treated as periodic

    Returns
    -------
    numpy.ndarray
    """
    V = np.asarray(V, dtype=float).copy()
    if V.ndim < 2 or V.ndim > 4:
        raise ValueError(f"filter121: Unsupported number of dimensions ({V.ndim}). Must be 2-4.")
    wgts   = np.array([0.25, 0.5, 0.25])
    x_axis = V.ndim - 1
    y_axis = V.ndim - 2
    V = _wgt_runave(V, wgts, x_axis, cyclic)
    V = _wgt_runave(V, wgts, y_axis, False)
    return V


# ============================================================
# find_var_name
# ============================================================

def find_var_name(dsource, var, lev):
    """Resolve a model-specific NetCDF variable name from Vtable files.

    Reads $GPLOT_DIR/tbl/Vtable.master and $GPLOT_DIR/tbl/Vtable.<MODEL>
    to map (dsource, var, lev) → model variable name string.

    Parameters
    ----------
    dsource : str, data source (e.g. 'GFS', 'HWRF', 'HAFS', 'GEFS', 'ECMWF')
    var     : str, simplified variable name (e.g. 'U', 'T', 'PRCP')
    lev     : str, vertical level string (e.g. '850', '10', '2', '6h')

    Returns
    -------
    str, model-specific variable name, or 'missing' if not found
    """
    gplot_dir = os.environ.get("GPLOT_DIR", "")

    # Determine old_var (append level for surface vars 10m, 2m)
    old_var = (var + lev) if lev in ("10", "2") else var

    # Read master Vtable
    master_file = os.path.join(gplot_dir, "tbl", "Vtable.master")
    master_vars, master_cats = [], []
    with open(master_file) as fh:
        for line in fh:
            parts = line.split()
            if len(parts) >= 2:
                master_vars.append(parts[0])
                master_cats.append(parts[1])

    if old_var not in master_vars:
        raise ValueError(f"find_var_name: Variable '{old_var}' not recognized.")
    n_cat = master_cats[master_vars.index(old_var)]

    # Select model Vtable
    if dsource in ("GFS", "GFS_an", "GFS_fcst", "GFS_d03"):
        vtable = "Vtable.GFS"
    elif "HWRF" in dsource:
        vtable = "Vtable.HWRF"
    elif "HAFS" in dsource:
        vtable = "Vtable.HAFS"
    elif dsource == "ERAi":
        vtable = "Vtable.ERAInterim"
    elif dsource == "GEFS":
        vtable = "Vtable.GEFS"
    elif dsource == "ECMWF":
        vtable = "Vtable.ECMWF"
    elif dsource == "HMON":
        vtable = "Vtable.HMON"
    else:
        vtable = f"Vtable.{dsource}"

    model_file = os.path.join(gplot_dir, "tbl", vtable)
    model_vars, model_cats = [], []
    with open(model_file) as fh:
        for line in fh:
            parts = line.split()
            if len(parts) >= 2:
                model_vars.append(parts[0])
                model_cats.append(parts[1])

    matches = [v.strip() for v, c in zip(model_vars, model_cats) if c == n_cat]
    if not matches:
        print(f"WARNING: find_var_name: Variable '{old_var}' not found in table for {dsource}.")
        return "missing"
    new_var = matches[0]

    # Temp fix: append time interval to precipitation variable names
    if dsource in ("GFS", "HAFS", "HMON"):
        if "PRCP" in var and "h" in lev:
            new_var = new_var + lev
        if "PRATE" in var and "h" in lev:
            new_var = new_var + lev

    return new_var


# ============================================================
# gen_plot_res
# ============================================================

def gen_plot_res(type_, ovlay=False):
    """Generate plot resource settings as a SimpleNamespace.

    Translates NCL GenPlotRes(type, OVLAY) to Python matplotlib/Cartopy settings.

    Plot type codes
    ---------------
    0-9   : cylindrical equidistant map
    10-19 : cross-section (pressure vs. distance)
    20-29 : polar cylindrical (Southern Hemisphere)
    30-39 : radius–pressure (R-Z)
    40-49 : Cartesian (x, y)
    50-59 : hodograph

    Data rendering (last digit)
    ---------------------------
    0 : contour fill only
    1 : contour lines only
    2 : contour fill + lines
    3 : wind vectors (line arrow)
    4 : curly vectors
    5 : wind barbs
    6 : streamlines
    7 : blank contour (no map)
    9 : map only

    Parameters
    ----------
    type_ : int
    ovlay : bool, if True omit map/background resources

    Returns
    -------
    SimpleNamespace
    """
    res   = SimpleNamespace()
    type2 = type_ % 10   # last digit → data rendering type

    res.draw     = False
    res.maximize = False

    if not ovlay:
        if type_ < 10:
            # Cylindrical equidistant
            res.map_fill                 = False
            res.geophys_line_color       = "gray10"
            res.geophys_line_width       = 2.5
            res.national_line_color      = "gray10"
            res.national_line_width      = 2.5
            res.state_line_color         = "gray10"
            res.state_line_width         = 1.5
            res.provincial_line_color    = "gray10"
            res.provincial_line_width    = 1.5
            res.grid_on                  = True
            res.grid_lat_spacing         = 10.0
            res.grid_lon_spacing         = 10.0
            res.grid_linestyle           = "--"
            res.grid_linewidth           = 1.5
            res.outline_boundaries       = "AllBoundaries"
            res.outline_specifiers       = "United States : States"
            if type2 in (0, 1, 2):
                res.outline_draw_order   = "Draw"
                res.fill_draw_order      = "PreDraw"
            elif type2 in (3, 4, 5):
                res.outline_draw_order   = "PreDraw"
                res.vector_draw_order    = "Draw"
            res.x_tick_mode              = "Explicit"
            res.x_tick_values            = np.linspace(-360., 540., 91)
            res.x_tick_labels            = [
                "0","10E","20E","30E","40E","50E","60E","70E",
                "80E","90E","100E","110E","120E","130E","140E","150E",
                "160E","170E","180","170W","160W","150W","140W","130W",
                "120W","110W","100W","90W","80W","70W","60W","50W",
                "40W","30W","20W","10W","0","10E","20E","30E",
                "40E","50E","60E","70E","80E","90E","100E","110E",
                "120E","130E","140E","150E","160E","170E","180","170W",
                "160W","150W","140W","130W","120W","110W","100W","90W",
                "80W","70W","60W","50W","40W","30W","20W","10W",
                "0","10E","20E","30E","40E","50E","60E","70E",
                "80E","90E","100E","110E","120E","130E","140E","150E",
                "160E","170E","180"]
            res.x_tick_fontsize          = 0.009
            res.x_major_tick_length      = 0.005
            res.y_tick_mode              = "Explicit"
            res.y_tick_values            = np.linspace(-90., 90., 19)
            res.y_tick_labels            = [
                "90S","80S","70S","60S","50S","40S",
                "30S","20S","10S","EQ","10N","20N","30N",
                "40N","50N","60N","70N","80N","90N"]
            res.y_tick_fontsize          = 0.009
            res.y_major_tick_length      = 0.005
            res.colorbar_label_fontsize  = 0.008
            res.colorbar_label_stride    = 4
            res.colorbar_orientation     = "vertical"
            res.colorbar_width           = 0.03
            res.colorbar_orthogonal_pos  = 0.005
            res.title_fontsize           = 0.012
            res.title_offset_y           = -0.004
            res.left_string_fontsize     = 0.010
            res.right_string_fontsize    = 0.010

        elif 10 <= type_ < 20:
            # Cross-section
            res.x_tick_fontsize          = 0.012
            res.x_major_tick_length      = 0.005
            res.x_tick_delta             = -0.5
            res.y_log                    = True
            res.y_reverse                = True
            res.y_tick_mode              = "Explicit"
            res.y_tick_values            = np.linspace(1000., 100., 19)
            res.y_tick_labels            = [
                "1000","","","850","","","700","","",
                "","500","","400","","300","","200","","100"]
            res.y_tick_fontsize          = 0.012
            res.y_major_tick_length      = 0.005
            res.y_tick_delta             = -0.5
            res.colorbar_label_fontsize  = 0.012
            res.colorbar_label_stride    = 4
            res.colorbar_orientation     = "vertical"
            res.colorbar_width           = 0.03
            res.colorbar_orthogonal_pos  = -0.018
            res.title_fontsize           = 0.012
            res.title_offset_y           = -0.004
            res.left_string_fontsize     = 0.009
            res.left_string_orth_pos     = -0.0025
            res.right_string_fontsize    = 0.009
            res.right_string_orth_pos    = -0.001

        elif 20 <= type_ < 30:
            # Polar cylindrical (SH)
            res.polar                    = "SH"
            res.tick_marks_on            = False
            res.map_fill                 = False
            res.map_outline              = False
            res.max_lat                  = -70.0
            res.colorbar_orientation     = "vertical"
            res.colorbar_width           = 0.03
            res.colorbar_orthogonal_pos  = 0.05
            res.title_fontsize           = 0.012
            res.title_offset_y           = -0.004
            res.left_string_fontsize     = 0.009
            res.left_string_orth_pos     = -0.0025
            res.right_string_fontsize    = 0.009
            res.right_string_orth_pos    = -0.001

        elif 30 <= type_ < 40:
            # Radius–Pressure (R-Z)
            res.y_reverse                = True
            res.y_log                    = True
            res.x_label                  = "Radius [km]"
            res.y_label                  = "Pressure [hPa]"
            res.colorbar_orientation     = "vertical"
            res.colorbar_width           = 0.03
            res.colorbar_orthogonal_pos  = 0.00
            res.title_fontsize           = 0.012
            res.title_offset_y           = -0.004
            res.left_string_fontsize     = 0.009
            res.left_string_orth_pos     = -0.0025
            res.right_string_fontsize    = 0.009
            res.right_string_orth_pos    = -0.001

        elif 40 <= type_ < 50:
            # Cartesian (x, y)
            res.grid_type                = "TriangularMesh"

        elif 50 <= type_ < 60:
            # Hodograph
            res.top_ticks_on             = False
            res.bottom_ticks_on          = False
            res.left_ticks_on            = False
            res.right_ticks_on           = False
            res.tick_length              = 0.0
            res.title_offset_y           = -0.004
            res.title_fontsize           = 0.012

    # Data rendering resources (last digit)
    if type2 == 0:
        res.contour_lines    = False
        res.contour_fill     = True
        res.fill_mode        = "RasterFill"
        res.raster_smoothing = True
    elif type2 == 1:
        res.contour_lines    = True
        res.contour_fill     = False
        res.line_width       = 5.5
        res.line_color       = "saddlebrown"
        res.line_labels      = True
        res.line_label_fontsize   = 0.006
        res.line_label_placement  = "constant"
        res.line_label_bg         = "transparent"
        res.right_string     = ""
        res.left_string      = ""
    elif type2 == 2:
        res.contour_lines    = True
        res.contour_fill     = True
        res.line_width       = 1.0
        res.line_color       = "black"
        res.line_labels      = True
        res.line_label_fontsize   = 0.007
        res.line_label_placement  = "constant"
        res.line_label_bg         = "transparent"
        res.fill_mode        = "RasterFill"
        res.raster_smoothing = True
        res.right_string     = ""
        res.left_string      = ""
    elif type2 == 3:
        res.vector_glyph         = "LineArrow"
        res.vector_ref_length    = 0.05
        res.vector_ref_color     = "black"
        res.vector_line_width    = 1.5
        res.vector_min_distance  = 0.02
        res.vector_color         = "black"
    elif type2 == 4:
        res.vector_glyph         = "CurlyVector"
        res.vector_ref_length    = 0.05
        res.vector_ref_color     = "black"
        res.vector_ref_on        = False
        res.vector_line_width    = 2.5
        res.vector_min_distance  = 0.025
        res.vector_arrowhead_max = 0.015
        res.vector_color         = "black"
    elif type2 == 5:
        res.vector_glyph         = "WindBarb"
        res.vector_ref_length    = 0.018
        res.vector_ref_on        = False
        res.vector_min_distance  = 0.02
        res.barb_color           = "black"
        res.barb_tick_length     = 0.4
        res.barb_tick_spacing    = 0.12
        res.barb_line_width      = 2.5
    elif type2 == 6:
        res.stream_line_width          = 2.0
        res.stream_color               = "saddlebrown"
        res.stream_arrow_length        = 0.003
        res.stream_arrow_stride        = 1
        res.stream_line_start_stride   = 1
        res.stream_step_size           = 0.001
        res.stream_min_arrow_spacing   = 0.01
        res.stream_min_distance        = 0.005
        res.stream_min_line_spacing    = 0.003
        res.stream_length_check_count  = 50
    elif type2 == 7:
        res._plot_type = -1  # blank contour (no map)
    elif type2 == 9:
        res._plot_type           = -1  # map only
        res.outline_draw_order   = "Draw"
        res.grid_draw_order      = "Draw"

    return res


# ============================================================
# get_auto_dir
# ============================================================

# Hardcoded JET/NOAA filesystem paths (legacy; environment-specific).
_AUTO_DIR_TABLE = {
    ("HWRF",          "EXPT"):    "hwrf-basinscale_multistorms-rev4_2015_realtime",
    ("HWRF",          "IDIR"):    "/lfs2/projects/hur-aoml/rthr-aoml/pytmp_2015-rev4_realtime/hwrf-basinscale_multistorms-rev4_2015_realtime/com/",
    ("HWRF",          "ATCFDIR"): "/lfs2/projects/hur-aoml/rthr-aoml/noscrub_2015-rev4_realtime/hwrf-basinscale_multistorms-rev4_2015_realtime/",
    ("HWRF15",        "EXPT"):    "hwrf-basinscale_multistorms-rev4_2015_realtime",
    ("HWRF15",        "IDIR"):    "/lfs2/projects/hur-aoml/rthr-aoml/pytmp_2015-rev4_realtime/hwrf-basinscale_multistorms-rev4_2015_realtime/com/",
    ("HWRF15",        "ATCFDIR"): "/lfs2/projects/hur-aoml/rthr-aoml/noscrub_2015-rev4_realtime/hwrf-basinscale_multistorms-rev4_2015_realtime/",
    ("HWRF16",        "EXPT"):    "hwrf-basinscale_multistorms-rev1_2016_realtime",
    ("HWRF16",        "IDIR"):    "/lfs2/projects/hur-aoml/rthr-aoml/pytmp_2016-rev1_realtime/hwrf-basinscale_multistorms-rev1_2016_realtime/com/",
    ("HWRF16",        "ATCFDIR"): "/lfs2/projects/hur-aoml/rthr-aoml/noscrub_2016-rev1_realtime/hwrf-basinscale_multistorms-rev1_2016_realtime/",
    ("HWRF16r1",      "EXPT"):    "hwrf-basinscale_multistorms-rev1_2016_history",
    ("HWRF16r1",      "IDIR"):    "/lfs2/projects/hur-aoml/rthr-aoml/pytmp_2016-rev1_history/hwrf-basinscale_multistorms-rev1_2016_history/com/",
    ("HWRF16r1",      "ATCFDIR"): "/lfs2/projects/hur-aoml/rthr-aoml/noscrub_2016-rev1_realtime/hwrf-basinscale_multistorms-rev1_2016_realtime/",
    ("HWRF16r2",      "EXPT"):    "hwrf-basinscale_multistorms-rev2_2016_history",
    ("HWRF16r2",      "IDIR"):    "/lfs2/projects/hur-aoml/Mu-Chieh.Ko/pytmp_2016-rev2_history/hwrf-basinscale_multistorms-rev2_2016_history/com/",
    ("HWRF16r2",      "ATCFDIR"): "/lfs2/projects/hur-aoml/Mu-Chieh.Ko/noscrub_2016-rev2_realtime/hwrf-basinscale_multistorms-rev2_2016_realtime/",
    ("HWRF17",        "EXPT"):    "HB17_v1_forecast",
    ("HWRF17",        "IDIR"):    "/lfs3/projects/hur-aoml/rthr-aoml/pytmp/HB17_v1_forecast/com/",
    ("HWRF17",        "ATCFDIR"): "/lfs3/projects/hur-aoml/rthr-aoml/noscrub/HB17_v1_forecast/",
    ("HWRF17EPS",     "EXPT"):    "H217_EPS_v1",
    ("HWRF17EPS",     "IDIR"):    "/lfs1/projects/hur-aoml/Ghassan.Alaka/pytmp/H217_EPS_v1/com/",
    ("HWRF17EPS",     "ATCFDIR"): "/lfs1/projects/hur-aoml/Ghassan.Alaka/noscrub/H217_EPS_v1/",
    ("HWRF18",        "EXPT"):    "H18L",
    ("HWRF18",        "IDIR"):    "/lfs2/projects/hur-aoml/Ghassan.Alaka/pytmp/H18L/com/",
    ("HWRF18",        "ATCFDIR"): "/lfs2/projects/hur-aoml/Ghassan.Alaka/noscrub/H18L/",
    ("HWRF18L",       "EXPT"):    "H18L",
    ("HWRF18L",       "IDIR"):    "/lfs2/projects/hur-aoml/Ghassan.Alaka/pytmp/H18L/com/",
    ("HWRF18L",       "ATCFDIR"): "/lfs2/projects/hur-aoml/Ghassan.Alaka/noscrub/H18L/",
    ("HWRF18LG",      "EXPT"):    "H8LG",
    ("HWRF18LG",      "IDIR"):    "/lfs2/projects/hur-aoml/Ghassan.Alaka/pytmp/H8LG/com/",
    ("HWRF18LG",      "ATCFDIR"): "/lfs2/projects/hur-aoml/Ghassan.Alaka/noscrub/H8LG/",
    ("HWRF18LS",      "EXPT"):    "H8LS",
    ("HWRF18LS",      "IDIR"):    "/lfs2/projects/hur-aoml/Ghassan.Alaka/pytmp/H8LS/com/",
    ("HWRF18LS",      "ATCFDIR"): "/lfs2/projects/hur-aoml/Ghassan.Alaka/noscrub/H8LS/",
    ("HWRF18W",       "EXPT"):    "H18W",
    ("HWRF18W",       "IDIR"):    "/lfs1/projects/hur-aoml/Ghassan.Alaka/pytmp/H18W/com/",
    ("HWRF18W",       "ATCFDIR"): "/lfs1/projects/hur-aoml/Ghassan.Alaka/noscrub/H18W/",
    ("GFS",           "EXPT"):    "GFS_Forecast",
    ("GFS",           "IDIR"):    "/lfs2/projects/hur-aoml/Ghassan.Alaka/GPLOT/GFSdata/",
    ("GFS",           "ATCFDIR"): "/lfs2/projects/hur-aoml/Ghassan.Alaka/noscrub/GFS_Forecast/",
    ("fvGFS_ATL",     "EXPT"):    "fvGFS_ATL",
    ("fvGFS_ATL",     "IDIR"):    "/lfs2/projects/hur-aoml/Ghassan.Alaka/pytmp/fvGFS_ATL/",
    ("fvGFS_ATL",     "ATCFDIR"): "/lfs2/projects/hur-aoml/Ghassan.Alaka/noscrub/fvGFS_ATL/",
}
# Aliases
for _src in ("HWRF_d03","HWRF15_d03","HB15"):
    for _t in ("EXPT","IDIR","ATCFDIR"):
        _AUTO_DIR_TABLE[(_src, _t)] = _AUTO_DIR_TABLE.get(("HWRF15", _t), "")
for _src in ("HB16","HWRF16_d03","HB16r1","HB16r2"):
    for _t in ("EXPT","IDIR","ATCFDIR"):
        _AUTO_DIR_TABLE[(_src, _t)] = _AUTO_DIR_TABLE.get(("HWRF16", _t), "")
for _src in ("HB17","HWRF17_d03"):
    for _t in ("EXPT","IDIR","ATCFDIR"):
        _AUTO_DIR_TABLE[(_src, _t)] = _AUTO_DIR_TABLE.get(("HWRF17", _t), "")
for _src in ("H18L","HWRF18_d03","HWRF18L_d03"):
    for _t in ("EXPT","IDIR","ATCFDIR"):
        _AUTO_DIR_TABLE[(_src, _t)] = _AUTO_DIR_TABLE.get(("HWRF18L", _t), "")
for _src in ("H8LG","HWRF18LG_d03"):
    for _t in ("EXPT","IDIR","ATCFDIR"):
        _AUTO_DIR_TABLE[(_src, _t)] = _AUTO_DIR_TABLE.get(("HWRF18LG", _t), "")
for _src in ("H8LS","HWRF18LS_d03"):
    for _t in ("EXPT","IDIR","ATCFDIR"):
        _AUTO_DIR_TABLE[(_src, _t)] = _AUTO_DIR_TABLE.get(("HWRF18LS", _t), "")
for _src in ("H18W","HWRF18W_d03"):
    for _t in ("EXPT","IDIR","ATCFDIR"):
        _AUTO_DIR_TABLE[(_src, _t)] = _AUTO_DIR_TABLE.get(("HWRF18W", _t), "")
for _src in ("GFS_d03","fvGFS_ATL_d03"):
    _base = "GFS" if "GFS" in _src and "fv" not in _src else "fvGFS_ATL"
    for _t in ("EXPT","IDIR","ATCFDIR"):
        _AUTO_DIR_TABLE[(_src, _t)] = _AUTO_DIR_TABLE.get((_base, _t), "")


def get_auto_dir(sys_env, dsource, type_):
    """Return hardcoded JET/NOAA directory paths for known HWRF/GFS experiments.

    This is a legacy function. Parameters are environment-specific.

    Parameters
    ----------
    sys_env : str, system name (currently only 'JET' supported)
    dsource : str, data source (e.g. 'HWRF16', 'GFS', 'fvGFS_ATL')
    type_   : str, one of 'EXPT', 'IDIR', 'ATCFDIR'

    Returns
    -------
    str, directory path or experiment name
    """
    valid_types = ("EXPT", "IDIR", "ATCFDIR")
    if type_ not in valid_types:
        raise ValueError(f"get_auto_dir: Request for '{type_}' not recognized.")
    if sys_env not in ("JET", "MAC"):
        raise ValueError(f"get_auto_dir: System '{sys_env}' not recognized.")
    if sys_env == "MAC":
        raise NotImplementedError("get_auto_dir: Mac environment not functional.")
    key = (dsource, type_)
    if key not in _AUTO_DIR_TABLE:
        raise ValueError(f"get_auto_dir: Data Source '{dsource}' not recognized.")
    return _AUTO_DIR_TABLE[key]


# ============================================================
# get_dmn_bds
# ============================================================

_DMN_BDS = {
    "basin":    [ 50., -15., -160.,   0.],
    "atl":      [ 50.,   0., -110.,   0.],
    "wlant":    [ 40.,   5., -110., -50.],
    "elant":    [ 35.,   5.,  -60.,   0.],
    "gom":      [ 35.,  10., -105., -70.],
    "eus":      [ 45.,  20.,  -85., -60.],
    "carolina": [ 41.,  31.,  -84., -74.],
    "epac":     [ 35.,   0., -140., -80.],
    "cpac":     [ 35.,   0., -180.,-140.],
    "wpac":     [ 45.,   0.,  100., 180.],
    "seasia":   [ 30.,   5.,   95., 125.],
    "bob":      [ 25.,   5.,   80., 100.],
    "arab":     [ 30.,   5.,   40.,  80.],
    "nio":      [ 30.,   0.,   50., 100.],
    "sio":      [  0., -40.,   30.,  90.],
    "aust":     [  0., -35.,   90., 160.],
    "spac":     [  0., -40.,  160., 240.],
    # Zero-extent domains: use full model grid
    "global":   [ 0.,  0.,  0.,  0.],
    "hwrf":     [ 0.,  0.,  0.,  0.],
    "d01":      [ 0.,  0.,  0.,  0.],
    "d02":      [ 0.,  0.,  0.,  0.],
    "d03":      [ 0.,  0.,  0.,  0.],
    "all_d03":  [ 0.,  0.,  0.,  0.],
    "tkfull":   [ 0.,  0.,  0.,  0.],
    "ships":    [ 0.,  0.,  0.,  0.],
    "data":     [ 0.,  0.,  0.,  0.],
    "core":     [ 0.,  0.,  0.,  0.],
    "storm":    [ 0.,  0.,  0.,  0.],
    "tcparent": [ 0.,  0.,  0.,  0.],
}


def get_dmn_bds(name_bds, d_source=""):
    """Return lat/lon bounding boxes for named domains.

    Parameters
    ----------
    name_bds : str or list of str, domain name(s) (case-insensitive)
    d_source : str, data source (reserved for future use)

    Returns
    -------
    list [latN, latS, lonW, lonE], or list of such lists for multiple domains
    """
    scalar = isinstance(name_bds, str)
    if scalar:
        name_bds = [name_bds]
    result = []
    for name in name_bds:
        key = name.lower().replace(" ", "_")
        result.append(list(_DMN_BDS.get(key, [0., 0., 0., 0.])))
    return result[0] if scalar else result


# ============================================================
# get_dmn_info
# ============================================================

def get_dmn_info(domain, dsource, info, do_value=False):
    """Return metadata for a named domain.

    Parameters
    ----------
    domain   : str, domain name
    dsource  : str, data source
    info     : str, one of 'bdstype', 'maxH', 'maxL', 'llbox'
    do_value : bool, fallback flag for bdstype=6

    Returns
    -------
    int or float
    """
    domain = domain.lower()
    valid_info = ("bdstype", "maxH", "maxL", "llbox")
    if info not in valid_info:
        raise ValueError(f"get_dmn_info: INFO '{info}' not recognized.")

    if info == "bdstype":
        _map = {
            "d02": 2, "d03": 3, "core": 3, "storm": 8,
            "tkfull": 4, "ships": 7, "global": 1, "alld03": 1,
        }
        if domain in _map:
            return _map[domain]
        if do_value:
            return 6
        return 0

    elif info == "maxH":
        _map = {
            "d03": 2, "core": 2, "storm": 4, "global": 25, "wmexico": 4, "alld03": 2,
        }
        return _map.get(domain, 10)

    elif info == "maxL":
        _map = {
            "d03": 2, "core": 2, "storm": 5, "global": 25, "wmexico": 4, "alld03": 2,
        }
        return _map.get(domain, 12)

    elif info == "llbox":
        _map = {
            "basin": 15., "bigd01": 15., "d01": 10., "atl": 10.,
            "wlant": 7.5, "clant": 7.5, "elant": 7.5, "epac": 7.5,
            "cpac": 7.5, "tkfull": 7.5, "d02": 7.5, "d03": 2.5,
            "core": 2.5, "storm": 7.5, "global": 20., "wmexico": 4.,
            "alld03": 2.5,
        }
        return _map.get(domain, 10.)


# ============================================================
# get_expt_info
# ============================================================

def get_expt_info(expts, info_type):
    """Read experiment information from $GPLOT_DIR/tbl/ExptInfo.dat.

    Parameters
    ----------
    expts     : str or list of str, experiment names
    info_type : str, one of 'expt', 'title'

    Returns
    -------
    list of str
    """
    valid_types = ("expt", "title")
    if info_type not in valid_types:
        raise ValueError(f"get_expt_info: infoType '{info_type}' not recognized.")

    gplot_dir = os.environ.get("GPLOT_DIR", "")
    dat_file  = os.path.join(gplot_dir, "tbl", "ExptInfo.dat")

    rows  = []
    names = []
    with open(dat_file) as fh:
        for line in fh:
            parts = [p.strip() for p in line.split(",")]
            if len(parts) >= 2:
                rows.append(parts)
                names.append(parts[0])

    scalar = isinstance(expts, str)
    if scalar:
        expts = [expts]

    col_idx = valid_types.index(info_type)  # 'expt'→0, 'title'→1
    result  = []
    for expt in expts:
        if expt not in names:
            print(f"WARNING: get_expt_info: EXPT '{expt}' not recognized. Assigning missing.")
            result.append("missing")
        else:
            # Use last matching entry (matches NCL exptind(dimsizes(exptind)-1))
            idx = max(i for i, n in enumerate(names) if n == expt)
            val = rows[idx][col_idx] if col_idx < len(rows[idx]) else "missing"
            result.append(val.strip())
    return result[0] if scalar else result


# ============================================================
# get_i_dir
# ============================================================

def get_i_dir(idir, expt, idate, sid, ensid):
    """Walk a directory structure to build an input file path.

    Tries each resource component (EXPT, ENSID, 'com', IDATE, SID)
    in the current directory, appending to the path when found.

    Parameters
    ----------
    idir  : str, base input directory
    expt  : str, experiment name
    idate : str, initialization date string
    sid   : str, storm ID
    ensid : str, ensemble member ID ('' if not applicable)

    Returns
    -------
    str, resolved directory path
    """
    d         = idir
    resources = [r for r in [expt, ensid, "com", idate, sid] if r]
    max_time  = len(resources)
    times     = 0
    while resources and times < max_time:
        for res in list(resources):
            try:
                entries = os.listdir(d)
            except OSError:
                entries = []
            if res in entries:
                d = os.path.join(d, res) + "/"
                resources.remove(res)
                break
        times += 1
    return d


# ============================================================
# get_file_tag  (main version with 2-D lookup table)
# ============================================================

_FILE_TAG_SOURCES = ("HWRF","HWRF15","HWRF16","HWRF17","HWRF18","NMMB","GFS","fvGFS_ATL")

_FILE_TAG_MASTER = {
    # (dsource, domain_code) → glob pattern  (domain_code: 0=other, 1=d02, 2=d03)
    ("HWRF",      0): "*hwrfprs.global.0p25.{FHR}.grb2",
    ("HWRF",      1): "*hwrfprs.synoptic.0p125.{FHR}.grb2",
    ("HWRF",      2): "*hwrfprs.storm.0p02.{FHR}.grb2",
    ("HWRF15",    0): "*hwrfprs.d1.0p25.{FHR}.grb2",
    ("HWRF15",    1): "*hwrfprs.d1.0p25.{FHR}.grb2",
    ("HWRF15",    2): "*hwrfprs.d3.0p03.{FHR}.grb2",
    ("HWRF16",    0): "*hwrfprs.global.0p25.{FHR}.grb2",
    ("HWRF16",    1): "*hwrfprs.synoptic.0p125.{FHR}.grb2",
    ("HWRF16",    2): "*hwrfprs.storm.0p02.{FHR}.grb2",
    ("HWRF17",    0): "*hwrfprs.global.0p25.{FHR}.grb2",
    ("HWRF17",    1): "*hwrfprs.synoptic.0p125.{FHR}.grb2",
    ("HWRF17",    2): "*hwrfprs.storm.0p02.{FHR}.grb2",
    ("HWRF18",    0): "*hwrfprs.global.0p25.{FHR}.grb2",
    ("HWRF18",    1): "*hwrfprs.synoptic.0p125.{FHR}.grb2",
    ("HWRF18",    2): "*hwrfprs.storm.0p015.{FHR}.grb2",
    ("NMMB",      0): "*nmbprs_d01.{FHR}.nc",
    ("NMMB",      1): "*nmbprs_d01.{FHR}.nc",
    ("NMMB",      2): "*nmbprs_d01.{FHR}.nc",
    ("GFS",       0): "gfs.t[0-9][0-9]z.pgrb2.0p25.{FHR}.grb2",
    ("GFS",       1): "gfs.t[0-9][0-9]z.pgrb2.0p25.{FHR}.grb2",
    ("GFS",       2): "gfs.t[0-9][0-9]z.pgrb2.0p25.{FHR}.grb2",
    ("fvGFS_ATL", 0): "atmos_sos.nest02_nested_ltd.nc.nc",
    ("fvGFS_ATL", 1): "atmos_sos.nest02_nested_ltd.nc.nc",
    ("fvGFS_ATL", 2): "atmos_sos.nest02_nested_ltd.nc.nc",
}


def get_file_tag(idir, dsource, expt, domain, idate, sid, fhr, itag, ocflag, ext, ensid):
    """Build an input file glob pattern.

    Parameters
    ----------
    idir   : str, input base directory
    dsource: str, data source
    expt   : str, experiment name
    domain : str, domain name (e.g. 'd03', 'd02', other)
    idate  : str, init date
    sid    : str, storm ID
    fhr    : str, forecast hour string (e.g. '006')
    itag   : str, custom file tag ('' to use lookup table)
    ocflag : int, ocean coupling flag (reserved)
    ext    : str, file extension override
    ensid  : str, ensemble member ID

    Returns
    -------
    str, full file glob pattern
    """
    if dsource not in _FILE_TAG_SOURCES:
        raise ValueError(f"get_file_tag: Data Source '{dsource}' not found.")

    ens_tag = f"{ensid}/" if ensid else ""
    idir2   = idir

    if itag == "":
        dom_code = 2 if domain == "d03" else (1 if domain == "d02" else 0)
        tmpl     = _FILE_TAG_MASTER.get((dsource, dom_code), "*{FHR}*")
        itag2    = tmpl.format(FHR=fhr)
    elif ext == "":
        itag2 = f"*{itag}*{fhr}*.grb2"
    else:
        itag2 = f"*{itag}*{fhr}*{ext}"

    return idir2 + itag2


def get_file_tag1(dsource, idir, idate, istorm, itag):
    """Build an all-forecast-hours file glob pattern (variant 1).

    Parameters
    ----------
    dsource : str, data source
    idir    : str, input directory
    idate   : str, init date
    istorm  : str, storm ID
    itag    : str, custom tag ('' for default)

    Returns
    -------
    str, file glob pattern
    """
    base = f"{idir}{idate}/{istorm}/"

    if "HWRF16" in dsource or "HWRF17" in dsource or "HWRF18" in dsource:
        return (base + f"*{itag}*.grb2") if itag else (base + "*hwrfprs.global.0p25.*.grb2")
    elif dsource in ("HWRF", "HWRF15"):
        return (base + f"*{itag}*.grb2") if itag else (base + "*hwrfprs.d1.0p25.*.grb2")
    elif dsource in ("HWRF_d03",):
        return (base + f"*{itag}*.grb2") if itag else (base + "*hwrfprs.d3.0p03.*.grb2")
    elif "d03" in dsource:
        return (base + f"*{itag}*.grb2") if itag else (base + "*hwrfprs.storm.0p02.*.grb2")
    elif dsource == "NMMB":
        return f"{idir}nmbprs_d01.*"
    elif dsource in ("GFS", "GFS_d03"):
        return f"{idir}{idate}/gfs*0p25*grb2"
    elif "fvGFS_ATL" in dsource:
        return (f"{idir}{idate}/{itag}.nc") if itag else (f"{idir}{idate}/atmos_sos.nest02_nested_ltd.nc")
    return base + "*.grb2"


def get_file_tag2(dsource, idir, idate, istorm, fhr, itag):
    """Build a single-forecast-hour file glob pattern (variant 2).

    Parameters
    ----------
    dsource : str, data source
    idir    : str, input directory
    idate   : str, init date
    istorm  : str, storm ID
    fhr     : str, forecast hour string
    itag    : str, custom tag ('' for default)

    Returns
    -------
    str, file glob pattern
    """
    base = f"{idir}{idate}/{istorm}/"

    if "HWRF16" in dsource or "HWRF17" in dsource or "HWRF18" in dsource:
        return (base + f"*{itag}*{fhr}.grb2") if itag else (base + f"*hwrfprs.global.0p25.{fhr}.grb2")
    elif dsource in ("HWRF", "HWRF15"):
        return (base + f"*{itag}.{fhr}.grb2") if itag else (base + f"*hwrfprs.d1.0p25.{fhr}.grb2")
    elif dsource == "HWRF_d03":
        return (base + f"*{itag}.{fhr}.grb2") if itag else (base + f"*hwrfprs.d3.0p03.{fhr}.grb2")
    elif "d03" in dsource:
        return (base + f"*{itag}.{fhr}.grb2") if itag else (base + f"*hwrfprs.storm.0p02.{fhr}.grb2")
    elif dsource == "NMMB":
        return f"{idir}nmbprs_d01.*"
    elif dsource == "GFS":
        return f"{idir}{idate}/gfs*0p25.{fhr}.grb2"
    elif "fvGFS_ATL" in dsource:
        return (f"{idir}{idate}/{itag}.nc") if itag else (f"{idir}{idate}/atmos_sos.nest02_nested_ltd.nc.nc")
    return base + f"*{fhr}*.grb2"


def get_file_tag3(idir, dsource, expt, domain, idate, sid, fhr, itag, level, ext, ensid):
    """Build a file glob pattern with explicit directory-level selection (variant 3).

    The ``level`` parameter selects the directory nesting structure
    (0–17, matching the NCL LEVEL variable).

    Returns
    -------
    str, full file glob pattern
    """
    ens_tag = f"{ensid}/" if ensid else ""

    _level_dirs = {
        17: f"{idir}{ens_tag}",
        16: f"{idir}{expt}/{ens_tag}{expt}/",
        15: f"{idir}{expt}/{ens_tag}com/",
        14: f"{idir}{expt}/{ens_tag}com/{sid}/",
        13: f"{idir}{expt}/{ens_tag}com/{sid}/{idate}/",
        12: f"{idir}{expt}/{ens_tag}com/{idate}/{sid}/",
        11: f"{idir}{expt}/{ens_tag}com/{idate}/",
        10: f"{idir}{expt}/",
         9: f"{idir}{ens_tag}com/",
         8: f"{idir}{ens_tag}com/{sid}/",
         7: f"{idir}{ens_tag}com/{sid}/{idate}/",
         6: f"{idir}{ens_tag}com/{idate}/{sid}/",
         5: f"{idir}{ens_tag}com/{idate}/",
         4: f"{idir}{ens_tag}{sid}/",
         3: f"{idir}{ens_tag}{sid}/{idate}/",
         2: f"{idir}{idate}/{ens_tag}{sid}/",
         1: f"{idir}{idate}/{ens_tag}",
         0: idir,
    }
    idir2 = _level_dirs.get(level, idir)

    if itag == "":
        dom_code = 2 if domain == "d03" else (1 if domain == "d02" else 0)
        tmpl = _FILE_TAG_MASTER.get((dsource, dom_code), "*{FHR}*.grb2")
        itag2 = tmpl.format(FHR=fhr)
        # HEDAS requires explicit itag
        if dsource == "HEDAS":
            raise ValueError("get_file_tag3: ITAG must be specified for HEDAS output.")
    elif ext == "":
        itag2 = f"*{itag}*{fhr}*.grb2"
    else:
        itag2 = f"*{itag}*{fhr}*{ext}"

    return idir2 + itag2


# ============================================================
# get_invest_sid
# ============================================================

def get_invest_sid(iyyyy, isid):
    """Look up invest SID from $GPLOT_DIR/tbl/SIDs_Old_New.dat.

    Parameters
    ----------
    iyyyy : str, 4-digit year
    isid  : str, TC storm ID (e.g. 'AL052021')

    Returns
    -------
    str, invest SID, or 'missing' if not found
    """
    gplot_dir = os.environ.get("GPLOT_DIR", "")
    dat_file  = os.path.join(gplot_dir, "tbl", "SIDs_Old_New.dat")

    if not os.path.isfile(dat_file):
        return "missing"

    sid_long_list, yyyy_list, tc_sid_list, inv_sid_list = [], [], [], []
    with open(dat_file) as fh:
        for line in fh:
            parts = line.split()
            if len(parts) >= 4:
                sid_long_list.append(parts[0])
                yyyy_list.append(parts[1])
                tc_sid_list.append(parts[2])
                inv_sid_list.append(parts[3])

    matches = [i for i, (y, s) in enumerate(zip(yyyy_list, tc_sid_list))
               if y == iyyyy and s == isid]

    if len(matches) != 1:
        return "missing"
    return inv_sid_list[matches[0]]


# ============================================================
# get_lat_lon_lbl  and  get_lat_lon_lbl2
# ============================================================

def _lon_label(lon, smooth=True):
    """Format a single longitude value as a string label."""
    if smooth:
        fmt = lambda v: str(int(round(v)))
    else:
        def fmt(v):
            s = f"{abs(v):.2f}"
            return s[:-3] if s.endswith(".00") else s

    if   -540 < lon < -360: return fmt(abs(360 + lon)) + "W"
    elif lon == -360:        return "0"
    elif -360 < lon < -180: return fmt(lon + 360) + "E"
    elif lon == -180:        return "180"
    elif -180 < lon < 0:    return fmt(abs(lon)) + "W"
    elif lon == 0:           return "0"
    elif 0   < lon < 180:   return fmt(lon) + "E"
    elif lon == 180:         return "180"
    elif 180 < lon < 360:   return fmt(360 - lon) + "W"
    elif lon == 360:         return "0"
    elif 360 < lon < 540:   return fmt(lon - 360) + "E"
    return ""


def _lat_label(lat, smooth=True):
    """Format a single latitude value as a string label."""
    if smooth:
        fmt = lambda v: str(int(round(v)))
    else:
        def fmt(v):
            s = f"{abs(v):.2f}"
            return s[:-3] if s.endswith(".00") else s

    if   lat < 0:  return fmt(abs(lat)) + "S"
    elif lat == 0: return "EQ"
    else:          return fmt(lat) + "N"


def get_lat_lon_lbl(lon_arr, lat_arr):
    """Generate explicit tick values and labels for lat/lon axes (smooth labels).

    Equivalent to NCL getLatLonLbl(res, LON, LAT) – returns a dict instead
    of mutating a resource object.

    Parameters
    ----------
    lon_arr : array-like, longitude tick positions
    lat_arr : array-like, latitude tick positions

    Returns
    -------
    dict with keys: x_values, x_labels, y_values, y_labels
    """
    lon_arr = np.asarray(lon_arr, dtype=float)
    lat_arr = np.asarray(lat_arr, dtype=float)
    if lat_arr.max() > 90 or lat_arr.min() < -90:
        raise ValueError("get_lat_lon_lbl: Latitude array out of bounds.")
    if lon_arr.max() > 540 or lon_arr.min() < -540:
        raise ValueError("get_lat_lon_lbl: Longitude array out of bounds.")
    return {
        "x_values": lon_arr.astype(int),
        "x_labels": [_lon_label(v, smooth=True) for v in lon_arr],
        "y_values": lat_arr.astype(int),
        "y_labels": [_lat_label(v, smooth=True) for v in lat_arr],
    }


def get_lat_lon_lbl2(lon_arr, lat_arr, smooth=True):
    """Generate tick values and labels for lat/lon axes (smooth or decimal).

    Equivalent to NCL getLatLonLbl2(res, LON, LAT, SMTH).
    When smooth=False, labels show decimal degrees (e.g. '12.5E').

    Returns
    -------
    dict with keys: x_values, x_labels, y_values, y_labels
    """
    lon_arr = np.asarray(lon_arr, dtype=float)
    lat_arr = np.asarray(lat_arr, dtype=float)
    if lat_arr.max() > 90 or lat_arr.min() < -90:
        raise ValueError("get_lat_lon_lbl2: Latitude array out of bounds.")
    if lon_arr.max() > 540 or lon_arr.min() < -540:
        raise ValueError("get_lat_lon_lbl2: Longitude array out of bounds.")
    return {
        "x_values": lon_arr.astype(int) if smooth else lon_arr,
        "x_labels": [_lon_label(v, smooth) for v in lon_arr],
        "y_values": lat_arr.astype(int) if smooth else lat_arr,
        "y_labels": [_lat_label(v, smooth) for v in lat_arr],
    }


# ============================================================
# get_model_info
# ============================================================

def get_model_info(models, info_type):
    """Read model information from $GPLOT_DIR/tbl/ModelInfo.dat.

    Parameters
    ----------
    models    : str or list of str, 4-letter model codes
    info_type : str, one of 'model', 'long_name', 'marker', 'color', 'title'

    Returns
    -------
    list of str or int
    """
    valid_types = ("model", "long_name", "marker", "color", "title")
    if info_type not in valid_types:
        raise ValueError(f"get_model_info: infoType '{info_type}' not recognized.")

    gplot_dir  = os.environ.get("GPLOT_DIR", "")
    dat_file   = os.path.join(gplot_dir, "tbl", "ModelInfo.dat")
    master_rows = []
    master_names= []
    with open(dat_file) as fh:
        for line in fh:
            parts = [p.strip() for p in line.split(",")]
            if len(parts) >= 2:
                master_rows.append(parts)
                master_names.append(parts[0][:4])

    col_idx = valid_types.index(info_type)

    scalar = isinstance(models, str)
    if scalar:
        models = [models]

    result = []
    for m in models:
        model = "GFSO" if "GFS" in m else m
        if model not in master_names:
            print(f"WARNING: get_model_info: MODEL '{model}' not recognized. Assigning missing.")
            result.append(None)
            continue
        idx = master_names.index(model)
        val = master_rows[idx][col_idx] if col_idx < len(master_rows[idx]) else None
        result.append(int(val) if info_type == "marker" and val is not None else val)
    return result[0] if scalar else result


# ============================================================
# get_model_info2  (hardcoded table)
# ============================================================

_MODEL_INFO2 = {
    # code: (color, marker, long_name_suffix, title, interp_code)
    "H18L":  ("brown",        7,  "2018 HWRF-Test-L",       "2018 HWRF Test-L",              "H8LI"),
    "H8LG":  ("brown",        7,  "2018 HWRF-Test-LG",      "2018 HWRF Test-LG",             "HLGI"),
    "H8LS":  ("brown",        7,  "2018 HWRF-Test-LS",      "2018 HWRF Test-LS",             "HLSI"),
    "H18W":  ("brown",        7,  "2018 HWRF-Test-W",       "2018 HWRF Test-W",              "H8WI"),
    "HP2H":  ("brown",        7,  "2018 HWRF-Test-2H",      "2018 HWRF Test-2H",             "H2HI"),
    "H2HI":  ("brown",        7,  "Interp. HP2H",           "2018 HWRF Test-2H (Early)",     "H2HI"),
    "HP3H":  ("brown",        7,  "2018 HWRF-Test-3H",      "2018 HWRF Test-3H",             "H3HI"),
    "H3HI":  ("brown",        7,  "Interp. HP3H",           "2018 HWRF Test-3H (Early)",     "H3HI"),
    "HP2F":  ("brown",        7,  "2018 HWRF-Test-2F",      "2018 HWRF Test-2F",             "H2FI"),
    "H2FI":  ("brown",        7,  "Interp. HP2F",           "2018 HWRF Test-2F (Early)",     "H2FI"),
    "H218":  ("brown",        7,  "2018 Oper. HWRF",        "2018 Oper. HWRF",               "H18I"),
    "H18I":  ("brown",        7,  "Interp. H218",           "2018 Oper. HWRF (Early)",       "H18I"),
    "HB17":  ("brown",        7,  "2017 HWRF-Basin",        "2017 Basin-Scale HWRF",         "HB7I"),
    "HB7I":  ("brown",        7,  "Interp. HB17",           "2017 Basin-Scale HWRF (Early)", "HB7I"),
    "HB16":  ("coral",        7,  "2016 HWRF-Basin",        "2016 Basin-Scale HWRF",         "HB6I"),
    "HB15":  ("deepskyblue",  7,  "2015 HWRF-Basin",        "2015 Basin-Scale HWRF",         "HB5I"),
    "HWRF":  ("purple",       9,  "Oper. HWRF",             "Operational HWRF",              "HWFI"),
    "HWFI":  ("purple",       9,  "Interp. HWRF",           "Operational HWRF (Early)",      "HWFI"),
    "OFCL":  ("red",         12,  "NHC Official",           "NHC Official Forecast",         "OFCI"),
    "OFCI":  ("red",         12,  "Interp. OFCL",           "NHC Official Forecast (Early)", "OFCI"),
    "AVNO":  ("blue",         6,  "Oper. GFS",              "Global Forecast System",        "AVNI"),
    "GFSO":  ("blue",         6,  "Oper. GFS",              "Global Forecast System",        "GFSI"),
    "AVNI":  ("blue",         6,  "Interp. GFS",            "Global Forecast System (Early)","AVNI"),
    "GFSI":  ("blue",         6,  "Interp. GFS",            "Global Forecast System (Early)","GFSI"),
    "HMON":  ("green",        4,  "Oper. HMON",             "Operational HMON",              "HMNI"),
    "HMNI":  ("green",        4,  "Interp. HMON",           "Operational HMON (Early)",      "HMNI"),
    "NVGM":  ("cyan",        11,  "Oper. NAVGEM",           "Operational NAVGEM",            "NVGI"),
    "NVGI":  ("cyan",        11,  "Interp. NVGM",           "Operational NAVGEM (Early)",    "NVGI"),
    "DSHP":  ("darkgreen",    6,  "Decay SHIPS",            "Decay SHIPS",                   ""),
    "LGEM":  ("cadetblue",    4,  "SHIPS LGEM",             "Logistic Growth Equation Model (LGEM)", ""),
    "CTCX":  ("darkorange",   8,  "COAMPS-TC",              "COAMPS-TC w/ GFS",              "CTCI"),
    "CTCI":  ("darkorange",   8,  "Interp. CTCX",           "Early COAMPS-TC",               "CTCI"),
    "TVCN":  ("darkslategray",10, "NHC Consensus",          "NHC Track Consensus",           ""),
    "IVCN":  ("darkslategray",10, "NHC Consensus",          "NHC Intensity Consensus",       ""),
    "FGFS":  ("seagreen3",   15,  "fvGFS ATL",              "fvGFS Atlantic 3km",            ""),
    "EGRR":  ("violetred",    3,  "UKMet",                  "UK Met Office",                 "EGRI"),
    "EGRI":  ("violetred",    3,  "Interp. UKMet",          "Early UK Met Office",           "EGRI"),
}


def get_model_info2(models, info_type, is_mstorm=True):
    """Return model metadata from the hardcoded lookup table.

    Parameters
    ----------
    models    : str or list of str, model codes
    info_type : str, one of 'color', 'marker', 'long_name', 'title', 'interp_code'
    is_mstorm : bool, if True auto-correct HWRF17/16 to HB17/HB16

    Returns
    -------
    list of str or int
    """
    valid_types = ("color", "marker", "long_name", "title", "interp_code")
    if info_type not in valid_types:
        raise ValueError(f"get_model_info2: infoType '{info_type}' not recognized.")
    col = valid_types.index(info_type)

    scalar = isinstance(models, str)
    if scalar:
        models = [models]

    result = []
    for m in models:
        if is_mstorm and "HWRF17" in m:
            model = "HB17"
        elif is_mstorm and "HWRF16" in m:
            model = "HB16"
        elif "fvGFS" in m:
            model = "FGFS"
        elif "GFS" in m:
            model = "GFSO"
        else:
            model = m

        if model not in _MODEL_INFO2:
            print(f"WARNING: get_model_info2: MODEL '{model}' not recognized. Assigning missing.")
            result.append(None)
            continue

        row = _MODEL_INFO2[model]
        if info_type == "long_name":
            result.append(f"{model}: {row[2]}")
        else:
            result.append(row[col if col < 2 else col - 1] if col != 2 else row[2])
    return result[0] if scalar else result


# ============================================================
# get_plot_title
# ============================================================

_PLOT_TITLES = {
    "RVO850_Z850_UV850":   "850mb Rel. Vorticity (10\u207b\u2075 s\u207b\u00b9, shaded), GPH (dam; lines), and Wind (kt; barbs)",
    "RVO500_Z500_UV500":   "500mb Rel. Vorticity (10\u207b\u2075 s\u207b\u00b9, shaded), GPH (dam; lines), and Wind (kt; barbs)",
    "RVO200_Z200_UV200":   "200mb Rel. Vorticity (10\u207b\u2075 s\u207b\u00b9, shaded), GPH (dam; lines), and Wind (kt; barbs)",
    "HGT500_MSLP":         "500mb Geopotential Height (dam; shaded), and MSLP (mb; lines, centers)",
    "HGT200_UV200_MSLP":   "200mb Geopotential Height (dam; shaded), Wind (kt; barbs), and MSLP (mb; centers)",
    "RH700400_UV700400_MSLP": "700-400mb Rel. Humidity (%; shaded), Wind (kt; barbs), and MSLP (mb; lines, centers)",
    "RVO850_UV200":        "850mb Rel. Vorticity (10\u207b\u2075 s\u207b\u00b9, shaded), and 200mb Wind (kt; barbs)",
    "UV850_MSLP":          "850mb Wind (kt; shaded, streamlines), MSLP (mb; centers)",
    "UV750_UV550":         "750mb Wind (kt; shaded, streamlines), 550mb Wind (kt, grey streamlines)",
    "RVO700_Z700_UV700":   "700mb Rel. Vorticity (10\u207b\u2075 s\u207b\u00b9, shaded), GPH (dam; contours), and Wind (kt; barbs)",
    "RH700_UV700_MSLP":    "700mb Relative Humidity (%; shaded), Wind (kt), and MSLP (mb; lines and centers)",
    "PRATE_MSLP_Z1000500": "Precip. Rate (mm/hr; shaded), MSLP (mb; lines, centers), 1000-500mb Z (dam; dashed lines)",
    "PRCP_MSLP_Z1000500":  "Bulk Precip. Rate (mm/hr; shaded), MSLP (mb; lines, centers), 1000-500mb Z (dam; dashed lines)",
    "PRATE_MSLP":          "Precip. Rate (mm/hr; shaded), MSLP (mb; centers)",
    "PRCP_MSLP":           "Bulk Precip. Rate (mm/hr; shaded), MSLP (mb; centers)",
    "TPRCP_MSLP":          "Total Precip. (inches; shaded), MSLP (mb; centers)",
    "TPW_MSLP":            "Total Precipitable Water (mm; shaded), MSLP (mb; centers)",
    "REFL_MSLP":           "Composite Reflectivity (dBz; shaded), MSLP (mb; centers)",
    "REFD_UV750":          "Reflectivity at 750mb (dBz; shaded), Wind at 750mb (kt; barbs)",
    "CAPE_HLCY_MSLP":      "CAPE (J/kg; shaded), Storm-Rel. Helicity (m\u00b2 s\u207b\u00b2; lines), MSLP (mb; centers)",
    "SHDL":                "Deep-Layer (200-850mb) Wind Shear (kt; shaded, lines)",
    "SHML":                "Mid-Layer (500-850mb) Wind Shear (kt; shaded, lines)",
    "SHSL":                "Shallow-Layer (700-850mb) Wind Shear (kt; shaded, lines)",
    "MSLP":                "Mean Sea-Level Pressure (mb; shaded, lines)",
    "UV10_MSLP":           "10m Wind (kt; shaded, streamlines), MSLP (mb; centers)",
    "T2":                  "2m Temperature (K; shaded)",
    "PRCP24_MSLP":         "24-h Precip. Rate (mm/hr; shaded), MSLP (mb; centers)",
    "UV_NS":               "Meridional Cross Section: Wind (kt; shaded)",
    "UV_EW":               "Zonal Cross Section: Wind (kt; shaded)",
    "SFDL":                "Deep-Layer (250-850mb) Steering Flow (kt; shaded, lines)",
    "SFML":                "Mid-Layer (500-850mb) Steering Flow (kt; shaded, lines)",
    "SFSL":                "Shallow-Layer (700-850mb) Steering Flow (kt; shaded, lines)",
    "TCCEN":               "Center Fixes [geopotential height centroid]",
    "TCCEN_zoom":          "Center Fixes [geopotential height centroid] (zoom)",
    "TCHODO":              "Near-Storm Hodograph [kts]",
    "TCHODO_zoom":         "Near-Storm Hodograph [kts] (zoom)",
    "T850_UV850_MSLP":     "850mb Temperature (K; shaded), 850mb Wind (kt; barbs), and MSLP (mb; centers)",
    "TPRCP_U10_MSLP":      "Total Precip. (inches; shaded), 10m Wind (kt, barbs), MSLP (mb; centers)",
    "PV200_UV200_MSLP":    "200mb Pot. Vorticity (PVU), 200mb Wind (kt; barbs), MSLP (mb; centers)",
    "DPT2_SST":            "2m DwPt. (K; shaded), SST (\u00b0C; lines), 10m Wind (kt;stmlines), MSLP (mb; centers)",
    "SST_T2":              "SST (\u00b0C; shaded), 2m Temp. (K; lines), 10m Wind (kt;stmlines), MSLP (mb; centers)",
    "LHFLX_MSLP":          "Latent Ht. Flx (Wm\u207b\u00b2; shaded), 10m Wind (kt; stmlines), MSLP (mb; centers)",
    "SHFLX_MSLP":          "Sensi. Ht. Flx (Wm\u207b\u00b2; shaded), 10m Wind (kt; stmlines), MSLP (mb; centers)",
}


def get_plot_title(short_name):
    """Return the plot title string for a given short_name key.

    Falls back to short_name itself if no match found.

    Parameters
    ----------
    short_name : str

    Returns
    -------
    str
    """
    return _PLOT_TITLES.get(short_name, short_name)


# ============================================================
# get_stm_thin
# ============================================================

def get_stm_thin(dsource, dmn):
    """Return the streamline thinning integer for a data source + domain.

    Reads $GPLOT_DIR/tbl/StreamlineThin.dat. Falls back to DEFAULT entry,
    then to 1 if nothing matches.

    Parameters
    ----------
    dsource : str, data source
    dmn     : str, domain name

    Returns
    -------
    int
    """
    gplot_dir = os.environ.get("GPLOT_DIR", "")
    dat_file  = os.path.join(gplot_dir, "tbl", "StreamlineThin.dat")

    specific = None
    default  = None
    try:
        with open(dat_file) as fh:
            for line in fh:
                parts = line.split()
                if len(parts) >= 3:
                    if parts[0] == dsource and parts[1] == dmn:
                        specific = int(parts[2])
                    elif parts[0] == "DEFAULT" and parts[1] == dmn:
                        default = int(parts[2])
    except FileNotFoundError:
        pass

    return specific if specific is not None else (default if default is not None else 1)


# ============================================================
# hbfilter  — Kurihara vortex removal (Kurihara et al. 1993, MWR)
# ============================================================

def hbfilter(input_arr, n_loops):
    """Apply the Kurihara hurricane vortex removal filter.

    Performs n_loops passes of a spectral low-pass filter in the zonal
    direction (for each latitude row) and then the meridional direction
    (for each longitude column).

    Parameters
    ----------
    input_arr : 2-D or 3-D numpy float array (..., lat, lon)
    n_loops   : int, number of filter passes

    Returns
    -------
    numpy.ndarray, filtered array of same shape as input_arr
    """
    C  = load_constants()
    M  = np.array([2., 3., 4., 2., 5., 6., 7., 2., 8., 9., 2.])
    nM = len(M)
    K  = 0.5 * (1.0 - np.cos(2.0 * C.pi / M))

    arr = np.asarray(input_arr, dtype=float)
    if arr.ndim == 1 or arr.ndim >= 4:
        raise ValueError(f"hbfilter: Data must be 2-D or 3-D (got {arr.ndim}-D).")

    original_2d = (arr.ndim == 2)
    output = arr[np.newaxis, :, :].copy() if original_2d else arr.copy()

    nz, ny, nx = output.shape
    xtu = np.full((nz, nx, nM), C.fval3, dtype=float)
    ytu = np.full((nz, ny, nM), C.fval3, dtype=float)

    # --- Zonal (x) filter ---
    for _ in range(n_loops):
        for jjj in range(ny - 1):          # NCL: do jjj=0,sz(1)-2
            xtu[:, 0,    :] = output[:, jjj, 0:1]       # boundary (broadcast nz→nz×nM)
            xtu[:, nx-1, :] = output[:, jjj, nx-1:nx]   # boundary
            xtu[:, 1:nx-1, 0] = (
                output[:, jjj, 1:nx-1]
                + K[0] * (output[:, jjj, :nx-2]
                          + output[:, jjj, 2:nx]
                          - 2.0 * output[:, jjj, 1:nx-1])
            )
            for n in range(1, nM):
                xtu[:, 1:nx-1, n] = (
                    xtu[:, 1:nx-1, n-1]
                    + K[n] * (xtu[:, :nx-2, n-1]
                              + xtu[:, 2:nx, n-1]
                              - 2.0 * xtu[:, 1:nx-1, n-1])
                )
            output[:, jjj, :nx-1] = xtu[:, :nx-1, nM-1]  # NCL: 0:sz(2)-2

    # --- Meridional (y) filter ---
    for _ in range(n_loops):
        for iii in range(nx):               # NCL: do iii=0,sz(2)-1
            ytu[:, 0,    :] = output[:, 0:1,    iii]    # boundary
            ytu[:, ny-1, :] = output[:, ny-1:ny, iii]   # boundary
            ytu[:, 1:ny-1, 0] = (
                output[:, 1:ny-1, iii]
                + K[0] * (output[:, :ny-2, iii]
                          + output[:, 2:ny, iii]
                          - 2.0 * output[:, 1:ny-1, iii])
            )
            for n in range(1, nM):
                ytu[:, 1:ny-1, n] = (
                    ytu[:, 1:ny-1, n-1]
                    + K[n] * (ytu[:, :ny-2, n-1]
                              + ytu[:, 2:ny, n-1]
                              - 2.0 * ytu[:, 1:ny-1, n-1])
                )
            output[:, :ny-1, iii] = ytu[:, :ny-1, nM-1]  # NCL: 0:sz(1)-2

    return output[0] if original_2d else output


# ============================================================
# is_str_subset2
# ============================================================

def is_str_subset2(my_str, sub):
    """Return a list of bools: True where sub is found in each element of my_str.

    Equivalent to NCL isStrSubset2(myStr, sub).
    If sub is empty string, all entries are True.

    Parameters
    ----------
    my_str : list of str
    sub    : str, substring to search for

    Returns
    -------
    list of bool
    """
    if sub == "":
        return [True] * len(my_str)
    return [sub in s for s in my_str]


# ============================================================
# level_convert
# ============================================================

def level_convert(old_lev, old_units, new_type, new_units):
    """Convert pressure levels between Pa/hPa in various output types.

    Parameters
    ----------
    old_lev   : array-like of numeric
    old_units : str, 'Pa' or 'hPa'
    new_type  : str, one of 'string', 'string4', 'float', 'integer'
    new_units : str, 'Pa' or 'hPa'

    Returns
    -------
    list or numpy.ndarray depending on new_type
    """
    valid_types = ("string", "string4", "float", "integer")
    valid_units = ("hPa", "Pa")
    if new_type not in valid_types:
        raise ValueError(f"level_convert: new_type '{new_type}' not supported.")
    if old_units not in valid_units:
        raise ValueError(f"level_convert: old_units '{old_units}' not supported.")
    if new_units not in valid_units:
        raise ValueError(f"level_convert: new_units '{new_units}' not supported.")

    mfact = 1.0
    if old_units != new_units:
        mfact = 0.01 if old_units == "Pa" else 100.0

    old_arr   = np.asarray(old_lev, dtype=float)
    converted = mfact * old_arr

    if new_type == "float":
        return converted.tolist()
    elif new_type == "integer":
        return converted.astype(int).tolist()
    elif new_type == "string":
        return [f"{v:3.0f}".strip() for v in converted]
    elif new_type == "string4":
        return [f"{int(v):04d}" for v in converted]


# ============================================================
# print_max_min
# ============================================================

def print_max_min(V, opt=0):
    """Print the maximum and minimum of array V.

    Parameters
    ----------
    V   : array-like
    opt : int, if 1 raise SystemExit after printing (matches NCL 'exit')
    """
    arr = np.asarray(V)
    print(f"Max: {np.max(arr)}")
    print(f"Min: {np.min(arr)}")
    if opt == 1:
        raise SystemExit(0)


# ============================================================
# remove_duplicates
# ============================================================

def remove_duplicates(arr):
    """Sort a numeric array and remove duplicate values.

    Equivalent to NCL remove_duplicates(IN).

    Parameters
    ----------
    arr : array-like

    Returns
    -------
    numpy.ndarray, sorted unique values
    """
    return np.unique(np.asarray(arr).ravel())


# ============================================================
# stat_plot_res
# ============================================================

def stat_plot_res(type_):
    """Generate stat/track plot resource settings as a SimpleNamespace.

    Equivalent to NCL StatPlotRes(type).

    Type codes
    ----------
    1  : map resources (track maps)
    2  : XY line plot resources
    3  : text resources A (small, left-aligned)
    4  : text resources B (small, left-aligned, font 25)
    5  : map main title text
    6  : map sub-title text
    7  : forecast hour label text
    8  : legend label text
    9  : XY main title text
    10 : XY sub-title text
    11 : best-track polyline
    12 : legend polyline
    13 : model track polyline
    14 : zero-line polyline
    15 : major grid-line polyline
    16 : best-track marker
    17 : legend marker
    18 : XY legend marker

    Returns
    -------
    SimpleNamespace
    """
    res = SimpleNamespace()

    if type_ == 1:
        res.draw                   = False
        res.maximize               = False
        res.map_limit_mode         = "LatLon"
        res.map_fill               = False
        res.outline_boundaries     = "AllBoundaries"
        res.outline_specifiers     = "United States : States"
        res.geophys_line_color     = "gray20"
        res.geophys_line_width     = 1.5
        res.national_line_color    = "gray20"
        res.national_line_width    = 1.5
        res.state_line_color       = "gray20"
        res.state_line_width       = 1.5
        res.shape_mode             = "FreeAspect"
        res.vp_width               = 0.8
        res.vp_height              = 0.4
        res.vp_y                   = 0.6
        res.x_major_tick_length    = 0.005
        res.x_major_tick_outward   = 0.0
        res.x_minor_tick_length    = 0.0025
        res.x_minor_tick_outward   = 0.0
        res.y_major_tick_length    = 0.005
        res.y_major_tick_outward   = 0.0
        res.y_minor_tick_length    = 0.0025
        res.y_minor_tick_outward   = 0.0
    elif type_ == 2:
        res.draw                   = False
        res.maximize               = False
        res.dash_pattern           = 0
        res.line_width             = 10.0
        res.mark_line_mode         = "MarkLines"
        res.marker_color           = "black"
        res.marker_size            = 0.020
        res.marker_width           = 8.0
        res.vp_width               = 0.8
        res.vp_height              = 0.4
        res.vp_y                   = 0.7
        res.title_font             = 22
        res.title_fontsize         = 0.025
        res.title_offset_y         = 0.05
        res.x_label                = "Forecast Lead Time [h]"
        res.x_label_offset_y       = 0.015
        res.y_label_offset_x       = 0.015
    elif type_ == 3:
        res.fontsize               = 0.009
        res.justify                = "CenterLeft"
    elif type_ == 4:
        res.justify                = "CenterLeft"
        res.font                   = 25
        res.color                  = "black"
        res.fontsize               = 0.009
    elif type_ == 5:
        res.fontsize               = 0.020
        res.justify                = "CenterCenter"
        res.color                  = "black"
        res.font                   = 25
    elif type_ == 6:
        res.fontsize               = 0.015
        res.justify                = "CenterCenter"
        res.color                  = "black"
        res.font                   = 25
    elif type_ == 7:
        res.justify                = "CenterLeft"
        res.fontsize               = 0.005
    elif type_ == 8:
        res.justify                = "CenterLeft"
        res.fontsize               = 0.009
    elif type_ == 9:
        res.fontsize               = 0.025
        res.justify                = "CenterCenter"
        res.color                  = "black"
        res.font                   = 25
    elif type_ == 10:
        res.fontsize               = 0.018
        res.justify                = "CenterCenter"
        res.color                  = "black"
        res.font                   = 25
    elif type_ == 11:
        res.line_width             = 15.0
    elif type_ == 12:
        res.line_width             = 5.0
    elif type_ == 13:
        res.line_width             = 8.0
    elif type_ == 14:
        res.line_width             = 5.0
        res.color                  = "black"
        res.dash_pattern           = 0
        res.draw_order             = "PreDraw"
    elif type_ == 15:
        res.line_width             = 1.5
        res.dash_pattern           = 11
        res.color                  = "grey70"
        res.draw_order             = "PreDraw"
    elif type_ == 16:
        res.marker_size            = 12.0
        res.marker_width           = 5.0
    elif type_ == 17:
        res.marker_size            = 8.0
        res.marker_width           = 3.0
    elif type_ == 18:
        res.marker_size            = 10.0
        res.marker_width           = 3.0

    return res


# ============================================================
# string_out
# ============================================================

def string_out(V, fmt=None):
    """Format an array as a comma-separated string.

    Equivalent to NCL stringOut(V).

    Parameters
    ----------
    V   : array-like (up to 100 elements used)
    fmt : str or None, Python %-style format string.
          Defaults to '%6.3f' for float, '%04d' for int, str() otherwise.

    Returns
    -------
    str
    """
    arr  = np.asarray(V).ravel()
    dtype = arr.dtype

    if fmt is None:
        if np.issubdtype(dtype, np.floating):
            fmt = "%6.3f"
        elif np.issubdtype(dtype, np.integer):
            fmt = "%04d"

    parts = []
    for i, v in enumerate(arr):
        if i >= 100:
            break
        if fmt and np.issubdtype(dtype, np.floating):
            parts.append(fmt % float(v))
        elif fmt and np.issubdtype(dtype, np.integer):
            parts.append(fmt % int(v))
        else:
            parts.append(str(v))
    return ",".join(parts)


# ============================================================
# test_plot_2d_map  /  test_plot_map  /  test_plot_contour
# ============================================================

def test_plot_2d_map(V, cn_levels):
    """Quick debug map plot of a 2-D lat/lon array. Displays to screen.

    Equivalent to NCL testPlot_2d_map(V, cnLevels).
    V must be a 2-D numpy array with named lat/lon coordinates.

    Parameters
    ----------
    V         : 2-D numpy array
    cn_levels : 1-D array-like of contour levels
    """
    if not HAS_MATPLOTLIB:
        raise ImportError("matplotlib required for test_plot_2d_map.")
    fig, ax = plt.subplots()
    cf = ax.contourf(V, levels=cn_levels, cmap="rainbow")
    plt.colorbar(cf, ax=ax)
    plt.title("TEST")
    plt.show()


def test_plot_map(V, cn_levels, file_name, plot_type="pdf"):
    """Debug map plot saved to file.

    Equivalent to NCL testPlot_map(V, cnLevels, fileName).

    Parameters
    ----------
    V         : 2-D numpy array
    cn_levels : 1-D array-like of contour levels
    file_name : str, output file path (without extension)
    plot_type : str, 'pdf' or 'png' (default 'pdf')
    """
    if not HAS_MATPLOTLIB:
        raise ImportError("matplotlib required for test_plot_map.")
    if plot_type not in ("pdf", "png", "x11"):
        raise ValueError(f"test_plot_map: plot_type '{plot_type}' not recognized.")
    fig, ax = plt.subplots()
    cf = ax.contourf(V, levels=cn_levels, cmap="rainbow")
    plt.colorbar(cf, ax=ax)
    plt.title("TEST")
    if plot_type == "x11":
        plt.show()
    else:
        fig.savefig(f"{file_name}.{plot_type}", bbox_inches="tight")
    plt.close(fig)


def test_plot_contour(V, cn_levels):
    """Quick debug contour-fill plot (no map). Displays to screen.

    Equivalent to NCL testPlot_contour(V, cnLevels).

    Parameters
    ----------
    V         : 2-D numpy array
    cn_levels : 1-D array-like of contour levels (ignored if length ≤ 1)
    """
    if not HAS_MATPLOTLIB:
        raise ImportError("matplotlib required for test_plot_contour.")
    fig, ax = plt.subplots()
    kw = {"levels": cn_levels} if len(np.asarray(cn_levels)) > 1 else {}
    cf = ax.contourf(V, cmap="rainbow", **kw)
    plt.colorbar(cf, ax=ax)
    plt.title("TEST")
    plt.show()
