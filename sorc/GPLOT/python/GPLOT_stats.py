#!/usr/bin/env python3
"""
GPLOT_stats.py

Python translation of GPLOT_stats.ncl (5383 lines).

Produces track/intensity guidance, trend, and verification graphics from
ATCF A-deck and B-deck files.  Six sequential steps mirror the NCL original:
  STEP 1 – Locate and validate ATCF files
  STEP 2 – Build merged ADECK, read BDECK, assemble forecast arrays
  STEP 3 – Guidance graphics (track maps, intensity/pressure XY plots)
  STEP 4 – Trend & lifetime graphics
  STEP 5 – Verification via MET-TC (tc_pairs / tc_stat)
  STEP 6 – Status-file check and final summary

Original NCL: GPLOT/sorc/GPLOT/ncl/GPLOT_stats.ncl
Translated:   GPLOT/sorc/GPLOT/python/GPLOT_stats.py

Authors: Translated by GPLOT Python migration (2024)
         Original NCL: Ghassan Alaka Jr., Mu-Chieh Ko, Lewis J. Gramer
"""

# ======================================================================
# IMPORTS
# ======================================================================
import os
import sys
import re
import glob
import math
import time
import shutil
import logging
import datetime
import subprocess
import numpy as np

try:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    import matplotlib.ticker as mticker
    import matplotlib.colors as mcolors
    import matplotlib.lines as mlines
    HAS_MPL = True
except ImportError:
    HAS_MPL = False

try:
    import cartopy.crs as ccrs
    import cartopy.feature as cfeature
    HAS_CARTOPY = True
except ImportError:
    HAS_CARTOPY = False

_SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(_SCRIPT_DIR, "modules"))
try:
    import gplot_util as gu
    import gplot_func as gf
    import gplot_main as gm
except ImportError as _e:
    sys.exit(f"ERROR: Cannot import GPLOT modules: {_e}")

FILL  = 1.0e20
LOG   = logging.getLogger("GPLOT_stats")

# ======================================================================
# INTENSITY COLOUR TABLE  (mirrors NCL "tcwinds1" colormap)
# ======================================================================
# kt thresholds:  <20  <34  <50  <64  <83  <96  <114  <137  >=137
_INT_THRESHOLDS = [20, 34, 50, 64, 83, 96, 114, 137]
_INT_COLORS = [
    "lightgrey",    # < 20 kt  (disturbance)
    "skyblue",      # < 34 kt  (tropical depression)
    "mediumseagreen",# < 50 kt  (tropical storm)
    "yellow",       # < 64 kt  (strong tropical storm)
    "orange",       # < 83 kt  (Category 1)
    "tomato",       # < 96 kt  (Category 2)
    "red",          # <114 kt  (Category 3)
    "firebrick",    # <137 kt  (Category 4)
    "darkviolet",   # >=137 kt (Category 5)
]

def _intensity_color(vmax_kt):
    """Return the intensity colour for a given Vmax in knots."""
    for thresh, clr in zip(_INT_THRESHOLDS, _INT_COLORS[:-1]):
        if vmax_kt < thresh:
            return clr
    return _INT_COLORS[-1]


# ======================================================================
# LOCK FILE HELPERS
# ======================================================================
def _lock(path, timeout=180):
    t0 = time.time()
    while True:
        try:
            fd = os.open(path, os.O_CREAT | os.O_EXCL | os.O_WRONLY)
            os.close(fd)
            return True
        except FileExistsError:
            if time.time() - t0 > timeout:
                return False
            time.sleep(2)

def _unlock(path):
    try:
        os.remove(path)
    except OSError:
        pass

def _write_status(status_file, text):
    lock = status_file + ".lock"
    if _lock(lock):
        try:
            with open(status_file, "w") as fh:
                fh.write(text + "\n")
        finally:
            _unlock(lock)


# ======================================================================
# POST-PROCESSING HELPER
# ======================================================================
def _post_process(ofile, do_rmwhite, do_gif):
    if do_rmwhite:
        subprocess.run(["convert", "-trim", ofile, ofile],
                       check=False, stdout=subprocess.DEVNULL,
                       stderr=subprocess.DEVNULL)
    if do_gif:
        gif = ofile.replace(".png", ".gif")
        subprocess.run(["convert", ofile, "+repage", f"gif:{gif}"],
                       check=False, stdout=subprocess.DEVNULL,
                       stderr=subprocess.DEVNULL)
        try:
            os.remove(ofile)
        except OSError:
            pass
        return gif
    return ofile


# ======================================================================
# MAP EXTENT HELPERS
# ======================================================================
def _auto_extent(lats_all, lons_all, pad_frac=0.05):
    """Compute a well-proportioned (2:1 lon:lat) map extent from data."""
    lats = np.asarray(lats_all, dtype=float)
    lons = np.asarray(lons_all, dtype=float)
    lats = lats[np.isfinite(lats)]
    lons = lons[np.isfinite(lons)]
    if lats.size == 0 or lons.size == 0:
        return [-90, 90, -180, 180]
    minlat, maxlat = lats.min(), lats.max()
    minlon, maxlon = lons.min(), lons.max()
    rng_lat = max(maxlat - minlat, 5.0)
    rng_lon = max(maxlon - minlon, 5.0)
    # Maintain ~2:1 lon:lat aspect
    ratio = rng_lat / rng_lon
    if ratio < 0.5:
        pad = 0.5 * abs(0.5 * rng_lon - rng_lat)
        maxlat += pad; minlat -= pad; rng_lat = maxlat - minlat
    elif ratio > 0.5:
        pad = 0.5 * abs(2.0 * rng_lat - rng_lon)
        maxlon += pad; minlon -= pad; rng_lon = maxlon - minlon
    return [minlat - pad_frac * rng_lat, maxlat + pad_frac * rng_lat,
            minlon - pad_frac * rng_lon, maxlon + pad_frac * rng_lon]


def _nice_grid_spacing(rng):
    if rng < 20:   return 2
    if rng < 75:   return 5
    return 10


# ======================================================================
# ATCF DATA HELPERS
# ======================================================================
def _parse_atcf_base(atcf_base):
    """Parse LONGSID, TC_NAME, TC_NUM, BASIN1, BASIN2 from ATCF filename."""
    longsid = os.path.splitext(atcf_base)[0].split(".")[0]
    chars   = list(longsid.lower())
    basin1  = chars[-1]
    tc_num  = "".join(chars[-3:-1])
    tc_name = "".join(chars[:-3]).upper()
    basin2_map = {"l":"al","e":"ep","c":"cp","w":"wp",
                  "s":"sh","p":"sh","a":"io","b":"io"}
    basin2 = basin2_map.get(basin1, "al")
    return longsid, tc_name, tc_num, basin1, basin2


def _idate_plus(idate_str, delta_hours):
    """Return IDATE string offset by delta_hours (can be negative)."""
    dt = datetime.datetime.strptime(idate_str, "%Y%m%d%H")
    dt2 = dt + datetime.timedelta(hours=delta_hours)
    return dt2.strftime("%Y%m%d%H")


def _merge_atcf_files(file_list, outfile):
    """Concatenate, deduplicate (by basin,num,init,fhr,model), and sort ATCF lines."""
    lines = []
    for fp in file_list:
        if fp and os.path.isfile(fp):
            with open(fp) as fh:
                lines.extend(fh.readlines())
    # Filter blank/short lines
    lines = [l for l in lines if len(l.split(",")) > 12]
    seen = set()
    unique = []
    for l in reversed(lines):               # keep last occurrence
        key = tuple(l.split(",")[:6])
        if key not in seen:
            seen.add(key)
            unique.append(l)
    unique.sort(key=lambda l: (l.split(",")[0].strip(),
                               l.split(",")[2].strip(),
                               l.split(",")[4].strip(),
                               int(l.split(",")[5].strip()) if l.split(",")[5].strip().lstrip("-").isdigit() else 0,
                               l.split(",")[11].strip() if len(l.split(",")) > 11 else ""))
    with open(outfile, "w") as fh:
        fh.writelines(unique)


def _read_atcf(atcf_path):
    """
    Read an ATCF A-deck file.
    Returns dict with arrays: lats, lons, fhrs, vmax, pmin, models, inits
    """
    lats, lons, fhrs, vmax, pmin = [], [], [], [], []
    models, inits = [], []
    if not os.path.isfile(atcf_path):
        return None
    with open(atcf_path) as fh:
        for line in fh:
            cols = [c.strip() for c in line.split(",")]
            if len(cols) < 12:
                continue
            try:
                models.append(cols[4].strip())
                inits.append(cols[2].strip())
                fhrs.append(int(cols[5].strip()))
                lat_str = cols[6].strip()
                lon_str = cols[7].strip()
                lat = float(lat_str[:-1]) / 10.0
                if lat_str[-1].upper() == "S":
                    lat = -lat
                lon = float(lon_str[:-1]) / 10.0
                if lon_str[-1].upper() == "W":
                    lon = -lon
                lats.append(lat)
                lons.append(lon)
                vmax.append(int(cols[8].strip()) if cols[8].strip().lstrip("-").isdigit() else 0)
                pmin.append(int(cols[9].strip()) if cols[9].strip().lstrip("-").isdigit() else 0)
            except (ValueError, IndexError):
                continue
    if not lats:
        return None
    return dict(lats=np.array(lats), lons=np.array(lons),
                fhrs=np.array(fhrs, dtype=int),
                vmax=np.array(vmax), pmin=np.array(pmin),
                models=np.array(models), inits=np.array(inits))


def _read_bdeck(bdeck_path):
    """
    Read ATCF B-deck best-track.
    Returns dict: lats, lons, vmax, pmin, valid_times, types
    """
    lats, lons, vmax, pmin, valid_times, types = [], [], [], [], [], []
    if not os.path.isfile(bdeck_path):
        return None
    with open(bdeck_path) as fh:
        for line in fh:
            cols = [c.strip() for c in line.split(",")]
            if len(cols) < 12:
                continue
            try:
                valid_times.append(cols[2].strip())
                lat_str = cols[6].strip()
                lon_str = cols[7].strip()
                lat = float(lat_str[:-1]) / 10.0
                if lat_str[-1].upper() == "S":
                    lat = -lat
                lon = float(lon_str[:-1]) / 10.0
                if lon_str[-1].upper() == "W":
                    lon = -lon
                lats.append(lat)
                lons.append(lon)
                vmax.append(int(cols[8].strip()) if cols[8].strip().lstrip("-").isdigit() else 0)
                pmin.append(int(cols[9].strip()) if cols[9].strip().lstrip("-").isdigit() else 0)
                types.append(cols[10].strip() if len(cols) > 10 else "")
            except (ValueError, IndexError):
                continue
    if not lats:
        return None
    return dict(lats=np.array(lats), lons=np.array(lons),
                vmax=np.array(vmax), pmin=np.array(pmin),
                valid_times=np.array(valid_times),
                types=np.array(types))


def _get_model_tracks(atcf, model, idate, max_fhr=120):
    """Extract (fhrs, lats, lons, vmax) arrays for one model/cycle."""
    if atcf is None:
        return None
    mask = (atcf["models"] == model) & (atcf["inits"] == idate) & (atcf["fhrs"] % 6 == 0)
    if not np.any(mask):
        return None
    idx  = np.where(mask)[0]
    order = np.argsort(atcf["fhrs"][idx])
    idx   = idx[order]
    mask2 = atcf["fhrs"][idx] <= max_fhr
    idx   = idx[mask2]
    if idx.size == 0:
        return None
    return dict(fhrs=atcf["fhrs"][idx],
                lats=atcf["lats"][idx],
                lons=atcf["lons"][idx],
                vmax=atcf["vmax"][idx])


# ======================================================================
# AUTO Y-AXIS GRID (mirrors NCL IntMjrGrd logic)
# ======================================================================
def _int_mjr_grid(vmin, vmax):
    rng = abs(vmax - vmin)
    if rng == 0:
        return np.array([vmin])
    if 1.2 * rng >= 100:
        step = 20
    elif 1.2 * rng >= 50:
        step = 10
    else:
        step = 5
    start = int(vmin - 0.10 * rng)
    start = start - (start % step) + step
    stop  = int(vmax + 0.10 * rng)
    stop  = stop - (stop % step)
    return np.arange(start, stop + step, step)


# ======================================================================
# LEGEND HELPER
# ======================================================================
def _add_legend(ax, model_names, colors, markers=None, ncols=4, loc="upper right"):
    handles = []
    for i, (nm, clr) in enumerate(zip(model_names, colors)):
        mk = markers[i] if markers else "o"
        h  = mlines.Line2D([], [], color=clr, marker=mk, linestyle="-",
                           linewidth=1.5, markersize=5, label=nm)
        handles.append(h)
    ax.legend(handles=handles, ncol=ncols, fontsize=6,
              loc=loc, framealpha=0.7, handlelength=2)


# ======================================================================
# STEP 3 – GUIDANCE GRAPHIC HELPERS
# ======================================================================

def _make_track_map(ax, best_lats, best_lons, best_fhrs, best_vmax,
                    model_tracks, model_names, model_colors, model_markers,
                    extent, tc_name, sid, idate, title, max_fhr,
                    do_markers=True, do_fhrlabels=True,
                    do_int_coding=False):
    """
    Draw a track guidance map on *ax*.
    If do_int_coding=True, colour track segments by intensity (tcwinds1).
    """
    proj = ccrs.PlateCarree()
    ax.set_extent([extent[2], extent[3], extent[0], extent[1]], crs=proj)
    ax.add_feature(cfeature.COASTLINE, linewidth=0.5)
    ax.add_feature(cfeature.BORDERS,   linewidth=0.3)
    ax.add_feature(cfeature.LAND,      facecolor="lightgrey", alpha=0.4)
    gl = ax.gridlines(draw_labels=True, linewidth=0.3, linestyle="--", alpha=0.4)
    gl.top_labels   = False
    gl.right_labels = False
    rng_lon = extent[3] - extent[2]
    rng_lat = extent[1] - extent[0]
    gl.xlocator = mticker.MultipleLocator(_nice_grid_spacing(rng_lon))
    gl.ylocator = mticker.MultipleLocator(_nice_grid_spacing(rng_lat))

    # Best track
    if best_lats is not None and len(best_lats) > 0:
        bl = np.asarray(best_lats); blo = np.asarray(best_lons)
        bv = np.asarray(best_vmax) if best_vmax is not None else None
        bf = np.asarray(best_fhrs) if best_fhrs is not None else None
        if do_int_coding and bv is not None:
            for i in range(len(bl) - 1):
                clr = _intensity_color(bv[i])
                ax.plot(blo[i:i+2], bl[i:i+2], "-", color=clr,
                        linewidth=4, transform=proj, zorder=4)
                ax.plot(blo[i], bl[i], "o", color=clr,
                        markersize=6, transform=proj, zorder=5)
        else:
            ax.plot(blo, bl, "-", color="black", linewidth=2,
                    transform=proj, zorder=4, label="BEST")
            ax.plot(blo, bl, "o", color="black", markersize=4,
                    transform=proj, zorder=5)
        if do_fhrlabels and bf is not None:
            for fi, (flat, flon) in enumerate(zip(bl, blo)):
                if int(bf[fi]) % 12 == 0:
                    ax.text(flon + 0.3, flat + 0.3, str(int(bf[fi])),
                            fontsize=5, transform=proj, color="black", zorder=6)

    # Model tracks
    legend_hdls = []
    if best_lats is not None:
        legend_hdls.append(mlines.Line2D([], [], color="black", linewidth=2,
                                         marker="o", markersize=4, label="BEST"))
    for mt, nm, clr in zip(model_tracks, model_names, model_colors):
        if mt is None:
            continue
        mk = "o"
        fl = np.asarray(mt["fhrs"]); la = np.asarray(mt["lats"]); lo = np.asarray(mt["lons"])
        vi = np.asarray(mt.get("vmax", np.zeros_like(fl)))
        mask = fl <= max_fhr
        fl, la, lo, vi = fl[mask], la[mask], lo[mask], vi[mask]
        if la.size == 0:
            continue
        if do_int_coding:
            for i in range(len(la) - 1):
                seg_clr = _intensity_color(vi[i])
                ax.plot(lo[i:i+2], la[i:i+2], "-", color=seg_clr,
                        linewidth=1.5, transform=proj, zorder=3)
            ax.plot(lo[-1], la[-1], "s", color=_intensity_color(vi[-1]),
                    markersize=8, transform=proj, zorder=4)
        else:
            ax.plot(lo, la, "-", color=clr, linewidth=1.5,
                    transform=proj, zorder=3)
            if do_markers:
                ax.plot(lo, la, marker=mk, color=clr, linestyle="",
                        markersize=4, transform=proj, zorder=4)
            if do_fhrlabels:
                for fi2, (flt, fln) in enumerate(zip(la, lo)):
                    if int(fl[fi2]) % 12 == 0:
                        ax.text(fln + 0.3, flt + 0.3, str(int(fl[fi2])),
                                fontsize=5, transform=proj, color=clr, zorder=5)
        legend_hdls.append(mlines.Line2D([], [], color=clr, linewidth=1.5,
                                         marker=mk if do_markers else None,
                                         markersize=4, label=nm))

    ax.legend(handles=legend_hdls, fontsize=6, loc="upper right",
              framealpha=0.7, ncol=min(4, len(legend_hdls)))
    ax.set_title(f"{title}\nStorm: {tc_name} ({sid.upper()})  Init: {idate}",
                 fontsize=9)


def _make_xy_plot(ax, bfhrs, bvals, model_data, model_names, model_colors,
                  model_markers, xlabel, ylabel, title, max_fhr,
                  do_markers=True):
    """
    Draw an intensity/pressure XY guidance plot on *ax*.
    bfhrs/bvals = best track arrays; model_data = list of (fhrs, vals) tuples.
    """
    all_vals = list(bvals[np.isfinite(bvals)] if bvals is not None and len(bvals) > 0 else [])
    for md in model_data:
        if md is not None:
            all_vals.extend(v for v in md[1] if np.isfinite(v) and v > 0)

    if all_vals:
        vmin, vmax = min(all_vals), max(all_vals)
        grid = _int_mjr_grid(vmin, vmax)
        ax.set_ylim(grid[0], grid[-1])
        ax.yaxis.set_major_locator(mticker.FixedLocator(grid))
    ax.set_xlim(-6, max_fhr + 6)

    fhr_ticks = list(range(0, max_fhr + 1, 12))
    ax.xaxis.set_major_locator(mticker.FixedLocator(fhr_ticks))
    ax.set_xlabel(xlabel, fontsize=9)
    ax.set_ylabel(ylabel, fontsize=9)
    ax.grid(True, linestyle="--", linewidth=0.4, alpha=0.5)
    ax.axvline(x=0, color="grey", linewidth=0.8)

    handles = []
    if bfhrs is not None and bvals is not None and len(bfhrs) > 0:
        mask = (np.asarray(bfhrs) <= max_fhr) & np.isfinite(bvals)
        ax.plot(np.asarray(bfhrs)[mask], bvals[mask], "-o", color="black",
                linewidth=2, markersize=5, zorder=5, label="BEST")
        handles.append(mlines.Line2D([], [], color="black", linewidth=2,
                                     marker="o", label="BEST"))

    for (mfhrs, mvals), nm, clr in zip(model_data, model_names, model_colors):
        if mfhrs is None or mvals is None or len(mfhrs) == 0:
            continue
        mfhrs = np.asarray(mfhrs); mvals = np.asarray(mvals, dtype=float)
        mask  = (mfhrs <= max_fhr) & (mvals > 0) & np.isfinite(mvals)
        if not np.any(mask):
            continue
        mk = "o" if do_markers else None
        ax.plot(mfhrs[mask], mvals[mask], "-", color=clr, linewidth=1.5,
                marker=mk, markersize=4, zorder=4, label=nm)
        handles.append(mlines.Line2D([], [], color=clr, linewidth=1.5,
                                     marker=mk, markersize=4, label=nm))

    ax.legend(handles=handles, fontsize=6, loc="best",
              framealpha=0.7, ncol=min(4, len(handles)))
    ax.set_title(title, fontsize=9)


# ======================================================================
# STEP 3 – PRODUCE ONE GUIDANCE GRAPHIC
# ======================================================================

def _save_guidance_plot(fig, ofile, do_rmwhite, do_gif):
    plt.tight_layout()
    plt.savefig(ofile, dpi=120, bbox_inches="tight")
    plt.close(fig)
    return _post_process(ofile, do_rmwhite, do_gif)


# ======================================================================
# STEP 4 – TREND HELPERS
# ======================================================================

_TREND_COLORS = [
    "#000000", "#c00000", "#0000c0", "#007000", "#c060c0",
    "#c08000", "#008080", "#804040", "#aaaaaa", "#60a060",
]


def _trend_cycle_colors(n):
    """Return n colours cycling newest→oldest (newest = black)."""
    return [_TREND_COLORS[i % len(_TREND_COLORS)] for i in range(n)]


# ======================================================================
# STEP 5 – MET-TC VERIFICATION HELPERS
# ======================================================================

def _run_mettc(cmd, log_path):
    """Run a shell command and capture output to *log_path*."""
    try:
        with open(log_path, "a") as fh:
            subprocess.run(cmd, shell=True, stdout=fh, stderr=subprocess.STDOUT,
                           check=False)
    except Exception as ex:
        LOG.warning("MET-TC command failed: %s – %s", cmd, ex)


def _read_tcstat_summary(tcst_path):
    """
    Parse tc_stat summary output.  Returns dict: model → {field: mean_value}.
    """
    result = {}
    if not os.path.isfile(tcst_path):
        return result
    with open(tcst_path) as fh:
        lines = fh.readlines()
    if len(lines) < 2:
        return result
    # Find COL_NAME and SUMMARY rows
    col_line = None
    for l in lines:
        if "COL_NAME:" in l:
            col_line = l
            break
    if col_line is None:
        return result
    cols = col_line.split()[1:]   # skip "COL_NAME:"
    try:
        i_model = cols.index("AMODEL")
        i_col   = cols.index("COLUMN")
        i_mean  = cols.index("MEAN")
    except ValueError:
        return result
    for l in lines:
        if not l.startswith("SUMMARY:"):
            continue
        parts = l.split()
        model = parts[i_model + 1]  # +1 for "SUMMARY:" prefix
        field = parts[i_col  + 1]
        try:
            mean_val = float(parts[i_mean + 1])
        except (ValueError, IndexError):
            continue
        if model not in result:
            result[model] = {}
        result[model][field] = mean_val
    return result



# ======================================================================
# MAIN DRIVER
# ======================================================================

def main():
    logging.basicConfig(level=logging.INFO,
                        format="%(asctime)s  %(levelname)s  %(message)s",
                        datefmt="%Y-%m-%d %H:%M:%S")

    print("MSG: GPLOT_stats.py started at", datetime.datetime.now())
    print("MSG: Welcome to GPLOT, Stats Module.")
    print("MSG: Produces track/intensity guidance, trend, and verification graphics.")
    print("")

    # ------------------------------------------------------------------
    # PART I – CONSTANTS (minimal; physical constants in gplot_util)
    # ------------------------------------------------------------------
    MS2KTS = 1.94384

    # ------------------------------------------------------------------
    # PART II – ENVIRONMENT VARIABLES
    # ------------------------------------------------------------------
    GPLOT_DIR     = os.environ.get("GPLOT_DIR",     "")
    IDATE         = os.environ.get("IDATE",         "")
    SID           = os.environ.get("SID",           "")
    ENSID         = os.environ.get("ENSID",         "")
    DSOURCE       = os.environ.get("DSOURCE",       "")
    EXPT          = os.environ.get("EXPT",          "")
    TIER          = os.environ.get("TIER",          "1")
    MASTER_NML_IN = os.environ.get("MASTER_NML_IN", "")
    verbose       = int(os.environ.get("verbose",   "0"))

    for val, name in [(GPLOT_DIR,"GPLOT_DIR"), (IDATE,"IDATE"), (SID,"SID")]:
        if not val:
            sys.exit(f"ERROR: Environment variable {name} is not set.")

    LOG.info("GPLOT_DIR = %s", GPLOT_DIR)
    LOG.info("IDATE     = %s", IDATE)
    LOG.info("SID       = %s", SID)
    LOG.info("ENSID     = %s", ENSID)
    LOG.info("DSOURCE   = %s", DSOURCE)

    # ------------------------------------------------------------------
    # PART III – MASTER NAMELIST
    # ------------------------------------------------------------------
    if MASTER_NML_IN and os.path.isfile(MASTER_NML_IN):
        nml_path = MASTER_NML_IN
    else:
        nml_path = os.path.join(GPLOT_DIR, "parm", "namelist.master.default")
    try:
        NML = gm.read_master_namelist(nml_path)
    except Exception as ex:
        LOG.warning("Could not read master namelist (%s): %s", nml_path, ex)
        NML = {}

    def _nml(key, default=""):
        return os.environ.get(key) or getattr(NML, key, default)

    DOMAIN         = _nml("DOMAIN",      "guidance")
    TIER           = _nml("TIER",        TIER)
    MCODE          = _nml("MCODE",       DSOURCE)
    MCODEI         = _nml("MCODEI",      "MISSING")
    MCODE12        = _nml("MCODE12",     "MISSING")
    MORIG          = _nml("MORIG",       MCODE)
    MACHINE        = _nml("MACHINE",     "")
    IDIR           = _nml("IDIR",        "")
    ODIR           = _nml("ODIR",        "")
    ODIR_TYPE      = int(_nml("ODIR_TYPE", "0"))
    ATCF1_DIR      = _nml("ATCF1_DIR",  "")
    ATCF1_TAG      = _nml("ATCF1_TAG",  "")
    ATCF2_DIR      = _nml("ATCF2_DIR",  "")
    ATCF2_TAG      = _nml("ATCF2_TAG",  "")
    ADECK_DIR      = _nml("ADECK_DIR",  "")
    BDECK_DIR      = _nml("BDECK_DIR",  "")
    INIT_HR        = int(_nml("INIT_HR",  "0"))
    FNL_HR         = int(_nml("FNL_HR",   "120"))
    MAX_FHR        = int(_nml("MAX_FHR",  str(FNL_HR)))
    nTrend         = int(_nml("nTrend",   "6"))
    DO_RMWHITE     = _nml("DO_RMWHITE",    "YES").upper() == "YES"
    DO_CONVERTGIF  = _nml("DO_CONVERTGIF", "NO").upper()  == "YES"
    DO_TITLES      = _nml("DO_TITLES",     "YES").upper() == "YES"
    DO_DISCLAIMER  = _nml("DO_DISCLAIMER", "YES").upper() == "YES"
    DO_MARKERS     = _nml("DO_MARKERS",    "YES").upper() == "YES"
    DO_FHRLABELS   = _nml("DO_FHRLABELS", "YES").upper() == "YES"
    DO_INTERP      = _nml("DO_INTERP",    "NO").upper()   == "YES"
    DO_SRCLBL      = _nml("DO_SRCLBL",    "YES").upper() == "YES"

    # Model lists (space or comma separated in namelist)
    def _model_list(key, default="MISSING"):
        raw = _nml(key, default)
        return [m.strip() for m in re.split(r"[,\s]+", raw) if m.strip()]

    TRKmodels    = _model_list("TRKmodels",    MCODE)
    TRKINTmodels = _model_list("TRKINTmodels", MCODE)
    INTmodels    = _model_list("INTmodels",    MCODE)
    PRSmodels    = _model_list("PRSmodels",    MCODE)
    TRKmodelsI   = _model_list("TRKmodelsI",   MCODEI)
    INTmodelsI   = _model_list("INTmodelsI",   MCODEI)
    TRKmodelsT   = _model_list("TRKmodelsT",   MCODE)
    INTmodelsT   = _model_list("INTmodelsT",   MCODE)
    PRSmodelsT   = _model_list("PRSmodelsT",   MCODE)
    etModels     = _model_list("etModels",     MCODEI)
    eiModels     = _model_list("eiModels",     MCODEI)
    ltModels     = _model_list("ltModels",     MCODE)
    liModels     = _model_list("liModels",     MCODE)

    # Lead times for XY plots
    lead_str = _nml("LEAD_TIMES", "0,12,24,36,48,60,72,84,96,108,120")
    LEAD_TIMES = [int(x) for x in re.split(r"[,\s]+", lead_str) if x.strip().lstrip("-").isdigit()]
    if not LEAD_TIMES:
        LEAD_TIMES = list(range(0, MAX_FHR + 1, 12))

    # Output directory
    if ODIR_TYPE == 1:
        ODIR = os.path.join(ODIR, "guidance").replace("//", "/")
    else:
        ODIR = os.path.join(ODIR, EXPT, ENSID, IDATE, "guidance").replace("//", "/")
    ODIR_ADECK = os.path.join(ODIR, "adeck")
    os.makedirs(ODIR,       exist_ok=True)
    os.makedirs(ODIR_ADECK, exist_ok=True)

    LOG.info("ODIR = %s", ODIR)
    LOG.info("MCODE = %s | MCODEI = %s", MCODE, MCODEI)

    # Status file
    STATUS_FILE = os.path.join(ODIR, f"status.{SID.lower()}.log")
    _write_status(STATUS_FILE, "working")

    # IDATE +6 h (for early forecast models)
    IDATE06 = _idate_plus(IDATE, 6)

    # ------------------------------------------------------------------
    # PART IV – GRAPHICS NAMELIST (product on/off flags)
    # ------------------------------------------------------------------
    var_nml = os.path.join(GPLOT_DIR, "parm", f"namelist.stats.{EXPT}")
    if not os.path.isfile(var_nml):
        var_nml = os.path.join(GPLOT_DIR, "parm", "namelist.stats.default")
    if not os.path.isfile(var_nml):
        LOG.warning("No stats namelist found; defaulting all products ON")
        var_nml = None

    def _flag(key, default=True):
        """Read True/False from stats namelist for a product key."""
        if var_nml is None:
            return default
        try:
            with open(var_nml) as fh:
                for line in fh:
                    cols = line.split()
                    if cols and cols[0] == key and len(cols) >= 2:
                        return cols[1].strip() in ("True", "YES", "1")
        except Exception:
            pass
        return default

    DO_GUIDANCE  = _flag("GUIDANCE",      True)
    DO_LT_GUIDE  = _flag("LATE_TK_GUIDE", True)  and DO_GUIDANCE
    DO_LTI_GUIDE = _flag("LATE_TKINT_GUIDE", True) and DO_GUIDANCE
    DO_ET_GUIDE  = _flag("EARLY_TK_GUIDE",True)  and DO_GUIDANCE
    DO_LI_GUIDE  = _flag("LATE_INT_GUIDE",True)  and DO_GUIDANCE
    DO_EI_GUIDE  = _flag("EARLY_INT_GUIDE",True) and DO_GUIDANCE
    DO_LP_GUIDE  = _flag("LATE_PRS_GUIDE",True)  and DO_GUIDANCE

    DO_TRENDS    = _flag("TRENDS",         True)
    DO_LT_TREND  = _flag("LATE_TK_TREND",  True)  and DO_TRENDS
    DO_LI_TREND  = _flag("LATE_INT_TREND", True)  and DO_TRENDS
    DO_LP_TREND  = _flag("LATE_PRS_TREND", True)  and DO_TRENDS

    DO_VERIFICATION = _flag("VERIFICATION",  True)
    DO_ET_VER    = _flag("EARLY_TK_VER",   True)  and DO_VERIFICATION
    DO_EI_VER    = _flag("EARLY_INT_VER",  True)  and DO_VERIFICATION
    DO_LT_VER    = _flag("LATE_TK_VER",    True)  and DO_VERIFICATION
    DO_LI_VER    = _flag("LATE_INT_VER",   True)  and DO_VERIFICATION

    # ------------------------------------------------------------------
    # STEP ONE – LOCATE ATCF FILE
    # ------------------------------------------------------------------
    print("\nMSG: ***************STEP ONE***************")
    print("MSG: Check out the ATCF file.")

    atcf_files_dat = os.path.join(ODIR, "ATCF_FILES.dat")
    ATCF_FILE = None

    if os.path.isfile(atcf_files_dat):
        with open(atcf_files_dat) as fh:
            candidates = [l.strip() for l in fh if SID.lower() in l.lower()]
        if candidates:
            ATCF_FILE = candidates[-1]

    if ATCF_FILE is None:
        for search_dir, tag in [(ATCF2_DIR, ATCF2_TAG), (ATCF1_DIR, ATCF1_TAG)]:
            if not search_dir:
                continue
            pat = os.path.join(search_dir, f"*{SID.lower()}*{IDATE}*{tag}*")
            hits = glob.glob(pat)
            if not hits:
                pat = os.path.join(search_dir, f"*{IDATE}*{SID.lower()}*{tag}*")
                hits = glob.glob(pat)
            if hits:
                ATCF_FILE = hits[-1]
                break

    if not ATCF_FILE or not os.path.isfile(ATCF_FILE):
        LOG.error("Could not find ATCF file for SID=%s IDATE=%s", SID, IDATE)
        _write_status(STATUS_FILE, "failed")
        sys.exit(1)

    LOG.info("ATCF_FILE = %s", ATCF_FILE)

    # Parse storm identity from ATCF filename
    ATCF_BASE = os.path.basename(ATCF_FILE)
    ATCF_DIR  = os.path.dirname(ATCF_FILE)
    LONGSID, TC_NAME, TC_NUM, BASIN1, BASIN2 = _parse_atcf_base(ATCF_BASE)

    YYYY = IDATE[:4]; MM = IDATE[4:6]; DD = IDATE[6:8]; HH = IDATE[8:10]

    SID2   = f"{BASIN2}{TC_NUM}{YYYY}"
    BDECK  = f"b{SID2}.dat"
    ADECK  = f"a{SID2}.dat"
    ADECK_M = f"a{SID2}_{MCODE.lower()}.dat"
    ADECK_I = f"a{SID2}_{MCODEI.lower()}.dat"
    ADECK_N = f"a{SID2}_new.dat"

    # Invest-number check (TC_NUM > 50 → skip verification)
    try:
        tc_num_int = int(TC_NUM)
    except ValueError:
        tc_num_int = 0
    if tc_num_int > 50:
        LOG.info("Invest TC detected; verification turned OFF.")
        DO_VERIFICATION = False
        DO_ET_VER = DO_EI_VER = DO_LT_VER = DO_LI_VER = False

    # Final vs temporary ATCF flag
    FNL_ATCF = (ATCF2_DIR and ATCF2_DIR in ATCF_FILE)

    # Determine old invest SID (predecessor)
    try:
        SID_OLD = gu.get_invest_sid(YYYY, SID) if hasattr(gu, "get_invest_sid") else None
    except Exception:
        SID_OLD = None

    # ------------------------------------------------------------------
    # STEP TWO – BUILD MERGED ADECK, READ BDECK, GATHER DATA ARRAYS
    # ------------------------------------------------------------------
    print("\nMSG: *************STEP TWO*************")
    print("MSG: Get the latest A-DECK and B-DECK.")

    ADIR = os.path.join(ODIR, "atcf")
    os.makedirs(ADIR, exist_ok=True)

    # Build merged ADECK
    adeck_m_path = os.path.join(ADIR, ADECK_M)
    adeck_n_path = os.path.join(ADIR, ADECK_N)

    # Start with model ATCF
    shutil.copy2(ATCF_FILE, adeck_m_path)

    # Rename model code if needed
    if MORIG and MORIG != MCODE:
        with open(adeck_m_path) as fh:
            content = fh.read().replace(MORIG, MCODE)
        with open(adeck_m_path, "w") as fh:
            fh.write(content)

    # Merge with NHC ADECK if available
    nhc_adeck = os.path.join(ADECK_DIR, ADECK) if ADECK_DIR else ""
    if os.path.isfile(nhc_adeck):
        shutil.copy2(nhc_adeck, os.path.join(ADIR, ADECK))
        _merge_atcf_files([os.path.join(ADIR, ADECK), adeck_m_path], adeck_n_path)
    else:
        shutil.copy2(adeck_m_path, adeck_n_path)

    # Merge with previously accumulated ADECK in ODIR_ADECK
    odir_adeck_n = os.path.join(ODIR_ADECK, ADECK_N)
    lock_adeck = odir_adeck_n + ".lock"
    if _lock(lock_adeck, 180):
        try:
            if os.path.isfile(odir_adeck_n):
                _merge_atcf_files([adeck_n_path, odir_adeck_n], odir_adeck_n)
            else:
                shutil.copy2(adeck_n_path, odir_adeck_n)
        finally:
            _unlock(lock_adeck)

    # Copy latest BDECK
    bdeck_src  = os.path.join(BDECK_DIR, BDECK) if BDECK_DIR else ""
    bdeck_path = os.path.join(ADIR, BDECK)
    if os.path.isfile(bdeck_src):
        shutil.copy2(bdeck_src, bdeck_path)
    elif not os.path.isfile(bdeck_path):
        LOG.warning("B-DECK not found; verification turned OFF.")
        DO_VERIFICATION = False
        DO_ET_VER = DO_EI_VER = DO_LT_VER = DO_LI_VER = False

    # Interpolation (NHC_interp) – if enabled
    if DO_INTERP and MCODEI != "MISSING" and MCODE12 != "MISSING":
        LOG.info("Producing interpolated forecasts via NHC_interp")
        interp_home = os.path.join(GPLOT_DIR, "sorc", "NHC_interp")
        interp_dir  = os.path.join(ADIR, "NHC_interp")
        if os.path.isdir(interp_home):
            shutil.rmtree(interp_dir, ignore_errors=True)
            shutil.copytree(interp_home, interp_dir)
            interp_odir  = os.path.join(interp_dir, "atcf")
            interp_file  = os.path.join(interp_odir, f"a{SID2}.gun")
            os.makedirs(interp_odir, exist_ok=True)
            # Symlink ADECK and BDECK
            try:
                os.symlink(adeck_n_path, os.path.join(interp_odir, ADECK))
                os.symlink(bdeck_path,   os.path.join(interp_odir, BDECK))
            except FileExistsError:
                pass
            # Patch namelist template
            nml_template = os.path.join(interp_dir, "intrfcst.input.template")
            nml_interp   = os.path.join(interp_dir, "intrfcst.input")
            if os.path.isfile(nml_template):
                with open(nml_template) as fh:
                    nml_txt = fh.read()
                nml_txt = (nml_txt.replace("AAAA", MCODE)
                                  .replace("BBBB", MCODE12)
                                  .replace("CCCC", MCODEI)
                                  .replace("DDD",  "006")
                                  .replace("EEE",  f"{FNL_HR:03d}"))
                with open(nml_interp, "w") as fh:
                    fh.write(nml_txt)
            exe = os.path.join(interp_dir, "run.sh")
            log = os.path.join(interp_odir, f"a{SID2}.log")
            _run_mettc(f"{exe} {SID2} doall {nml_interp}", log)
            if os.path.isfile(interp_file):
                _merge_atcf_files([adeck_n_path, interp_file], adeck_n_path)
                shutil.copy2(interp_file.replace(".gun", ""),
                             os.path.join(ADIR, ADECK_I))
        else:
            LOG.warning("NHC_interp not found at %s; skipping interpolation", interp_home)

    # Read merged ADECK
    atcf = _read_atcf(adeck_n_path)
    if atcf is None:
        LOG.error("Failed to read merged ADECK: %s", adeck_n_path)
        _write_status(STATUS_FILE, "failed")
        sys.exit(1)

    # Collect all cycle dates for this model
    mask_mc = (atcf["models"] == MCODE) & (atcf["fhrs"] % 6 == 0)
    if not np.any(mask_mc):
        LOG.error("No cycles for %s in merged ADECK", MCODE)
        _write_status(STATUS_FILE, "failed")
        sys.exit(1)
    all_inits = sorted(set(atcf["inits"][mask_mc]))
    # Filter to within 7 days of IDATE
    idate_dt = datetime.datetime.strptime(IDATE, "%Y%m%d%H")
    all_inits = [d for d in all_inits
                 if abs((datetime.datetime.strptime(d, "%Y%m%d%H") - idate_dt).total_seconds()) <= 7*86400]

    LOG.info("All valid cycles: %s", all_inits)

    # Read BDECK
    bdeck_data = _read_bdeck(bdeck_path)

    # Best track time series
    bt_fhrs  = []
    bt_lats  = []
    bt_lons  = []
    bt_vmax  = []
    bt_pmin  = []

    for ddd in range(-30, MAX_FHR + 1, 6):
        valid_dt   = idate_dt + datetime.timedelta(hours=ddd)
        valid_str  = valid_dt.strftime("%Y%m%d%H")
        bt_fhrs.append(ddd)
        if bdeck_data is not None:
            mask_b = bdeck_data["valid_times"] == valid_str
            if np.any(mask_b):
                i = np.where(mask_b)[0][0]
                bt_lats.append(float(bdeck_data["lats"][i]))
                bt_lons.append(float(bdeck_data["lons"][i]))
                bt_vmax.append(float(bdeck_data["vmax"][i]))
                bt_pmin.append(float(bdeck_data["pmin"][i]))
                continue
        bt_lats.append(np.nan)
        bt_lons.append(np.nan)
        bt_vmax.append(np.nan)
        bt_pmin.append(np.nan)

    bt_fhrs = np.array(bt_fhrs); bt_lats = np.array(bt_lats)
    bt_lons = np.array(bt_lons); bt_vmax = np.array(bt_vmax)
    bt_pmin = np.array(bt_pmin)

    # Check for TC cycles in best track (for verification)
    TC_FOUND = False
    if bdeck_data is not None:
        for d in all_inits:
            mask_b = bdeck_data["valid_times"] == d
            if np.any(mask_b):
                i = np.where(mask_b)[0][0]
                if bdeck_data["types"][i] in ("HU","TS","TD","SS","SD"):
                    TC_FOUND = True
                    break
    if not TC_FOUND:
        LOG.info("No TC cycles found in best track; verification turned OFF.")
        DO_VERIFICATION = False
        DO_ET_VER = DO_EI_VER = DO_LT_VER = DO_LI_VER = False

    # Get model information (colors, markers, long_names)
    def _model_color(model):
        try:
            return gu.get_model_info(model, "color")
        except Exception:
            return None
    def _model_marker(model):
        try:
            return gu.get_model_info(model, "marker")
        except Exception:
            return "o"
    def _model_longname(model):
        try:
            return gu.get_model_info(model, "long_name") or model
        except Exception:
            return model

    # ------------------------------------------------------------------
    # STEP THREE – GUIDANCE GRAPHICS
    # ------------------------------------------------------------------
    print("\nMSG: *************STEP THREE*************")
    print("MSG: PRODUCE GUIDANCE GRAPHICS.")
    _write_status(STATUS_FILE, "working")

    produced = []   # track output files

    if DO_GUIDANCE and HAS_MPL:

        # ---- LATE TRACK GUIDANCE -----------------------------------
        if DO_LT_GUIDE:
            LOG.info("Producing LATE TRACK GUIDANCE map")
            model_tracks = [_get_model_tracks(atcf, m, IDATE, MAX_FHR)
                            for m in TRKmodels]
            good_models  = [(mt, m) for mt, m in zip(model_tracks, TRKmodels) if mt]
            if good_models:
                all_lats = list(bt_lats[np.isfinite(bt_lats)])
                all_lons = list(bt_lons[np.isfinite(bt_lons)])
                for mt, _ in good_models:
                    all_lats += list(mt["lats"]); all_lons += list(mt["lons"])
                extent = _auto_extent(all_lats, all_lons)

                fig = plt.figure(figsize=(10, 8))
                ax  = fig.add_subplot(1,1,1,
                      projection=ccrs.PlateCarree() if HAS_CARTOPY else None)
                clrs = [_model_color(m) or "#333333" for _, m in good_models]
                _make_track_map(ax, bt_lats, bt_lons, bt_fhrs, bt_vmax,
                                [mt for mt,_ in good_models],
                                [m  for _,m  in good_models],
                                clrs, None, extent,
                                TC_NAME, SID, IDATE,
                                "Late Track Guidance", MAX_FHR,
                                do_markers=DO_MARKERS,
                                do_fhrlabels=DO_FHRLABELS)
                ofile = os.path.join(ODIR, f"TrackGuidance.{LONGSID}.{IDATE}.late.png")
                produced.append(_save_guidance_plot(fig, ofile, DO_RMWHITE, DO_CONVERTGIF))
            else:
                LOG.warning("No late track model data; skipping DO_LT_GUIDE")

        # ---- LATE TRACK / INTENSITY-CODED GUIDANCE -----------------
        if DO_LTI_GUIDE:
            LOG.info("Producing LATE TRACK/INTENSITY-CODED GUIDANCE map")
            model_tracks = [_get_model_tracks(atcf, m, IDATE, MAX_FHR)
                            for m in TRKINTmodels]
            good_models  = [(mt, m) for mt, m in zip(model_tracks, TRKINTmodels) if mt]
            if good_models:
                all_lats = list(bt_lats[np.isfinite(bt_lats)])
                all_lons = list(bt_lons[np.isfinite(bt_lons)])
                for mt, _ in good_models:
                    all_lats += list(mt["lats"]); all_lons += list(mt["lons"])
                extent = _auto_extent(all_lats, all_lons)

                fig = plt.figure(figsize=(10, 8))
                ax  = fig.add_subplot(1,1,1,
                      projection=ccrs.PlateCarree() if HAS_CARTOPY else None)
                _make_track_map(ax, bt_lats, bt_lons, bt_fhrs, bt_vmax,
                                [mt for mt,_ in good_models],
                                [m  for _,m  in good_models],
                                [None]*len(good_models), None, extent,
                                TC_NAME, SID, IDATE,
                                "Late Track Guidance (Intensity-Coded)", MAX_FHR,
                                do_markers=DO_MARKERS,
                                do_fhrlabels=DO_FHRLABELS,
                                do_int_coding=True)
                # Add intensity colour bar
                if HAS_CARTOPY:
                    cbar_ax = fig.add_axes([0.15, 0.02, 0.7, 0.025])
                    cmap_c  = mcolors.ListedColormap(_INT_COLORS)
                    norm    = mcolors.BoundaryNorm([0]+_INT_THRESHOLDS+[200], cmap_c.N)
                    sm      = plt.cm.ScalarMappable(cmap=cmap_c, norm=norm)
                    sm.set_array([])
                    cb = fig.colorbar(sm, cax=cbar_ax, orientation="horizontal")
                    cb.set_label("Max Wind [kt]", fontsize=8)
                    cb.set_ticks(_INT_THRESHOLDS)
                ofile = os.path.join(ODIR, f"TrackIntensityGuidance.{LONGSID}.{IDATE}.late.png")
                produced.append(_save_guidance_plot(fig, ofile, DO_RMWHITE, DO_CONVERTGIF))

        # ---- EARLY TRACK GUIDANCE ----------------------------------
        if DO_ET_GUIDE:
            LOG.info("Producing EARLY TRACK GUIDANCE map (cycle %s)", IDATE06)
            model_tracks = [_get_model_tracks(atcf, m, IDATE06, MAX_FHR)
                            for m in TRKmodelsI]
            good_models  = [(mt, m) for mt, m in zip(model_tracks, TRKmodelsI) if mt]
            if good_models:
                all_lats = list(bt_lats[np.isfinite(bt_lats)])
                all_lons = list(bt_lons[np.isfinite(bt_lons)])
                for mt, _ in good_models:
                    all_lats += list(mt["lats"]); all_lons += list(mt["lons"])
                extent = _auto_extent(all_lats, all_lons)

                fig = plt.figure(figsize=(10, 8))
                ax  = fig.add_subplot(1,1,1,
                      projection=ccrs.PlateCarree() if HAS_CARTOPY else None)
                clrs = [_model_color(m) or "#666666" for _, m in good_models]
                _make_track_map(ax, bt_lats, bt_lons, bt_fhrs, bt_vmax,
                                [mt for mt,_ in good_models],
                                [m  for _,m  in good_models],
                                clrs, None, extent,
                                TC_NAME, SID, IDATE06,
                                "Early Track Guidance", MAX_FHR,
                                do_markers=DO_MARKERS,
                                do_fhrlabels=DO_FHRLABELS)
                if ODIR_TYPE == 1:
                    early_odir = ODIR
                else:
                    early_odir = ODIR.replace(IDATE, IDATE06)
                    os.makedirs(early_odir, exist_ok=True)
                ofile = os.path.join(early_odir,
                                     f"TrackGuidance.{LONGSID}.{IDATE06}.early.png")
                produced.append(_save_guidance_plot(fig, ofile, DO_RMWHITE, DO_CONVERTGIF))

        # ---- LATE INTENSITY GUIDANCE --------------------------------
        if DO_LI_GUIDE:
            LOG.info("Producing LATE INTENSITY GUIDANCE plot")
            model_data  = []
            model_names = []
            model_clrs  = []
            for m in INTmodels:
                mt = _get_model_tracks(atcf, m, IDATE, MAX_FHR)
                if mt:
                    model_data.append((mt["fhrs"], mt["vmax"]))
                    model_names.append(_model_longname(m))
                    model_clrs.append(_model_color(m) or "#333333")

            vals_mask = np.isfinite(bt_fhrs) & np.isfinite(bt_vmax) & (bt_fhrs <= MAX_FHR)
            if model_data:
                fig, ax = plt.subplots(figsize=(10, 6))
                _make_xy_plot(ax, bt_fhrs[vals_mask], bt_vmax[vals_mask],
                              model_data, model_names, model_clrs, None,
                              "Forecast Hour", "Wind Speed [kt]",
                              f"Late Intensity Guidance\n{TC_NAME} ({SID.upper()}) Init: {IDATE}",
                              MAX_FHR, do_markers=DO_MARKERS)
                ofile = os.path.join(ODIR, f"IntensityGuidance.{LONGSID}.{IDATE}.late.png")
                produced.append(_save_guidance_plot(fig, ofile, DO_RMWHITE, DO_CONVERTGIF))

        # ---- EARLY INTENSITY GUIDANCE --------------------------------
        if DO_EI_GUIDE:
            LOG.info("Producing EARLY INTENSITY GUIDANCE plot (cycle %s)", IDATE06)
            model_data  = []
            model_names = []
            model_clrs  = []
            for m in INTmodelsI:
                mt = _get_model_tracks(atcf, m, IDATE06, MAX_FHR)
                if mt:
                    model_data.append((mt["fhrs"], mt["vmax"]))
                    model_names.append(_model_longname(m))
                    model_clrs.append(_model_color(m) or "#666666")

            vals_mask = np.isfinite(bt_fhrs) & np.isfinite(bt_vmax) & (bt_fhrs <= MAX_FHR)
            if model_data:
                fig, ax = plt.subplots(figsize=(10, 6))
                _make_xy_plot(ax, bt_fhrs[vals_mask], bt_vmax[vals_mask],
                              model_data, model_names, model_clrs, None,
                              "Forecast Hour", "Wind Speed [kt]",
                              f"Early Intensity Guidance\n{TC_NAME} ({SID.upper()}) Init: {IDATE06}",
                              MAX_FHR, do_markers=DO_MARKERS)
                if ODIR_TYPE == 1:
                    early_odir2 = ODIR
                else:
                    early_odir2 = ODIR.replace(IDATE, IDATE06)
                    os.makedirs(early_odir2, exist_ok=True)
                ofile = os.path.join(early_odir2,
                                     f"IntensityGuidance.{LONGSID}.{IDATE06}.early.png")
                produced.append(_save_guidance_plot(fig, ofile, DO_RMWHITE, DO_CONVERTGIF))

        # ---- LATE PRESSURE GUIDANCE ---------------------------------
        if DO_LP_GUIDE:
            LOG.info("Producing LATE PRESSURE GUIDANCE plot")
            model_data  = []
            model_names = []
            model_clrs  = []
            for m in PRSmodels:
                mt = _get_model_tracks(atcf, m, IDATE, MAX_FHR)
                if mt:
                    pvals = np.where(mt["pmin"] > 0, mt["pmin"].astype(float), np.nan)
                    model_data.append((mt["fhrs"], pvals))
                    model_names.append(_model_longname(m))
                    model_clrs.append(_model_color(m) or "#333333")

            vals_mask = np.isfinite(bt_fhrs) & np.isfinite(bt_pmin) & (bt_fhrs <= MAX_FHR) & (bt_pmin > 0)
            if model_data:
                fig, ax = plt.subplots(figsize=(10, 6))
                _make_xy_plot(ax, bt_fhrs[vals_mask], bt_pmin[vals_mask],
                              model_data, model_names, model_clrs, None,
                              "Forecast Hour", "Min Pressure [hPa]",
                              f"Late Pressure Guidance\n{TC_NAME} ({SID.upper()}) Init: {IDATE}",
                              MAX_FHR, do_markers=DO_MARKERS)
                ofile = os.path.join(ODIR, f"PressureGuidance.{LONGSID}.{IDATE}.late.png")
                produced.append(_save_guidance_plot(fig, ofile, DO_RMWHITE, DO_CONVERTGIF))

    # ------------------------------------------------------------------
    # STEP FOUR – TREND & LIFETIME GRAPHICS
    # ------------------------------------------------------------------
    print("\nMSG: *************STEP FOUR*************")
    print("MSG: PRODUCE TREND & LIFETIME GRAPHICS.")
    _write_status(STATUS_FILE, "working")

    if DO_TRENDS and HAS_MPL:

        # ---- LATE TRACK TRENDS -------------------------------------
        if DO_LT_TREND:
            for model_name in TRKmodelsT:
                LOG.info("Track trend: %s", model_name)
                # Collect last nTrend cycles (6-hourly back)
                cycle_tracks = []
                cycle_dates  = []
                for nn in range(nTrend):
                    dt_str = _idate_plus(IDATE, -6 * nn)
                    mt = _get_model_tracks(atcf, model_name, dt_str, MAX_FHR)
                    if mt is not None:
                        # Shift FHRs so they're relative to IDATE
                        shifted_fhrs = mt["fhrs"] - 6 * nn
                        cycle_tracks.append(
                            dict(fhrs=shifted_fhrs,
                                 lats=mt["lats"], lons=mt["lons"]))
                        cycle_dates.append(dt_str)

                if not cycle_tracks:
                    continue

                all_lats = list(bt_lats[np.isfinite(bt_lats)])
                all_lons = list(bt_lons[np.isfinite(bt_lons)])
                for ct in cycle_tracks:
                    all_lats += list(ct["lats"]); all_lons += list(ct["lons"])
                extent = _auto_extent(all_lats, all_lons)
                colors = _trend_cycle_colors(len(cycle_tracks))

                fig = plt.figure(figsize=(10, 8))
                ax  = fig.add_subplot(1,1,1,
                      projection=ccrs.PlateCarree() if HAS_CARTOPY else None)
                _make_track_map(ax,
                                bt_lats, bt_lons, bt_fhrs, bt_vmax,
                                cycle_tracks, cycle_dates, colors, None,
                                extent, TC_NAME, SID, IDATE,
                                f"{model_name} Track Trend", MAX_FHR,
                                do_markers=DO_MARKERS, do_fhrlabels=DO_FHRLABELS)
                ofile = os.path.join(ODIR,
                         f"TrackTrend.{model_name}.{LONGSID}.{IDATE}.png")
                produced.append(_save_guidance_plot(fig, ofile, DO_RMWHITE, DO_CONVERTGIF))

                # ---- LIFETIME TRACK (all cycles) -------------------
                life_tracks = []
                life_dates  = []
                for d in all_inits:
                    mt = _get_model_tracks(atcf, model_name, d, MAX_FHR)
                    if mt is not None:
                        offset = int(
                            (datetime.datetime.strptime(d, "%Y%m%d%H") -
                             datetime.datetime.strptime(all_inits[0], "%Y%m%d%H")
                            ).total_seconds() / 3600)
                        life_tracks.append(
                            dict(fhrs=mt["fhrs"] + offset,
                                 lats=mt["lats"], lons=mt["lons"]))
                        life_dates.append(d)

                if life_tracks:
                    all_lats2 = list(bt_lats[np.isfinite(bt_lats)])
                    all_lons2 = list(bt_lons[np.isfinite(bt_lons)])
                    for ct in life_tracks:
                        all_lats2 += list(ct["lats"]); all_lons2 += list(ct["lons"])
                    extent2 = _auto_extent(all_lats2, all_lons2)
                    colors2 = _trend_cycle_colors(len(life_tracks))

                    fig2 = plt.figure(figsize=(10, 8))
                    ax2  = fig2.add_subplot(1,1,1,
                           projection=ccrs.PlateCarree() if HAS_CARTOPY else None)
                    _make_track_map(ax2,
                                    bt_lats, bt_lons, bt_fhrs, bt_vmax,
                                    life_tracks, life_dates, colors2, None,
                                    extent2, TC_NAME, SID, IDATE,
                                    f"{model_name} Lifetime Tracks", MAX_FHR,
                                    do_markers=DO_MARKERS, do_fhrlabels=False)
                    ofile2 = os.path.join(ODIR,
                              f"AllTracks.{model_name}.{LONGSID}.{IDATE}.png")
                    produced.append(_save_guidance_plot(fig2, ofile2, DO_RMWHITE, DO_CONVERTGIF))

        # ---- LATE INTENSITY TRENDS ---------------------------------
        if DO_LI_TREND:
            for model_name in INTmodelsT:
                LOG.info("Intensity trend: %s", model_name)
                cycle_fhrs   = []
                cycle_vmax   = []
                cycle_dates  = []
                for nn in range(nTrend):
                    dt_str = _idate_plus(IDATE, -6 * nn)
                    mt = _get_model_tracks(atcf, model_name, dt_str, MAX_FHR)
                    if mt is not None:
                        shifted = mt["fhrs"] - 6 * nn
                        cycle_fhrs.append(shifted)
                        cycle_vmax.append(mt["vmax"].astype(float))
                        cycle_dates.append(dt_str)

                if not cycle_fhrs:
                    continue

                all_vals = [v for arr in cycle_vmax for v in arr if v > 0 and np.isfinite(v)]
                bt_v_mask = np.isfinite(bt_vmax) & (bt_fhrs <= MAX_FHR) & (bt_vmax > 0)
                if bt_v_mask.any():
                    all_vals += list(bt_vmax[bt_v_mask])
                if not all_vals:
                    continue

                fig, ax = plt.subplots(figsize=(10, 6))
                colors = _trend_cycle_colors(len(cycle_fhrs))
                model_data = list(zip(cycle_fhrs, cycle_vmax))
                _make_xy_plot(ax, bt_fhrs[bt_v_mask], bt_vmax[bt_v_mask],
                              model_data, cycle_dates, colors, None,
                              "Forecast Hour", "Wind Speed [kt]",
                              f"{model_name} Intensity Trend\n{TC_NAME} ({SID.upper()}) Init: {IDATE}",
                              MAX_FHR, do_markers=DO_MARKERS)
                ofile = os.path.join(ODIR,
                         f"IntensityTrend.{model_name}.{LONGSID}.{IDATE}.png")
                produced.append(_save_guidance_plot(fig, ofile, DO_RMWHITE, DO_CONVERTGIF))

                # Lifetime intensity
                life_fhrs = []; life_vmax = []; life_dates2 = []
                for d in all_inits:
                    mt = _get_model_tracks(atcf, model_name, d, MAX_FHR)
                    if mt is not None:
                        offset = int(
                            (datetime.datetime.strptime(d, "%Y%m%d%H") -
                             datetime.datetime.strptime(all_inits[0], "%Y%m%d%H")
                            ).total_seconds() / 3600)
                        life_fhrs.append(mt["fhrs"] + offset)
                        life_vmax.append(mt["vmax"].astype(float))
                        life_dates2.append(d)
                if life_fhrs:
                    fig2, ax2 = plt.subplots(figsize=(10, 6))
                    colors2 = _trend_cycle_colors(len(life_fhrs))
                    _make_xy_plot(ax2, None, None,
                                  list(zip(life_fhrs, life_vmax)),
                                  life_dates2, colors2, None,
                                  "Elapsed Hours", "Wind Speed [kt]",
                                  f"{model_name} Lifetime Intensity\n{TC_NAME} ({SID.upper()})",
                                  max(f.max() for f in life_fhrs),
                                  do_markers=False)
                    ofile2 = os.path.join(ODIR,
                              f"AllIntensity.{model_name}.{LONGSID}.{IDATE}.png")
                    produced.append(_save_guidance_plot(fig2, ofile2, DO_RMWHITE, DO_CONVERTGIF))

        # ---- LATE PRESSURE TRENDS -----------------------------------
        if DO_LP_TREND:
            LOG.info("Pressure trend: %s", MCODE)
            cycle_fhrs  = []; cycle_pmin = []; cycle_dates = []
            for nn in range(nTrend):
                dt_str = _idate_plus(IDATE, -6 * nn)
                mt = _get_model_tracks(atcf, MCODE, dt_str, MAX_FHR)
                if mt is not None:
                    pvals = np.where(mt["pmin"] > 0, mt["pmin"].astype(float), np.nan)
                    cycle_fhrs.append(mt["fhrs"] - 6 * nn)
                    cycle_pmin.append(pvals)
                    cycle_dates.append(dt_str)

            bt_p_mask = np.isfinite(bt_pmin) & (bt_fhrs <= MAX_FHR) & (bt_pmin > 0)
            if cycle_fhrs:
                fig, ax = plt.subplots(figsize=(10, 6))
                colors = _trend_cycle_colors(len(cycle_fhrs))
                _make_xy_plot(ax,
                              bt_fhrs[bt_p_mask] if bt_p_mask.any() else None,
                              bt_pmin[bt_p_mask] if bt_p_mask.any() else None,
                              list(zip(cycle_fhrs, cycle_pmin)),
                              cycle_dates, colors, None,
                              "Forecast Hour", "Min Pressure [hPa]",
                              f"{MCODE} Pressure Trend\n{TC_NAME} ({SID.upper()}) Init: {IDATE}",
                              MAX_FHR, do_markers=DO_MARKERS)
                ofile = os.path.join(ODIR,
                         f"PressureTrend.{MCODE}.{LONGSID}.{IDATE}.png")
                produced.append(_save_guidance_plot(fig, ofile, DO_RMWHITE, DO_CONVERTGIF))

    # ------------------------------------------------------------------
    # STEP FIVE – VERIFICATION via MET-TC
    # ------------------------------------------------------------------
    print("\nMSG: *************STEP FIVE*************")
    print("MSG: FORECAST VERIFICATION")
    _write_status(STATUS_FILE, "working")

    if DO_VERIFICATION:
        mettc_dir  = os.path.join(ODIR, "mettc")
        os.makedirs(mettc_dir, exist_ok=True)

        MET_PATH   = os.environ.get("MET_PATH", "")
        tc_pairs   = os.path.join(MET_PATH, "tc_pairs")  if MET_PATH else "tc_pairs"
        tc_stat    = os.path.join(MET_PATH, "tc_stat")   if MET_PATH else "tc_stat"
        adeckFile  = adeck_n_path
        bdeckFile  = bdeck_path

        # Copy config templates
        parm_mettc = os.path.join(GPLOT_DIR, "parm", "mettc", "config")
        tcp_conf   = os.path.join(mettc_dir, f"TCPairsConfig.{SID}.{IDATE}")
        tcsf_conf  = os.path.join(mettc_dir, f"TCStatConfig_filter.{SID}.{IDATE}")
        tcss_conf  = os.path.join(mettc_dir, f"TCStatConfig_summary.{SID}.{IDATE}")

        for src, dst in [("TCPairsConfig_match",   tcp_conf),
                         ("TCStatConfig_filter",    tcsf_conf),
                         ("TCStatConfig_summary",   tcss_conf)]:
            src_path = os.path.join(parm_mettc, src)
            if os.path.isfile(src_path) and not os.path.isfile(dst):
                shutil.copy2(src_path, dst)

        NHC_str_names = '["LEVEL","LEVEL","LEVEL","LEVEL","LEVEL"];'
        NHC_str_vals  = '["HU","TS","TD","SS","SD"];'
        init_end_str  = f'"{YYYY}{MM}{DD}_{HH}0000";'

        def _sed(fpath, pattern, replacement):
            try:
                with open(fpath) as fh:
                    txt = fh.read()
                txt = re.sub(pattern, replacement, txt)
                with open(fpath, "w") as fh:
                    fh.write(txt)
            except Exception:
                pass

        def _model_expr(model_list):
            quoted = ", ".join(f'"{m}"' for m in model_list if m)
            return f"[{quoted}];"

        def _run_verification(models, tag, tcp_out, tcsf_out, tcss_prefix):
            """Run tc_pairs + tc_stat for one model set; return per-model error dict."""
            if not os.path.isfile(tcp_conf):
                return {}

            m_expr = _model_expr(models)
            _sed(tcp_conf,  r"^model =.*",        f"model = {m_expr}")
            _sed(tcp_conf,  r"^init_end =.*",      f"init_end = {init_end_str}")
            _sed(tcsf_conf, r"^amodel =.*",        f"amodel = {m_expr}")
            _sed(tcsf_conf, r"^init_end = .*",     f"init_end = {init_end_str}")
            _sed(tcsf_conf, r"^column_str_name =.*", f"column_str_name = {NHC_str_names}")
            _sed(tcsf_conf, r"^column_str_val  =.*", f"column_str_val  = {NHC_str_vals}")
            _sed(tcsf_conf, r"^init_str_name =.*",   f"init_str_name = {NHC_str_names}")
            _sed(tcsf_conf, r"^init_str_val  =.*",   f"init_str_val  = {NHC_str_vals}")

            # Run tc_pairs
            tcp_cmd  = (f"{tc_pairs} -v 0 -adeck {adeckFile} -bdeck {bdeckFile}"
                        f" -config {tcp_conf} -out {tcp_out}"
                        f" -log {tcp_out}.log")
            _run_mettc(tcp_cmd, f"{tcp_out}.run.log")

            # tc_stat filter
            filt_job = f'jobs = ["-job filter -dump_row {tcsf_out}.tcst"];'
            _sed(tcsf_conf, r"^jobs = .*", filt_job)
            tcsf_cmd = f"{tc_stat} -v 0 -lookin {tcp_out}.tcst -config {tcsf_conf}"
            _run_mettc(tcsf_cmd, f"{tcsf_out}.run.log")

            # tc_stat summary per lead time
            summ_job = ('jobs = ["-job summary -column TRACK -column WIND'
                        ' -column TI -column AMAX_WIND-BMAX_WIND'
                        ' -by AMODEL"];')
            _sed(tcss_conf, r"^amodel =.*", f"amodel = {m_expr}")
            _sed(tcss_conf, r"^jobs =.*",   summ_job)

            errors = {}   # {model: {fhr: {field: value}}}
            for fhr in LEAD_TIMES:
                fhr_str = f"{fhr:03d}"
                tcss_out = f"{tcss_prefix}.f{fhr_str}.tcst"
                _sed(tcss_conf, r"lead\s*=.*",
                     f'lead = ["{fhr:02d}0000"];')
                tcs_cmd = (f"{tc_stat} -v 0 -lookin {tcsf_out}.tcst"
                           f" -config {tcss_conf} -out {tcss_out}"
                           f" -log {tcss_out}.log")
                _run_mettc(tcs_cmd, f"{tcss_out}.run.log")
                data = _read_tcstat_summary(tcss_out)
                for mdl, fields in data.items():
                    if mdl not in errors:
                        errors[mdl] = {}
                    errors[mdl][fhr] = fields
            return errors

        def _make_verification_xy(errors, models, model_colors, fields,
                                  field_labels, tag, ofile_prefix):
            """One XY error plot per field."""
            for field, ylabel in zip(fields, field_labels):
                xvals_by_model = {}
                yvals_by_model = {}
                for mdl in models:
                    if mdl not in errors:
                        continue
                    xs, ys = [], []
                    for fhr in sorted(errors[mdl]):
                        val = errors[mdl][fhr].get(field)
                        if val is not None and np.isfinite(val):
                            xs.append(fhr); ys.append(val)
                    if xs:
                        xvals_by_model[mdl] = xs
                        yvals_by_model[mdl] = ys
                if not xvals_by_model:
                    continue

                fig, ax = plt.subplots(figsize=(10, 6))
                ax.set_xlabel("Forecast Hour"); ax.set_ylabel(ylabel)
                ax.grid(True, linestyle="--", alpha=0.4)
                ax.axvline(x=0, color="grey", linewidth=0.8)
                handles = []
                for mdl, clr in zip(models, model_colors):
                    if mdl not in xvals_by_model:
                        continue
                    mk = "o" if DO_MARKERS else None
                    ax.plot(xvals_by_model[mdl], yvals_by_model[mdl],
                            "-", color=clr, linewidth=2, marker=mk,
                            markersize=5, label=mdl)
                    handles.append(mlines.Line2D([], [], color=clr,
                                                 linewidth=2, label=mdl))
                ax.legend(handles=handles, fontsize=7, loc="best", framealpha=0.7)
                ax.set_title(f"{field} – {tag}\n{TC_NAME} ({SID.upper()}) Init: {IDATE}",
                             fontsize=9)
                plt.tight_layout()
                ofile = os.path.join(ODIR, f"{ofile_prefix}.{field}.{LONGSID}.{IDATE}.{tag}.png")
                plt.savefig(ofile, dpi=120, bbox_inches="tight")
                plt.close(fig)
                produced.append(_post_process(ofile, DO_RMWHITE, DO_CONVERTGIF))

        # Late track verification
        if DO_LT_VER and os.path.isfile(tcp_conf):
            LOG.info("Verifying late track models: %s", ltModels)
            lt_errs = _run_verification(
                ltModels,
                "lateTRK",
                os.path.join(mettc_dir, f"tcpairs.{SID}.{IDATE}.lateTRK"),
                os.path.join(mettc_dir, f"tcstat.filter.{SID}.{IDATE}.lateTRK"),
                os.path.join(mettc_dir, f"tcstat.summary.{SID}.{IDATE}.lateTRK"))
            lt_clrs = [_model_color(m) or "#333333" for m in ltModels]
            if lt_errs:
                _make_verification_xy(lt_errs, ltModels, lt_clrs,
                                      ["TK_ERR","ALTK_ERR","CRTK_ERR"],
                                      ["Track Error [nmi]","Along-Track Error [nmi]",
                                       "Cross-Track Error [nmi]"],
                                      "late", "Verification")

        # Late intensity verification
        if DO_LI_VER and os.path.isfile(tcp_conf):
            LOG.info("Verifying late intensity models: %s", liModels)
            li_errs = _run_verification(
                liModels,
                "lateINT",
                os.path.join(mettc_dir, f"tcpairs.{SID}.{IDATE}.lateINT"),
                os.path.join(mettc_dir, f"tcstat.filter.{SID}.{IDATE}.lateINT"),
                os.path.join(mettc_dir, f"tcstat.summary.{SID}.{IDATE}.lateINT"))
            li_clrs = [_model_color(m) or "#333333" for m in liModels]
            if li_errs:
                _make_verification_xy(li_errs, liModels, li_clrs,
                                      ["WIND","AMAX_WIND-BMAX_WIND"],
                                      ["Wind Speed Error [kt]","Bias [kt]"],
                                      "late", "Verification")

        # Early track verification
        if DO_ET_VER and os.path.isfile(tcp_conf):
            LOG.info("Verifying early track models: %s", etModels)
            et_errs = _run_verification(
                etModels,
                "earlyTRK",
                os.path.join(mettc_dir, f"tcpairs.{SID}.{IDATE}.earlyTRK"),
                os.path.join(mettc_dir, f"tcstat.filter.{SID}.{IDATE}.earlyTRK"),
                os.path.join(mettc_dir, f"tcstat.summary.{SID}.{IDATE}.earlyTRK"))
            et_clrs = [_model_color(m) or "#666666" for m in etModels]
            if et_errs:
                _make_verification_xy(et_errs, etModels, et_clrs,
                                      ["TK_ERR","ALTK_ERR","CRTK_ERR"],
                                      ["Track Error [nmi]","Along-Track Error [nmi]",
                                       "Cross-Track Error [nmi]"],
                                      "early", "Verification")

        # Early intensity verification
        if DO_EI_VER and os.path.isfile(tcp_conf):
            LOG.info("Verifying early intensity models: %s", eiModels)
            ei_errs = _run_verification(
                eiModels,
                "earlyINT",
                os.path.join(mettc_dir, f"tcpairs.{SID}.{IDATE}.earlyINT"),
                os.path.join(mettc_dir, f"tcstat.filter.{SID}.{IDATE}.earlyINT"),
                os.path.join(mettc_dir, f"tcstat.summary.{SID}.{IDATE}.earlyINT"))
            ei_clrs = [_model_color(m) or "#666666" for m in eiModels]
            if ei_errs:
                _make_verification_xy(ei_errs, eiModels, ei_clrs,
                                      ["WIND","AMAX_WIND-BMAX_WIND"],
                                      ["Wind Speed Error [kt]","Bias [kt]"],
                                      "early", "Verification")

    # ------------------------------------------------------------------
    # STEP SIX – STATUS CHECK & FINAL SUMMARY
    # ------------------------------------------------------------------
    print("\nMSG: *************STEP SIX*************")
    print("MSG: CHECK GRAPHICAL PRODUCTION.")
    _write_status(STATUS_FILE, "working")

    # Minimal output check
    found_any = bool(produced)
    if DO_GUIDANCE:
        if DO_LT_GUIDE:
            pat = os.path.join(ODIR, f"TrackGuidance*{IDATE}.late.*")
            found_any = bool(glob.glob(pat))
    elif DO_TRENDS:
        if DO_LT_TREND:
            pat = os.path.join(ODIR, f"TrackTrend.{MCODE}*{IDATE}.*")
            found_any = bool(glob.glob(pat))
    elif DO_VERIFICATION:
        if DO_LT_VER:
            pat = os.path.join(ODIR, f"Verification*TK_ERR*{IDATE}.late.*")
            found_any = bool(glob.glob(pat))

    final_status = "complete" if FNL_ATCF else "incomplete"
    _write_status(STATUS_FILE, final_status)

    n_new = len(produced)
    print(f"\nMSG: GPLOT_stats.py completed at {datetime.datetime.now()}")
    print(f"MSG: Status → {final_status}")
    print(f"MSG: Produced {n_new} output file(s) this run.")
    for f in produced[:10]:
        print(f"  {f}")
    if n_new > 10:
        print(f"  ... and {n_new - 10} more")
    print("")


# ======================================================================
# ENTRY POINT
# ======================================================================
if __name__ == "__main__":
    main()
