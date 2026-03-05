#!/usr/bin/env python
"""
GPLOT_maps.py  -  2D map and cross-section graphics driver for GPLOT.

Produces filled-contour, wind-vector, streamline, and contour-line graphics
from model NetCDF output, overlaid on Cartopy map projections or vertical
cross-section axes.

Usage:
    Set environment variables then:
        python GPLOT_maps.py
    Optional env vars (override namelist):
        MASTER_NML_IN   - path to master namelist file
        VAR_NML         - path to graphics namelist file
        DOMAIN, TIER, DSOURCE, IDATE, SID, ENSID, verbose, ...

Environment variables (required):
    GPLOT_DIR   - root of the GPLOT installation

Original NCL: sorc/GPLOT/ncl/GPLOT_maps.ncl (3269 lines)
"""

import os
import sys
import glob
import math
import datetime
import subprocess
import tempfile
import time
from types import SimpleNamespace

import numpy as np

try:
    import xarray as xr
    HAS_XARRAY = True
except ImportError:
    HAS_XARRAY = False
    print("WARNING: xarray not available. NetCDF reading will be limited.")

try:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    import matplotlib.patheffects as pe
    HAS_MATPLOTLIB = True
except ImportError:
    HAS_MATPLOTLIB = False
    print("ERROR: matplotlib not available. Cannot produce graphics.")
    sys.exit(1)

try:
    import cartopy.crs as ccrs
    import cartopy.feature as cfeature
    HAS_CARTOPY = True
except ImportError:
    HAS_CARTOPY = False
    print("WARNING: cartopy not available. Map projections will be limited.")

try:
    from scipy.ndimage import maximum_filter, minimum_filter
    HAS_SCIPY = True
except ImportError:
    HAS_SCIPY = False

# ── GPLOT module imports ───────────────────────────────────────────────────
_GPLOT_DIR = os.environ.get("GPLOT_DIR", "")
sys.path.insert(0, os.path.join(_GPLOT_DIR, "sorc/GPLOT/python/modules"))
import gplot_util as gu
import gplot_func as gf
import gplot_main as gm

_FVAL = 9.96921e+36
_MS2KTS = 1.94384449


# ══════════════════════════════════════════════════════════════════════════════
# Helper utilities
# ══════════════════════════════════════════════════════════════════════════════

def _open_dataset(path, **kwargs):
    """Open a dataset, converting .grb2 to NetCDF via wgrib2 if needed.

    For GRIB2 files, runs ``wgrib2 -netcdf`` to produce a temporary NetCDF,
    loads it eagerly into memory, deletes the temp file, and returns the
    in-memory Dataset.  All other files are opened normally with xarray.
    """
    if path.endswith(".grb2"):
        tmp = tempfile.NamedTemporaryFile(suffix=".nc", delete=False)
        tmp.close()
        try:
            subprocess.run(
                ["wgrib2", path, "-netcdf", tmp.name],
                check=True,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.PIPE,
            )
            ds = xr.open_dataset(tmp.name, **kwargs).load()
        finally:
            if os.path.exists(tmp.name):
                os.remove(tmp.name)
        return ds
    return xr.open_dataset(path, **kwargs)


def _find_ds_with_var(datasets, dsource, var, lev):
    """Return (ds, vname) for the first dataset containing the named variable."""
    vname = gu.find_var_name(dsource, var, lev)
    for ds in datasets:
        if ds is not None and vname in ds:
            return ds, vname
    return None, vname


def _sanitize_streamplot_data(*arrays):
    """Replace NaN/inf values with 0 so matplotlib streamplot does not hang.

    matplotlib.streamplot enters an infinite loop when the velocity field
    contains NaN or inf values.  NCL's gsn_csm_streamline handled missing
    values natively, but the Python translation must sanitize explicitly.
    """
    return tuple(np.nan_to_num(a, nan=0.0, posinf=0.0, neginf=0.0) for a in arrays)


def _lev_ns_from_ds(ds, dname):
    """Read a vertical coordinate and return a SimpleNamespace for gplot_func readers."""
    arr = ds[dname].values.astype(float)
    units = str(ds[dname].attrs.get("units", "hPa"))
    mf = 100.0 if units.lower() == "pa" else 1.0
    return SimpleNamespace(values=arr, units=units, mf=mf)


def _local_extrema(field2d, mode):
    """
    Identify local maxima ('max') or minima ('min') in a 2D field.
    Returns dict: xi (col indices), yi (row indices), val (values).
    """
    f = np.where(np.isfinite(field2d), field2d, np.nan)
    nn = np.nan_to_num
    if HAS_SCIPY:
        if mode == "max":
            ref = maximum_filter(nn(f, nan=-1e30), size=5)
            mask = (f == ref) & np.isfinite(f)
        else:
            ref = minimum_filter(nn(f, nan=1e30), size=5)
            mask = (f == ref) & np.isfinite(f)
    else:
        # Manual 8-neighbour comparison fallback
        pad = np.pad(f, 1, constant_values=np.nan)
        mask = np.ones(f.shape, dtype=bool)
        for di in (-1, 0, 1):
            for dj in (-1, 0, 1):
                if di == 0 and dj == 0:
                    continue
                nbr = pad[1+di:f.shape[0]+1+di, 1+dj:f.shape[1]+1+dj]
                mask &= (f >= nbr) if mode == "max" else (f <= nbr)
        mask &= np.isfinite(f)
    yi, xi = np.where(mask)
    return {"xi": xi.astype(int), "yi": yi.astype(int), "val": f[yi, xi]}


def _filter_extrema(ext, threshold, compare_op, max_n, boco, pct_lat, pct_lon, dxi):
    """
    Filter local extrema: apply threshold, boundary exclusion, and proximity
    thinning.  Returns dict with arrays xi, yi, val (max_n entries max).
    """
    xi, yi, val = ext["xi"].copy(), ext["yi"].copy(), ext["val"].copy()
    keep = np.where(compare_op(val, threshold))[0]
    if len(keep) == 0:
        return {"xi": np.array([], int), "yi": np.array([], int), "val": np.array([])}
    xi, yi, val = xi[keep], yi[keep], val[keep]
    # Sort: highs high-first, lows low-first
    order = np.argsort(-val) if compare_op(np.array([2.0]), 1.0)[0] else np.argsort(val)
    xi, yi, val = xi[order], yi[order], val[order]
    return {"xi": xi[:max_n], "yi": yi[:max_n], "val": val[:max_n]}


def _draw_hl_markers(ax, field2d, lat, lon, boco, pct_lat, pct_lon, max_h, max_l,
                     dx, sc_domain, tr):
    """
    Find MSLP local H/L centres and annotate the map axes.
    Replicates NCL Overlay #5 logic.
    """
    if field2d is None or not np.any(np.isfinite(field2d)):
        return
    dxi = max(1, int(math.floor(2.0 / (dx if dx > 0 else 1.0))))
    ratio = pct_lon / pct_lat if pct_lat else 1.0
    hl_pct = 5.0 if ratio > 1.5 else (3.0 if ratio >= 0.5 else 2.0)
    pe_white = [pe.withStroke(linewidth=3, foreground="white")]

    def _mark_group(color, letter, xi_arr, yi_arr, val_arr):
        # Boundary + proximity filter
        good = np.ones(len(xi_arr), bool)
        for m in range(len(xi_arr)):
            if not good[m]:
                continue
            lx, ly = float(lon[xi_arr[m]]), float(lat[yi_arr[m]])
            if (lx < boco[2] + 3*pct_lon or lx > boco[3] - 3*pct_lon or
                    ly > boco[0] - 5*pct_lat or ly < boco[1] + 3*pct_lat):
                good[m] = False
                continue
            for n in range(m+1, len(xi_arr)):
                if good[n] and abs(xi_arr[n]-xi_arr[m]) <= dxi and abs(yi_arr[n]-yi_arr[m]) <= dxi:
                    good[n] = False
        for m in range(len(xi_arr)):
            if not good[m]:
                continue
            lx, ly = float(lon[xi_arr[m]]), float(lat[yi_arr[m]])
            ax.text(lx, ly, letter, color=color, fontsize=10, fontweight="bold",
                    ha="center", va="center", transform=tr, path_effects=pe_white)
            ax.text(lx, ly + hl_pct*pct_lat, f"{int(val_arr[m])}", color=color,
                    fontsize=7, ha="center", va="center", transform=tr,
                    path_effects=pe_white)

    if not sc_domain:
        highs = _local_extrema(field2d, "max")
        filt_h = _filter_extrema(highs, 1012.0, lambda v, t: v >= t, max_h,
                                 boco, pct_lat, pct_lon, dxi)
        _mark_group("blue", "H", filt_h["xi"], filt_h["yi"], filt_h["val"])

    lows = _local_extrema(field2d, "min")
    filt_l = _filter_extrema(lows, 1012.0, lambda v, t: v <= t, max_l,
                             boco, pct_lat, pct_lon, dxi)
    _mark_group("red", "L", filt_l["xi"], filt_l["yi"], filt_l["val"])


def _update_plotted_files(plotted_files, ifile, natcf_now, all_atcf):
    """Replicate 'sed -i ... && echo ... && sort -u ...' pattern."""
    ifile_base = os.path.basename(ifile)
    lines = []
    if os.path.exists(plotted_files):
        with open(plotted_files) as fh:
            lines = [ln for ln in fh.readlines() if ifile_base not in ln]
    lines.append(f"{ifile} {natcf_now} {all_atcf}\n")
    lines = sorted(set(lines))
    with open(plotted_files, "w") as fh:
        fh.writelines(lines)


def _lock_file(lock_path, timeout=180):
    """Acquire a file lock (lockfile -r-1 -l 180 equivalent)."""
    t0 = time.time()
    while time.time() - t0 < timeout:
        try:
            fd = os.open(lock_path, os.O_CREAT | os.O_EXCL | os.O_WRONLY)
            os.close(fd)
            return True
        except FileExistsError:
            time.sleep(1)
    return False


def _unlock_file(lock_path):
    try:
        os.remove(lock_path)
    except OSError:
        pass


def _post_process_image(ofile, do_rmwhite, do_convertgif):
    """Trim whitespace and/or convert PNG to GIF using ImageMagick."""
    png = ofile + ".png"
    for _ in range(30):
        if os.path.exists(png):
            break
        time.sleep(1)
    if not os.path.exists(png):
        print(f"WARNING: Output file not found: {png}")
        return
    gif = ofile + ".gif"
    if do_rmwhite and do_convertgif:
        subprocess.run(["convert", "-trim", png, "+repage", f"gif:{gif}"], check=False)
        try:
            os.remove(png)
        except OSError:
            pass
    elif do_rmwhite:
        subprocess.run(["convert", "-trim", png, png], check=False)
    elif do_convertgif:
        subprocess.run(["convert", png, "+repage", f"gif:{gif}"], check=False)
        try:
            os.remove(png)
        except OSError:
            pass


def _format_valid_date(idate_clean, fhr):
    """Build a 'valid at' date string from init date + forecast hour."""
    try:
        dt_init = datetime.datetime.strptime(idate_clean, "%Y%m%d%H")
        dt_valid = dt_init + datetime.timedelta(hours=int(fhr))
        return dt_valid.strftime("%HZ %a, %b %d %Y")
    except Exception:
        return "unknown"


def _find_var_nml(gplot_dir, expt, domain, tier, var_nml_env=None):
    """
    Replicate the VAR_NML search logic from the NCL script.
    Returns path to graphics namelist file or None.
    """
    parm = os.path.join(gplot_dir, "parm")
    candidates = []
    if var_nml_env:
        for p in [os.path.join(parm, var_nml_env), var_nml_env]:
            if os.path.exists(p):
                return p
    for fname in [
        f"namelist.maps.{expt}.{domain}.{tier}",
        f"namelist.maps.{expt}.{tier}",
        f"namelist.maps.{domain}.{tier}",
        f"namelist.maps.default.{tier}",
        "namelist.maps.default",
    ]:
        p = os.path.join(parm, fname)
        if os.path.exists(p):
            return p
    return None


def _read_gfx_namelist(var_nml_path):
    """
    Parse a whitespace-separated graphics namelist into lists of tuples.
    Returns: G_BASE, G_OV1, G_OV2, G_OV25, G_OV3, G_OV4, G_OV5, FNAME, What2Plot
    Each G_* is a list of (var, lev) tuples; FNAME is a list of strings.
    """
    with open(var_nml_path) as fh:
        raw_lines = fh.readlines()

    # Keep only non-blank lines (namelist files often have trailing blanks)
    lines = [l for l in raw_lines if l.strip()]

    # Split on any whitespace (spaces or tabs), matching NCL str_split behavior
    headers = lines[0].split()

    def _col(colname):
        try:
            idx = headers.index(colname)
        except ValueError:
            return [""] * (len(lines) - 1)
        out = []
        for i in range(1, len(lines)):
            fields = lines[i].split()
            if idx < len(fields):
                out.append(fields[idx].strip())
            else:
                out.append("")
        return out

    def _clean(lst):
        return ["" if v in ("N/A", "NA", "") else v for v in lst]

    plot_on = [v.strip() in ("True", "TRUE", "1", "true") for v in _col("PLOT_ON")]
    n = len(plot_on)

    base_var = _clean(_col("BASE_CN_FILL"))
    base_lev = _clean(_col("LEV1"))
    ov1_var = _clean(_col("OV_VC_WIND"))
    ov1_lev = _clean(_col("LEV4"))
    ov2_var = _clean(_col("OV_STLINE"))
    ov2_lev = _clean(_col("LEV6"))
    ov25_var = _clean(_col("OV_STLINE2"))
    ov25_lev = _clean(_col("LEV7"))
    ov3_var = _clean(_col("OV_CN_LINE2"))
    ov3_lev = _clean(_col("LEV3"))
    ov4_var = _clean(_col("OV_CN_LINE"))
    ov4_lev = _clean(_col("LEV2"))
    ov5_var = _clean(_col("OV_MAX_MIN"))
    ov5_lev = _clean(_col("LEV5"))
    fname = _clean(_col("FILE_NAME"))

    G_BASE = list(zip(base_var, base_lev))
    G_OV1 = list(zip(ov1_var, ov1_lev))
    G_OV2 = list(zip(ov2_var, ov2_lev))
    G_OV25 = list(zip(ov25_var, ov25_lev))
    G_OV3 = list(zip(ov3_var, ov3_lev))
    G_OV4 = list(zip(ov4_var, ov4_lev))
    G_OV5 = list(zip(ov5_var, ov5_lev))

    return G_BASE, G_OV1, G_OV2, G_OV25, G_OV3, G_OV4, G_OV5, fname, plot_on


def _get_dim_info(ds, dsource, dnames):
    """
    Scan dimension names in *dnames* and return a dict with keys
    'lat_name', 'lon_name', 'lev_name', 'lev_type' (None/isobar/height).
    """
    info = {"lat_name": None, "lon_name": None,
            "lev_name": None, "lev_type": None}
    for d in dnames:
        dl = d.lower()
        if "lat" in dl or "grid_yt" in dl:
            info["lat_name"] = d
        elif "lon" in dl or "grid_xt" in dl:
            info["lon_name"] = d
        elif any(k in dl for k in ("isbl", "pres")):
            info["lev_name"] = d
            info["lev_type"] = "isobar"
        elif "lev" in dl or dl.startswith("lv"):
            info["lev_name"] = d
            info["lev_type"] = "isobar"
        elif "htgl" in dl:
            info["lev_name"] = d
            info["lev_type"] = "height"
    return info




# ══════════════════════════════════════════════════════════════════════════════
# Main driver
# ══════════════════════════════════════════════════════════════════════════════

def main():  # noqa: C901

    print(f"MSG: GPLOT_maps.py started at {datetime.datetime.now()}")
    print("MSG: Welcome to GPLOT, Maps Module.")
    print("MSG: The Maps Module produces graphical products for particular domains.")
    print("")

    # ── Part I: Physical constants ─────────────────────────────────────────
    consts = gu.load_constants()
    ms2kts = getattr(consts, "ms2kts", _MS2KTS)

    # ── Part II: Environment variables ────────────────────────────────────
    GPLOT_DIR = os.environ.get("GPLOT_DIR", "")
    if not GPLOT_DIR:
        print("ERROR: $GPLOT_DIR must be set as an environmental variable.")
        sys.exit(1)
    print(f"MSG: Found this GPLOT location: {GPLOT_DIR}")

    USER = os.environ.get("USER", "")
    HOME = os.environ.get("HOME", "")
    ENSID = os.environ.get("ENSID", "")
    verbose_str = os.environ.get("verbose", "0")
    try:
        verbose = int(verbose_str)
    except ValueError:
        verbose = 0
    print(f"MSG: Verbose level = {verbose}")

    # ── Part III: Master Namelist ──────────────────────────────────────────
    MASTER_NML_IN = os.environ.get("MASTER_NML_IN", "")
    default_nml = os.path.join(GPLOT_DIR, "nmlist", "namelist.master.default")

    if MASTER_NML_IN:
        for candidate in [MASTER_NML_IN,
                          os.path.join(GPLOT_DIR, "nmlist", MASTER_NML_IN)]:
            if os.path.exists(candidate):
                MASTER_NML = candidate
                break
        else:
            print(f"ERROR: Can't find master namelist --> {MASTER_NML_IN}")
            sys.exit(1)
    elif os.path.exists(default_nml):
        MASTER_NML = default_nml
    else:
        print(f"ERROR: Can't find master namelist --> {default_nml}")
        sys.exit(1)

    print(f"MSG: Using this master namelist:  {MASTER_NML}")
    NML = gm.read_master_namelist(MASTER_NML)
    if NML is None:
        print(f"ERROR: Failed to parse master namelist: {MASTER_NML}")
        sys.exit(1)

    # Allow env-var overrides of each namelist field
    def _nml(key, default=None):
        return os.environ.get(key, getattr(NML, key, default))

    DOMAIN = _nml("DOMAIN", "")
    TIER = _nml("TIER", "1")
    DSOURCE = _nml("DSOURCE", "")
    MACHINE = _nml("MACHINE", "")
    IDATE = _nml("IDATE", "")
    SID = _nml("SID", "NONE")
    ENSID = _nml("ENSID", ENSID) or ""
    EXPT = _nml("EXPT", "")
    IDIR = _nml("IDIR", "")
    ITAG = _nml("ITAG", "")
    EXT = _nml("EXT", "nc")
    ODIR_raw = _nml("ODIR", "")
    ODIR_TYPE = int(_nml("ODIR_TYPE", "2"))
    INIT_HR = int(_nml("INIT_HR", "0"))
    FNL_HR = int(_nml("FNL_HR", "126"))
    FMT_HR = _nml("FMT_HR", "%03i")
    DT = int(_nml("DT", "6"))
    IS_MSTORM = str(_nml("IS_MSTORM", "False")).lower() in ("true", "1", "yes")
    DO_RMWHITE = str(_nml("DO_RMWHITE", "True")).lower() in ("true", "1", "yes")
    DO_SRCLBL = str(_nml("DO_SRCLBL", "True")).lower() in ("true", "1", "yes")
    PIV = float(_nml("MAP_PIV", "0"))
    DO_CONVERTGIF = str(_nml("DO_CONVERTGIF", "True")).lower() in ("true", "1", "yes")
    NMAX = int(_nml("MAP_NMAX", "100"))
    ATCF_REQD = str(_nml("ATCF_REQD", "False")).lower() in ("true", "1", "yes")
    ATCF1_DIR = _nml("ATCF1_DIR", "NONE") or "NONE"
    BDECK_DIR = _nml("BDECK_DIR", "NONE") or "NONE"
    FORCE = str(_nml("FORCE", "False")).lower() in ("true", "1", "yes")
    hr_offset = os.environ.get("hr_offset", "")

    # Correct IDATE for GFS (strip "gfs." prefix)
    if DSOURCE == "GFS":
        IDATE = IDATE.replace("gfs.", "")

    # Domain override if DSOURCE contains "d03"
    if "d03" in DSOURCE:
        DOMAIN = "d03"

    # Build ODIR based on ODIR_TYPE
    if ODIR_TYPE == 1:
        ODIR = os.path.normpath(os.path.join(ODIR_raw, DOMAIN)) + "/"
    else:
        ODIR = os.path.normpath(os.path.join(
            ODIR_raw, EXPT, ENSID or "", IDATE, DOMAIN)) + "/"
    ODIR = ODIR.replace("//", "/")
    print(f"MSG: I will place graphics in this location --> {ODIR}")

    # ── Part IV: Print settings ────────────────────────────────────────────
    print("MSG: Running GPLOT with these settings:")
    print(f"MSG:   Multi-Storm:        {'ON' if IS_MSTORM else 'OFF'}")
    print(f"MSG:   Experiment Name:    {EXPT}")
    print(f"MSG:   Data Source:        {DSOURCE}")
    print(f"MSG:   Forecast Cycle:     {IDATE}")
    print(f"MSG:   Storm ID:           {SID}")
    print(f"MSG:   Domain/Region:      {DOMAIN}")
    print(f"MSG:   Graphics Tier:      {TIER}")
    if ENSID:
        print(f"MSG:   Ensemble Member:    {ENSID}")
    print(f"MSG:   Input Directory:    {IDIR}")
    print(f"MSG:   Output Directory:   {ODIR}")
    print(f"MSG:   System Environment: {MACHINE}")
    if isinstance(ITAG, (list, tuple)):
        print(f"MSG:   Input File Tag(s):  {','.join(str(t) for t in ITAG)}")
    else:
        print(f"MSG:   Input File Tag(s):  {ITAG}")
    print(f"MSG:   Initial/Final Hour: {INIT_HR}/{FNL_HR}")
    print(f"MSG:   Hour Format:        {FMT_HR}")
    print(f"MSG:   Time Step [h]:      {DT}")
    print(f"MSG:   ATCF Required:      {'ON' if ATCF_REQD else 'OFF'}")
    print(f"MSG:   Trim graphics:      {'ON' if DO_RMWHITE else 'OFF'}")
    print(f"MSG:   Source Labels:      {'ON' if DO_SRCLBL else 'OFF'}")
    print(f"MSG:   Convert to GIF:     {'ON' if DO_CONVERTGIF else 'OFF'}")

    # ── Part V: Set variables ──────────────────────────────────────────────
    os.makedirs(ODIR, exist_ok=True)

    # Domain bounds
    BOCO = list(gu.get_dmn_bds(DOMAIN, DSOURCE))   # [latN, latS, lonW, lonE]

    # Storm-centred configuration flags
    NSTDMN = ["d03", "alld03", "storm", "core"]
    SC_DOMAIN = DOMAIN in ["d02", "d03", "tkfull", "hwrf", "alld03",
                           "storm", "core", "tcparent"]
    SC_DSOURCE = False   # only set explicitly in NCL for some HWRF configs
    SC_GRAPHICS = SC_DOMAIN or SC_DSOURCE
    if SC_GRAPHICS:
        print("MSG: Determined that these graphics are storm-centered.")
        if SID == "NONE" and ATCF_REQD:
            print("ERROR: No storms identified, but ATCF is required.")
            sys.exit(1)

    ONE_FILE = "fvgfs" in DSOURCE.lower()
    if ONE_FILE and DOMAIN == "d03":
        DT = 6

    SIDtag = ("." + SID.upper()) if SC_GRAPHICS else ""
    OUT_OF_BDS = False

    # ── Part VI: Read Graphics Namelist ────────────────────────────────────
    VAR_NML_ENV = os.environ.get("VAR_NML", "")
    VAR_NML = _find_var_nml(GPLOT_DIR, EXPT, DOMAIN, TIER,
                            VAR_NML_ENV if VAR_NML_ENV else None)
    if VAR_NML is None:
        print("ERROR: Graphics namelist not available. Something went wrong.")
        sys.exit(1)
    print(f"MSG: Using this graphics namelist: {VAR_NML}")

    (G_BASE, G_OV1, G_OV2, G_OV25,
     G_OV3, G_OV4, G_OV5,
     FNAME_all, What2Plot) = _read_gfx_namelist(VAR_NML)

    # Apply GFS / NSTDMN exclusions matching NCL logic
    for i, fname_i in enumerate(FNAME_all):
        if DSOURCE == "GFS" and fname_i in ("REFL_MSLP", "REFL_UV750"):
            What2Plot[i] = False
        if DOMAIN in NSTDMN and fname_i in ("PRCP_MSLP_Z1000500", "PRCP_MSLP"):
            What2Plot[i] = False

    if not any(What2Plot):
        print("ERROR: No graphics turned on in this namelist. Exiting.")
        sys.exit(1)

    # Subset to active plots only
    active_idx = [i for i, on in enumerate(What2Plot) if on]
    G_BASE2  = [G_BASE[i]  for i in active_idx]
    G_OV1_2  = [G_OV1[i]   for i in active_idx]
    G_OV2_2  = [G_OV2[i]   for i in active_idx]
    G_OV25_2 = [G_OV25[i]  for i in active_idx]
    G_OV3_2  = [G_OV3[i]   for i in active_idx]
    G_OV4_2  = [G_OV4[i]   for i in active_idx]
    G_OV5_2  = [G_OV5[i]   for i in active_idx]
    FNAME2   = [FNAME_all[i] for i in active_idx]
    nPlots   = len(FNAME2)
    print(f"MSG: Will produce the following graphics:")
    print(f"MSG: {','.join(FNAME2)}")

    # ── Part VII: Get input files ──────────────────────────────────────────
    print("MSG: Get the list of input files for graphical production.")
    IFILES_LIST = os.path.join(ODIR, f"UnplottedFiles.{DOMAIN}.{TIER}{SIDtag}.log")
    print(f"IFILES_LIST={IFILES_LIST}")
    if not os.path.exists(IFILES_LIST):
        print("ERROR: No files listed for graphical production.")
        sys.exit(1)

    with open(IFILES_LIST) as fh:
        iFiles = [ln.rstrip("\n") for ln in fh if ln.strip()]
    if not iFiles:
        print("ERROR: No input files found. Something went wrong.")
        sys.exit(1)

    nFiles = len(iFiles)
    # Check which files are "final" (not modified in last 10 min)
    FNL_IFILES = []
    for f_path in iFiles:
        result = subprocess.run(
            ["find", f_path, "-mmin", "+10"], capture_output=True, text=True)
        FNL_IFILES.append(bool(result.stdout.strip()))

    # Forecast hour array and PlottedFiles path
    ALL_FHR_FILE = os.path.join(ODIR, f"AllForecastHours.{DOMAIN}.dat")
    STATUS_FILE = os.path.join(ODIR, f"GPlot.{DOMAIN}.status")
    PLOTTED_FILES = os.path.join(ODIR, f"PlottedFiles.{DOMAIN}.dat")

    FHR = list(range(INIT_HR, FNL_HR + 1, DT))
    if os.path.exists(ALL_FHR_FILE):
        try:
            with open(ALL_FHR_FILE) as fh:
                fhr_data = [int(ln.strip()) for ln in fh if ln.strip()]
            if fhr_data:
                FHR = fhr_data
        except Exception:
            pass

    # ── Part VIII: ATCF reading loop ───────────────────────────────────────
    ATCF_FILES = [ATCF1_DIR]
    ALL_SID = []
    ALL_LONGSID = []
    ALL_SNUM = []
    FNL_ATCF = []
    tcLats_list = []
    tcLons_list = []
    tcFHR_list = []
    tcINT_list = []
    NATCF = 0
    ALL_ATCF = False

    if SID != "NONE":
        ALL_SID = SID.split(",") if "," in SID else [SID]
        ALL_LONGSID = ALL_SID[:]
        ALL_SNUM = [s[2:4] if len(s) >= 4 else "00" for s in ALL_SID]

    for sss, sid_s in enumerate(ALL_SID):
        atcf_ns = None
        if ATCF1_DIR != "NONE" and ATCF1_DIR:
            # Search for ATCF file
            atcf_candidates = glob.glob(
                os.path.join(ATCF1_DIR, f"*{sid_s.lower()}*")) + \
                glob.glob(os.path.join(ATCF1_DIR, f"*{sid_s.upper()}*"))
            if atcf_candidates:
                atcf_file = atcf_candidates[0]
                try:
                    atcf_ns = gf.adeck_read(atcf_file, None, SimpleNamespace(
                        dsource=DSOURCE, idate=IDATE, sid=sid_s))
                except Exception as e:
                    print(f"WARNING: Could not read ATCF for {sid_s}: {e}")

        if atcf_ns is not None and hasattr(atcf_ns, "lat"):
            tcLats_list.append(np.asarray(atcf_ns.lat, float))
            tcLons_list.append(np.asarray(atcf_ns.lon, float))
            tcFHR_list.append(np.asarray(atcf_ns.lead, float))
            tcINT_list.append(np.asarray(getattr(atcf_ns, "vmax",
                              np.full_like(atcf_ns.lat, np.nan)), float))
            FNL_ATCF.append(True)
            NATCF += 1
        else:
            # Missing ATCF: fill with NaN arrays
            n_fhr = len(FHR)
            tcLats_list.append(np.full(n_fhr, np.nan))
            tcLons_list.append(np.full(n_fhr, np.nan))
            tcFHR_list.append(np.array(FHR, float))
            tcINT_list.append(np.full(n_fhr, np.nan))
            FNL_ATCF.append(False)
            NATCF += 1

    ALL_ATCF = all(FNL_ATCF)
    LONGSID_LIST = ",".join(ALL_LONGSID) if ALL_LONGSID else ""

    # ── Part IX: Main file loop ────────────────────────────────────────────
    DO_PLOT = True
    SC_FHR = True
    FILE_DONE = [False] * nFiles

    for fff in range(nFiles):
        # i. Find the FHR for this file
        # Extract FHR from filename (look for "f###" pattern)
        import re
        m = re.search(r"f(\d+)", os.path.basename(iFiles[fff]))
        if m:
            file_fhr = int(m.group(1))
        else:
            print(f"WARNING: Could not determine FHR for {iFiles[fff]}. Skipping.")
            continue

        fff_fhr_idx = None
        for idx, fh in enumerate(FHR):
            if fh == file_fhr:
                fff_fhr_idx = idx
                break
        if fff_fhr_idx is None:
            if verbose >= 1:
                print(f"MSG: FHR {file_fhr} not in expected FHR list. Skipping.")
            continue

        # ii. Open the primary input file
        try:
            f1 = _open_dataset(iFiles[fff])
        except Exception as e:
            print(f"WARNING: Cannot open {iFiles[fff]}: {e}")
            continue

        # iii. Look for additional input files (f2, f3, f4) using ITAG variations
        itag_list = ITAG if isinstance(ITAG, list) else [ITAG]
        open_files = [f1, None, None, None]
        for ttt_i, tag in enumerate(itag_list[1:4], 1):
            alt = iFiles[fff].replace(itag_list[0], tag) if itag_list else ""
            if alt and os.path.exists(alt):
                try:
                    open_files[ttt_i] = _open_dataset(alt)
                except Exception:
                    pass
        datasets = [ds for ds in open_files if ds is not None]

        # iv. Get dimension names from the primary file
        f = f1
        dims_in_file = list(f.dims.keys())

        # v. Determine number of time steps
        time_dims = [d for d in dims_in_file if "time" in d.lower() or d == "t"]
        if time_dims:
            n_times = f.dims[time_dims[0]]
        else:
            n_times = 1

        # vi. Update BOCO for HWRF domains from actual data
        BOCO_now = list(BOCO)  # mutable copy per file
        do_pivot = PIV != 0
        doPivotLon = do_pivot
        pivot_lon = PIV

        if DOMAIN in ("d01", "hwrf"):
            # Read MSLP to detect actual outer domain bounds
            mslp_vname = gu.find_var_name(DSOURCE, "MSLP", "")
            if mslp_vname in f:
                TEST = f[mslp_vname].values
                if TEST.ndim == 3:
                    TEST = TEST[0]
                TEST = np.where(np.isfinite(TEST), TEST, np.nan)
                for dname in dims_in_file:
                    dl = dname.lower()
                    if "lat" in dl or "grid_yt" in dl:
                        lat_vals = f[dname].values
                        lat_m = np.where(np.isfinite(TEST),
                                         np.broadcast_to(lat_vals[:, None] if lat_vals.ndim == 1
                                                         else lat_vals, TEST.shape), np.nan)
                        BOCO_now[0] = float(np.nanmax(lat_m))
                        BOCO_now[1] = float(np.nanmin(lat_m))
                    if "lon" in dl or "grid_xt" in dl:
                        lon_vals = f[dname].values
                        lon_m = np.where(np.isfinite(TEST),
                                         np.broadcast_to(lon_vals[None, :] if lon_vals.ndim == 1
                                                         else lon_vals, TEST.shape), np.nan)
                        BOCO_now[2] = float(np.nanmin(lon_m))
                        BOCO_now[3] = float(np.nanmax(lon_m))
                        if doPivotLon:
                            pivot_lon = math.ceil(BOCO_now[3]) + 1

        # vii. Get domain info
        B = gu.get_dmn_info(DOMAIN, DSOURCE, "bdstype", False)
        if SC_GRAPHICS and not ATCF_REQD and SID == "NONE":
            B = 1

        # viii. Ensure BOCO is correctly ordered
        if BOCO_now[3] < BOCO_now[2]:
            BOCO_now[2], BOCO_now[3] = BOCO_now[3], BOCO_now[2]
        if BOCO_now[0] < BOCO_now[1]:
            BOCO_now[0], BOCO_now[1] = BOCO_now[1], BOCO_now[0]
        if not doPivotLon:
            pivot_lon = PIV

        # Inner time loop
        UV10max = None
        MSLPmin = None
        MSLP_MARKERS = False

        for ttt in range(n_times):
            FHR3_now = file_fhr   # same FHR for all time steps in this file
            idate_clean = IDATE.replace("gfs.", "")
            valid_date = _format_valid_date(idate_clean, FHR3_now + ttt * DT
                                            if n_times > 1 else FHR3_now)

            # ix. Get TC lat/lon for this time step
            ti = fff_fhr_idx  # index into TC track arrays
            tc_lat_now = np.full(NATCF, np.nan)
            tc_lon_now = np.full(NATCF, np.nan)
            for sss in range(NATCF):
                fhr_arr = tcFHR_list[sss]
                lat_arr = tcLats_list[sss]
                lon_arr = tcLons_list[sss]
                match_idx = np.where(fhr_arr == FHR3_now)[0]
                if len(match_idx) > 0 and not np.isnan(lat_arr[match_idx[0]]):
                    tc_lat_now[sss] = lat_arr[match_idx[0]]
                    tc_lon_now[sss] = lon_arr[match_idx[0]]

            # x. Read lat/lon/lev dimensions
            dim_info = _get_dim_info(f, DSOURCE, dims_in_file)
            lat_arr = None
            lon_arr = None
            lev_ns = None
            flipFlag = False
            mf = 1.0

            if dim_info["lat_name"] and dim_info["lat_name"] in f:
                lat_raw = f[dim_info["lat_name"]].values.astype(float)
                # Call gf.get_dim_lat to subset to BOCO
                lat_ns = gf.get_dim_lat(f, dim_info["lat_name"], BOCO_now,
                                        tc_lat_now, tc_lon_now, ti, B)
                if lat_ns is not None and not np.isnan(lat_ns.lat[0]):
                    lat_arr = lat_ns.lat
                    BOCO_now = list(lat_ns.BOCO)
                else:
                    print("WARNING: Model domain and graphic domain do not overlap. Nothing to do.")
                    OUT_OF_BDS = True

            if dim_info["lon_name"] and dim_info["lon_name"] in f and not OUT_OF_BDS:
                if not doPivotLon:
                    BOCO_now_with_piv = list(BOCO_now)
                else:
                    BOCO_now_with_piv = list(BOCO_now)
                    # attach pivot as SimpleNamespace attribute equivalent
                lon_ns_tc = tc_lon_now if SC_DOMAIN else [_FVAL] * NATCF
                lon_ns_tc_lat = tc_lat_now if SC_DOMAIN else [_FVAL] * NATCF
                # Use SimpleNamespace to pass BOCO with pivot
                boco_ns = SimpleNamespace(
                    north=BOCO_now[0], south=BOCO_now[1],
                    west=BOCO_now[2], east=BOCO_now[3],
                    PivotLon=pivot_lon, doPivot=doPivotLon)
                lon_ns = gf.get_dim_lon(f, dim_info["lon_name"], BOCO_now,
                                        lon_ns_tc, lon_ns_tc_lat, ti, B,
                                        piv_lon=pivot_lon,
                                        do_pivot=doPivotLon)
                if lon_ns is not None and not np.isnan(lon_ns.lon[0]):
                    lon_arr = lon_ns.lon
                    flipFlag = getattr(lon_ns, "flipFlag", False)
                    BOCO_now = list(lon_ns.BOCO)
                else:
                    print("WARNING: Model domain and graphic domain do not overlap. Nothing to do.")
                    OUT_OF_BDS = True

            if dim_info["lev_name"] and not OUT_OF_BDS:
                lev_ns = _lev_ns_from_ds(f, dim_info["lev_name"])
                mf = lev_ns.mf

            if OUT_OF_BDS:
                # Mark file processed and move on
                NATCF_NOW_oob = sum(
                    1 for sss in range(NATCF)
                    if not np.isnan(tc_lat_now[sss]))
                _update_plotted_files(PLOTTED_FILES, iFiles[fff],
                                      NATCF_NOW_oob, ALL_ATCF)
                break

            if lat_arr is None or lon_arr is None:
                print("WARNING: Dimension arrays could not be read. Skipping.")
                continue

            mf = mf if mf else 1.0
            pct_lat = abs(BOCO_now[0] - BOCO_now[1]) / 100.0
            pct_lon = abs(BOCO_now[3] - BOCO_now[2]) / 100.0

            # Compute BOCO2 (lat/lon actual extent)
            BOCO2 = [float(np.nanmax(lat_arr)), float(np.nanmin(lat_arr)),
                     float(np.nanmin(lon_arr)), float(np.nanmax(lon_arr))]

            # myFlags dict
            myFlags = {"flipFlag": flipFlag, "rmVortex": False,
                       "icen": (BOCO2[2] + BOCO2[3]) / 2.0,
                       "jcen": (BOCO2[0] + BOCO2[1]) / 2.0,
                       "radius": max(abs(BOCO2[0] - (BOCO2[0]+BOCO2[1])/2),
                                     abs(BOCO2[2] - (BOCO2[2]+BOCO2[3])/2)),
                       "N": 11}

            if verbose >= 1:
                print(f"MSG: BOCO = {BOCO_now}")
                print(f"MSG: Dimensions have been read: {dims_in_file}")

            # xi. Get UV10max and MSLPmin for storm-centred domains
            lonF = lon_arr
            if DOMAIN in NSTDMN and UV10max is None:
                ds_uv, vname_uv = _find_ds_with_var(datasets, DSOURCE, "UV", "10")
                if ds_uv is None:
                    print("WARNING: Variable UV10 not found. Setting as missing.")
                    UV10max = np.nan
                else:
                    try:
                        uv10_ns = gf.get_var2d(
                            ds_uv, DSOURCE,
                            ("UV", "10", str(ttt)),
                            list(ds_uv.dims.keys()),
                            BOCO2, 1.0, myFlags, lonF)
                        UV10max = float(np.nanmax(uv10_ns.data)) if uv10_ns else np.nan
                    except Exception:
                        UV10max = np.nan

            if DOMAIN in NSTDMN and MSLPmin is None:
                ds_ms, vname_ms = _find_ds_with_var(datasets, DSOURCE, "MSLP", "")
                if ds_ms is None:
                    print("WARNING: Variable MSLP not found. Setting as missing.")
                    MSLPmin = np.nan
                else:
                    try:
                        mslp_ns = gf.get_var2d(
                            ds_ms, DSOURCE,
                            ("MSLP", "", str(ttt)),
                            list(ds_ms.dims.keys()),
                            BOCO2, 1.0, myFlags, lonF)
                        MSLPmin = float(np.nanmin(mslp_ns.data)) if mslp_ns else np.nan
                    except Exception:
                        MSLPmin = np.nan

            # ── Per-plot loop ────────────────────────────────────────────
            for ppp in range(nPlots):
                base_var, base_lev = G_BASE2[ppp]
                ov1_var, ov1_lev = G_OV1_2[ppp]
                ov2_var, ov2_lev = G_OV2_2[ppp]
                ov25_var, ov25_lev = G_OV25_2[ppp]
                ov3_var, ov3_lev = G_OV3_2[ppp]
                ov4_var, ov4_lev = G_OV4_2[ppp]
                ov5_var, ov5_lev = G_OV5_2[ppp]

                is_xsect = ("x" in base_lev or "y" in base_lev) if base_lev else False

                # Build output file path
                if SC_GRAPHICS:
                    ofile = os.path.join(
                        ODIR,
                        f"{FNAME2[ppp]}.{IDATE}.{DOMAIN}"
                        f".f{FHR3_now:03d}")
                else:
                    ofile = os.path.join(
                        ODIR,
                        f"{FNAME2[ppp]}.{IDATE}.{DOMAIN}"
                        f".f{FHR3_now:03d}")

                # Find file containing base variable
                ds_base, _ = _find_ds_with_var(datasets, DSOURCE, base_var, base_lev)
                if ds_base is None:
                    print(f"WARNING: Base variable {base_var} not found. Skipping plot {FNAME2[ppp]}.")
                    continue

                # Read base variable
                try:
                    if is_xsect:
                        Vbase_ns = gf.get_var_xc(
                            ds_base, DSOURCE,
                            (base_var, base_lev),
                            list(ds_base.dims.keys()),
                            BOCO_now, mf, myFlags, lonF)
                    else:
                        Vbase_ns = gf.get_var2d(
                            ds_base, DSOURCE,
                            (base_var, base_lev, str(ttt)),
                            list(ds_base.dims.keys()),
                            BOCO2, mf, myFlags, lonF)
                except Exception as e:
                    print(f"WARNING: Could not read {base_var}: {e}")
                    continue

                if Vbase_ns is None:
                    print(f"WARNING: {base_var} returned None. Skipping {FNAME2[ppp]}.")
                    continue

                Vbase = Vbase_ns.data if hasattr(Vbase_ns, "data") else np.array(Vbase_ns)

                # Handle bulk PRCP (HWRF/HAFS/GFS)
                if base_var == "PRCP" and DSOURCE in ("HWRF", "HAFS", "GFS"):
                    if ONE_FILE:
                        print("WARNING: Bulk precip rate not yet supported for ONE_FILE. Skipping...")
                        continue
                    if FHR3_now == 0:
                        print("WARNING: Can't compute bulk precip rate at 0-h. Skipping...")
                        continue
                    hr_off = int(hr_offset) if hr_offset else 3
                    if FHR3_now < hr_off:
                        print(f"WARNING: Can't compute bulk precip at {FHR3_now}-h. Skipping...")
                        continue
                    FHR4 = FHR3_now - hr_off
                    fmt_hr_py = FMT_HR.replace("%0", "%0").replace("i", "d")
                    FHRB = f"f{FHR4:{fmt_hr_py.lstrip('%')}}."
                    ifile_b = iFiles[fff].replace(
                        f"f{FHR3_now:{fmt_hr_py.lstrip('%')}}.", FHRB)
                    if not os.path.exists(ifile_b):
                        print(f"WARNING: Previous precipitation file not found. Skipping...")
                        continue
                    if FHR4 > 0:
                        try:
                            f_prev = _open_dataset(ifile_b)
                            g_base2_lev = ""
                            if DSOURCE == "GFS":
                                g_base2_lev = f"{FHR4}h" if FHR4 >= 9 else ""
                            elif DSOURCE == "HAFS":
                                g_base2_lev = "3h" if FHR4 >= 6 else ""
                            Vbase2_ns = gf.get_var2d(
                                f_prev, DSOURCE,
                                (base_var, g_base2_lev, str(ttt)),
                                list(f_prev.dims.keys()),
                                BOCO2, mf, myFlags, lonF)
                            if Vbase2_ns is not None:
                                Vbase = (Vbase - Vbase2_ns.data) / float(hr_off)
                            f_prev.close()
                        except Exception as e:
                            print(f"WARNING: PRCP bulk rate computation failed: {e}")

                # Get colormap and levels
                myColormap = gu.define_cmap_name(base_var, base_lev)
                myLevels = gu.define_levels(base_var, base_lev)
                cmap_fill = gu.define_cmap_fill(base_var, base_lev, myColormap)

                if verbose >= 1:
                    print(f"MSG: myColormap = {myColormap}")
                    print(f"MSG: Workstation & resources have been set.")

                # ── Setup figure ──────────────────────────────────────
                if is_xsect:
                    fig, ax = plt.subplots(figsize=(10, 6))
                    tr = None
                else:
                    if HAS_CARTOPY:
                        clon = (BOCO_now[2] + BOCO_now[3]) / 2.0
                        proj = ccrs.PlateCarree(central_longitude=clon)
                        fig, ax = plt.subplots(
                            subplot_kw={"projection": proj}, figsize=(10, 8))
                        ax.set_extent([BOCO_now[2], BOCO_now[3],
                                       BOCO_now[1], BOCO_now[0]],
                                      crs=ccrs.PlateCarree())
                        ax.add_feature(cfeature.COASTLINE, linewidth=0.5)
                        ax.add_feature(cfeature.BORDERS, linewidth=0.3)
                        ax.add_feature(cfeature.STATES, linewidth=0.2,
                                       edgecolor="grey")
                        if DOMAIN in NSTDMN:
                            gl = ax.gridlines(draw_labels=True, linewidth=0.5,
                                              linestyle="--", color="grey",
                                              x_inline=False, y_inline=False)
                        else:
                            gl = ax.gridlines(draw_labels=True, linewidth=0.3,
                                              linestyle=":", color="grey",
                                              x_inline=False, y_inline=False)
                        tr = ccrs.PlateCarree()
                    else:
                        fig, ax = plt.subplots(figsize=(10, 8))
                        tr = None

                # ── Base Plot ─────────────────────────────────────────
                if verbose >= 1:
                    print(f"MSG: Working on base shaded contours (G_BASE={base_var})")

                try:
                    if is_xsect:
                        lev_plot = Vbase_ns.lev if hasattr(Vbase_ns, "lev") else np.arange(Vbase.shape[0])
                        xax = Vbase_ns.lon if hasattr(Vbase_ns, "lon") else lon_arr
                        cf = ax.contourf(xax, lev_plot / mf, Vbase,
                                         levels=myLevels if myLevels is not None else 20,
                                         cmap=myColormap if cmap_fill is None else None,
                                         colors=None, extend="both")
                        ax.invert_yaxis()
                        ax.set_xlabel("Longitude (°E)" if "x" in base_lev else "Latitude (°N)")
                        ax.set_ylabel("Pressure (hPa)")
                    else:
                        plot_kw = dict(
                            levels=myLevels if myLevels is not None else 20,
                            extend="both")
                        if cmap_fill is not None:
                            plot_kw["cmap"] = None
                            # use listed colors
                            import matplotlib.colors as mcolors
                            plot_kw["cmap"] = mcolors.ListedColormap(cmap_fill)
                        else:
                            plot_kw["cmap"] = myColormap
                        if tr:
                            plot_kw["transform"] = tr
                        cf = ax.contourf(lon_arr, lat_arr, Vbase, **plot_kw)
                    plt.colorbar(cf, ax=ax, shrink=0.7, pad=0.02)
                except Exception as e:
                    print(f"WARNING: Base contourf failed for {FNAME2[ppp]}: {e}")
                    plt.close(fig)
                    continue

                if verbose >= 1:
                    print("MSG: Base plot function has finished")

                # ── Overlay #1: Wind Vectors ──────────────────────────
                if ov1_var:
                    if verbose >= 1:
                        print(f"MSG: Working on wind vectors (G_OV1={ov1_var})")
                    ds_ov1, _ = _find_ds_with_var(datasets, DSOURCE, ov1_var, ov1_lev)
                    if ds_ov1 is None:
                        print(f"WARNING: Variable {ov1_var} not found. Skipping wind vectors.")
                    else:
                        ov1_xsect = "x" in ov1_lev if ov1_lev else False
                        ov1_ysect = "y" in ov1_lev if ov1_lev else False
                        ov1_rmv = ov1_lev in ("02500850m", "05000850m", "07000850m")
                        ov1_flags = myFlags if ov1_rmv else {**myFlags, "rmVortex": False}
                        try:
                            if ov1_xsect:
                                UOv1_ns = gf.get_var_xc(ds_ov1, DSOURCE, ("U", ov1_lev),
                                                         list(ds_ov1.dims.keys()), BOCO_now,
                                                         mf, myFlags, lonF)
                                VOv1_ns = gf.get_var_xc(ds_ov1, DSOURCE, ("W", ov1_lev),
                                                         list(ds_ov1.dims.keys()), BOCO_now,
                                                         mf, myFlags, lonF)
                                if UOv1_ns and VOv1_ns:
                                    lev_xc = UOv1_ns.lev / mf
                                    xax_xc = UOv1_ns.lon
                                    ax.quiver(xax_xc, lev_xc, UOv1_ns.data, VOv1_ns.data)
                            elif ov1_ysect:
                                UOv1_ns = gf.get_var_xc(ds_ov1, DSOURCE, ("V", ov1_lev),
                                                         list(ds_ov1.dims.keys()), BOCO_now,
                                                         mf, myFlags, lonF)
                                VOv1_ns = gf.get_var_xc(ds_ov1, DSOURCE, ("W", ov1_lev),
                                                         list(ds_ov1.dims.keys()), BOCO_now,
                                                         mf, myFlags, lonF)
                                if UOv1_ns and VOv1_ns:
                                    lev_xc = UOv1_ns.lev / mf
                                    xax_xc = UOv1_ns.lat
                                    ax.quiver(xax_xc, lev_xc, UOv1_ns.data, VOv1_ns.data)
                            else:
                                UOv1_ns = gf.get_var2d(ds_ov1, DSOURCE,
                                                        ("U", ov1_lev, str(ttt)),
                                                        list(ds_ov1.dims.keys()),
                                                        BOCO2, mf, ov1_flags, lonF)
                                VOv1_ns = gf.get_var2d(ds_ov1, DSOURCE,
                                                        ("V", ov1_lev, str(ttt)),
                                                        list(ds_ov1.dims.keys()),
                                                        BOCO2, mf, ov1_flags, lonF)
                                if UOv1_ns and VOv1_ns:
                                    # Determine barbs vs curly vectors
                                    type_ov1 = ov1_lev[-1] if ov1_lev else ""
                                    thin = 3
                                    if type_ov1 == "c" or FNAME2[ppp] == "UV850_MSLP":
                                        qkw = dict(scale=500)
                                        if tr:
                                            qkw["transform"] = tr
                                        ax.quiver(lon_arr[::thin], lat_arr[::thin],
                                                  UOv1_ns.data[::thin, ::thin],
                                                  VOv1_ns.data[::thin, ::thin], **qkw)
                                    else:
                                        bkw = dict(length=5, barbcolor="darkblue",
                                                   flagcolor="darkblue", linewidth=0.5)
                                        if DOMAIN in NSTDMN:
                                            bkw["linewidth"] = 1.5
                                        if FNAME2[ppp] == "RH700_UV700_MSLP":
                                            bkw["barbcolor"] = "black"
                                            bkw["flagcolor"] = "black"
                                        if tr:
                                            bkw["transform"] = tr
                                        ax.barbs(lon_arr[::thin], lat_arr[::thin],
                                                 UOv1_ns.data[::thin, ::thin],
                                                 VOv1_ns.data[::thin, ::thin], **bkw)
                        except Exception as e:
                            print(f"WARNING: Wind vector overlay failed: {e}")

                # ── Overlay #2A: Bottom Streamlines ───────────────────
                if ov2_var:
                    if verbose >= 1:
                        print(f"MSG: Working on bottom streamlines (G_OV2={ov2_var})")
                    ds_ov2, _ = _find_ds_with_var(datasets, DSOURCE, ov2_var, ov2_lev)
                    if ds_ov2 is None:
                        print(f"WARNING: Variable {ov2_var} not found. Skipping OV2.")
                    else:
                        ov2_rmv = ov2_lev in ("02500850m", "05000850m", "07000850m")
                        ov2_flags = myFlags if ov2_rmv else {**myFlags, "rmVortex": False}
                        try:
                            UOv2_ns = gf.get_var2d(ds_ov2, DSOURCE,
                                                    ("U", ov2_lev, str(ttt)),
                                                    list(ds_ov2.dims.keys()),
                                                    BOCO2, mf, ov2_flags, lonF)
                            VOv2_ns = gf.get_var2d(ds_ov2, DSOURCE,
                                                    ("V", ov2_lev, str(ttt)),
                                                    list(ds_ov2.dims.keys()),
                                                    BOCO2, mf, ov2_flags, lonF)
                            if UOv2_ns and VOv2_ns:
                                zs = gu.get_stm_thin(DSOURCE, DOMAIN)
                                skw = dict(color="dimgray", linewidth=1.0, density=1.5)
                                if FNAME2[ppp] in ("SHDL", "SHML", "SHSL"):
                                    skw["color"] = "black"
                                if DOMAIN in NSTDMN:
                                    skw["color"] = "black"
                                u2, v2 = _sanitize_streamplot_data(
                                    UOv2_ns.data[::zs, ::zs],
                                    VOv2_ns.data[::zs, ::zs])
                                ax.streamplot(lon_arr[::zs], lat_arr[::zs],
                                              u2, v2, **skw)
                        except Exception as e:
                            print(f"WARNING: OV2 streamline failed: {e}")

                # ── Overlay #2B: Top Streamlines ──────────────────────
                if ov25_var:
                    if verbose >= 1:
                        print(f"MSG: Working on top streamlines (G_OV25={ov25_var})")
                    ds_ov25, _ = _find_ds_with_var(datasets, DSOURCE, ov25_var, ov25_lev)
                    if ds_ov25 is None:
                        print(f"WARNING: Variable {ov25_var} not found. Skipping OV25.")
                    else:
                        ov25_rmv = ov25_lev in ("02500850m", "05000850m", "07000850m")
                        ov25_flags = myFlags if ov25_rmv else {**myFlags, "rmVortex": False}
                        try:
                            UOv25_ns = gf.get_var2d(ds_ov25, DSOURCE,
                                                     ("U", ov25_lev, str(ttt)),
                                                     list(ds_ov25.dims.keys()),
                                                     BOCO2, mf, ov25_flags, lonF)
                            VOv25_ns = gf.get_var2d(ds_ov25, DSOURCE,
                                                     ("V", ov25_lev, str(ttt)),
                                                     list(ds_ov25.dims.keys()),
                                                     BOCO2, mf, ov25_flags, lonF)
                            if UOv25_ns and VOv25_ns:
                                zs = gu.get_stm_thin(DSOURCE, DOMAIN)
                                skw25 = dict(color="grey", linewidth=1.5, density=1.5)
                                if FNAME2[ppp] in ("SHDL", "SHML", "SHSL"):
                                    skw25["color"] = "black"
                                if DOMAIN in NSTDMN:
                                    skw25.update({"color": "lightgrey", "linewidth": 2.0})
                                u25, v25 = _sanitize_streamplot_data(
                                    UOv25_ns.data[::zs, ::zs],
                                    VOv25_ns.data[::zs, ::zs])
                                ax.streamplot(lon_arr[::zs], lat_arr[::zs],
                                              u25, v25, **skw25)
                        except Exception as e:
                            print(f"WARNING: OV25 streamline failed: {e}")

                # ── Overlay #3: Bottom Contour Lines ──────────────────
                if ov3_var:
                    if verbose >= 1:
                        print(f"MSG: Working on bottom contour lines (G_OV3={ov3_var})")
                    ds_ov3, _ = _find_ds_with_var(datasets, DSOURCE, ov3_var, ov3_lev)
                    if ds_ov3 is None:
                        print(f"WARNING: Variable {ov3_var} not found. Skipping OV3.")
                    else:
                        try:
                            VOv3_ns = gf.get_var2d(ds_ov3, DSOURCE,
                                                    (ov3_var, ov3_lev, str(ttt)),
                                                    list(ds_ov3.dims.keys()),
                                                    BOCO2, mf, myFlags, lonF)
                            if VOv3_ns:
                                ov3_kw = dict(colors="darkgrey", linewidths=1.0)
                                if FNAME2[ppp] == "RH700_UV700_MSLP":
                                    ov3_kw["colors"] = "black"
                                if tr:
                                    ov3_kw["transform"] = tr
                                if ov3_var == "HGT" and ov3_lev == "05001000d":
                                    ov3_levs = np.linspace(510., 588., 14)
                                    ov3_kw["levels"] = ov3_levs
                                    ov3_kw["linewidths"] = 2.5
                                cs3 = ax.contour(lon_arr, lat_arr, VOv3_ns.data, **ov3_kw)
                                ax.clabel(cs3, inline=True, fontsize=6, fmt="%g")
                        except Exception as e:
                            print(f"WARNING: OV3 contour failed: {e}")

                # ── Overlay #4: Top Contour Lines ─────────────────────
                if ov4_var:
                    if verbose >= 1:
                        print(f"MSG: Working on top contour lines (G_OV4={ov4_var})")
                    ds_ov4, _ = _find_ds_with_var(datasets, DSOURCE, ov4_var, ov4_lev)
                    if ds_ov4 is None:
                        print(f"WARNING: Variable {ov4_var} not found. Skipping OV4.")
                    else:
                        try:
                            VOv4_ns = gf.get_var2d(ds_ov4, DSOURCE,
                                                    (ov4_var, ov4_lev, str(ttt)),
                                                    list(ds_ov4.dims.keys()),
                                                    BOCO2, mf, myFlags, lonF)
                            if VOv4_ns:
                                ov4_kw = dict(colors="navy", linewidths=2.0)
                                if ov4_var == "HGT":
                                    hgt_levels = {
                                        "850": np.linspace(130., 180., 11),
                                        "700": np.linspace(272., 340., 18),
                                        "500": np.linspace(528., 594., 12),
                                    }
                                    if ov4_lev in hgt_levels:
                                        ov4_kw["levels"] = hgt_levels[ov4_lev]
                                    ov4_kw["linewidths"] = 3.0
                                elif ov4_var == "MSLP":
                                    ov4_kw["levels"] = np.linspace(980., 1040., 31)
                                    ov4_kw["linewidths"] = 1.5
                                elif ov4_var == "RVO":
                                    ov4_kw["levels"] = np.linspace(2., 50., 13)
                                elif ov4_var == "HLCY":
                                    ov4_kw["levels"] = np.linspace(100., 1000., 10)
                                    ov4_kw["colors"] = "blue4"
                                elif ov4_var == "T" and ov4_lev == "2":
                                    ov4_kw["levels"] = np.linspace(290., 310., 6)
                                if base_var in ("RH", "PRCP"):
                                    ov4_kw["colors"] = "gray"
                                if base_var == "HGT":
                                    ov4_kw["colors"] = "blue3"
                                if tr:
                                    ov4_kw["transform"] = tr
                                cs4 = ax.contour(lon_arr, lat_arr, VOv4_ns.data, **ov4_kw)
                                ax.clabel(cs4, inline=True, fontsize=6, fmt="%g")
                        except Exception as e:
                            print(f"WARNING: OV4 contour failed: {e}")

                # ── Overlay #5: MSLP H/L Markers ─────────────────────
                if ov5_var:
                    if verbose >= 1:
                        print(f"MSG: Working on MSLP markers (G_OV5={ov5_var})")
                    if ov5_var != "MSLP":
                        print("ERROR: MSLP Markers must be created from MSLP.")
                    elif not MSLP_MARKERS:
                        ds_ov5, _ = _find_ds_with_var(datasets, DSOURCE, ov5_var, ov5_lev)
                        if ds_ov5 is None:
                            print(f"WARNING: Variable {ov5_var} not found. Skipping OV5.")
                        else:
                            try:
                                VOv5_ns = gf.get_var2d(ds_ov5, DSOURCE,
                                                        (ov5_var, ov5_lev, str(ttt)),
                                                        list(ds_ov5.dims.keys()),
                                                        BOCO2, mf, myFlags, lonF)
                                if VOv5_ns:
                                    VOv5 = VOv5_ns.data.copy()
                                    # Mask terrain: read PSFC if available
                                    ds_psfc, _ = _find_ds_with_var(datasets, DSOURCE, "PSFC", "")
                                    if ds_psfc is not None:
                                        psfc_ns = gf.get_var2d(ds_psfc, DSOURCE,
                                                                ("PSFC", "", str(ttt)),
                                                                list(ds_psfc.dims.keys()),
                                                                BOCO2, mf, myFlags, lonF)
                                        if psfc_ns is not None:
                                            sigma = psfc_ns.data / VOv5
                                            VOv5 = np.where(sigma <= 0.90, np.nan, VOv5)
                                    VOv5 = np.where(VOv5 <= 0, np.nan, VOv5)
                                    dx_val = abs(float(lon_arr[1]) - float(lon_arr[0])) if len(lon_arr) > 1 else 1.0
                                    max_h = gu.get_dmn_info(DOMAIN, DSOURCE, "maxH", False)
                                    max_l = gu.get_dmn_info(DOMAIN, DSOURCE, "maxL", False)
                                    llbox = gu.get_dmn_info(DOMAIN, DSOURCE, "llbox", False)
                                    try:
                                        max_h = int(max_h)
                                    except Exception:
                                        max_h = 5
                                    try:
                                        max_l = int(max_l)
                                    except Exception:
                                        max_l = 5
                                    _draw_hl_markers(ax, VOv5, lat_arr, lon_arr,
                                                     BOCO_now, pct_lat, pct_lon,
                                                     max_h, max_l, dx_val,
                                                     DOMAIN in NSTDMN, tr)
                                    MSLP_MARKERS = True
                            except Exception as e:
                                print(f"WARNING: OV5 MSLP markers failed: {e}")

                # ── Overlay #6: Storm Labels ──────────────────────────
                if ATCF1_DIR != "NONE" and not SC_GRAPHICS:
                    for sss in range(NATCF):
                        fhr_arr = tcFHR_list[sss]
                        lat_arr_tc = tcLats_list[sss]
                        lon_arr_tc = tcLons_list[sss]
                        int_arr_tc = tcINT_list[sss]
                        match_idx = np.where(fhr_arr == FHR3_now)[0]
                        if len(match_idx) == 0:
                            continue
                        mi = match_idx[0]
                        if np.isnan(lat_arr_tc[mi]):
                            continue
                        my_lat = lat_arr_tc[mi]
                        my_lon = lon_arr_tc[mi]
                        my_lat1 = my_lat - 6.0 * pct_lat
                        print(f"MSG: Adding Storm ID label for {ALL_SID[sss]} at {my_lat1},{my_lon}")
                        gf.add_sid_label(ax, ALL_SID[sss].upper(), my_lon, my_lat1, 0.011)
                        if FNAME2[ppp] in ("SHDL", "SHML", "SHSL", "SFDL", "SFML", "SFSL"):
                            tc_int_val = float(int_arr_tc[mi]) if not np.isnan(int_arr_tc[mi]) else 0.0
                            gf.add_storm_marker(ax, tc_int_val, ALL_SNUM[sss],
                                                my_lon, my_lat, 0.014)

                # ── Overlay #7: Titles and Annotations ───────────────
                # Get viewport approximation (matplotlib normalized coords)
                fig.canvas.draw()
                bb = ax.get_position()
                vp_lft = bb.x0
                vp_top = bb.y1
                vp_wid = bb.width
                vp_hgt = bb.height
                vp_rgt = vp_lft + vp_wid
                vp_bot = vp_top - vp_hgt

                txhgt = 0.012 if DOMAIN in NSTDMN else 0.010

                gf.add_graphic_title(ax, FNAME2[ppp], vp_lft,
                                     vp_top + 0.023, txhgt)

                idate_fmt = gu.change_time_fmt(
                    IDATE.replace("gfs.", ""), "YYYYMMDDHH",
                    "HHz DOW, Month DD YYYY", 0)
                time_title = (f"Init: {idate_fmt}  "
                              f"Forecast Hour:[{FHR3_now:03d}]  "
                              f"valid at {valid_date}")
                gf.add_time_title(ax, time_title, vp_lft,
                                  vp_top + 0.008, 0.010 if DOMAIN in NSTDMN else 0.008,
                                  "left")

                if LONGSID_LIST:
                    gf.add_storm_title(ax, LONGSID_LIST.upper(), vp_rgt,
                                       vp_top + 0.008,
                                       0.010 if DOMAIN in NSTDMN else 0.008,
                                       "right")

                if DOMAIN in NSTDMN:
                    if UV10max is not None and not math.isnan(UV10max):
                        gf.add_vmax_label(ax, UV10max, vp_lft + vp_wid,
                                          vp_top + 0.035, 0.009, "right")
                    if MSLPmin is not None and not math.isnan(MSLPmin):
                        gf.add_mslp_label(ax, MSLPmin, vp_lft + vp_wid,
                                          vp_top + 0.022, 0.009, "right")

                if DO_SRCLBL:
                    gf.add_model_title(ax, EXPT, ENSID, vp_lft,
                                       vp_top + 0.04, txhgt)
                    gf.add_disclaimer(ax, EXPT,
                                      BOCO_now[2] + 2*pct_lon,
                                      BOCO_now[1] + 2*pct_lon,
                                      txhgt, "left")

                # ── Save figure ───────────────────────────────────────
                try:
                    plt.savefig(ofile + ".png", dpi=150, bbox_inches="tight")
                    if verbose >= 1:
                        print(f"MSG: Saved {ofile}.png")
                except Exception as e:
                    print(f"WARNING: Could not save {ofile}.png: {e}")
                finally:
                    plt.close(fig)

                _post_process_image(ofile, DO_RMWHITE, DO_CONVERTGIF)

            # end plot loop (ppp)
        # end time loop (ttt)

        # ── Post-file: check FILE_DONE, update PlottedFiles ───────────────
        print(f"MSG: Checking that all graphics were produced for forecast hour {file_fhr:03d}.")
        file_done = True
        for ppp in range(nPlots):
            if file_fhr < 3 and ("PRCP_" in FNAME2[ppp]):
                continue
            if file_fhr < 3 and "REFL" in FNAME2[ppp]:
                continue
            if file_fhr < 24 and "PRCP24_" in FNAME2[ppp]:
                continue
            if DSOURCE == "GFS" and "REFL" in FNAME2[ppp]:
                continue
            if DOMAIN in NSTDMN and "PRCP" in FNAME2[ppp]:
                continue
            ext_check = "gif" if DO_CONVERTGIF else "png"
            if SC_GRAPHICS:
                pattern = os.path.join(
                    ODIR, f"*{LONGSID_LIST.lower()}.{FNAME2[ppp]}.{IDATE}.{DOMAIN}"
                          f".f{file_fhr:03d}.{ext_check}")
            else:
                pattern = os.path.join(
                    ODIR, f"{FNAME2[ppp]}.{IDATE}.{DOMAIN}"
                          f".f{file_fhr:03d}.{ext_check}")
            if not glob.glob(pattern):
                print(f"MSG: Graphics not found for {FNAME2[ppp]}.")
                file_done = False
                break

        FILE_DONE[fff] = file_done

        if FILE_DONE[fff]:
            print(f"MSG: All graphics found for forecast hour {file_fhr:03d}.")
            NATCF_NOW = sum(
                1 for sss in range(NATCF)
                if not np.isnan(tc_lat_now[sss]))
            tcMAXHR_val = max(
                (float(np.nanmax(tcFHR_list[sss])) for sss in range(NATCF)
                 if len(tcFHR_list[sss]) > 0), default=0.0)
            if (ATCF_REQD and file_fhr <= tcMAXHR_val) or (not ATCF_REQD):
                _update_plotted_files(PLOTTED_FILES, iFiles[fff], NATCF_NOW, ALL_ATCF)

    # ── Part X: Write status file ──────────────────────────────────────────
    LOCK_FILE = STATUS_FILE + ".lock"
    _lock_file(LOCK_FILE, timeout=180)
    print(f"MSG: {STATUS_FILE} has been locked ({LOCK_FILE}).")

    if not SC_FHR:
        print("MSG: COMPLETE! No more tracker info at these forecast lead times.")
        with open(STATUS_FILE, "w") as fh:
            fh.write("complete\n")
    elif OUT_OF_BDS:
        print("MSG: COMPLETE! Nothing to do: domain does not overlap with model domain.")
        with open(STATUS_FILE, "w") as fh:
            fh.write("complete\n")
    elif ATCF_REQD and not ALL_ATCF:
        print("MSG: INCOMPLETE! Graphics produced, but might be missing ATCF info.")
        with open(STATUS_FILE, "w") as fh:
            fh.write("incomplete\n")
    elif not all(FNL_IFILES):
        print("MSG: INCOMPLETE! New input files might be available.")
        with open(STATUS_FILE, "w") as fh:
            fh.write("incomplete\n")
    elif not all(FILE_DONE):
        print("MSG: INCOMPLETE! Some graphics are missing.")
        with open(STATUS_FILE, "w") as fh:
            fh.write("incomplete\n")
    else:
        print("MSG: COMPLETE! Graphics were produced!")
        with open(STATUS_FILE, "w") as fh:
            fh.write("complete\n")

    _unlock_file(LOCK_FILE)
    print(f"MSG: {STATUS_FILE} has been unlocked ({LOCK_FILE}).")

    # ── Part XI: Final summary ─────────────────────────────────────────────
    print("")
    print("MSG: Ran GPLOT with these settings:")
    print("********************")
    print(f"MSG:   Basin-Scale HWRF:   {'ON' if IS_MSTORM else 'OFF'}")
    print(f"MSG:   Experiment Name:    {EXPT}")
    print(f"MSG:   Data Source:        {DSOURCE}")
    print(f"MSG:   Forecast Cycle:     {IDATE}")
    print(f"MSG:   Storm ID:           {SID}")
    print(f"MSG:   Domain/Region:      {DOMAIN}")
    print(f"MSG:   Graphics Tier:      {TIER}")
    if ENSID:
        print(f"MSG:   Ensemble Member:    {ENSID}")
    print(f"MSG:   Input Directory:    {IDIR}")
    print(f"MSG:   Output Directory:   {ODIR}")
    print(f"MSG:   System Environment: {MACHINE}")
    print(f"MSG:   Initial/Final Hour: {INIT_HR}/{FNL_HR}")
    print(f"MSG:   Time Step [h]:      {DT}")
    print(f"MSG:   ATCF Required:      {'ON' if ATCF_REQD else 'OFF'}")
    print(f"MSG:   Trim graphics:      {'ON' if DO_RMWHITE else 'OFF'}")
    print(f"MSG:   Source Labels:      {'ON' if DO_SRCLBL else 'OFF'}")
    print(f"MSG:   Convert to GIF:     {'ON' if DO_CONVERTGIF else 'OFF'}")
    print(f"MSG:   Plotted Files file: {PLOTTED_FILES}")
    print(f"MSG:   Status file:        {STATUS_FILE}")
    print("********************")

    print("")
    print(f"MSG: GPLOT_maps.py finished running at {datetime.datetime.now()}.")


if __name__ == "__main__":
    main()
