"""
Plot utilities for GPLOT Python modules.

Replaces GenPlotRes() from GPLOT_util.ncl and the add_*_title procedures
from GPLOT_func.ncl. Provides cartopy map setup, title/label placement,
storm marker drawing, and output file handling (PNG->GIF conversion,
whitespace trimming).
"""

import glob
import os
import subprocess
import time
import logging
from datetime import datetime, timedelta

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import cartopy.crs as ccrs
import cartopy.feature as cfeature

logger = logging.getLogger(__name__)


def configure_cartopy(cartopy_dir=None):
    """Point cartopy at an offline shapefile cache.

    On HPC compute nodes without outbound internet, cartopy will
    repeatedly fail trying to download Natural Earth shapefiles
    from naturalearthdata.com. Setting
    ``cartopy.config['pre_existing_data_dir']`` to a populated cache
    makes cartopy use the on-disk files instead -- cartopy checks this
    key before any download attempt, so the override propagates to
    every subsequent ``cfeature.<X>`` /
    ``shapereader.natural_earth(...)`` call without further plumbing.

    Resolution order (first candidate that is an *existing directory* wins;
    a non-empty but nonexistent path -- e.g. the
    ``/PLEASE/SET/THE/CARTOPY/DIR`` placeholder -- is skipped, NOT treated
    as a hard stop, so the fallback still applies):
      1. Explicit ``cartopy_dir`` argument (typically from
         ``nml.get('CARTOPY_DIR')``).
      2. ``CARTOPY_DATA_DIR`` environment variable (lets the per-machine
         batch script export a host-wide fallback from
         ``batch.defaults.${MACHINE}`` when the namelist hasn't been
         updated).
      3. No-op -- cartopy keeps its default download-then-cache flow
         (works fine on dev hosts with internet access).

    The path should be the parent of ``shapefiles/`` -- cartopy
    resolves ``<cartopy_dir>/shapefiles/natural_earth/<physical|cultural>/<file>.shp``
    on its own.

    Diagnostic prints (visible in the GPLOT job log) make it
    obvious from a single grep whether the offline cache is wired up,
    falling back to the env var, or unconfigured.

    Args:
        cartopy_dir: Path to a directory holding a pre-populated cartopy
            shapefile cache (typically a sysadmin-managed location like
            ``/home/role.aoml-hafs1/.local/share/cartopy``). If empty,
            None, or not an existing directory, the env var is consulted;
            if that's also missing/invalid, the function is a no-op.
    """
    # Walk the candidates in priority order and pick the FIRST one that is an
    # existing directory. Critically, a non-empty but invalid path (e.g. the
    # 'CARTOPY_DIR = /PLEASE/SET/THE/CARTOPY/DIR' placeholder in
    # namelist.master.HAFS_Default) must NOT short-circuit the env-var
    # fallback -- otherwise cartopy gets no offline cache and freezes trying
    # to download Natural Earth data on an offline compute node.
    candidates = [
        (cartopy_dir, 'CARTOPY_DIR namelist entry'),
        (os.environ.get('CARTOPY_DATA_DIR', ''), 'CARTOPY_DATA_DIR env var'),
    ]

    chosen = None
    source = None
    skipped = []
    for path, src in candidates:
        if not path:
            continue
        if os.path.isdir(path):
            chosen = path
            source = src
            break
        # Non-empty but not a real directory: note it and keep falling through.
        skipped.append(f'{path!r} (from {src})')

    if chosen is None:
        # Diagnostic so the HPC log shows exactly why the offline cache wasn't
        # wired up. Use print() in addition to the logger so it lands in stdout
        # regardless of logging config.
        if skipped:
            msg = ('MSG: configure_cartopy: no valid cartopy cache -- tried '
                   + ', '.join(skipped)
                   + ' -- cartopy will attempt downloads')
            print(msg)
            logger.warning(msg)
        else:
            msg = ('MSG: configure_cartopy: no CARTOPY_DIR (namelist) and no '
                   'CARTOPY_DATA_DIR (env) -- cartopy will attempt downloads')
            print(msg)
            logger.info(msg)
        return

    if skipped:
        # Surface that we fell past an invalid higher-priority entry.
        msg = ('MSG: configure_cartopy: skipped invalid '
               + ', '.join(skipped))
        print(msg)
        logger.warning(msg)

    import cartopy
    cartopy.config['pre_existing_data_dir'] = chosen
    msg = f'MSG: configure_cartopy: pre_existing_data_dir = {chosen} (from {source})'
    print(msg)
    logger.info(msg)


def load_county_state_shapes(cartopy_dir):
    """Build COUNTIES and STATES ShapelyFeatures for plot_airsea_pbl.

    Replaces the legacy module-scope hardwired-path loader in
    ``plot_airsea_pbl.py`` (the one that picked between
    ``/home/role.aoml-hafs1/.local/share/cartopy`` and
    ``/home/ahazelto/.local/share/cartopy`` via try/except). Now driven
    entirely by the ``CARTOPY_DIR`` namelist entry, so a single edit
    in the master namelist controls the path on every host.

    The returned (COUNTIES, STATES) tuple consists of:
      - ``COUNTIES``: built from ``countyl010g.shp`` (US Census Bureau,
        not Natural Earth -- has to be manually placed under the
        cartopy cache at
        ``<cartopy_dir>/shapefiles/natural_earth/cultural/``).
      - ``STATES``: built from ``ne_50m_admin_1_states_provinces_lakes``
        resolved via cartopy's standard ``natural_earth()`` lookup,
        which respects the ``pre_existing_data_dir`` set by
        :func:`configure_cartopy` earlier in main().

    If ``cartopy_dir`` is empty / missing, or the county shapefile
    isn't found, returns empty features with a warning so the script
    can still produce maps without the county/state overlays
    (typical for laptop dev runs).
    """
    import warnings
    from cartopy import feature as cfeature, crs as ccrs
    from cartopy.io import shapereader as shpreader
    empty = cfeature.ShapelyFeature([], ccrs.PlateCarree())

    # STATES -- Natural Earth, found via cartopy's standard lookup
    # (which uses pre_existing_data_dir if configure_cartopy set it).
    try:
        states_path = shpreader.natural_earth(
            category='cultural', resolution='50m',
            name='admin_1_states_provinces_lakes')
        states = list(shpreader.Reader(states_path).geometries())
        STATES = cfeature.ShapelyFeature(states, ccrs.PlateCarree())
    except Exception as exc:
        warnings.warn(f'Could not load states shapefile ({exc}); '
                      f'state overlays will be empty.')
        STATES = empty

    # COUNTIES -- non-Natural-Earth file, must build path manually
    # from CARTOPY_DIR. If CARTOPY_DIR is unset (typical laptop),
    # skip without erroring.
    COUNTIES = empty
    if cartopy_dir:
        county_path = os.path.join(cartopy_dir, 'shapefiles',
                                   'natural_earth', 'cultural',
                                   'countyl010g.shp')
        if os.path.isfile(county_path):
            counties = list(shpreader.Reader(county_path).geometries())
            COUNTIES = cfeature.ShapelyFeature(counties, ccrs.PlateCarree())
        else:
            warnings.warn(f'County shapefile not found at {county_path}; '
                          f'county overlays will be empty.')

    return COUNTIES, STATES


def setup_map_axes(ax, bounds, projection=None, resolution='50m'):
    """
    Configure a cartopy axes with geographic features for map plots.

    Matches the NCL GenPlotRes cylindrical equidistant setup with
    coastlines, political boundaries, and gridlines.

    Parameters
    ----------
    ax : matplotlib.axes.Axes
        A cartopy GeoAxes instance.
    bounds : tuple
        (lat_n, lat_s, lon_w, lon_e) domain boundaries.
    projection : cartopy.crs.Projection, optional
        Map projection (default: PlateCarree).
    resolution : str, optional
        Feature resolution ('10m', '50m', '110m'). Default '50m'.
    """
    lat_n, lat_s, lon_w, lon_e = bounds

    ax.set_extent([lon_w, lon_e, lat_s, lat_n], crs=ccrs.PlateCarree())

    # Add geographic features
    ax.add_feature(cfeature.COASTLINE.with_scale(resolution), linewidth=0.8, edgecolor='black')
    ax.add_feature(cfeature.BORDERS.with_scale(resolution), linewidth=0.5, edgecolor='gray')
    ax.add_feature(cfeature.STATES.with_scale(resolution), linewidth=0.3, edgecolor='gray')

    # Gridlines
    gl = ax.gridlines(draw_labels=True, linewidth=0.5, color='gray',
                      alpha=0.5, linestyle='--')
    gl.top_labels = False
    gl.right_labels = False

    # Determine appropriate gridline spacing based on domain size
    lat_span = lat_n - lat_s
    lon_span = lon_e - lon_w
    max_span = max(lat_span, lon_span)

    if max_span <= 10:
        spacing = 2
    elif max_span <= 30:
        spacing = 5
    elif max_span <= 60:
        spacing = 10
    else:
        spacing = 20

    gl.xlocator = mticker.MultipleLocator(spacing)
    gl.ylocator = mticker.MultipleLocator(spacing)


def create_figure(figsize=(12, 9), projection=None):
    """
    Create a new figure with a cartopy map projection.

    Parameters
    ----------
    figsize : tuple, optional
        Figure size in inches (width, height).
    projection : cartopy.crs.Projection, optional
        Map projection (default: PlateCarree).

    Returns
    -------
    tuple
        (fig, ax) matplotlib figure and cartopy axes.
    """
    if projection is None:
        projection = ccrs.PlateCarree()

    fig = plt.figure(figsize=figsize)
    ax = fig.add_subplot(1, 1, 1, projection=projection)
    return fig, ax


def add_titles(ax, expt, var_title, fhr, idate, longsid='',
               vmax=None, mslp=None, ensid=''):
    """
    Add standard GPLOT titles to a plot.

    Left title: experiment name, variable, initialization time, forecast hour.
    Right title: storm info (VMAX, MSLP, storm ID).

    Parameters
    ----------
    ax : matplotlib.axes.Axes
        The axes to add titles to.
    expt : str
        Experiment name.
    var_title : str
        Variable/plot title string.
    fhr : int
        Forecast hour.
    idate : str
        Initialization date (YYYYMMDDHH).
    longsid : str, optional
        Long storm ID (e.g., 'sinlaku04w').
    vmax : int, optional
        Maximum wind speed (kt).
    mslp : int, optional
        Minimum sea level pressure (mb).
    ensid : str, optional
        Ensemble member ID.
    """
    # Format the valid time
    try:
        init_dt = datetime.strptime(idate, '%Y%m%d%H')
        valid_dt = init_dt + timedelta(hours=fhr)
        valid_str = valid_dt.strftime('%HZ %a, %b %d %Y')
        init_str = init_dt.strftime('%Y%m%d%H')
    except ValueError:
        valid_str = ''
        init_str = idate

    # Build left title
    left_parts = [expt]
    if ensid:
        left_parts[0] += f' (mem{ensid})'
    left_parts.append(var_title)
    left_parts.append(f'Init: {init_str}   FHR: {fhr:03d}')
    left_parts.append(f'Valid: {valid_str}')
    left_title = '\n'.join(left_parts)

    # Build right title
    right_parts = []
    if longsid:
        right_parts.append(longsid.upper())
    if vmax is not None and vmax > 0:
        right_parts.append(f'VMAX= {vmax} kt')
    if mslp is not None and mslp > 0:
        right_parts.append(f'PMIN= {mslp} hPa')
    right_title = '\n'.join(right_parts)

    ax.set_title(left_title, fontsize=11, fontweight='bold', loc='left')
    if right_title:
        ax.set_title(right_title, fontsize=11, color='brown', loc='right')


def add_disclaimer(ax, expt=''):
    """
    Add an experimental product disclaimer inside the plot axes.

    Positioned just above the bottom edge of the map itself rather than
    below the axes, so the horizontal colorbar doesn't overlap it.

    Parameters
    ----------
    ax : matplotlib.axes.Axes
        The axes to add the disclaimer to.
    expt : str, optional
        Experiment name (used to customize disclaimer).
    """
    msg = '*Experimental Product of NOAA/AOML/HRD*'
    ax.text(0.5, 0.02, msg, transform=ax.transAxes,
            fontsize=7, ha='center', va='bottom', color='black',
            zorder=15,
            bbox=dict(boxstyle='round,pad=0.2', facecolor='white',
                      edgecolor='none', alpha=0.6))


def add_storm_marker(ax, lat, lon, intensity=None, label='',
                     transform=None):
    """
    Draw a tropical cyclone marker on a map.

    Parameters
    ----------
    ax : matplotlib.axes.Axes
        The map axes.
    lat : float
        Latitude of the TC center.
    lon : float
        Longitude of the TC center.
    intensity : int, optional
        Wind intensity (kt) for color coding.
    label : str, optional
        Text label for the marker.
    transform : cartopy.crs.Projection, optional
        Coordinate transform (default: PlateCarree).
    """
    if transform is None:
        transform = ccrs.PlateCarree()

    # Color based on Saffir-Simpson scale
    if intensity is None or intensity < 34:
        color = 'blue'
        size = 6
    elif intensity < 64:
        color = 'green'
        size = 8
    elif intensity < 83:
        color = 'yellow'
        size = 10
    elif intensity < 96:
        color = 'orange'
        size = 11
    elif intensity < 113:
        color = 'red'
        size = 12
    elif intensity < 137:
        color = 'darkred'
        size = 13
    else:
        color = 'purple'
        size = 14

    ax.plot(lon, lat, 'o', color=color, markersize=size,
            markeredgecolor='black', markeredgewidth=0.5,
            transform=transform, zorder=10)

    if label:
        ax.text(lon + 0.3, lat + 0.3, label, fontsize=7,
                transform=transform, zorder=11)


def convert_to_gif(png_path, remove_png=True):
    """
    Convert a PNG file to GIF using ImageMagick, retrying on transient
    failures.

    The most common failure mode is ImageMagick reading the PNG before
    matplotlib has fully flushed it to NFS-backed scratch (-> "improper
    image header"). Those failures resolve within a few seconds, so try
    a small backoff sequence before giving up. A genuinely corrupt PNG
    (zero-byte / truncated) fails on each retry; total wall-time wasted
    on the unrecoverable case is bounded by the sum of the backoffs.

    Parameters
    ----------
    png_path : str
        Path to the PNG file.
    remove_png : bool, optional
        If True, remove the PNG after a successful conversion. Default True.

    Returns
    -------
    str
        Path to the created GIF file on success, or the original PNG
        path if every retry failed.
    """
    gif_path = png_path.replace('.png', '.gif')

    # Backoffs in seconds before each attempt. The 0 means "try once
    # immediately"; the 3 and 8 give NFS / disk buffers room to settle
    # if the first error was a half-written PNG.
    backoffs = (0, 3, 8)
    last_err = None

    for attempt_idx, delay in enumerate(backoffs, start=1):
        if delay > 0:
            time.sleep(delay)
        try:
            result = subprocess.run(
                ['convert', png_path, '+repage', f'gif:{gif_path}'],
                capture_output=True, text=True, timeout=60
            )
            if result.returncode == 0:
                if remove_png:
                    try:
                        os.remove(png_path)
                    except OSError:
                        pass
                if attempt_idx > 1:
                    # Surface successful retries at WARNING so the operator
                    # can see that we hit (and recovered from) a flaky write.
                    logger.warning(
                        f"convert_to_gif: succeeded on attempt {attempt_idx} "
                        f"after transient failure(s): {png_path}")
                else:
                    logger.debug(f"Converted to GIF: {gif_path}")
                return gif_path
            else:
                last_err = (result.stderr or '').strip() or \
                           f"rc={result.returncode}"
        except (subprocess.TimeoutExpired, FileNotFoundError) as e:
            last_err = repr(e)

    # All retries exhausted -- log once (vs. once per attempt) and
    # leave the PNG in place for the end-of-run sweep.
    logger.warning(
        f"ImageMagick convert failed after {len(backoffs)} attempt(s) "
        f"on {png_path}: {last_err}")
    return png_path


def sweep_orphan_pngs(odir, recursive=False):
    """
    Catch-all sweep: convert any leftover .png files in ``odir`` to .gif.

    Save-figure pipeline does ``savefig -> trim_whitespace -> convert_to_gif``
    and removes the .png on a successful convert. If ImageMagick has a
    transient hiccup (NFS lag producing an "improper image header" read,
    timeout, etc.) the .png is left behind and no .gif is written. The
    on-disk gate in maps/ships/etc. then sees the missing .gif on the next
    spawn iteration and re-renders the whole FHR -- wasteful, and not
    guaranteed to succeed.

    Call this once per module right before writing the final
    ``status=complete`` so any orphan .png from the just-finished run gets
    a retry conversion. Tries once with a brief settle to give NFS / disk
    buffers a chance to flush. Always returns; never raises -- a sweep
    failure must not block the module from completing.

    Parameters
    ----------
    odir : str
        Output directory to scan.
    recursive : bool, optional
        If True, recurse into subdirectories. Default False (matches the
        flat-output convention used by every module today).

    Returns
    -------
    dict
        ``{'retried_ok': int, 'orphans_cleaned': int, 'still_failed': int,
           'inspected': int}``. Logged as a single MSG line; escalated to
        WARNING only if ``still_failed > 0``.
    """
    if not odir or not os.path.isdir(odir):
        return {'retried_ok': 0, 'orphans_cleaned': 0,
                'still_failed': 0, 'inspected': 0}

    pattern = os.path.join(odir, '**', '*.png') if recursive else \
              os.path.join(odir, '*.png')
    pngs = sorted(glob.glob(pattern, recursive=recursive))

    if not pngs:
        return {'retried_ok': 0, 'orphans_cleaned': 0,
                'still_failed': 0, 'inspected': 0}

    # Brief settle for NFS / disk buffers before retrying conversion.
    # If our caller just finished writing these files, the headers may
    # not yet be readable on a different node.
    time.sleep(2)

    retried_ok = 0
    orphans_cleaned = 0
    still_failed = 0
    for png in pngs:
        # Skip ones a concurrent process already cleaned up.
        if not os.path.isfile(png):
            continue
        gif = png[:-4] + '.gif'
        if os.path.isfile(gif):
            # GIF already exists; PNG is orphan leftover (rare -- means
            # convert_to_gif succeeded but the post-convert os.remove
            # didn't land, typically NFS flake). Drop the orphan.
            try:
                os.remove(png)
                orphans_cleaned += 1
            except OSError as e:
                logger.debug(f"sweep_orphan_pngs: could not remove "
                             f"orphan {png}: {e}")
            continue
        # No gif yet -- retry the convert.
        result = convert_to_gif(png)
        # convert_to_gif returns the gif path on success, or the png
        # path on failure.
        if result.endswith('.gif') and os.path.isfile(result):
            retried_ok += 1
        else:
            still_failed += 1

    summary = (f"sweep_orphan_pngs[{odir}]: inspected={len(pngs)} "
               f"retried_ok={retried_ok} orphans_cleaned={orphans_cleaned} "
               f"still_failed={still_failed}")
    if still_failed > 0:
        logger.warning(summary + " -- some PNGs could not be converted; "
                       "they will be retried on the next spawn iteration.")
    elif retried_ok > 0 or orphans_cleaned > 0:
        logger.info(summary)
    else:
        logger.debug(summary)

    return {'retried_ok': retried_ok, 'orphans_cleaned': orphans_cleaned,
            'still_failed': still_failed, 'inspected': len(pngs)}


def trim_whitespace(png_path):
    """
    Trim white space from the edges of a PNG image using ImageMagick.

    Parameters
    ----------
    png_path : str
        Path to the PNG file.
    """
    try:
        subprocess.run(
            ['convert', png_path, '-trim', '+repage', png_path],
            capture_output=True, text=True, timeout=60
        )
    except (subprocess.TimeoutExpired, FileNotFoundError) as e:
        logger.warning(f"Whitespace trimming failed: {e}")


def save_figure(fig, ofile, do_trim=True, do_gif=True, dpi=150):
    """
    Save a matplotlib figure with optional trimming and GIF conversion.

    Parameters
    ----------
    fig : matplotlib.figure.Figure
        The figure to save.
    ofile : str
        Output file path (without extension).
    do_trim : bool, optional
        Whether to trim whitespace (default True).
    do_gif : bool, optional
        Whether to convert to GIF (default True).
    dpi : int, optional
        Output DPI (default 150).

    Returns
    -------
    str
        Path to the final output file.
    """
    png_path = f"{ofile}.png"

    # Ensure output directory exists
    os.makedirs(os.path.dirname(png_path) or '.', exist_ok=True)

    fig.savefig(png_path, bbox_inches='tight', dpi=dpi)
    plt.close(fig)
    logger.info(f"Saved: {png_path}")

    if do_trim:
        trim_whitespace(png_path)

    if do_gif:
        return convert_to_gif(png_path)

    return png_path


def read_spawn_file_list(odir, domain, tier, sid):
    """
    Read the per-FHR GRIB2 list that ``spawn_*.sh`` prepared for this run.

    The spawn shell does directory-layout discovery via its ``IDIR_OPTS``
    list (~30 variants covering HAFS, HWRF, HFSA, GFS, ECMWF, ensemble,
    etc.) and writes the resulting (file_path, fhr) pairs to a matching
    pair of logs under ``odir``:

    - ``UnplottedFiles.<domain>.<tier>{.<sid>}.log`` — one GRIB2 path per line
    - ``AllForecastHours.<domain>.<tier>{.<sid>}.log`` — matching FHRs

    The storm-tag suffix is only added by spawn for storm-centered (SC=True)
    domains (hwrf, d03, d02, tkfull, alld03, storm, core, tcparent per
    ``spawn_maps.sh:456-458``). We try the tagged path first and fall back
    to the un-tagged path so this works for both groups without Python
    needing to mirror the shell's SC list.

    Parameters
    ----------
    odir : str
        Output directory (where spawn wrote the lists).
    domain : str
        Domain name (e.g. ``d03``, ``atl``).
    tier : str
        Tier name (e.g. ``Tier1``).
    sid : str
        Storm ID, e.g. ``09L``. Used to build the storm-tagged filename;
        the helper uppercases it internally to match spawn's
        ``STORMTAG=".${STORM^^}"`` convention.

    Returns
    -------
    list of (int, str) or None
        Sorted list of ``(fhr, grib_path)`` pairs, or ``None`` if neither
        log file exists. Callers should fall back to their legacy
        discovery (``find_grib_files``) when this returns ``None`` so
        standalone-Python dev/test workflows keep working.
    """
    sid_up = sid.upper() if sid else ''
    candidates = []
    if sid_up:
        candidates.append(
            (os.path.join(odir, f'UnplottedFiles.{domain}.{tier}.{sid_up}.log'),
             os.path.join(odir, f'AllForecastHours.{domain}.{tier}.{sid_up}.log')))
    candidates.append(
        (os.path.join(odir, f'UnplottedFiles.{domain}.{tier}.log'),
         os.path.join(odir, f'AllForecastHours.{domain}.{tier}.log')))

    for unplotted_log, fhr_log in candidates:
        if not (os.path.isfile(unplotted_log) and os.path.isfile(fhr_log)):
            continue
        with open(unplotted_log) as f:
            files = [ln.strip() for ln in f if ln.strip()]
        with open(fhr_log) as f:
            fhrs = [int(ln.strip()) for ln in f if ln.strip()]
        if not files or not fhrs:
            return []
        if len(files) != len(fhrs):
            # Spawn writes them in lockstep; a length mismatch means one
            # is stale. Bail out -- the caller's fallback will recover.
            return None
        return sorted(zip(fhrs, files), key=lambda p: p[0])

    return None


def update_plotted_file(plotted_log, input_file, status=1):
    """
    Update the PlottedFiles log to track completed graphics.

    Parameters
    ----------
    plotted_log : str
        Path to the PlottedFiles log.
    input_file : str
        Input file that was processed.
    status : int, optional
        Status code (1 = complete).
    """
    os.makedirs(os.path.dirname(plotted_log) or '.', exist_ok=True)

    # Remove existing entry for this file
    existing_lines = []
    if os.path.isfile(plotted_log):
        with open(plotted_log, 'r') as f:
            existing_lines = [l for l in f if input_file not in l]

    with open(plotted_log, 'w') as f:
        for line in existing_lines:
            f.write(line)
        f.write(f"{input_file} {status}\n")


def get_plot_title(var_name):
    """
    Get a human-readable plot title for a plot filename/short-name.

    Replaces ``getPlotTitle()`` from ``GPLOT_util.ncl``. Covers every
    plot recipe in the stock Tier1/Tier2 maps namelists; anything
    missing falls back to the raw name with underscores replaced.

    Parameters
    ----------
    var_name : str
        Plot short-name from the maps namelist FILE_NAME column
        (e.g., 'SHML', 'UV10_MSLP', 'RH700400_UV700400_MSLP').

    Returns
    -------
    str
        Human-readable title.
    """
    _TITLE_MAP = {
        # Surface / MSLP
        'MSLP': 'Mean Sea Level Pressure',
        'UV10_MSLP': '10-m Wind Speed & MSLP',
        'T2': '2-m Temperature',

        # Relative vorticity + heights + wind
        'RVO850_Z850_UV850': '850 hPa Relative Vorticity, Heights, & Wind',
        'RVO700_Z700_UV700': '700 hPa Relative Vorticity, Heights, & Wind',
        'RVO500_Z500_UV500': '500 hPa Relative Vorticity, Heights, & Wind',
        'RVO200_Z200_UV200': '200 hPa Relative Vorticity, Heights, & Wind',
        'RVO850_UV200': '850 hPa Relative Vorticity & 200 hPa Wind',

        # Humidity
        'RH700_UV700_MSLP': '700 hPa Relative Humidity, Wind, & MSLP',
        'RH700400_UV700400_MSLP':
            '700-400 hPa Mean Relative Humidity, Wind, & MSLP',

        # Geopotential heights
        'HGT500_MSLP': '500 hPa Geopotential Height & MSLP',
        'HGT200_UV200_MSLP': '200 hPa Geopotential Height, Wind, & MSLP',

        # Wind & shear
        'UV850_MSLP': '850 hPa Wind & MSLP',
        'SHDL': 'Deep-Layer (200-850 hPa) Wind Shear',
        'SHML': 'Mid-Layer (500-850 hPa) Wind Shear',
        'SHSL': 'Shallow-Layer (700-850 hPa) Wind Shear',
        'SFDL': 'Deep-Layer (250-850 hPa) Steering Flow',
        'SFML': 'Mid-Layer (500-850 hPa) Steering Flow',
        'SFSL': 'Shallow-Layer (700-850 hPa) Steering Flow',

        # Potential vorticity
        'PV200_UV200_MSLP':
            '200 hPa Potential Vorticity, Wind, & MSLP',

        # CAPE / Helicity
        'CAPE_HLCY_MSLP': 'CAPE, Storm-Relative Helicity, & MSLP',

        # Temperature profiles
        'T850_UV850_MSLP': '850 hPa Temperature, Wind, & MSLP',

        # Precipitation
        'PRCP_MSLP': 'Precipitation Rate & MSLP',
        'PRATE_MSLP': 'Precipitation Rate & MSLP',
        'TPRCP_MSLP': 'Total Precipitation & MSLP',
        'PRCP24_MSLP': '24-h Precipitation Rate & MSLP',
        'TPRCP_U10_MSLP': 'Total Precipitation, 10-m Wind, & MSLP',
        'PRCP_MSLP_Z1000500':
            'Precipitation Rate, MSLP, & 1000-500 hPa Thickness',
        'PRATE_MSLP_Z1000500':
            'Precipitation Rate, MSLP, & 1000-500 hPa Thickness',

        # TPW / reflectivity
        'TPW_MSLP': 'Total Precipitable Water & MSLP',
        'REFL_MSLP': 'Composite Reflectivity & MSLP',
        'REFD_UV750': '750 hPa Reflectivity & Wind',

        # Air-sea / surface fluxes
        'SST_T2': 'SST, 2-m Temperature, 10-m Wind, & MSLP',
        'DPT2_SST': '2-m Dewpoint, SST, 10-m Wind, & MSLP',
        'LHFLX_MSLP': 'Latent Heat Flux, 10-m Wind, & MSLP',
        'SHFLX_MSLP': 'Sensible Heat Flux, 10-m Wind, & MSLP',

        # Simulated satellite. Titles trimmed from the legacy
        # "Simulated XX Brightness Temperature ..." form so the
        # left-aligned title row doesn't overflow into the
        # right-aligned VMAX/MSLP block on narrow domains.
        'SIMIR':       'Simulated IR (Band 13, 10.3 um)',
        'SIMIR_SHDL':  'Simulated IR (B13) & 200-850 hPa Shear',
        'SIMWV_UPPER': 'Simulated WV Upper Level (Band 8, 6.2 um)',
        'SIMWV_MID':   'Simulated WV Mid Level (Band 9, 6.9 um)',
        'SBTAGR13toa': 'Simulated IR (Band 13, 10.3 um)',
        'SBTAGR8toa':  'Simulated WV Upper Level (Band 8, 6.2 um)',
        'SBTAGR9toa':  'Simulated WV Mid Level (Band 9, 6.9 um)',

        # SHIPS-style vortex diagnostics
        'TCCEN': 'Center Fixes (Geopotential Height Centroid)',
        'TCCEN_zoom': 'Center Fixes (Geopotential Height Centroid) [zoom]',
        'TCHODO': 'Near-Storm Hodograph',
        'TCHODO_zoom': 'Near-Storm Hodograph [zoom]',
        'UV_NS': 'Meridional Cross-Section: Wind',
        'UV_EW': 'Zonal Cross-Section: Wind',
    }
    return _TITLE_MAP.get(var_name, var_name.replace('_', ' '))
