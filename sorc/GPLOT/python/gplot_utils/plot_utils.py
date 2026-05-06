"""
Plot utilities for GPLOT Python modules.

Replaces GenPlotRes() from GPLOT_util.ncl and the add_*_title procedures
from GPLOT_func.ncl. Provides cartopy map setup, title/label placement,
storm marker drawing, and output file handling (PNG->GIF conversion,
whitespace trimming).
"""

import os
import subprocess
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

    Resolution order:
      1. Explicit ``cartopy_dir`` argument (typically from
         ``nml.get('CARTOPY_DIR')``).
      2. ``CARTOPY_DATA_DIR`` environment variable (lets a modulefile
         set a host-wide fallback when the namelist hasn't been
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
            ``/home/role.aoml-hafs1/.local/share/cartopy``). If empty
            or None, the env var is consulted; if that's also empty,
            the function is a no-op.
    """
    source = 'CARTOPY_DIR namelist entry'
    chosen = cartopy_dir
    if not chosen:
        chosen = os.environ.get('CARTOPY_DATA_DIR', '')
        source = 'CARTOPY_DATA_DIR env var'

    if not chosen:
        # Diagnostic so the HPC log shows exactly why the offline
        # cache wasn't wired up. Use print() in addition to the
        # logger so it lands in stdout regardless of logging config.
        msg = ('MSG: configure_cartopy: no CARTOPY_DIR (namelist) and no '
               'CARTOPY_DATA_DIR (env) -- cartopy will attempt downloads')
        print(msg)
        logger.info(msg)
        return

    if not os.path.isdir(chosen):
        msg = (f'MSG: configure_cartopy: path {chosen!r} (from {source}) '
               f'does not exist -- cartopy will attempt downloads')
        print(msg)
        logger.warning(msg)
        return

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
    Convert a PNG file to GIF using ImageMagick.

    Parameters
    ----------
    png_path : str
        Path to the PNG file.
    remove_png : bool, optional
        If True, remove the PNG after conversion (default True).

    Returns
    -------
    str
        Path to the created GIF file, or the original PNG if conversion failed.
    """
    gif_path = png_path.replace('.png', '.gif')

    try:
        result = subprocess.run(
            ['convert', png_path, '+repage', f'gif:{gif_path}'],
            capture_output=True, text=True, timeout=60
        )
        if result.returncode == 0:
            if remove_png:
                os.remove(png_path)
            logger.debug(f"Converted to GIF: {gif_path}")
            return gif_path
        else:
            logger.warning(f"ImageMagick convert failed: {result.stderr}")
            return png_path
    except (subprocess.TimeoutExpired, FileNotFoundError) as e:
        logger.warning(f"GIF conversion failed: {e}")
        return png_path


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

        # Simulated satellite. Title shortened from "Simulated IR
        # Brightness Temperature ..." so the left-aligned title row
        # doesn't overflow into the right-aligned VMAX/MSLP block.
        'SIMIR': 'Simulated IR (Band 13, 10.3 um)',
        'SBTAGR13toa': 'Simulated IR (Band 13, 10.3 um)',

        # SHIPS-style vortex diagnostics
        'TCCEN': 'Center Fixes (Geopotential Height Centroid)',
        'TCCEN_zoom': 'Center Fixes (Geopotential Height Centroid) [zoom]',
        'TCHODO': 'Near-Storm Hodograph',
        'TCHODO_zoom': 'Near-Storm Hodograph [zoom]',
        'UV_NS': 'Meridional Cross-Section: Wind',
        'UV_EW': 'Zonal Cross-Section: Wind',
    }
    return _TITLE_MAP.get(var_name, var_name.replace('_', ' '))
