#!/usr/bin/env python3
"""
GPLOT Stats Module - Multi-model track/intensity comparison plots.

Replaces sorc/GPLOT/ncl/GPLOT_stats.ncl (~5400 lines).
Reads ATCF A-deck (model forecasts) and B-deck (best track) files,
produces guidance, trend, and lifetime graphics.

Usage:
    python GPLOT_stats.py --idate 2025102300 --sid 13L --master-nml /path/to/namelist

Plot types produced:
  Guidance:  Track, TrackIntensity, Intensity, Pressure (late & early)
  Trends:    Track, Intensity, Pressure overlaid for last N cycles
  Lifetime:  All-cycle track and intensity spaghetti plots
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
import matplotlib.colors as mcolors
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import numpy as np
import cartopy.crs as ccrs
import cartopy.feature as cfeature

# Add the parent directory to sys.path for gplot_utils import
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from gplot_utils.namelist import read_master_namelist, read_stats_namelist
from gplot_utils.atcf import (read_atcf, read_bdeck, parse_storm_info,
                                derive_longsid, walk_files_depth_limited,
                                atcf_from_listfile)
from gplot_utils.plot_utils import (save_figure, add_disclaimer,
                                    add_storm_marker, configure_cartopy,
                                    sweep_orphan_pngs)

logger = logging.getLogger('GPLOT_stats')


# ============================================================
# X11 -> matplotlib color normalization
# ============================================================

# NCL/ModelInfo.dat uses X11 rgb.txt color names, some of which
# (the numeric-shade suffix variants like `deeppink4`, `pink1`,
# `royalblue3`) are not part of matplotlib's CSS4 color set.  We
# translate them to hex via the standard X11 rgb.txt values.
_X11_RGB_OVERRIDES = {
    # deeppink
    'deeppink1': '#FF1493', 'deeppink2': '#EE1289',
    'deeppink3': '#CD1076', 'deeppink4': '#8B0A50',
    # pink
    'pink1': '#FFB5C5', 'pink2': '#EEA9B8',
    'pink3': '#CD919E', 'pink4': '#8B636C',
    # hotpink
    'hotpink1': '#FF6EB4', 'hotpink2': '#EE6AA7',
    'hotpink3': '#CD6090', 'hotpink4': '#8B3A62',
    # red
    'red1': '#FF0000', 'red2': '#EE0000',
    'red3': '#CD0000', 'red4': '#8B0000',
    # orange
    'orange1': '#FFA500', 'orange2': '#EE9A00',
    'orange3': '#CD8500', 'orange4': '#8B5A00',
    # plum
    'plum1': '#FFBBFF', 'plum2': '#EEAEEE',
    'plum3': '#CD96CD', 'plum4': '#8B668B',
    # salmon
    'salmon1': '#FF8C69', 'salmon2': '#EE8262',
    'salmon3': '#CD7054', 'salmon4': '#8B4C39',
    # rosybrown
    'rosybrown1': '#FFC1C1', 'rosybrown2': '#EEB4B4',
    'rosybrown3': '#CD9B9B', 'rosybrown4': '#8B6969',
    # seagreen
    'seagreen1': '#54FF9F', 'seagreen2': '#4EEE94',
    'seagreen3': '#43CD80', 'seagreen4': '#2E8B57',
    # royalblue
    'royalblue1': '#4876FF', 'royalblue2': '#436EEE',
    'royalblue3': '#3A5FCD', 'royalblue4': '#27408B',
    # goldenrod
    'goldenrod1': '#FFC125', 'goldenrod2': '#EEB422',
    'goldenrod3': '#CD9B1D', 'goldenrod4': '#8B6914',
    # cadetblue
    'cadetblue1': '#98F5FF', 'cadetblue2': '#8EE5EE',
    'cadetblue3': '#7AC5CD', 'cadetblue4': '#53868B',
    # darkolivegreen
    'darkolivegreen1': '#CAFF70', 'darkolivegreen2': '#BCEE68',
    'darkolivegreen3': '#A2CD5A', 'darkolivegreen4': '#6E8B3D',
    # purple
    'purple1': '#9B30FF', 'purple2': '#912CEE',
    'purple3': '#7D26CD', 'purple4': '#551A8B',
    # mediumpurple
    'mediumpurple1': '#AB82FF', 'mediumpurple2': '#9F79EE',
    'mediumpurple3': '#8968CD', 'mediumpurple4': '#5D478B',
    # maroon
    'maroon1': '#FF34B3', 'maroon2': '#EE30A7',
    'maroon3': '#CD2990', 'maroon4': '#8B1C62',
    # sienna
    'sienna1': '#FF8247', 'sienna2': '#EE7942',
    'sienna3': '#CD6839', 'sienna4': '#8B4726',
    # tomato
    'tomato1': '#FF6347', 'tomato2': '#EE5C42',
    'tomato3': '#CD4F39', 'tomato4': '#8B3626',
    # violetred (no suffix and 1-4 variants; matplotlib has only
    # mediumvioletred/palevioletred)
    'violetred':  '#D02090',
    'violetred1': '#FF3E96', 'violetred2': '#EE3A8C',
    'violetred3': '#CD3278', 'violetred4': '#8B2252',
    # skyblue
    'skyblue1': '#87CEFF', 'skyblue2': '#7EC0EE',
    'skyblue3': '#6CA6CD', 'skyblue4': '#4A708B',
    # turquoise
    'turquoise1': '#00F5FF', 'turquoise2': '#00E5EE',
    'turquoise3': '#00C5CD', 'turquoise4': '#00868B',
}

# gray1..gray100 / grey1..grey100 -> matplotlib grayscale float string
_GRAY_RE = re.compile(r'^(gr[ea]y)(\d+)$', re.IGNORECASE)


def _normalize_color(name):
    """Translate X11/NCL-style color names into matplotlib-acceptable values.

    Handles numeric-suffix variants (`deeppink4`, `pink1`, ...),
    `gray<N>`/`grey<N>` (0-100), and otherwise defers to matplotlib's
    own name resolution.  Unknown names fall back to black with a warning.
    """
    if not name:
        return 'black'
    lname = name.lower()
    if lname in _X11_RGB_OVERRIDES:
        return _X11_RGB_OVERRIDES[lname]
    m = _GRAY_RE.match(lname)
    if m:
        try:
            pct = int(m.group(2))
            if 0 <= pct <= 100:
                return str(pct / 100.0)
        except ValueError:
            pass
    try:
        mcolors.to_rgba(name)
        return name
    except (ValueError, TypeError):
        logger.warning(
            f"Unrecognized color '{name}', falling back to black")
        return 'black'


# ============================================================
# Model info loader
# ============================================================

def load_model_info(gplot_dir):
    """
    Load model color/marker/name info.

    Prefers `tbl/ModelInfo.python.dat` (matplotlib-native markers and
    colors).  Falls back to legacy `tbl/ModelInfo.dat` (NCL marker
    indices and NCL X11 color names), translating via
    `_ncl_marker_to_mpl` and `_normalize_color`.

    Returns dict mapping model_id -> {color, marker, long_name}.
    """
    tbl_dir = os.path.join(gplot_dir, 'tbl')
    python_path = os.path.join(tbl_dir, 'ModelInfo.python.dat')
    legacy_path = os.path.join(tbl_dir, 'ModelInfo.dat')

    if os.path.isfile(python_path):
        info_path = python_path
        is_python_format = True
    elif os.path.isfile(legacy_path):
        info_path = legacy_path
        is_python_format = False
        logger.info(
            f"Using legacy ModelInfo.dat (NCL format); "
            f"consider migrating to ModelInfo.python.dat")
    else:
        logger.warning(
            f"No ModelInfo table found in {tbl_dir}")
        return {}

    models = {}
    with open(info_path, 'r') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#'):
                continue
            parts = [p.strip() for p in line.split(',')]
            if len(parts) < 4:
                continue
            mid = parts[0].strip()
            long_name = parts[1].strip()
            marker_token = parts[2].strip()
            color_token = parts[3].strip()

            if is_python_format:
                # Marker is already a matplotlib marker string.
                mpl_marker = marker_token if marker_token else 'o'
                # Color is expected to be matplotlib-valid already,
                # but run it through the normalizer as a safety net
                # (handles hex, xkcd:, CSS4 names, grayscale floats,
                # and any lingering X11 variants).
                color = _normalize_color(color_token)
            else:
                try:
                    marker = int(marker_token)
                except ValueError:
                    marker = 'o'
                mpl_marker = _ncl_marker_to_mpl(marker)
                color = _normalize_color(color_token)

            models[mid] = {
                'color': color,
                'marker': mpl_marker,
                'long_name': long_name,
            }

    return models


def _ncl_marker_to_mpl(ncl_idx):
    """Map NCL marker index to matplotlib marker string."""
    _MAP = {
        0: 'o', 1: '+', 2: '*', 3: '.', 4: 'x',
        5: 's', 6: 'D', 7: '^', 8: 'v', 9: '<',
        10: '>', 11: 'p', 12: 'h', 13: '8', 14: 'd',
        15: 'P', 16: 'X',
    }
    if isinstance(ncl_idx, int):
        return _MAP.get(ncl_idx, 'o')
    return 'o'


# Default fallback colors for models not in ModelInfo.dat
_DEFAULT_COLORS = [
    'red', 'blue', 'green', 'orange', 'purple', 'brown', 'cyan',
    'magenta', 'olive', 'teal', 'navy', 'maroon', 'coral', 'gold',
    'darkviolet', 'deepskyblue', 'limegreen', 'tomato',
]


def get_model_style(model_id, model_info, idx=0):
    """Get color and marker for a model, with fallback."""
    if model_id in model_info:
        return model_info[model_id]['color'], model_info[model_id]['marker']
    return _DEFAULT_COLORS[idx % len(_DEFAULT_COLORS)], 'o'


# ============================================================
# ATCF data extraction helpers
# ============================================================

def extract_model_data(adeck_df, model_list, idate, max_fhr=126, dt=6):
    """
    Extract forecast data arrays for each model from the merged A-deck.

    Returns dict: model_id -> DataFrame with fhr, lat, lon, vmax, mslp.
    """
    result = {}
    for model_id in model_list:
        mask = ((adeck_df['model'] == model_id) &
                (adeck_df['cycle'] == idate) &
                (adeck_df['fhr'] <= max_fhr) &
                (adeck_df['fhr'] % dt == 0))
        mdf = adeck_df[mask].sort_values('fhr').reset_index(drop=True)
        if len(mdf) > 0:
            result[model_id] = mdf
    return result


def extract_trend_data(adeck_df, model_id, idate, n_trend=6,
                       max_fhr=126, dt=6):
    """
    Extract trend data: last n_trend cycles for a model.

    Returns list of (cycle_str, DataFrame) tuples, most recent last.
    """
    # Find all cycles for this model
    model_mask = adeck_df['model'] == model_id
    all_cycles = sorted(adeck_df[model_mask]['cycle'].unique())

    # Keep only cycles <= idate
    all_cycles = [c for c in all_cycles if c <= idate]

    # Filter orphaned cycles (gap > 168h from idate)
    idate_dt = datetime.strptime(idate, '%Y%m%d%H')
    valid_cycles = []
    for c in all_cycles:
        try:
            c_dt = datetime.strptime(c, '%Y%m%d%H')
            if abs((idate_dt - c_dt).total_seconds()) <= 168 * 3600:
                valid_cycles.append(c)
        except ValueError:
            continue

    # Take last n_trend
    trend_cycles = valid_cycles[-n_trend:]

    result = []
    for cyc in trend_cycles:
        mask = ((adeck_df['model'] == model_id) &
                (adeck_df['cycle'] == cyc) &
                (adeck_df['fhr'] <= max_fhr) &
                (adeck_df['fhr'] % dt == 0))
        mdf = adeck_df[mask].sort_values('fhr').reset_index(drop=True)
        if len(mdf) > 0:
            result.append((cyc, mdf))

    return result


def extract_lifetime_data(adeck_df, model_id, idate, max_fhr=126, dt=6):
    """
    Extract all available cycles for a model (lifetime spaghetti).

    Returns list of (cycle_str, DataFrame) tuples.
    """
    model_mask = adeck_df['model'] == model_id
    all_cycles = sorted(adeck_df[model_mask]['cycle'].unique())

    # Filter to cycles <= idate and within 168h
    idate_dt = datetime.strptime(idate, '%Y%m%d%H')
    valid_cycles = []
    for c in all_cycles:
        try:
            c_dt = datetime.strptime(c, '%Y%m%d%H')
            if c <= idate and abs((idate_dt - c_dt).total_seconds()) <= 168 * 3600:
                valid_cycles.append(c)
        except ValueError:
            continue

    result = []
    for cyc in valid_cycles:
        mask = ((adeck_df['model'] == model_id) &
                (adeck_df['cycle'] == cyc) &
                (adeck_df['fhr'] <= max_fhr) &
                (adeck_df['fhr'] % dt == 0))
        mdf = adeck_df[mask].sort_values('fhr').reset_index(drop=True)
        if len(mdf) > 0:
            result.append((cyc, mdf))

    return result


# ============================================================
# Map bounds computation
# ============================================================

def compute_map_bounds(model_data, bdeck_df=None, padding_frac=0.05):
    """
    Compute map bounds that encompass all model tracks and best track.
    """
    all_lats = []
    all_lons = []

    for mid, mdf in model_data.items():
        all_lats.extend(mdf['lat'].tolist())
        all_lons.extend(mdf['lon'].tolist())

    if bdeck_df is not None and len(bdeck_df) > 0:
        all_lats.extend(bdeck_df['lat'].tolist())
        all_lons.extend(bdeck_df['lon'].tolist())

    if not all_lats:
        return (-90, 90, -180, 180)

    lat_min, lat_max = min(all_lats), max(all_lats)

    # Dateline-aware longitude bounds. A CPac/WPac track crossing 180
    # has signed lons jumping +179.9 -> -179.9, so a naive min/max
    # spans nearly the whole globe and the panel collapses into a
    # world-wide strip with every track huddled at one edge. Compute
    # the span in both the signed [-180, 180] and [0, 360] conventions
    # and keep whichever is tighter -- storm tracks are compact, so
    # the right convention is unambiguous. When the [0, 360] form wins
    # (only happens for 180-crossing tracks) the returned bounds have
    # lon_e > 180, which _guidance_projection pairs with a
    # central_longitude=180 map downstream, mirroring the GPLOT_maps
    # convention for dateline-crossing panels.
    lons_signed = [((l + 180.0) % 360.0) - 180.0 for l in all_lons]
    lons_360 = [l % 360.0 for l in all_lons]
    span_signed = max(lons_signed) - min(lons_signed)
    span_360 = max(lons_360) - min(lons_360)
    # Signed convention wins ties: when a track straddles neither seam
    # the two spans are equal up to float rounding, and the 1-degree
    # margin keeps modulo noise from flipping an ordinary Atlantic
    # panel onto the shifted projection. A genuine 180-crossing makes
    # span_360 smaller by tens of degrees, far past the margin.
    if span_360 < span_signed - 1.0:
        lon_min, lon_max = min(lons_360), max(lons_360)
    else:
        lon_min, lon_max = min(lons_signed), max(lons_signed)

    rng_lat = lat_max - lat_min
    rng_lon = lon_max - lon_min

    # Pad
    pad_lat = max(rng_lat * padding_frac, 2.0)
    pad_lon = max(rng_lon * padding_frac, 2.0)

    lat_s = lat_min - pad_lat
    lat_n = lat_max + pad_lat
    lon_w = lon_min - pad_lon
    lon_e = lon_max + pad_lon

    # Enforce aspect ratio
    rng_lat = lat_n - lat_s
    rng_lon = lon_e - lon_w

    if rng_lat > 0 and rng_lon > 0:
        ratio = rng_lat / rng_lon
        if ratio < 0.5:
            expand = 0.5 * (0.5 * rng_lon - rng_lat)
            lat_n += expand
            lat_s -= expand
        elif ratio > 2.0:
            expand = 0.5 * (2.0 * rng_lat - rng_lon)
            lon_e += expand
            lon_w -= expand

    return (lat_n, lat_s, lon_w, lon_e)


def _wrap_track_lons(df, lon_w, lon_e):
    """
    Return a copy of ``df`` with its 'lon' column wrapped to the
    convention of the panel bounds ([0, 360] when the panel crosses
    the dateline, signed [-180, 180] otherwise).

    Track lines are drawn with matplotlib Line2D artists, whose
    vertices are projected point-by-point with no antimeridian
    splitting (unlike contour geometries) -- a signed +179 -> -179
    jump in a dateline-crossing track otherwise draws a full-width
    horizontal streak across the panel. Wrapping the data into the
    bounds convention makes consecutive vertices numerically adjacent
    so the segments stay contiguous.
    """
    if df is None or len(df) == 0 or 'lon' not in df:
        return df
    if lon_e > 180 or lon_w < -180:
        return df.assign(lon=df['lon'] % 360.0)
    return df.assign(lon=((df['lon'] + 180.0) % 360.0) - 180.0)


def _guidance_projection(lon_w, lon_e):
    """
    Map projection for a guidance panel given its lon bounds.

    Bounds with an edge past +-180 come from compute_map_bounds
    choosing the [0, 360] convention for a dateline-crossing track;
    they need cartopy's central_longitude=180 or set_extent silently
    falls back to a global extent. Track lines/markers keep
    ``transform=ccrs.PlateCarree()`` -- cartopy wraps each vertex into
    the shifted frame, and per-point overlays are too small for the
    transform!=projection cost that matters on gridded maps panels.
    """
    if lon_e > 180 or lon_w < -180:
        return ccrs.PlateCarree(central_longitude=180)
    return ccrs.PlateCarree()


# ============================================================
# Plotting functions
# ============================================================

def plot_track_guidance(model_data, bdeck_df, model_info, idate,
                        longsid, expt, odir, variant='late',
                        do_gif=True, do_trim=True, do_markers=True,
                        do_fhr_labels=True, do_disclaimer=True):
    """
    Plot late/early track guidance map with multi-model tracks.
    """
    if not model_data:
        logger.warning("No model data for track guidance")
        return None

    bounds = compute_map_bounds(model_data, bdeck_df)
    lat_n, lat_s, lon_w, lon_e = bounds
    bdeck_df = _wrap_track_lons(bdeck_df, lon_w, lon_e)
    model_data = {mid: _wrap_track_lons(mdf, lon_w, lon_e)
                  for mid, mdf in model_data.items()}

    fig = plt.figure(figsize=(12, 10))
    ax = fig.add_subplot(1, 1, 1,
                         projection=_guidance_projection(lon_w, lon_e))
    ax.set_extent([lon_w, lon_e, lat_s, lat_n], crs=ccrs.PlateCarree())

    ax.add_feature(cfeature.COASTLINE, linewidth=0.8)
    ax.add_feature(cfeature.BORDERS, linewidth=0.5, edgecolor='gray')
    ax.add_feature(cfeature.LAND, facecolor='lightgray', alpha=0.3)
    gl = ax.gridlines(draw_labels=True, linewidth=0.5, alpha=0.5,
                      linestyle='--')
    gl.top_labels = False
    gl.right_labels = False

    legend_handles = []

    # Plot best track
    if bdeck_df is not None and len(bdeck_df) > 0:
        ax.plot(bdeck_df['lon'], bdeck_df['lat'], 'k-', linewidth=2.5,
                transform=ccrs.PlateCarree(), zorder=5, label='Best Track')
        # Storm markers at each position
        for _, row in bdeck_df.iterrows():
            add_storm_marker(ax, row['lat'], row['lon'],
                             intensity=row.get('vmax'))

    # Plot model tracks
    for idx, (model_id, mdf) in enumerate(model_data.items()):
        color, marker = get_model_style(model_id, model_info, idx)
        label = model_info.get(model_id, {}).get('long_name', model_id)

        ax.plot(mdf['lon'], mdf['lat'], '-', color=color, linewidth=1.5,
                transform=ccrs.PlateCarree(), zorder=3)

        if do_markers:
            # Markers every 24h
            mk_mask = mdf['fhr'] % 24 == 0
            ax.plot(mdf.loc[mk_mask, 'lon'], mdf.loc[mk_mask, 'lat'],
                    marker=marker, color=color, linestyle='none',
                    markersize=6, markeredgecolor='black',
                    markeredgewidth=0.3, transform=ccrs.PlateCarree(),
                    zorder=4, label=label)
        else:
            # Invisible marker for legend only
            ax.plot([], [], '-', color=color, linewidth=1.5, label=label)

        if do_fhr_labels:
            for _, row in mdf[mk_mask].iterrows():
                ax.text(row['lon'] + 0.3, row['lat'] + 0.3,
                        f"{row['fhr']:.0f}", fontsize=6, color=color,
                        transform=ccrs.PlateCarree(), zorder=6)

    # Title
    idate_str = idate
    try:
        idt = datetime.strptime(idate, '%Y%m%d%H')
        idate_str = idt.strftime('%Y%m%d%H')
    except ValueError:
        pass

    ax.set_title(f'{expt}\nTrack Guidance ({variant.capitalize()})\n'
                 f'Init: {idate_str}',
                 fontsize=11, fontweight='bold', loc='left')
    ax.set_title(f'{longsid.upper()}', fontsize=11, color='brown',
                 loc='right')

    # Legend
    ax.legend(fontsize=7, loc='lower left', ncol=2, framealpha=0.8)

    if do_disclaimer:
        add_disclaimer(ax)

    ofile = os.path.join(odir, f'TrackGuidance.{longsid}.{idate}.{variant}')
    return save_figure(fig, ofile, do_trim=do_trim, do_gif=do_gif)


def plot_intensity_guidance(model_data, bdeck_df, model_info, idate,
                            longsid, expt, odir, variant='late',
                            max_fhr=126, do_gif=True, do_trim=True,
                            do_markers=True, do_disclaimer=True):
    """
    Plot late/early intensity (Vmax) guidance line plot.
    """
    if not model_data:
        return None

    fig, ax = plt.subplots(figsize=(12, 8))

    # Best track
    if bdeck_df is not None and len(bdeck_df) > 0 and 'lead_time' in bdeck_df.columns:
        bt = bdeck_df[(bdeck_df['lead_time'] >= -24) &
                      (bdeck_df['lead_time'] <= max_fhr)]
        if len(bt) > 0:
            ax.plot(bt['lead_time'], bt['vmax'], 'k-', linewidth=2.5,
                    label='Best Track', zorder=5)

    # Model forecasts
    for idx, (model_id, mdf) in enumerate(model_data.items()):
        color, marker = get_model_style(model_id, model_info, idx)
        label = model_info.get(model_id, {}).get('long_name', model_id)

        ax.plot(mdf['fhr'], mdf['vmax'], '-', color=color, linewidth=1.5,
                label=label, zorder=3)
        if do_markers:
            mk = mdf[mdf['fhr'] % 24 == 0]
            ax.plot(mk['fhr'], mk['vmax'], marker=marker, color=color,
                    linestyle='none', markersize=5, zorder=4)

    # Saffir-Simpson category lines
    for thresh, cat_label in [(34, 'TS'), (64, 'Cat1'), (83, 'Cat2'),
                               (96, 'Cat3'), (113, 'Cat4'), (137, 'Cat5')]:
        ax.axhline(y=thresh, color='lightgray', linestyle='--',
                   linewidth=0.5, zorder=1)

    ax.set_xlabel('Forecast Hour', fontsize=12)
    ax.set_ylabel('Max Wind Speed (kt)', fontsize=12)
    ax.set_xlim(0, max_fhr)
    ax.set_title(f'{expt}\nIntensity Guidance ({variant.capitalize()})\n'
                 f'Init: {idate}',
                 fontsize=11, fontweight='bold', loc='left')
    ax.set_title(f'{longsid.upper()}', fontsize=11, color='brown',
                 loc='right')
    ax.legend(fontsize=7, loc='best', ncol=2, framealpha=0.8)
    ax.grid(True, alpha=0.3)

    if do_disclaimer:
        ax.text(0.5, -0.08, '*Experimental Product of NOAA/AOML/HRD*',
                transform=ax.transAxes, fontsize=8, ha='center', color='black')

    ofile = os.path.join(odir,
                         f'IntensityGuidance.{longsid}.{idate}.{variant}')
    return save_figure(fig, ofile, do_trim=do_trim, do_gif=do_gif)


def plot_pressure_guidance(model_data, bdeck_df, model_info, idate,
                           longsid, expt, odir, variant='late',
                           max_fhr=126, do_gif=True, do_trim=True,
                           do_markers=True, do_disclaimer=True):
    """
    Plot late pressure (MSLP) guidance line plot.
    """
    if not model_data:
        return None

    fig, ax = plt.subplots(figsize=(12, 8))

    # Best track
    if bdeck_df is not None and len(bdeck_df) > 0 and 'lead_time' in bdeck_df.columns:
        bt = bdeck_df[(bdeck_df['lead_time'] >= -24) &
                      (bdeck_df['lead_time'] <= max_fhr) &
                      (bdeck_df['mslp'] > 0)]
        if len(bt) > 0:
            ax.plot(bt['lead_time'], bt['mslp'], 'k-', linewidth=2.5,
                    label='Best Track', zorder=5)

    # Model forecasts
    for idx, (model_id, mdf) in enumerate(model_data.items()):
        color, marker = get_model_style(model_id, model_info, idx)
        label = model_info.get(model_id, {}).get('long_name', model_id)

        prs = mdf[mdf['mslp'] > 0]
        if len(prs) > 0:
            ax.plot(prs['fhr'], prs['mslp'], '-', color=color,
                    linewidth=1.5, label=label, zorder=3)
            if do_markers:
                mk = prs[prs['fhr'] % 24 == 0]
                ax.plot(mk['fhr'], mk['mslp'], marker=marker, color=color,
                        linestyle='none', markersize=5, zorder=4)

    ax.set_xlabel('Forecast Hour', fontsize=12)
    ax.set_ylabel('Min Sea Level Pressure (hPa)', fontsize=12)
    ax.set_xlim(0, max_fhr)
    ax.invert_yaxis()
    ax.set_title(f'{expt}\nPressure Guidance ({variant.capitalize()})\n'
                 f'Init: {idate}',
                 fontsize=11, fontweight='bold', loc='left')
    ax.set_title(f'{longsid.upper()}', fontsize=11, color='brown',
                 loc='right')
    ax.legend(fontsize=7, loc='best', ncol=2, framealpha=0.8)
    ax.grid(True, alpha=0.3)

    ofile = os.path.join(odir,
                         f'PressureGuidance.{longsid}.{idate}.{variant}')
    return save_figure(fig, ofile, do_trim=do_trim, do_gif=do_gif)


def plot_track_intensity_guidance(model_data, bdeck_df, model_info, idate,
                                  longsid, expt, odir, variant='late',
                                  do_gif=True, do_trim=True,
                                  do_disclaimer=True):
    """
    Plot track guidance map with intensity-colored track segments.
    """
    if not model_data:
        return None

    bounds = compute_map_bounds(model_data, bdeck_df)
    lat_n, lat_s, lon_w, lon_e = bounds
    bdeck_df = _wrap_track_lons(bdeck_df, lon_w, lon_e)
    model_data = {mid: _wrap_track_lons(mdf, lon_w, lon_e)
                  for mid, mdf in model_data.items()}

    fig = plt.figure(figsize=(12, 10))
    ax = fig.add_subplot(1, 1, 1,
                         projection=_guidance_projection(lon_w, lon_e))
    ax.set_extent([lon_w, lon_e, lat_s, lat_n], crs=ccrs.PlateCarree())

    ax.add_feature(cfeature.COASTLINE, linewidth=0.8)
    ax.add_feature(cfeature.BORDERS, linewidth=0.5, edgecolor='gray')
    ax.add_feature(cfeature.LAND, facecolor='lightgray', alpha=0.3)
    gl = ax.gridlines(draw_labels=True, linewidth=0.5, alpha=0.5,
                      linestyle='--')
    gl.top_labels = False
    gl.right_labels = False

    # Saffir-Simpson color thresholds
    def _ss_color(vmax):
        if vmax < 34:
            return 'blue'
        elif vmax < 64:
            return 'green'
        elif vmax < 83:
            return 'yellow'
        elif vmax < 96:
            return 'orange'
        elif vmax < 113:
            return 'red'
        elif vmax < 137:
            return 'darkred'
        return 'purple'

    # Best track
    if bdeck_df is not None and len(bdeck_df) > 0:
        for i in range(len(bdeck_df) - 1):
            r0 = bdeck_df.iloc[i]
            r1 = bdeck_df.iloc[i + 1]
            ax.plot([r0['lon'], r1['lon']], [r0['lat'], r1['lat']],
                    color=_ss_color(r0['vmax']), linewidth=3,
                    transform=ccrs.PlateCarree(), zorder=5)

    # Model tracks with intensity colors
    for idx, (model_id, mdf) in enumerate(model_data.items()):
        if len(mdf) < 2:
            continue
        for i in range(len(mdf) - 1):
            r0 = mdf.iloc[i]
            r1 = mdf.iloc[i + 1]
            ax.plot([r0['lon'], r1['lon']], [r0['lat'], r1['lat']],
                    color=_ss_color(r0['vmax']), linewidth=1.5,
                    transform=ccrs.PlateCarree(), zorder=3)

    ax.set_title(f'{expt}\nTrack & Intensity Guidance ({variant.capitalize()})\n'
                 f'Init: {idate}',
                 fontsize=11, fontweight='bold', loc='left')
    ax.set_title(f'{longsid.upper()}', fontsize=11, color='brown',
                 loc='right')

    if do_disclaimer:
        add_disclaimer(ax)

    ofile = os.path.join(odir,
                         f'TrackIntensityGuidance.{longsid}.{idate}.{variant}')
    return save_figure(fig, ofile, do_trim=do_trim, do_gif=do_gif)


def plot_track_trend(trend_data, bdeck_df, idate, model_id,
                     longsid, expt, odir, do_gif=True, do_trim=True,
                     do_disclaimer=True):
    """
    Plot track trend: last N cycles overlaid on a map.
    """
    if not trend_data:
        return None

    # Collect all positions for bounds
    all_model_data = {}
    for i, (cyc, mdf) in enumerate(trend_data):
        all_model_data[f'{model_id}_{cyc}'] = mdf

    bounds = compute_map_bounds(all_model_data, bdeck_df)
    lat_n, lat_s, lon_w, lon_e = bounds
    bdeck_df = _wrap_track_lons(bdeck_df, lon_w, lon_e)
    trend_data = [(cyc, _wrap_track_lons(mdf, lon_w, lon_e))
                  for cyc, mdf in trend_data]

    fig = plt.figure(figsize=(12, 10))
    ax = fig.add_subplot(1, 1, 1,
                         projection=_guidance_projection(lon_w, lon_e))
    ax.set_extent([lon_w, lon_e, lat_s, lat_n], crs=ccrs.PlateCarree())

    ax.add_feature(cfeature.COASTLINE, linewidth=0.8)
    ax.add_feature(cfeature.BORDERS, linewidth=0.5, edgecolor='gray')
    ax.add_feature(cfeature.LAND, facecolor='lightgray', alpha=0.3)
    gl = ax.gridlines(draw_labels=True, linewidth=0.5, alpha=0.5,
                      linestyle='--')
    gl.top_labels = False
    gl.right_labels = False

    # Best track
    if bdeck_df is not None and len(bdeck_df) > 0:
        ax.plot(bdeck_df['lon'], bdeck_df['lat'], 'k-', linewidth=2.5,
                transform=ccrs.PlateCarree(), zorder=5, label='Best Track')

    # Color ramp: light to dark blue (newest = darkest)
    n = len(trend_data)
    blues = plt.cm.Blues(np.linspace(0.3, 0.9, max(n, 1)))

    for i, (cyc, mdf) in enumerate(trend_data):
        color = blues[i]
        lw = 1.0 + 0.5 * (i / max(n - 1, 1))
        label = cyc
        ax.plot(mdf['lon'], mdf['lat'], '-', color=color, linewidth=lw,
                transform=ccrs.PlateCarree(), zorder=3, label=label)

    ax.set_title(f'{expt} ({model_id})\nTrack Trend (Last {n} Cycles)\n'
                 f'Init: {idate}',
                 fontsize=11, fontweight='bold', loc='left')
    ax.set_title(f'{longsid.upper()}', fontsize=11, color='brown',
                 loc='right')
    ax.legend(fontsize=7, loc='lower left', framealpha=0.8)

    ofile = os.path.join(odir,
                         f'TrackTrend.{model_id}.{longsid}.{idate}')
    return save_figure(fig, ofile, do_trim=do_trim, do_gif=do_gif)


def plot_lifetime_tracks(lifetime_data, bdeck_df, idate, model_id,
                         longsid, expt, odir, do_gif=True, do_trim=True):
    """
    Plot all-cycle lifetime track spaghetti.
    """
    if not lifetime_data:
        return None

    all_model_data = {}
    for i, (cyc, mdf) in enumerate(lifetime_data):
        all_model_data[f'{model_id}_{cyc}'] = mdf

    bounds = compute_map_bounds(all_model_data, bdeck_df)
    lat_n, lat_s, lon_w, lon_e = bounds
    bdeck_df = _wrap_track_lons(bdeck_df, lon_w, lon_e)
    lifetime_data = [(cyc, _wrap_track_lons(mdf, lon_w, lon_e))
                     for cyc, mdf in lifetime_data]

    fig = plt.figure(figsize=(12, 10))
    ax = fig.add_subplot(1, 1, 1,
                         projection=_guidance_projection(lon_w, lon_e))
    ax.set_extent([lon_w, lon_e, lat_s, lat_n], crs=ccrs.PlateCarree())

    ax.add_feature(cfeature.COASTLINE, linewidth=0.8)
    ax.add_feature(cfeature.BORDERS, linewidth=0.5, edgecolor='gray')
    ax.add_feature(cfeature.LAND, facecolor='lightgray', alpha=0.3)
    gl = ax.gridlines(draw_labels=True, linewidth=0.5, alpha=0.5,
                      linestyle='--')
    gl.top_labels = False
    gl.right_labels = False

    # Best track
    if bdeck_df is not None and len(bdeck_df) > 0:
        ax.plot(bdeck_df['lon'], bdeck_df['lat'], 'k-', linewidth=2.5,
                transform=ccrs.PlateCarree(), zorder=5)

    # Rainbow colors for cycles
    n = len(lifetime_data)
    colors = plt.cm.rainbow(np.linspace(0, 1, max(n, 1)))

    for i, (cyc, mdf) in enumerate(lifetime_data):
        ax.plot(mdf['lon'], mdf['lat'], '-', color=colors[i],
                linewidth=0.8, alpha=0.7,
                transform=ccrs.PlateCarree(), zorder=3)

    ax.set_title(f'{expt} ({model_id})\nLifetime Tracks ({n} Cycles)\n'
                 f'Through: {idate}',
                 fontsize=11, fontweight='bold', loc='left')
    ax.set_title(f'{longsid.upper()}', fontsize=11, color='brown',
                 loc='right')

    ofile = os.path.join(odir,
                         f'AllTracks.{model_id}.{longsid}.{idate}')
    return save_figure(fig, ofile, do_trim=do_trim, do_gif=do_gif)


def plot_intensity_trend(trend_data, bdeck_df, idate, model_id,
                         longsid, expt, odir, max_fhr=126,
                         do_gif=True, do_trim=True):
    """
    Plot intensity trend: last N cycles overlaid.
    Uses valid-time offset axis.
    """
    if not trend_data:
        return None

    fig, ax = plt.subplots(figsize=(12, 8))
    idate_dt = datetime.strptime(idate, '%Y%m%d%H')

    # Best track
    if bdeck_df is not None and len(bdeck_df) > 0 and 'lead_time' in bdeck_df.columns:
        bt = bdeck_df[(bdeck_df['lead_time'] >= -30) &
                      (bdeck_df['lead_time'] <= max_fhr)]
        if len(bt) > 0:
            ax.plot(bt['lead_time'], bt['vmax'], 'k-', linewidth=2.5,
                    label='Best Track', zorder=5)

    n = len(trend_data)
    blues = plt.cm.Blues(np.linspace(0.3, 0.9, max(n, 1)))

    for i, (cyc, mdf) in enumerate(trend_data):
        cyc_dt = datetime.strptime(cyc, '%Y%m%d%H')
        offset = (cyc_dt - idate_dt).total_seconds() / 3600.0
        valid_hrs = mdf['fhr'].values + offset

        ax.plot(valid_hrs, mdf['vmax'], '-', color=blues[i],
                linewidth=1.0 + 0.5 * (i / max(n - 1, 1)),
                label=cyc, zorder=3)

    # SS thresholds
    for thresh in [34, 64, 83, 96, 113, 137]:
        ax.axhline(y=thresh, color='lightgray', linestyle='--',
                   linewidth=0.5, zorder=1)

    ax.set_xlabel('Forecast Hour (relative to current cycle)', fontsize=12)
    ax.set_ylabel('Max Wind Speed (kt)', fontsize=12)
    ax.set_xlim(-30, max_fhr)
    ax.set_title(f'{expt} ({model_id})\nIntensity Trend (Last {n} Cycles)\n'
                 f'Init: {idate}',
                 fontsize=11, fontweight='bold', loc='left')
    ax.set_title(f'{longsid.upper()}', fontsize=11, color='brown',
                 loc='right')
    ax.legend(fontsize=7, loc='best', framealpha=0.8)
    ax.grid(True, alpha=0.3)

    ofile = os.path.join(odir,
                         f'IntensityTrend.{model_id}.{longsid}.{idate}')
    return save_figure(fig, ofile, do_trim=do_trim, do_gif=do_gif)


def plot_lifetime_intensity(lifetime_data, bdeck_df, idate, model_id,
                            longsid, expt, odir, max_fhr=126,
                            do_gif=True, do_trim=True):
    """
    Plot all-cycle lifetime intensity spaghetti.
    """
    if not lifetime_data:
        return None

    fig, ax = plt.subplots(figsize=(12, 8))
    idate_dt = datetime.strptime(idate, '%Y%m%d%H')

    # Best track
    if bdeck_df is not None and len(bdeck_df) > 0 and 'lead_time' in bdeck_df.columns:
        bt = bdeck_df[(bdeck_df['lead_time'] >= -168) &
                      (bdeck_df['lead_time'] <= max_fhr)]
        if len(bt) > 0:
            ax.plot(bt['lead_time'], bt['vmax'], 'k-', linewidth=2.5,
                    zorder=5)

    n = len(lifetime_data)
    colors = plt.cm.rainbow(np.linspace(0, 1, max(n, 1)))

    for i, (cyc, mdf) in enumerate(lifetime_data):
        cyc_dt = datetime.strptime(cyc, '%Y%m%d%H')
        offset = (cyc_dt - idate_dt).total_seconds() / 3600.0
        valid_hrs = mdf['fhr'].values + offset

        ax.plot(valid_hrs, mdf['vmax'], '-', color=colors[i],
                linewidth=0.8, alpha=0.7, zorder=3)

    ax.set_xlabel('Forecast Hour (relative to current cycle)', fontsize=12)
    ax.set_ylabel('Max Wind Speed (kt)', fontsize=12)
    ax.set_title(f'{expt} ({model_id})\nLifetime Intensity ({n} Cycles)\n'
                 f'Through: {idate}',
                 fontsize=11, fontweight='bold', loc='left')
    ax.set_title(f'{longsid.upper()}', fontsize=11, color='brown',
                 loc='right')
    ax.grid(True, alpha=0.3)

    ofile = os.path.join(odir,
                         f'AllIntensity.{model_id}.{longsid}.{idate}')
    return save_figure(fig, ofile, do_trim=do_trim, do_gif=do_gif)


def plot_pressure_trend(trend_data, bdeck_df, idate, model_id,
                        longsid, expt, odir, max_fhr=126,
                        do_gif=True, do_trim=True):
    """
    Plot pressure trend for a single model (MCODE).
    """
    if not trend_data:
        return None

    fig, ax = plt.subplots(figsize=(12, 8))
    idate_dt = datetime.strptime(idate, '%Y%m%d%H')

    # Best track
    if bdeck_df is not None and len(bdeck_df) > 0 and 'lead_time' in bdeck_df.columns:
        bt = bdeck_df[(bdeck_df['lead_time'] >= -30) &
                      (bdeck_df['lead_time'] <= max_fhr) &
                      (bdeck_df['mslp'] > 0)]
        if len(bt) > 0:
            ax.plot(bt['lead_time'], bt['mslp'], 'k-', linewidth=2.5,
                    label='Best Track', zorder=5)

    n = len(trend_data)
    blues = plt.cm.Blues(np.linspace(0.3, 0.9, max(n, 1)))

    for i, (cyc, mdf) in enumerate(trend_data):
        cyc_dt = datetime.strptime(cyc, '%Y%m%d%H')
        offset = (cyc_dt - idate_dt).total_seconds() / 3600.0
        valid_hrs = mdf['fhr'].values + offset

        prs = mdf[mdf['mslp'] > 0]
        valid_hrs_p = prs['fhr'].values + offset

        if len(prs) > 0:
            ax.plot(valid_hrs_p, prs['mslp'], '-', color=blues[i],
                    linewidth=1.0 + 0.5 * (i / max(n - 1, 1)),
                    label=cyc, zorder=3)

    ax.set_xlabel('Forecast Hour (relative to current cycle)', fontsize=12)
    ax.set_ylabel('Min Sea Level Pressure (hPa)', fontsize=12)
    ax.set_xlim(-30, max_fhr)
    ax.invert_yaxis()
    ax.set_title(f'{expt} ({model_id})\nPressure Trend (Last {n} Cycles)\n'
                 f'Init: {idate}',
                 fontsize=11, fontweight='bold', loc='left')
    ax.set_title(f'{longsid.upper()}', fontsize=11, color='brown',
                 loc='right')
    ax.legend(fontsize=7, loc='best', framealpha=0.8)
    ax.grid(True, alpha=0.3)

    ofile = os.path.join(odir,
                         f'PressureTrend.{model_id}.{longsid}.{idate}')
    return save_figure(fig, ofile, do_trim=do_trim, do_gif=do_gif)


# ============================================================
# Main entry point
# ============================================================

def parse_args():
    parser = argparse.ArgumentParser(
        description='GPLOT Stats Module - Multi-model guidance/trend plots')
    parser.add_argument('--idate', required=True,
                        help='Initialization date (YYYYMMDDHH)')
    parser.add_argument('--sid', required=True,
                        help='Storm ID (e.g., 13L)')
    parser.add_argument('--master-nml', default='namelist.master.default',
                        help='Master namelist path')
    parser.add_argument('--force', action='store_true',
                        help='Force re-production')
    parser.add_argument('--ensid', default='',
                        help='Ensemble member ID')
    parser.add_argument('--odir', default=None,
                        help='Override output directory')
    parser.add_argument('--atcf-dir', default=None,
                        help='Override ATCF search directory')
    parser.add_argument('-v', '--verbose', action='count', default=0,
                        help='Increase verbosity')
    return parser.parse_args()


def _status_path(odir, sidlong):
    """Build the spawn_stats-compatible status file path."""
    return os.path.join(odir, f'status.{sidlong}.log')


def _write_status(status_file, value):
    """Write module status used by spawn_stats.sh."""
    os.makedirs(os.path.dirname(status_file) or '.', exist_ok=True)
    with open(status_file, 'w') as f:
        f.write(f'{value}\n')
    logger.info(f"Wrote stats status: {value} -> {status_file}")


def _sidlong_from_atcf_filename(atcf_file, sid):
    """
    Parse SIDLONG (e.g., '13l' or 'ten10l') from ATCF basename.

    spawn_stats.sh derives the same token from ATCF filename segments and
    uses it in status.<sidlong>.log, so mirror that convention here.
    """
    base = os.path.basename(atcf_file)
    for part in base.split('.'):
        # Match spawn_stats.sh behavior:
        #   if the last 3 chars are [0-9]{2}[a-z], use the FULL token.
        # This preserves forms like "ten10l" (not just "10l").
        if len(part) >= 3 and re.match(r'^[0-9]{2}[A-Za-z]$',
                                       part[-3:]):
            return part.lower()
    return sid.lower()


def main():
    args = parse_args()

    # Configure logging
    log_level = logging.WARNING
    if args.verbose >= 1:
        log_level = logging.INFO
    if args.verbose >= 2:
        log_level = logging.DEBUG
    logging.basicConfig(level=log_level,
                        format='%(name)s %(levelname)s: %(message)s')

    # Determine GPLOT_DIR
    gplot_dir = os.environ.get('GPLOT_DIR')
    if gplot_dir is None:
        # Derive from script location
        # Script lives at <GPLOT>/sorc/GPLOT/python/GPLOT_stats.py, so
        # three "..".join()s get us back to the GPLOT root.
        gplot_dir = os.path.abspath(
            os.path.join(os.path.dirname(__file__), '..', '..', '..'))
    os.environ['GPLOT_DIR'] = gplot_dir

    # Read master namelist
    nml_path = args.master_nml
    if not os.path.isabs(nml_path):
        # Search in parm/ directory
        parm_path = os.path.join(gplot_dir, 'parm', nml_path)
        if os.path.isfile(parm_path):
            nml_path = parm_path
    nml = read_master_namelist(nml_path)
    configure_cartopy(nml.get('CARTOPY_DIR'))

    # Override with CLI args
    idate = args.idate or nml.get('IDATE', '')
    sid = args.sid or nml.get('SID', '')
    force = args.force or nml.get('FORCE', False)
    ensid = args.ensid or nml.get('ENSID', '')
    # Normalize the deterministic sentinels to an empty member tag so the
    # output path has no member subdir for non-ensemble runs (batch_stats.sh
    # passes "XX" for deterministic). "00" remains a valid ensemble member.
    if ensid.strip().upper() in ('XX', 'MISSING', '0', ''):
        ensid = ''
    else:
        ensid = ensid.strip()

    expt = nml.get('EXPT', 'GPLOT')
    dsource = nml.get('DSOURCE', 'HAFS')
    mcode = nml.get('MCODE', dsource)
    odir_base = args.odir or nml.get('ODIR', '.')
    odir_type = nml.get('ODIR_TYPE', 1)
    max_fhr = nml.get('FNL_HR', 126)
    dt = nml.get('DT', 6)
    n_trend = nml.get('NTREND', 6)
    do_gif = nml.get('DO_CONVERTGIF', True)
    do_trim = nml.get('DO_RMWHITE', True)
    do_markers = nml.get('DO_MARKERS', True)
    do_fhr_labels = nml.get('DO_FHRLABELS', True)
    do_disclaimer = nml.get('DO_DISCLAIMER', True)

    # Model lists from namelist (NCL uses TRKM, INTM, PRSM, etc.)
    trk_models = nml.get('TRKM', nml.get('TRKMODELS', [mcode]))
    int_models = nml.get('INTM', nml.get('INTMODELS', [mcode]))
    prs_models = nml.get('PRSM', nml.get('PRSMODELS', [mcode]))
    trk_models_t = nml.get('TRKMT', nml.get('TRKMODELST', [mcode]))
    int_models_t = nml.get('INTMT', nml.get('INTMODELST', [mcode]))
    prs_models_t = nml.get('PRSMT', nml.get('PRSMODELST', [mcode]))

    if isinstance(trk_models, str):
        trk_models = [trk_models]
    if isinstance(int_models, str):
        int_models = [int_models]
    if isinstance(prs_models, str):
        prs_models = [prs_models]
    if isinstance(trk_models_t, str):
        trk_models_t = [trk_models_t]
    if isinstance(int_models_t, str):
        int_models_t = [int_models_t]
    if isinstance(prs_models_t, str):
        prs_models_t = [prs_models_t]

    # ATCF directories
    atcf1_dir = nml.get('ATCF1_DIR', '')
    atcf2_dir = nml.get('ATCF2_DIR', '')
    bdeck_dir = nml.get('BDECK_DIR', '')
    adeck_dir = nml.get('ADECK_DIR', '')

    # CLI override for ATCF directory
    if args.atcf_dir:
        atcf2_dir = args.atcf_dir
        atcf1_dir = args.atcf_dir

    # Output directory. The ensemble member tag (empty for deterministic) sits
    # between the cycle (idate) and 'guidance', matching the member subdir that
    # spawn_stats.sh creates (ODIR/EXPT/CYCLE/ENSID/guidance). os.path.join
    # drops the empty component, so deterministic paths are unchanged.
    if odir_type == 0:
        odir = os.path.join(odir_base, expt, idate, ensid, 'guidance')
    else:
        odir = os.path.join(odir_base, ensid, 'guidance')
    os.makedirs(odir, exist_ok=True)
    status_sidlong = sid.lower()
    status_file = _status_path(odir, status_sidlong)

    logger.info(f"GPLOT_stats: IDATE={idate}, SID={sid}, EXPT={expt}")
    logger.info(f"  Output: {odir}")

    # Load model info
    model_info = load_model_info(gplot_dir)

    # Read stats namelist
    stats_nml_path = os.path.join(gplot_dir, 'parm',
                                   f'namelist.stats.{expt}')
    if not os.path.isfile(stats_nml_path):
        stats_nml_path = os.path.join(gplot_dir, 'parm',
                                       'namelist.stats.default')
    stats_flags = {}
    if os.path.isfile(stats_nml_path):
        stats_flags = read_stats_namelist(stats_nml_path)

    do_guidance = stats_flags.get('GUIDANCE', True)
    do_trends = stats_flags.get('TRENDS', True)
    do_lt_guide = stats_flags.get('LATE_TK_GUIDE', True)
    do_lti_guide = stats_flags.get('LATE_TKINT_GUIDE', True)
    do_li_guide = stats_flags.get('LATE_INT_GUIDE', True)
    do_lp_guide = stats_flags.get('LATE_PRS_GUIDE', True)
    # Early-cycle (IDATE+6h) variants matching the NCL TrackGuidance
    # and IntensityGuidance .early panels.
    do_et_guide = stats_flags.get('EARLY_TK_GUIDE', True)
    do_ei_guide = stats_flags.get('EARLY_INT_GUIDE', True)
    do_lt_trend = stats_flags.get('LATE_TK_TREND', True)
    do_li_trend = stats_flags.get('LATE_INT_TREND', True)
    do_lp_trend = stats_flags.get('LATE_PRS_TREND', True)

    # Next cycle (used by the early-variant guidance plots).
    try:
        idate_early = (datetime.strptime(idate, '%Y%m%d%H')
                       + timedelta(hours=6)).strftime('%Y%m%d%H')
    except ValueError:
        idate_early = None

    # --------------------------------------------------------
    # Step 1: Find ATCF file and determine LONGSID
    # --------------------------------------------------------
    atcf_dirs = [d for d in [atcf2_dir, atcf1_dir] if d]
    atcf_file = None
    atcf_tag = nml.get('ATCF2_TAG', nml.get('ATCF1_TAG', ''))

    # Search for ATCF file (prefer files with trak/atcf in name).
    # When ATCF2_TAG / ATCF1_TAG is set in the namelist (e.g.
    # `hfsb_multistorm.trak.atcfunix`), prefer files whose basename
    # contains that tag so we don't accidentally pick up sibling
    # files like `.parent.trak.atcfunix` or `.storm2.trak.atcfunix`
    # that happen to glob-match the same SID/date pattern.  The
    # namelist-configured tag is assumed to point at the merged /
    # highest-resolution track file the experiment wants to use.
    _ATCF_EXTENSIONS = ('atcfunix', '.dat', 'atcf')

    def _rank_match(path):
        """Sort key: smaller is better.  Prefer exact ATCF tag match,
        then files without '.parent.' in the name, then lexicographic."""
        bn = os.path.basename(path)
        tag_match = 0 if (atcf_tag and atcf_tag in bn) else 1
        parent_penalty = 1 if '.parent.' in bn else 0
        return (tag_match, parent_penalty, bn)

    for adir in atcf_dirs:
        if not os.path.isdir(adir):
            continue
        patterns = [
            os.path.join(adir, f'*{sid.lower()}*{idate}*trak*'),
            os.path.join(adir, f'*{sid.lower()}*{idate}*atcf*'),
            os.path.join(adir, f'*{sid.lower()}*{idate}*.dat'),
            os.path.join(adir, f'*{idate}*{sid.lower()}*trak*'),
            os.path.join(adir, f'*{sid.lower()}*{idate}*'),
        ]
        for pat in patterns:
            matches = glob.glob(pat)
            # Filter out binary files (GRIB2, idx) and per-fhr / .all
            # variants that are intermediate outputs, not the final
            # merged track.
            matches = [m for m in matches
                       if not m.endswith(('.grb2', '.grb', '.idx',
                                          '.grib2', '.orig'))]
            matches = [m for m in matches
                       if not any(os.path.basename(m).endswith(sfx)
                                  for sfx in ('.all',))
                       and not re.search(r'\.f\d{3,4}$',
                                         os.path.basename(m))]
            if matches:
                matches.sort(key=_rank_match)
                atcf_file = matches[0]
                break
        if atcf_file:
            break

    # Tier 2: bounded recursive walk over the same ATCF dirs. The flat globs
    # above are non-recursive, so a track nested under e.g.
    # com/<cycle>/<storm>/ is missed when ATCF*_DIR points higher. Depth-capped
    # at 4 so a large ATCF*_DIR can't trigger an unbounded walk. Mirrors the
    # robustness in maps/ships find_atcf_file.
    if atcf_file is None:
        walked = [full for full, bn in
                  walk_files_depth_limited(atcf_dirs, max_depth=4)
                  if sid.lower() in bn.lower() and idate in bn
                  and ('trak' in bn.lower() or 'atcf' in bn.lower()
                       or bn.endswith('.dat'))
                  and not full.endswith(('.grb2', '.grb', '.idx',
                                         '.grib2', '.orig'))
                  and not bn.endswith('.all')
                  and not re.search(r'\.f\d{3,4}$', bn)]
        if walked:
            walked.sort(key=_rank_match)
            atcf_file = walked[0]

    # Tier 3: fall back to the spawn-written ATCF_FILES.dat in the output dir
    # (the spawn already resolved the path via a recursive find), matching what
    # polar/airsea/maps/ships do.
    if atcf_file is None:
        atcf_file = atcf_from_listfile(odir, sid)
        if atcf_file:
            logger.warning(f"ATCF not found under namelist dirs; using spawn's "
                           f"ATCF_FILES.dat --> {atcf_file}")

    if atcf_file is None:
        logger.error(f"No ATCF file found for SID={sid}, IDATE={idate}")
        _write_status(status_file, 'failed')
        sys.exit(1)

    status_sidlong = _sidlong_from_atcf_filename(atcf_file, sid)
    status_file = _status_path(odir, status_sidlong)
    _write_status(status_file, 'working')

    logger.info(f"Using ATCF: {atcf_file}")

    # Derive the ATCF short ID (e.g., 'al132025') directly from the CLI
    # --sid and cycle year rather than from the filename.  HAFS filenames
    # like '08l.2023082618.hfsb_multistorm.parent.trak' don't follow the
    # compact "{name}{NN}{b}" form parse_storm_info expects, so feeding
    # the filename in produced garbage basin/number values and broke the
    # NHC A-deck / B-deck lookup.
    _basin_map = {
        'l': 'al', 'e': 'ep', 'c': 'cp', 'w': 'wp',
        's': 'sh', 'p': 'sh', 'a': 'io', 'b': 'io',
    }
    try:
        snum = sid[:2]
        basin1 = sid[2].lower()
        basin2 = _basin_map.get(basin1, '')
    except (IndexError, AttributeError):
        snum = '00'
        basin1 = ''
        basin2 = ''
    sid2 = f"{basin2}{snum}{idate[:4]}"
    # longsid is derived after the B-deck read below so the storm-name
    # column-28 fallback is available when the ATCF filename is bare
    # (e.g., '13l.2025102100.hfsb_multistorm.trak.atcfunix').
    longsid = sid.lower()

    # --------------------------------------------------------
    # Step 2: Read ATCF data
    # --------------------------------------------------------
    # Read every wind-radii row, then collapse the (cycle, model, fhr)
    # multiplicity to a single row per record. ATCF stores operational
    # dynamical-model forecasts as three rows per fhr (wr=34/50/64) and
    # statistical models as a single wr=0 row; filtering by an exact
    # threshold drops one camp or the other. lat/lon/vmax/mslp are
    # identical across the three threshold rows, so keep='first' is
    # safe for every downstream plotter.
    adeck_df = read_atcf(atcf_file, wind_radii=None)
    if len(adeck_df) > 0:
        adeck_df = adeck_df.drop_duplicates(
            subset=['cycle', 'model', 'fhr'], keep='first'
        ).reset_index(drop=True)

    if len(adeck_df) == 0:
        logger.error("No valid ATCF data read")
        _write_status(status_file, 'failed')
        sys.exit(1)

    # Relabel the originating model (MORIG) with the display code
    # (MCODE) so downstream lookups in TRKM/INTM/PRSM lists and
    # ModelInfo.dat use the experiment's preferred label.  This mirrors
    # the NCL GPLOT behavior of rewriting the ADECK file in-place with
    # sed 's/MORIG/MCODE/g', but done in-memory on the DataFrame.
    morig = nml.get('MORIG', mcode)
    if morig and morig != mcode:
        n_renamed = (adeck_df['model'] == morig).sum()
        if n_renamed > 0:
            adeck_df.loc[adeck_df['model'] == morig, 'model'] = mcode
            logger.info(f"Relabeled {n_renamed} rows: "
                        f"{morig} -> {mcode}")

    # Also try to read NHC A-deck for additional models. _merge_atcf
    # de-duplicates by (cycle, model, fhr) so the wr=34/50/64
    # multiplicity collapses on the merge.
    if adeck_dir:
        nhc_adeck = os.path.join(adeck_dir, f'a{sid2}.dat')
        if not os.path.isdir(adeck_dir):
            logger.warning(f"ADECK_DIR does not exist on this host: "
                           f"{adeck_dir}")
        elif not os.path.isfile(nhc_adeck):
            logger.warning(f"NHC A-deck not found: {nhc_adeck}")
        else:
            nhc_df = read_atcf(nhc_adeck, wind_radii=None)
            if len(nhc_df) > 0:
                adeck_df = _merge_atcf(adeck_df, nhc_df)
                logger.info(f"Merged NHC A-deck: {nhc_adeck}")
    n_models = adeck_df['model'].nunique()
    logger.info(f"ATCF after merge: {len(adeck_df)} rows, "
                f"{n_models} distinct models: "
                f"{sorted(adeck_df['model'].unique())}")

    # Read B-deck
    bdeck_df = None
    if bdeck_dir:
        bdeck_path = os.path.join(bdeck_dir, f'b{sid2}.dat')
        if os.path.isfile(bdeck_path):
            bdeck_df = read_bdeck(bdeck_path, idate=idate)
            logger.info(f"Read B-deck: {len(bdeck_df)} rows")

    # Now that both A-deck and B-deck are loaded, resolve the long
    # storm id used in output filenames and titles. Priority:
    # ATCF filename's '<name><sid>' prefix (legacy NCL convention) ->
    # B-deck column-28 storm_name at idate ->
    # A-deck column-28 storm_name at idate -> bare sid.
    # Pass both DataFrames separately so derive_longsid can fall
    # through to the operational a-deck's per-cycle name when the
    # b-deck doesn't have a record for the run cycle.
    longsid = derive_longsid(atcf_file, sid, bdeck_df,
                             idate=idate, adeck_df=adeck_df)
    logger.info(f"LONGSID resolved to: {longsid}")

    # --------------------------------------------------------
    # Step 3: Guidance plots
    # --------------------------------------------------------
    if do_guidance:
        logger.info("=== Step 3: Guidance Plots ===")

        if do_lt_guide:
            trk_data = extract_model_data(adeck_df, trk_models, idate,
                                          max_fhr, dt)
            result = plot_track_guidance(
                trk_data, bdeck_df, model_info, idate, longsid, expt,
                odir, variant='late', do_gif=do_gif, do_trim=do_trim,
                do_markers=do_markers, do_fhr_labels=do_fhr_labels,
                do_disclaimer=do_disclaimer)
            if result:
                logger.info(f"  Created: {result}")

        if do_et_guide and idate_early is not None:
            trk_data_early = extract_model_data(adeck_df, trk_models,
                                                idate_early, max_fhr, dt)
            result = plot_track_guidance(
                trk_data_early, bdeck_df, model_info, idate_early,
                longsid, expt, odir, variant='early', do_gif=do_gif,
                do_trim=do_trim, do_markers=do_markers,
                do_fhr_labels=do_fhr_labels,
                do_disclaimer=do_disclaimer)
            if result:
                logger.info(f"  Created: {result}")

        if do_lti_guide:
            trk_data = extract_model_data(adeck_df, trk_models, idate,
                                          max_fhr, dt)
            result = plot_track_intensity_guidance(
                trk_data, bdeck_df, model_info, idate, longsid, expt,
                odir, variant='late', do_gif=do_gif, do_trim=do_trim,
                do_disclaimer=do_disclaimer)
            if result:
                logger.info(f"  Created: {result}")

        if do_li_guide:
            int_data = extract_model_data(adeck_df, int_models, idate,
                                          max_fhr, dt)
            result = plot_intensity_guidance(
                int_data, bdeck_df, model_info, idate, longsid, expt,
                odir, variant='late', max_fhr=max_fhr, do_gif=do_gif,
                do_trim=do_trim, do_markers=do_markers,
                do_disclaimer=do_disclaimer)
            if result:
                logger.info(f"  Created: {result}")

        if do_ei_guide and idate_early is not None:
            int_data_early = extract_model_data(adeck_df, int_models,
                                                idate_early, max_fhr, dt)
            result = plot_intensity_guidance(
                int_data_early, bdeck_df, model_info, idate_early,
                longsid, expt, odir, variant='early', max_fhr=max_fhr,
                do_gif=do_gif, do_trim=do_trim, do_markers=do_markers,
                do_disclaimer=do_disclaimer)
            if result:
                logger.info(f"  Created: {result}")

        if do_lp_guide:
            prs_data = extract_model_data(adeck_df, prs_models, idate,
                                          max_fhr, dt)
            result = plot_pressure_guidance(
                prs_data, bdeck_df, model_info, idate, longsid, expt,
                odir, variant='late', max_fhr=max_fhr, do_gif=do_gif,
                do_trim=do_trim, do_markers=do_markers,
                do_disclaimer=do_disclaimer)
            if result:
                logger.info(f"  Created: {result}")

    # --------------------------------------------------------
    # Step 4: Trend and Lifetime plots
    # --------------------------------------------------------
    if do_trends:
        logger.info("=== Step 4: Trend & Lifetime Plots ===")

        for model_id in trk_models_t:
            if do_lt_trend:
                trend_data = extract_trend_data(
                    adeck_df, model_id, idate, n_trend, max_fhr, dt)
                result = plot_track_trend(
                    trend_data, bdeck_df, idate, model_id, longsid,
                    expt, odir, do_gif=do_gif, do_trim=do_trim,
                    do_disclaimer=do_disclaimer)
                if result:
                    logger.info(f"  Created: {result}")

                # Also produce lifetime tracks
                life_data = extract_lifetime_data(
                    adeck_df, model_id, idate, max_fhr, dt)
                result = plot_lifetime_tracks(
                    life_data, bdeck_df, idate, model_id, longsid,
                    expt, odir, do_gif=do_gif, do_trim=do_trim)
                if result:
                    logger.info(f"  Created: {result}")

        for model_id in int_models_t:
            if do_li_trend:
                trend_data = extract_trend_data(
                    adeck_df, model_id, idate, n_trend, max_fhr, dt)
                result = plot_intensity_trend(
                    trend_data, bdeck_df, idate, model_id, longsid,
                    expt, odir, max_fhr=max_fhr, do_gif=do_gif,
                    do_trim=do_trim)
                if result:
                    logger.info(f"  Created: {result}")

                # Also produce lifetime intensity
                life_data = extract_lifetime_data(
                    adeck_df, model_id, idate, max_fhr, dt)
                result = plot_lifetime_intensity(
                    life_data, bdeck_df, idate, model_id, longsid,
                    expt, odir, max_fhr=max_fhr, do_gif=do_gif,
                    do_trim=do_trim)
                if result:
                    logger.info(f"  Created: {result}")

        # Pressure trend (single model = MCODE)
        if do_lp_trend:
            trend_data = extract_trend_data(
                adeck_df, mcode, idate, n_trend, max_fhr, dt)
            result = plot_pressure_trend(
                trend_data, bdeck_df, idate, mcode, longsid,
                expt, odir, max_fhr=max_fhr, do_gif=do_gif,
                do_trim=do_trim)
            if result:
                logger.info(f"  Created: {result}")

    # Retry-convert any orphan .png left behind by transient ImageMagick
    # failures. If the retry also fails, write status='incomplete' so
    # the workflow re-invokes us.
    sweep_result = sweep_orphan_pngs(odir)
    if sweep_result.get('still_failed', 0) > 0:
        logger.warning(
            f"GPLOT_stats: {sweep_result['still_failed']} PNG(s) still "
            f"unconverted after sweep; writing status='incomplete'.")
        _write_status(status_file, 'incomplete')
    else:
        _write_status(status_file, 'complete')
    logger.info("GPLOT_stats complete.")


def _merge_atcf(df1, df2):
    """Merge two ATCF DataFrames, deduplicating by cycle+model+fhr."""
    merged = pd.concat([df1, df2], ignore_index=True)
    # Keep last occurrence (matches NCL's tac | sort -u behavior)
    merged = merged.drop_duplicates(
        subset=['cycle', 'model', 'fhr'], keep='last')
    return merged.sort_values(['model', 'cycle', 'fhr']).reset_index(drop=True)


# Need pandas for _merge_atcf
import pandas as pd


if __name__ == '__main__':
    main()
