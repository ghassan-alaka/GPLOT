"""
Colormap and contour level definitions for GPLOT.

Replaces defineCMAP_name(), defineCMAP_fill(), and defineLevels() from
GPLOT_util.ncl. Provides a registry mapping (variable, level) pairs to
matplotlib colormaps and contour level arrays.

Supports loading colormaps from:
  - NCL .rgb format files (sorc/GPLOT/python/colormaps/) — kept for the
    published banding conventions of the maps module's reflectivity, IR
    BT, MSLP, wind, precip, and shear palettes. No NCL runtime dep; the
    file format is just RGB triples.
  - Python .txt format files (sorc/GPLOT/python/colormaps/) — normalized
    0-1 RGB triples.
  - Built-in matplotlib colormaps
"""

import os
import logging

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors

logger = logging.getLogger(__name__)

# Cache for loaded colormaps
_cmap_cache = {}


def load_rgb_colormap(filepath, name=None):
    """
    Load an NCL-format .rgb colormap file.

    NCL .rgb format: first line may be 'ncolors=N' or just start with
    RGB triplets (0-255 integer or 0-1 float range).

    Parameters
    ----------
    filepath : str
        Path to the .rgb file.
    name : str, optional
        Name for the colormap.

    Returns
    -------
    matplotlib.colors.ListedColormap
    """
    if filepath in _cmap_cache:
        return _cmap_cache[filepath]

    if not os.path.isfile(filepath):
        raise FileNotFoundError(f"Colormap file not found: {filepath}")

    if name is None:
        name = os.path.splitext(os.path.basename(filepath))[0]

    # Read lines, skip comments and header
    data = []
    with open(filepath, 'r') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#') or line.startswith(';'):
                continue
            if line.lower().startswith('ncolors'):
                continue

            parts = line.split()
            if len(parts) >= 3:
                try:
                    r, g, b = float(parts[0]), float(parts[1]), float(parts[2])
                    data.append([r, g, b])
                except ValueError:
                    continue

    if not data:
        raise ValueError(f"No valid RGB data in {filepath}")

    colors = np.array(data)

    # Normalize to 0-1 if values are in 0-255 range
    if colors.max() > 1.0:
        colors = colors / 255.0

    cmap = mcolors.ListedColormap(colors, name=name)
    _cmap_cache[filepath] = cmap
    return cmap


def load_txt_colormap(filepath, name=None):
    """
    Load a Python-format .txt colormap file (normalized 0-1 RGB floats).

    Parameters
    ----------
    filepath : str
        Path to the .txt colormap file.
    name : str, optional
        Name for the colormap.

    Returns
    -------
    matplotlib.colors.ListedColormap
    """
    if filepath in _cmap_cache:
        return _cmap_cache[filepath]

    if name is None:
        name = os.path.splitext(os.path.basename(filepath))[0]

    colors = np.genfromtxt(filepath)
    if colors.ndim != 2 or colors.shape[1] < 3:
        raise ValueError(f"Invalid colormap format in {filepath}")

    colors = colors[:, :3]  # Take only RGB columns
    if colors.max() > 1.0:
        colors = colors / 255.0

    cmap = mcolors.ListedColormap(colors, name=name)
    _cmap_cache[filepath] = cmap
    return cmap


# Registry: maps (variable, level_hint) -> colormap name or file
# This replaces defineCMAP_name() from GPLOT_util.ncl
_CMAP_REGISTRY = {
    # Wind fields
    ('UV', ''): 'tcwinds1.rgb',
    ('UV', '10'): 'tcwinds1.rgb',
    ('UV10', ''): 'tcwinds1.rgb',

    # Vorticity. Use 'hot_r' (white -> yellow -> red -> black) so
    # low/negative values fade to white while strong cyclonic cores
    # stand out as black. build_discrete_cmap reserves the first
    # (white) and last (black) samples for set_under / set_over, so
    # negative RVO (below the 0-kt floor) also renders white.
    ('RVO', ''): 'hot_r',
    ('AVO', ''): 'RdBu_r',

    # Temperature
    ('T', ''): 'RdBu_r',
    ('T', '2'): 'jet',
    ('T2', ''): 'jet',
    ('DPT', ''): 'BrBG',
    ('SST', ''): 'jet',

    # Surface fluxes (W/m^2)
    ('LHFLX', ''): 'plasma',
    ('SHFLX', ''): 'plasma',

    # CAPE / helicity
    ('CAPE', ''): 'YlOrRd',
    ('HLCY', ''): 'YlOrRd',

    # Potential vorticity (10^-6 K m^2 kg^-1 s^-1 -- PVU)
    ('PV', ''): 'RdBu_r',

    # Relative humidity
    ('RH', ''): 'BrBG',

    # Geopotential height. Use a rainbow-style colormap so the narrow
    # dynamic range of tropical upper-level heights shows meaningful
    # contrast (viridis is too perceptually flat in the green-yellow
    # band that dominates 500 hPa data).
    ('HGT', ''): 'turbo',

    # MSLP
    ('MSLP', ''): 'mslp2.rgb',

    # Precipitation
    ('PRCP', ''): 'tprcp2.rgb',
    ('TPRCP', ''): 'tprcp2.rgb',
    ('PRATE', ''): 'tprcp2.rgb',

    # TPW
    ('TPW', ''): 'BrBG',

    # Reflectivity
    ('REFL', ''): 'REFD.rgb',
    ('REFD', ''): 'REFD.rgb',

    # Shear (deep / mid / shallow-layer). Uses a refined 17-color
    # palette: white <5 kt, pale green 5-10, green 10-15, yellow 15-20,
    # orange 20-25, red 25-30, darkening reds 30-50, dark red at 50+
    # with progressively darker shades above 55 kt.
    ('SHDL', ''): 'shear2.rgb',

    # Divergence (SHIPS)
    ('D200', ''): 'RdBu_r',

    # Simulated IR brightness temperature (model satellite output).
    # McIDAS-style palette: magenta/pink for the coldest cloud tops,
    # deepening reds for strong convection, yellow/green through the
    # mid-troposphere, and grays/browns for warm surfaces.
    ('SIMIR', ''): 'irsat.rgb',
    ('SBTAGR13toa', ''): 'irsat.rgb',

    # --- Ocean maps ---
    # Isotherm depths (m): Blues_r so shallow = dark blue, deep = white
    ('ISO26', ''): 'Blues_r',
    ('ISO20', ''): 'Blues_r',
    # Ocean heat content (kJ/cm²): warm = deep red
    ('OHC', ''): 'Reds',
    ('OHC', 'HYCOM'): 'Reds',
    # Tendencies (signed): diverging
    ('DOHC', ''): 'seismic',
    ('DSST', ''): 'seismic',
    ('DSSH', ''): 'seismic',
    # Sea-surface height (cm): diverging around 0
    ('SSH', ''): 'seismic',
    # Mixed-layer depth (m)
    ('MLD', ''): 'Blues',
    # Surface heat flux (W/m²): diverging
    ('SHF', ''): 'seismic',
    ('SHF', 'HYCOM'): 'seismic',
    # Dynamic potential intensity (m/s)
    ('DPI', ''): 'YlOrRd',
    # Sea-surface salinity (psu)
    ('SSS', ''): 'Blues',
    # Mixed-layer temperature (°C)
    ('MLT', ''): 'Reds',
    # Mixed-layer salinity (psu)
    ('MLS', ''): 'Blues',
    # Ocean surface convergence / vorticity (10⁻⁵ s⁻¹): diverging
    ('CONV', ''): 'seismic',
    ('VORT_OCN', ''): 'seismic',

    # --- Air-sea / PBL ---
    # Turbulent enthalpy flux and total net heat flux (W/m²)
    ('TURB_FLUX', ''): 'plasma',
    ('TOTAL_FLUX', ''): 'plasma',
    # Equivalent potential temperature (K)
    ('THETA_E', ''): 'jet',
    ('THETA_E', '550'): 'jet',
    ('THETA_E', '700'): 'jet',
    ('THETA_E', '850'): 'jet',
    # Air-sea temperature contrast (K) and humidity ratio
    ('DELTA_T', ''): 'seismic',
    ('DELTA_Q', ''): 'seismic',
    # Surface wind gusts (kt) and gust factor (dimensionless)
    ('GUSTS', ''): 'jet',
    ('GUST_FACTOR', ''): 'Reds',
}

# Registry: maps variable -> contour levels
# This replaces defineLevels() from GPLOT_util.ncl
_LEVEL_REGISTRY = {
    # Wind speed (kt). Breakpoints align with the Saffir-Simpson scale
    # (TD, TS, Cat1, Cat2, Cat3, Cat4, Cat5) so the fill colors map to
    # operationally meaningful intensity categories, matching the
    # HAFS-A "Moving-nest-3km / 10m UV MSLP" product palette.
    # 9 breaks = 8 interior bins; build_discrete_cmap reserves the
    # tcwinds1.rgb endpoints (white for <10 kt, purple for >137 kt)
    # as the under/over extensions so all 10 source colors are used.
    ('UV', ''): np.array([10, 20, 34, 50, 64, 83, 96, 114, 137]),
    ('UV', '10'): np.array([10, 20, 34, 50, 64, 83, 96, 114, 137]),
    ('UV10', ''): np.array([10, 20, 34, 50, 64, 83, 96, 114, 137]),

    # Relative vorticity (10^-5 s^-1). Range starts at 5 (not 0) so
    # the 0-5 "noise floor" of ambient vorticity falls into the
    # set_under color (white on hot_r), leaving only the coherent
    # cyclonic signal >=5 colored. Negative values also render white.
    # A level-less ('RVO', '') fallback ensures every pressure level
    # (850, 700, 500, 200, ...) uses the same scale so Tier2 vorticity
    # plots render consistently with Tier1.
    ('RVO', ''): np.arange(5, 75, 5),
    ('RVO', '850'): np.arange(5, 75, 5),
    ('RVO', '700'): np.arange(5, 75, 5),
    ('RVO', '500'): np.arange(5, 75, 5),
    ('RVO', '200'): np.arange(5, 75, 5),
    ('AVO', ''): np.arange(-20, 52, 2),

    # Temperature (K)
    ('T', '850'): np.arange(240, 312, 2),
    ('T', '500'): np.arange(230, 280, 2),
    ('T', '200'): np.arange(190, 240, 2),
    # Near-surface temperature (2 m above ground; HAFS surface 't' is in K)
    ('T', '2'): np.arange(270, 312, 1),
    ('T2', ''): np.arange(270, 312, 1),

    # Sea-surface temperature (K). Range 290-305 K (~17-32 C) focuses
    # the colorbar on the TC-relevant window: below 293 K (20 C) TC
    # genesis is inhibited, and above 305 K (32 C) only occurs in
    # small-area, short-duration patches that don't need their own
    # color band. 0.5 K steps give enough resolution to see the
    # ~1-2 K SST cold wake and pre-storm warm anomalies that matter
    # for TC intensity, rather than the coarse 1 K color bands that
    # blur those signals. Cooler water (< 290 K) renders in set_under
    # and rare >32 C pockets use set_over.
    ('SST', ''): np.arange(290, 305.5, 0.5),

    # 2-m dewpoint (K). Range 275-301 K (~2-28 C) spans the practical
    # tropical / subtropical envelope; dewpoints above ~297 K in TC
    # inflow regions are favorable for intensification. Below-range
    # drier air maps to set_under (dry browns/yellows) to highlight
    # dry intrusions.
    ('DPT', ''): np.arange(275, 302, 1),

    # Surface fluxes (W/m^2). Latent range wider than sensible since TC
    # enthalpy input is dominated by latent heat.
    ('LHFLX', ''): np.arange(0, 1050, 50),
    ('SHFLX', ''): np.arange(-100, 520, 20),

    # CAPE (J/kg)
    ('CAPE', ''): np.arange(0, 4200, 200),

    # Storm-relative helicity (m^2/s^2, 0-3 km)
    ('HLCY', ''): np.arange(0, 630, 30),

    # Potential vorticity (PVU). Range -10 to +10 is symmetric about
    # zero so the RdBu_r diverging cmap puts near-zero PV (tropical
    # tropospheric background) in near-white, and strong +/- departures
    # stand out clearly. At 200 hPa this cleanly distinguishes tropical
    # low-PV air (white/pale red) from midlatitude stratospheric
    # intrusions / PV streamers (deep reds) and Southern Hemisphere
    # negative PV (deep blues).
    ('PV', ''): np.arange(-10, 10.5, 0.5),

    # Relative humidity (%). Range is 5-95 (inner bins) rather than
    # 0-100 since 0 and 100 are hard physical limits: draping fill on
    # the endpoints wastes colorbar space that could go to meaningful
    # gradients in between. With extend='both' + build_discrete_cmap,
    # values < 5 fall into set_under (driest browns) and > 95 into
    # set_over (wettest teals), so the extrema are still visible
    # without compressing the informative range.
    # The level-less ('RH', '') fallback catches compound level codes
    # such as '07000400a' (700-400 hPa layer mean) which otherwise miss
    # the registry and fall back to contourf's auto-picked levels.
    ('RH', ''): np.arange(5, 100, 5),
    ('RH', '700'): np.arange(5, 100, 5),
    ('RH', '500'): np.arange(5, 100, 5),

    # Geopotential height (dam -- grib_reader converts GRIB2 m -> dam).
    # At tropical latitudes the variability on each isobaric surface is
    # narrow (~15-25 dam), so levels are tuned tightly around the
    # climatological range to give the fill meaningful color contrast.
    # draw_map() additionally overlays labeled HGT contour lines when
    # HGT is the base fill so exact values remain readable.
    ('HGT', '850'): np.arange(135, 165, 1),
    ('HGT', '700'): np.arange(300, 330, 1),
    ('HGT', '500'): np.arange(572, 598, 1),
    # 200 hPa heights (dam): widened from the tropics-only 1225-1265
    # band so the d01 parent domain captures mid-latitude troughs and
    # ridges. 5 dam (50 m) interval keeps the colorbar legible across
    # the ~120 dam dynamic range from polar jet to tropical tropopause.
    ('HGT', '200'): np.arange(1150, 1271, 5),

    # MSLP (hPa)
    ('MSLP', ''): np.arange(900, 1060, 4),

    # Precipitation (mm or mm/hr)
    ('PRCP', ''): np.array([0, 0.5, 1, 2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 75, 100]),
    ('TPRCP', ''): np.array([0, 0.5, 1, 2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 75, 100]),

    # TPW (mm)
    ('TPW', ''): np.arange(0, 82, 2),

    # Reflectivity (dBZ)
    ('REFL', ''): np.arange(-10, 80, 5),
    ('REFD', ''): np.arange(-10, 80, 5),

    # Shear (kt). Range focused on the hurricane-relevant band:
    # below 5 kt saturates to white, above 50 kt saturates to a single
    # dark red (same color, not progressively darker -- strong shear is
    # strong shear). 5 kt bins across the 5-50 band keep the
    # green -> yellow -> orange -> red gradient legible.
    ('SHDL', ''): np.arange(5, 55, 5),

    # Simulated IR brightness temperature (K). Range 150-320 K covers
    # from overshooting-top cirrus (~150 K) to warm desert surfaces
    # (~320 K) at 5 K intervals. 34 bins pair 1:1 with irsat.rgb's 36
    # colors (34 interior + under + over).
    ('SIMIR', ''): np.arange(150, 325, 5),
    ('SBTAGR13toa', ''): np.arange(150, 325, 5),

    # --- Ocean maps ---
    # Isotherm depths (m)
    ('ISO26', ''): np.arange(0, 165, 5),
    ('ISO20', ''): np.arange(0, 310, 10),
    # OHC (kJ/cm²): full range used by MOM6; HYCOM uses narrower range
    ('OHC', ''): np.arange(0, 205, 5),
    ('OHC', 'HYCOM'): np.arange(60, 125, 5),
    # Tendencies
    ('DOHC', ''): np.arange(-40, 42, 2),
    ('DSST', ''): np.arange(-3.0, 3.2, 0.2),
    ('DSSH', ''): np.arange(-20, 21, 1),
    # SSH (cm)
    ('SSH', ''): np.arange(-100, 105, 5),
    # MLD (m)
    ('MLD', ''): np.arange(0, 165, 5),
    # Surface heat flux (W/m²)
    ('SHF', ''): np.arange(-800, 850, 50),
    ('SHF', 'HYCOM'): np.arange(-1600, 1650, 50),
    # Dynamic potential intensity (m/s)
    ('DPI', ''): np.arange(0, 31, 1),
    # Salinity (psu)
    ('SSS', ''): np.arange(32.0, 38.2, 0.2),
    ('MLS', ''): np.arange(32.0, 38.2, 0.2),
    # Temperature (°C)
    ('MLT', ''): np.arange(26.0, 30.2, 0.2),
    # Ocean surface convergence / vorticity (10⁻⁵ s⁻¹)
    ('CONV', ''): np.arange(-20.0, 20.5, 0.5),
    ('VORT_OCN', ''): np.arange(-20.0, 20.5, 0.5),

    # --- Air-sea / PBL ---
    # Heat fluxes (W/m²)
    ('TURB_FLUX', ''): np.arange(-400, 1450, 50),
    ('TOTAL_FLUX', ''): np.arange(-500, 2050, 50),
    # Equivalent potential temperature (K)
    ('THETA_E', ''): np.arange(330, 382, 2),
    ('THETA_E', '550'): np.arange(330, 382, 2),
    ('THETA_E', '700'): np.arange(330, 382, 2),
    ('THETA_E', '850'): np.arange(330, 382, 2),
    # Air-sea contrasts
    ('DELTA_T', ''): np.arange(-6.0, 12.2, 0.2),
    ('DELTA_Q', ''): np.arange(1.05, 1.202, 0.002),
    # Gusts (kt) and gust factor
    ('GUSTS', ''): np.arange(0, 142, 2),
    ('GUST_FACTOR', ''): np.arange(1.0, 2.05, 0.05),
}


def get_colormap(var, level='', gplot_dir=None):
    """
    Get the matplotlib colormap for a variable.

    Parameters
    ----------
    var : str
        Variable name (e.g., 'MSLP', 'UV', 'RVO').
    level : str, optional
        Level hint (e.g., '850', '10').
    gplot_dir : str, optional
        GPLOT root directory for loading custom colormaps.

    Returns
    -------
    matplotlib.colors.Colormap
    """
    if gplot_dir is None:
        gplot_dir = os.environ.get('GPLOT_DIR', '.')

    # Look up in registry
    cmap_name = _CMAP_REGISTRY.get((var, level))
    if cmap_name is None:
        cmap_name = _CMAP_REGISTRY.get((var, ''))
    if cmap_name is None:
        logger.debug(f"No colormap registered for ({var}, {level}), using viridis")
        return plt.cm.viridis

    # Check if it's a file-based colormap. Both .rgb (legacy NCL HLU
    # palette format, kept for the maps module's published banding
    # conventions) and .txt (normalized 0-1 RGB triples) live in the
    # same directory now: sorc/GPLOT/python/colormaps/.
    cmap_dir = os.path.join(gplot_dir, 'sorc', 'GPLOT', 'python', 'colormaps')
    if cmap_name.endswith('.rgb'):
        rgb_path = os.path.join(cmap_dir, cmap_name)
        if os.path.isfile(rgb_path):
            return load_rgb_colormap(rgb_path, name=cmap_name.replace('.rgb', ''))

    if cmap_name.endswith('.txt'):
        txt_path = os.path.join(cmap_dir, cmap_name)
        if os.path.isfile(txt_path):
            return load_txt_colormap(txt_path, name=cmap_name.replace('.txt', ''))

    # Try matplotlib built-in
    try:
        return plt.cm.get_cmap(cmap_name)
    except ValueError:
        logger.warning(f"Colormap '{cmap_name}' not found, using viridis")
        return plt.cm.viridis


def get_contour_levels(var, level=''):
    """
    Get the contour levels for a variable.

    Parameters
    ----------
    var : str
        Variable name.
    level : str, optional
        Level hint.

    Returns
    -------
    np.ndarray
        Array of contour levels.
    """
    levels = _LEVEL_REGISTRY.get((var, level))
    if levels is None:
        levels = _LEVEL_REGISTRY.get((var, ''))
    if levels is None:
        logger.debug(f"No contour levels registered for ({var}, {level})")
        return None
    return levels


def get_norm(levels):
    """
    Get a BoundaryNorm for the given contour levels.

    Parameters
    ----------
    levels : np.ndarray
        Contour level boundaries.

    Returns
    -------
    matplotlib.colors.BoundaryNorm
    """
    if levels is None:
        return None
    # Size the BoundaryNorm to the number of bins (not 256). This avoids
    # the pitfall where combining BoundaryNorm(levels, 256) with a
    # ListedColormap of <256 colors causes every value above the first
    # bin to clip to the final (extend-high) color.
    return mcolors.BoundaryNorm(levels, len(levels) - 1)


def resample_cmap(cmap, n_colors):
    """
    Return a ListedColormap with exactly ``n_colors`` colors by
    resampling the input colormap.

    Needed because our NCL-derived ``.rgb`` palettes often have a fixed
    number of colors (e.g., 27) that don't match the number of contour
    level bins in use.  Passing such a palette directly to
    ``contourf(levels=...)`` works but matplotlib will silently repeat
    colors; explicitly resampling gives predictable, evenly-spaced
    color assignment.

    Parameters
    ----------
    cmap : matplotlib.colors.Colormap
    n_colors : int
        Desired number of discrete colors.

    Returns
    -------
    matplotlib.colors.ListedColormap
    """
    if n_colors <= 0:
        return cmap

    if isinstance(cmap, mcolors.ListedColormap):
        orig = np.asarray(cmap.colors)
        if len(orig) == n_colors:
            return cmap
        # Evenly sample the original color list (nearest-index sampling)
        idx = np.linspace(0, len(orig) - 1, n_colors).round().astype(int)
        return mcolors.ListedColormap(orig[idx], name=f'{cmap.name}_r{n_colors}')

    # Continuous cmap -> sample to n_colors
    try:
        return mcolors.ListedColormap(cmap(np.linspace(0, 1, n_colors)),
                                      name=f'{cmap.name}_r{n_colors}')
    except Exception:
        return cmap


def build_discrete_cmap(cmap, n_bins, extend='both'):
    """
    Build a ListedColormap sized for contourf with ``n_bins`` interior
    bins and the given ``extend`` mode. The first and/or last colors of
    the source palette are reserved for ``set_under`` / ``set_over`` so
    all source colors get used in evenly spaced order, with ``extend``
    determining how many of those colors land outside the plotted range.

    This is how the NCL .rgb wind palette (tcwinds1.rgb, 10 colors) maps
    naturally onto the 8 Saffir-Simpson interior bins plus ``<10`` white
    (under) and ``>137`` purple (over) extensions without dropping the
    orange/blue intermediate colors that a plain ``resample_cmap`` would
    skip when 10 source colors are forced into 8 bins.

    Parameters
    ----------
    cmap : matplotlib.colors.Colormap
    n_bins : int
        Number of interior bins (typically ``len(levels) - 1``).
    extend : {'neither', 'min', 'max', 'both'}
        Matches the ``extend`` argument passed to ``contourf``.

    Returns
    -------
    matplotlib.colors.ListedColormap
        Colormap with exactly ``n_bins`` colors; ``set_under`` /
        ``set_over`` are set from the reserved source colors when the
        corresponding extend direction is requested.
    """
    if n_bins <= 0:
        return cmap

    if isinstance(cmap, mcolors.ListedColormap):
        orig = np.asarray(cmap.colors)
    else:
        orig = cmap(np.linspace(0, 1, 256))

    n_src = len(orig)
    need_under = extend in ('min', 'both')
    need_over = extend in ('max', 'both')
    total = n_bins + int(need_under) + int(need_over)

    idx = np.linspace(0, n_src - 1, total).round().astype(int)
    picked = orig[idx]

    start = 1 if need_under else 0
    end = len(picked) - 1 if need_over else len(picked)
    inner = picked[start:end]

    result = mcolors.ListedColormap(inner, name=f'{cmap.name}_d{n_bins}')
    if need_under:
        result.set_under(tuple(picked[0]))
    if need_over:
        result.set_over(tuple(picked[-1]))
    return result


def clear_cache():
    """Clear the colormap cache."""
    _cmap_cache.clear()
