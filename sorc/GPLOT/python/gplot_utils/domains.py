"""
Domain boundary definitions for GPLOT.

Replaces getDmnBds() from GPLOT_util.ncl and reads DomainInfo.dat from
the tbl/ directory. Provides domain boundary lookups and storm-centered
domain detection.
"""

import os
import logging

logger = logging.getLogger(__name__)

# Hard-coded domain boundaries: (lat_n, lat_s, lon_w, lon_e)
# These match the NCL getDmnBds() function definitions.
# Longitudes follow the convention where W is negative unless
# the domain crosses the date line.
_DOMAIN_BOUNDS = {
    # Full global
    'global': (90.0, -90.0, -180.0, 180.0),

    # Basin-scale domains
    'basin': (60.0, -5.0, -110.0, 10.0),
    'atl': (55.0, 0.0, -110.0, -5.0),
    'eus': (55.0, 15.0, -100.0, -55.0),
    'elant': (55.0, 0.0, -90.0, -40.0),
    'clant': (55.0, 0.0, -80.0, -30.0),
    'wlant': (55.0, 0.0, -70.0, -20.0),
    'cpac': (55.0, 0.0, -180.0, -120.0),
    'epac': (55.0, 0.0, -140.0, -80.0),
    'wpac': (55.0, -5.0, 100.0, 180.0),
    'gom': (32.0, 17.0, -100.0, -78.0),
    'wmexico': (32.0, 12.0, -115.0, -90.0),
    'bob': (25.0, 5.0, 75.0, 100.0),

    # HWRF parent domain (placeholder -- actual bounds come from GRIB2 file)
    'd01': None,
    'bigd01': None,
    'hwrf': None,

    # Storm-centered domains (bounds set dynamically from ATCF)
    'd03': None,
    'alld03': None,
    'core': None,
    'storm': None,
}

# Nest number for each domain (1=parent/global, 3=storm-centered)
_DOMAIN_NEST = {
    'global': 1, 'basin': 1, 'atl': 1, 'eus': 1,
    'elant': 1, 'clant': 1, 'wlant': 1,
    'cpac': 1, 'epac': 1, 'wpac': 1,
    'gom': 1, 'wmexico': 1, 'bob': 1,
    'd01': 1, 'bigd01': 1, 'hwrf': 1,
    'd03': 3, 'alld03': 3, 'core': 3, 'storm': 3,
}


def is_storm_centered(domain):
    """
    Check if a domain is storm-centered (nest 3).

    Parameters
    ----------
    domain : str
        Domain name (e.g., 'd03', 'atl', 'hwrf').

    Returns
    -------
    bool
        True if the domain is storm-centered.
    """
    return _DOMAIN_NEST.get(domain.lower(), 1) == 3


# Domains for which output filenames + plot titles should embed the
# storm-specific identifier (longsid). Storm-following nests (d03)
# and the legacy HWRF outer domain show one TC per panel, so the
# storm name belongs in the filename + title. Larger domains (d01,
# atl, basin, global, ...) may carry multiple storms in a single
# plot for multistorm / global runs and should be storm-agnostic.
_STORM_NAMED_DOMAINS = frozenset({'d03', 'hwrf'})


def is_storm_named_filename(domain):
    """Whether output filenames + titles for ``domain`` should
    include the storm-specific identifier.

    True for d03 and hwrf; False for all large-scale domains
    (d01, atl, basin, global, ...). Distinct from
    ``is_storm_centered`` (which keys off NEST=3 in
    DomainInfo.dat) because hwrf is logically storm-centric for
    naming purposes despite being declared NEST=1 in the registry.
    """
    return domain.lower() in _STORM_NAMED_DOMAINS


# Default half-width (degrees) for each storm-centered domain.  These
# match the NCL GPLOT_util.ncl conventions:
#   d03   -> ±4°   (tight vortex-core view)
#   core  -> ±4°   (same as d03)
#   storm -> ±6°   (vortex-following, moving-nest HAFS)
#   alld03 / (unlisted) -> falls through to _STORM_BOX_DEFAULT.
# They may be overridden via the ``box_degrees`` argument.
_STORM_BOX_HALFWIDTH = {
    'd03': 6.0,
    'alld03': 10.0,
    'core': 4.0,
    # HWRF outer parent: ~100 deg wide TC-centered panel, matching the
    # legacy NCL "Oper. HWRF" outer-domain plot (see e.g.
    # https://storm.aoml.noaa.gov/hwrfx/.../HWRF-2020/Hwrf/Sea%20Level%20Pressure/).
    # Triggered via is_storm_named_filename branch in get_domain_bounds.
    # 50 deg halfwidth captures the full operational HWRF d01 footprint
    # (the visible data extent in the operational post output is
    # typically ~95-100 deg wide); the earlier 40 deg crop was lopping
    # off real data on the eastern / western edges.
    'hwrf': 50.0,
    'storm': 6.0,
}
_STORM_BOX_DEFAULT = 6.0


def get_domain_bounds(domain, tc_lat=None, tc_lon=None, box_degrees=None):
    """
    Get the geographic boundaries for a domain.

    For storm-centered domains (d03, storm, core), returns a box centered
    on the TC position. For fixed domains, returns the pre-defined bounds.
    For parent domains (d01, hwrf), returns None (bounds come from data).

    Parameters
    ----------
    domain : str
        Domain name.
    tc_lat : float, optional
        TC latitude (required for storm-centered domains).
    tc_lon : float, optional
        TC longitude (required for storm-centered domains).
    box_degrees : float, optional
        Half-width of storm-centered box in degrees.  If ``None``, use
        the per-domain default from ``_STORM_BOX_HALFWIDTH`` (falling
        back to ``_STORM_BOX_DEFAULT`` = 6°).  The d03 moving-nest HAFS
        data is ~±10°; the default ±6° trims the empty margin and keeps
        the focus on the storm core.

    Returns
    -------
    tuple or None
        (lat_n, lat_s, lon_w, lon_e) or None if domain bounds
        must come from the data file.
    """
    domain_lower = domain.lower()

    # Storm-centered bounds are computed when either:
    #   * The domain is in the NEST=3 registry (d03/core/storm/alld03)
    #   * OR it's a storm-named-filename domain that *also* has an entry
    #     in _STORM_BOX_HALFWIDTH (currently just 'hwrf'). This second
    #     case is the legacy operational-HWRF outer panel -- declared
    #     NEST=1 because the model's d01 IS technically the parent, but
    #     plotted as a TC-centered ~80 deg box because that's what the
    #     legacy NCL hwrf plots looked like.
    is_sc = is_storm_centered(domain_lower)
    is_hwrf_panel = (is_storm_named_filename(domain_lower)
                     and domain_lower in _STORM_BOX_HALFWIDTH
                     and not is_sc)
    if is_sc or is_hwrf_panel:
        if tc_lat is None or tc_lon is None:
            logger.warning(f"Storm-centered domain '{domain}' requires TC position")
            return None
        if box_degrees is None:
            box_degrees = _STORM_BOX_HALFWIDTH.get(
                domain_lower, _STORM_BOX_DEFAULT)
        lat_n = tc_lat + box_degrees
        lat_s = tc_lat - box_degrees
        lon_w = tc_lon - box_degrees
        lon_e = tc_lon + box_degrees
        # Keep the box contiguous when a wide hwrf-style halfwidth
        # around a Pacific storm spills past +/-180. Shift the whole
        # box by 360 so it lives in [-180, 360] without splitting at
        # the dateline. Downstream cartopy + data-lon-alignment
        # handle the resulting lon_e > 180 just fine.
        if lon_w < -180:
            lon_w += 360
            lon_e += 360
        elif lon_e > 360:
            lon_w -= 360
            lon_e -= 360
        return (lat_n, lat_s, lon_w, lon_e)

    bounds = _DOMAIN_BOUNDS.get(domain_lower)
    if bounds is None:
        logger.debug(f"Domain '{domain}' has no pre-defined bounds (from data)")

    return bounds


def get_nest_number(domain):
    """
    Get the nest number for a domain.

    Parameters
    ----------
    domain : str
        Domain name.

    Returns
    -------
    int
        Nest number (1 for parent/global, 3 for storm-centered).
    """
    return _DOMAIN_NEST.get(domain.lower(), 1)


def load_domain_info(gplot_dir=None):
    """
    Load domain information from tbl/DomainInfo.dat.

    Parameters
    ----------
    gplot_dir : str, optional
        GPLOT root directory.

    Returns
    -------
    dict
        Mapping of domain name -> nest number.
    """
    if gplot_dir is None:
        gplot_dir = os.environ.get('GPLOT_DIR', '.')

    info_path = os.path.join(gplot_dir, 'tbl', 'DomainInfo.dat')
    if not os.path.isfile(info_path):
        logger.warning(f"DomainInfo.dat not found: {info_path}")
        return dict(_DOMAIN_NEST)

    result = {}
    with open(info_path, 'r') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith(';') or line.startswith('#'):
                continue
            parts = line.split()
            if len(parts) >= 2 and parts[0].upper() != 'DOMAIN':
                try:
                    result[parts[0].lower()] = int(parts[1])
                except ValueError:
                    continue

    # Merge with defaults
    merged = dict(_DOMAIN_NEST)
    merged.update(result)
    return merged
