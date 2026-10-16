"""
Ensemble-member helpers for GPLOT entry points.

A HAFS ensemble runs many members of one configuration. The spawn scripts
loop over members and pass each member id through ``--ensid``; the Python
entry points then (a) write output into a per-member sub-directory and
(b) select the requested storm from the member's multi-storm ``00L`` ATCF.

All helpers are no-ops for deterministic runs (``ensid`` empty / sentinel),
so wiring them in never changes non-ensemble behavior.
"""

import logging

import numpy as np

logger = logging.getLogger(__name__)

# Sentinels the spawn/batch layer uses to mean "not an ensemble member".
# Note "00" is intentionally NOT here -- it is a valid member id.
_DETERMINISTIC_SENTINELS = frozenset({'', 'XX', 'MISSING', '0'})

# ATCF basin one-letter code (from the SID, e.g. '13L') -> ATCF long code
# used in column 0 of an atcfunix record.
_BASIN_LONG = {
    'L': 'AL',  # North Atlantic
    'E': 'EP',  # Eastern North Pacific
    'C': 'CP',  # Central North Pacific
    'W': 'WP',  # Western North Pacific
    'A': 'AL',
    'B': 'IO',  # North Indian Ocean (Bay of Bengal / Arabian Sea)
    'S': 'SH',  # Southern Hemisphere
    'P': 'SH',
}


def normalize_ensid(ensid):
    """Return the canonical member id, or '' for deterministic runs.

    Maps the deterministic sentinels ('', 'XX', 'MISSING', '0') to '' so the
    caller can treat "no member" uniformly. Any other value is stripped and
    returned as-is (the spawn layer already zero-pads, e.g. '03', '12').
    """
    if ensid is None:
        return ''
    s = str(ensid).strip()
    if s.upper() in _DETERMINISTIC_SENTINELS:
        return ''
    return s


def member_segment(ensid):
    """Path segment for a member sub-directory: the member id or '' if none.

    Splice this into an output path so deterministic runs get no extra
    directory level::

        seg = member_segment(ensid)                 # '' or e.g. '03'
        odir = os.path.join(base, expt, idate, seg, module)  # seg='' drops out
    """
    return normalize_ensid(ensid)


def is_ensemble(ensid):
    """True if ``ensid`` denotes a real ensemble member (not deterministic)."""
    return normalize_ensid(ensid) != ''


def filter_atcf_for_storm(atcf_data, basinid, snum):
    """Restrict ATCF rows to a single storm (basin + number).

    Ensemble member ATCF files are ``00L``-named and may hold every storm in
    the cycle, so the requested storm must be selected by basin + storm
    number. ``atcf_data`` is the 2-D string array from ``np.genfromtxt`` of an
    atcfunix file (column 0 = basin long code like 'AL', column 1 = storm
    number like '13').

    Returns the filtered array. If the basin can't be mapped, the input is
    returned unchanged (fail open rather than drop everything).
    """
    long_basin = _BASIN_LONG.get(str(basinid).strip().upper())
    if long_basin is None:
        logger.warning(f"filter_atcf_for_storm: unrecognized basin "
                       f"'{basinid}'; not filtering")
        return atcf_data
    if atcf_data is None or atcf_data.ndim != 2 or atcf_data.shape[1] < 2:
        logger.warning("filter_atcf_for_storm: unexpected ATCF shape; "
                       "not filtering")
        return atcf_data

    # Normalize the target storm number ('13', '03', ' 3' -> '3') for a
    # leading-zero-insensitive compare.
    snum_norm = (str(snum).strip().lstrip('0') or '0')
    keep = np.zeros(atcf_data.shape[0], dtype=bool)
    for i in range(atcf_data.shape[0]):
        b = str(atcf_data[i, 0]).strip().upper()
        n = (str(atcf_data[i, 1]).strip().lstrip('0') or '0')
        keep[i] = (b == long_basin) and (n == snum_norm)
    filtered = atcf_data[keep]
    if filtered.shape[0] == 0:
        logger.warning(f"filter_atcf_for_storm: no rows matched "
                       f"{long_basin}{snum_norm}; keeping all rows")
        return atcf_data
    return filtered


def filter_atcf_df(df, basinid, snum):
    """Restrict a read_atcf() DataFrame to a single storm (basin + number).

    DataFrame analogue of ``filter_atcf_for_storm`` for entry points that use
    ``gplot_utils.atcf.read_atcf`` (columns ``basin`` like 'AL' and
    ``storm_num`` like '13'). Ensemble member ATCFs are multi-storm, so the
    requested storm is selected by basin + number. Returns ``df`` unchanged if
    the basin can't be mapped, the columns are absent, or nothing matches
    (fail open).
    """
    long_basin = _BASIN_LONG.get(str(basinid).strip().upper())
    if long_basin is None or df is None or len(df) == 0:
        return df
    if 'basin' not in df.columns or 'storm_num' not in df.columns:
        logger.warning("filter_atcf_df: DataFrame missing basin/storm_num; "
                       "not filtering")
        return df
    snum_norm = (str(snum).strip().lstrip('0') or '0')
    basin_col = df['basin'].astype(str).str.strip().str.upper()
    num_col = (df['storm_num'].astype(str).str.strip()
               .str.lstrip('0').replace('', '0'))
    mask = (basin_col == long_basin) & (num_col == snum_norm)
    out = df[mask]
    if len(out) == 0:
        logger.warning(f"filter_atcf_df: no rows matched {long_basin}"
                       f"{snum_norm}; keeping all rows")
        return df
    return out
