"""
ATCF (Automated Tropical Cyclone Forecast) file reader for GPLOT.

Replaces atcfRead(), adeckRead(), and bdeckRead() from GPLOT_func.ncl.
Reads standard ATCF A-deck (model forecast) and B-deck (best track) files
into pandas DataFrames.

ATCF format reference (comma-separated, fixed-width fields):
  Col 0: Basin (AL, EP, WP, etc.)
  Col 1: Storm number (01-50 for named, 80-99 for invest)
  Col 2: Forecast cycle (YYYYMMDDHH)
  Col 3: Technique number (usually 03)
  Col 4: Model ID (HFSB, AVNO, etc.)
  Col 5: Forecast hour (000, 003, 006, ...)
  Col 6: Latitude (e.g., 144N, 055S) in tenths of degrees
  Col 7: Longitude (e.g., 743W, 1200E) in tenths of degrees
  Col 8: Max sustained wind (kt)
  Col 9: Min sea level pressure (mb)
  Col 10: Development type (XX, DB, TD, TS, HU, etc.)
  Col 11: Wind radii threshold (34, 50, 64 kt)
  Col 12: Wind radii code (NEQ, etc.)
  Col 13-16: Wind radii NE, SE, SW, NW (nm)
  Col 17: Outer closed isobar pressure (mb)
  Col 18: Outer closed isobar radius (nm)
  Col 19: Radius of maximum wind (nm)
  ...additional columns vary by model
"""

import os
import logging

import numpy as np
import pandas as pd

logger = logging.getLogger(__name__)


def _parse_latlon(lat_str, lon_str):
    """
    Parse ATCF latitude/longitude strings to decimal degrees.

    ATCF format: latitude as NNN[N|S] (tenths of degrees),
                 longitude as NNNN[E|W] (tenths of degrees).

    Examples: '144N' -> 14.4, '743W' -> -74.3, '1200E' -> 120.0

    Parameters
    ----------
    lat_str : str
        Latitude string (e.g., '144N', '055S').
    lon_str : str
        Longitude string (e.g., '743W', '1200E').

    Returns
    -------
    tuple[float, float]
        (latitude, longitude) in decimal degrees.
        Longitude is negative for W, latitude is negative for S.
    """
    lat_str = lat_str.strip()
    lon_str = lon_str.strip()

    # Latitude
    if lat_str.endswith('N'):
        lat = float(lat_str[:-1]) / 10.0
    elif lat_str.endswith('S'):
        lat = -float(lat_str[:-1]) / 10.0
    else:
        lat = float(lat_str) / 10.0

    # Longitude
    if lon_str.endswith('W'):
        lon = -float(lon_str[:-1]) / 10.0
    elif lon_str.endswith('E'):
        lon = float(lon_str[:-1]) / 10.0
    else:
        lon = float(lon_str) / 10.0

    return lat, lon


def read_atcf(filepath, model_id=None, cycle=None, wind_radii=34):
    """
    Read an ATCF A-deck (model forecast) file.

    Filters to the specified wind radii threshold (default 34 kt) to
    avoid duplicate rows at the same forecast hour. Optionally filters
    by model ID and/or forecast cycle.

    Parameters
    ----------
    filepath : str
        Path to the ATCF file.
    model_id : str, optional
        Model identifier to filter by (e.g., 'HFSB').
    cycle : str, optional
        Forecast cycle (YYYYMMDDHH) to filter by.
    wind_radii : int, optional
        Wind radii threshold to filter by (default 34). Use 0 for entries
        without wind radii info.

    Returns
    -------
    pd.DataFrame
        DataFrame with columns: basin, storm_num, cycle, tech_num,
        model, fhr, lat, lon, vmax, mslp, dev_type, wind_radii_thresh,
        rad_ne, rad_se, rad_sw, rad_nw, outer_pressure, outer_radius, rmw,
        storm_dir (deg, -99 if missing), storm_speed (tenths of knots).
    """
    if not os.path.isfile(filepath):
        logger.warning(f"ATCF file not found: {filepath}")
        return pd.DataFrame()

    records = []
    with open(filepath, 'r') as f:
        for line in f:
            fields = [s.strip() for s in line.split(',')]
            if len(fields) < 12:
                continue

            try:
                basin = fields[0]
                storm_num = fields[1].strip()
                fcst_cycle = fields[2].strip()
                tech_num = fields[3].strip()
                model = fields[4].strip()
                fhr = int(fields[5])

                lat, lon = _parse_latlon(fields[6], fields[7])
                vmax = int(fields[8]) if fields[8].strip() else -99
                mslp = int(fields[9]) if fields[9].strip() else -99
                dev_type = fields[10].strip() if len(fields) > 10 else 'XX'
                wr_thresh = int(fields[11]) if len(fields) > 11 and fields[11].strip() else 0

                # Wind radii (NE, SE, SW, NW) in nautical miles
                rad_ne = int(fields[13]) if len(fields) > 13 and fields[13].strip() else 0
                rad_se = int(fields[14]) if len(fields) > 14 and fields[14].strip() else 0
                rad_sw = int(fields[15]) if len(fields) > 15 and fields[15].strip() else 0
                rad_nw = int(fields[16]) if len(fields) > 16 and fields[16].strip() else 0

                # Outer closed isobar
                outer_pres = int(fields[17]) if len(fields) > 17 and fields[17].strip() and fields[17].strip() != '-99' else -99
                outer_rad = int(fields[18]) if len(fields) > 18 and fields[18].strip() and fields[18].strip() != '-99' else -99

                # RMW
                rmw = int(fields[19]) if len(fields) > 19 and fields[19].strip() and fields[19].strip() != '-99' else -99

                # Storm motion: direction (deg, col 25) and speed (kt*10, col 26)
                def _opt_int(idx):
                    if len(fields) > idx and fields[idx].strip() and fields[idx].strip() != '-99':
                        try:
                            return int(fields[idx])
                        except ValueError:
                            return -99
                    return -99

                storm_dir   = _opt_int(25)
                storm_speed = _opt_int(26)  # tenths of knots

                # Storm name (column 28, 0-indexed 27). 'INVEST' is a
                # legitimate operational label for a pre-genesis
                # system and is kept so longsid renders cleanly as
                # 'invest13l' for those cycles. 'UNNAMED' and
                # 'NAMELESS' are degenerate no-name sentinels -- treat
                # them as empty so the longsid logic falls through to
                # the bare sid.
                storm_name = ''
                if len(fields) > 27:
                    raw = fields[27].strip().upper()
                    if raw and raw not in ('UNNAMED', 'NAMELESS'):
                        storm_name = raw

            except (ValueError, IndexError):
                continue

            records.append({
                'basin': basin,
                'storm_num': storm_num,
                'cycle': fcst_cycle,
                'tech_num': tech_num,
                'model': model,
                'fhr': fhr,
                'lat': lat,
                'lon': lon,
                'vmax': vmax,
                'mslp': mslp,
                'dev_type': dev_type,
                'wind_radii_thresh': wr_thresh,
                'rad_ne': rad_ne,
                'rad_se': rad_se,
                'rad_sw': rad_sw,
                'rad_nw': rad_nw,
                'outer_pressure': outer_pres,
                'outer_radius': outer_rad,
                'rmw': rmw,
                'storm_dir': storm_dir,
                'storm_speed': storm_speed,
                'storm_name': storm_name,
            })

    if not records:
        logger.warning(f"No valid records in ATCF file: {filepath}")
        return pd.DataFrame()

    df = pd.DataFrame(records)

    # Apply filters
    if model_id is not None:
        df = df[df['model'] == model_id]

    if cycle is not None:
        df = df[df['cycle'] == cycle]

    # Filter to single wind-radii threshold to avoid duplicate fhr rows
    if wind_radii is not None:
        df = df[df['wind_radii_thresh'] == wind_radii]

    # Sort by forecast hour
    df = df.sort_values('fhr').reset_index(drop=True)

    logger.info(f"Read ATCF: {filepath} -> {len(df)} rows"
                f" (model={model_id}, cycle={cycle}, wr={wind_radii})")
    return df


def read_bdeck(filepath, idate=None):
    """
    Read a best-track (B-deck) file.

    B-deck files use the same ATCF format but with model ID 'BEST'
    and forecast hour 0 (positions are actual, not forecast).

    Parameters
    ----------
    filepath : str
        Path to the B-deck file.
    idate : str, optional
        If provided, compute lead_time relative to this cycle (YYYYMMDDHH).

    Returns
    -------
    pd.DataFrame
        DataFrame with columns: basin, storm_num, datetime, lat, lon,
        vmax, mslp, dev_type, rmw. If idate is provided, also includes
        lead_time (hours relative to idate).
    """
    if not os.path.isfile(filepath):
        logger.warning(f"B-deck file not found: {filepath}")
        return pd.DataFrame()

    records = []
    with open(filepath, 'r') as f:
        for line in f:
            fields = [s.strip() for s in line.split(',')]
            if len(fields) < 10:
                continue

            try:
                basin = fields[0]
                storm_num = fields[1].strip()
                dt_str = fields[2].strip()
                model = fields[4].strip()

                lat, lon = _parse_latlon(fields[6], fields[7])
                vmax = int(fields[8]) if fields[8].strip() else -99
                mslp = int(fields[9]) if fields[9].strip() else -99
                dev_type = fields[10].strip() if len(fields) > 10 else 'XX'
                wr_thresh = int(fields[11]) if len(fields) > 11 and fields[11].strip() else 0
                rmw = int(fields[19]) if len(fields) > 19 and fields[19].strip() and fields[19].strip() != '-99' else -99
                # ATCF v0.1 column 28 (0-indexed 27) is the storm
                # name. 'INVEST' is a legitimate operational label
                # for a pre-genesis system and is kept so longsid
                # renders cleanly as 'invest13l' for those cycles.
                # 'UNNAMED' / 'NAMELESS' are degenerate no-name
                # sentinels -- treated as empty so the longsid logic
                # falls through to the bare sid.
                storm_name = ''
                if len(fields) > 27:
                    raw = fields[27].strip().upper()
                    if raw and raw not in ('UNNAMED', 'NAMELESS'):
                        storm_name = raw

            except (ValueError, IndexError):
                continue

            # B-deck entries with wind_radii_thresh > 34 are duplicates
            if wr_thresh > 34:
                continue

            records.append({
                'basin': basin,
                'storm_num': storm_num,
                'datetime': dt_str,
                'model': model,
                'lat': lat,
                'lon': lon,
                'vmax': vmax,
                'mslp': mslp,
                'dev_type': dev_type,
                'rmw': rmw,
                'storm_name': storm_name,
            })

    if not records:
        logger.warning(f"No valid records in B-deck file: {filepath}")
        return pd.DataFrame()

    df = pd.DataFrame(records)

    # Compute lead_time if idate is provided
    if idate is not None:
        from datetime import datetime, timedelta
        try:
            base_dt = datetime.strptime(idate, '%Y%m%d%H')
            lead_times = []
            for dt_str in df['datetime']:
                try:
                    row_dt = datetime.strptime(dt_str, '%Y%m%d%H')
                    delta = (row_dt - base_dt).total_seconds() / 3600.0
                    lead_times.append(delta)
                except ValueError:
                    lead_times.append(np.nan)
            df['lead_time'] = lead_times
        except ValueError:
            logger.warning(f"Could not parse idate '{idate}' for lead time computation")

    df = df.sort_values('datetime').reset_index(drop=True)

    logger.info(f"Read B-deck: {filepath} -> {len(df)} rows")
    return df


def parse_storm_info(longsid):
    """
    Parse a long storm ID string (e.g., 'sinlaku04w') into components.

    Parameters
    ----------
    longsid : str
        Long storm ID (e.g., 'sinlaku04w', 'harvey09l').

    Returns
    -------
    dict
        Dictionary with keys: name, number, basin1, basin2, sid.
        basin1 is 1-character (l, e, w, etc.)
        basin2 is 2-character (al, ep, wp, etc.)
        sid is the short ID (e.g., '04W', '09L')
    """
    longsid = longsid.strip().lower()
    if len(longsid) < 4:
        return {'name': longsid, 'number': '00', 'basin1': '', 'basin2': '', 'sid': ''}

    basin1 = longsid[-1]
    number = longsid[-3:-1]
    name = longsid[:-3].upper()

    basin_map = {
        'l': 'al', 'e': 'ep', 'c': 'cp', 'w': 'wp',
        's': 'sh', 'p': 'sh', 'a': 'io', 'b': 'io',
    }
    basin2 = basin_map.get(basin1, '')
    sid = f"{number}{basin1}".upper()

    return {
        'name': name,
        'number': number,
        'basin1': basin1,
        'basin2': basin2,
        'sid': sid,
    }


def derive_longsid(atcf_file, sid, bdeck_df=None, idate=None,
                   adeck_df=None, ensid=''):
    """
    Derive the long storm identifier (e.g. 'melissa13l') for output
    filenames and plot titles, with the priority chain:

      1. ATCF basename's first dot-separated segment, when it
         carries '<name><sid>' (legacy NCL convention; filenames
         like 'melissa13l.2025102100.trak.atcfunix').
      2. B-deck ``storm_name`` column at the run cycle (idate).
      3. A-deck ``storm_name`` column at the run cycle (idate).
      4. Bare sid (lowercase) -- last resort.

    When ``idate`` is supplied, steps 2-3 only consider rows whose
    timestamp matches the run cycle. If no rows match in a given
    source, that source is **skipped** -- the function does NOT fall
    through to the latest name in the whole DataFrame. This is the
    fix for a real-world failure: post-season b-decks label every
    record with the storm's eventual name (e.g. MELISSA at all
    timestamps once a system is named), which under the previous
    fall-through logic leaked 'melissa13l' onto retrospective runs
    of pre-genesis cycles where the operational a-deck still held
    'INVEST'. With separate b-deck / a-deck inputs and no
    whole-DataFrame fallback, the operational a-deck's per-cycle
    INVEST label wins for those pre-genesis runs -> 'invest13l'.

    When ``idate`` is None (legacy callers), each source is consulted
    in full and the latest non-empty name wins, matching the
    behavior before the cycle-aware change.

    Parameters
    ----------
    atcf_file : str or None
        Path to the ATCF a-deck used by the run, or None.
    sid : str
        Short storm id (e.g. '13L'). Lowercased internally.
    bdeck_df : pandas.DataFrame, optional
        DataFrame returned by ``read_bdeck``. Tried first.
    idate : str, optional
        Initialization time (YYYYMMDDHH). Enables the cycle-aware
        filter; when omitted the whole-DataFrame view is used.
    adeck_df : pandas.DataFrame, optional
        DataFrame returned by ``read_atcf`` (the operational /
        experiment a-deck). Tried after the b-deck so the
        operational per-cycle name wins when the b-deck doesn't
        have a record for that cycle.

    Returns
    -------
    str
        Lowercase longsid such as 'melissa13l' / 'invest13l', or the
        bare sid in lowercase when no name source is available.
    """
    sid_lc = (sid or '').lower()

    # Treat the deterministic sentinels as "no member" (callers normally pass
    # a normalized ensid, but guard here too). "00" stays a real member.
    _ens = str(ensid or '').strip()
    if _ens.upper() in ('XX', 'MISSING', '0'):
        _ens = ''

    # 1. ATCF filename first segment, if richer than the sid alone.
    # Skipped for ensemble members: their ATCF files are 00L-named, so the
    # filename can't identify the real storm -- fall through to the b-deck /
    # a-deck storm name (or the bare sid) instead.
    if atcf_file and not _ens:
        first_seg = os.path.basename(atcf_file).split('.')[0]
        if first_seg and first_seg.lower() != sid_lc \
                and first_seg.lower().endswith(sid_lc) \
                and len(first_seg) > len(sid_lc):
            logger.debug(f"longsid: '{first_seg.lower()}' (from filename)")
            return first_seg.lower()

    def _name_from(df, label):
        """Return the latest non-empty storm_name from `df`, after
        optional cycle filtering by ``idate``. Returns None when no
        usable name exists in the (possibly filtered) view."""
        if df is None or len(df) == 0 \
                or 'storm_name' not in df.columns:
            return None
        if idate is not None:
            # b-deck records carry per-row 'datetime'; a-deck rows
            # carry 'cycle' (the forecast init time).
            if 'datetime' in df.columns:
                hit = df[df['datetime'] == idate]
            elif 'cycle' in df.columns:
                hit = df[df['cycle'] == idate]
            else:
                hit = df  # source has no time column; treat as
                          # legacy whole-DF lookup.
        else:
            hit = df
        if len(hit) == 0:
            # Cycle-aware filter found nothing in this source -- DO
            # NOT fall through to the whole-DataFrame view, which
            # would let a post-season MELISSA label leak onto a
            # pre-genesis cycle. The caller can still recover via
            # the next source (a-deck) or the bare-sid fallback.
            return None
        names = [str(n).strip() for n in hit['storm_name'].dropna()
                 if str(n).strip()]
        if not names:
            return None
        # When `hit` is the cycle-filtered slice this list is
        # typically length 1 (BEST track is one record per cycle).
        # The trailing [-1] also handles the unfiltered legacy
        # case (idate=None) by picking the most recent name.
        return names[-1].lower()

    # 2 + 3: cycle-filtered lookups in priority order (b-deck wins
    # when it has the cycle; otherwise the a-deck's per-cycle name
    # is the source of truth).
    for df, label in ((bdeck_df, 'bdeck'), (adeck_df, 'adeck')):
        name = _name_from(df, label)
        if name:
            logger.debug(f"longsid: '{name}{sid_lc}' "
                         f"(from {label}, idate={idate})")
            return f"{name}{sid_lc}"

    # 4. Bare sid.
    logger.debug(f"longsid: '{sid_lc}' (bare sid fallback)")
    return sid_lc


def atcf_from_listfile(odir, sid=''):
    """Fallback ATCF lookup: read the spawn-written ``ATCF_FILES.dat``.

    The spawn scripts locate the ATCF with a recursive ``find`` and write the
    resolved full path(s) to ``<odir>/ATCF_FILES.dat`` (this is what polar and
    airsea consume). The maps and ships modules instead run their own
    non-recursive ``find_atcf_file`` glob over ``ATCF*_DIR``; when those
    namelist dirs point above the actual file (e.g. the ATCF lives under
    ``com/<cycle>/<storm>/``) that glob misses it. This helper lets maps/ships
    fall back to the spawn's already-resolved path.

    Returns an existing ATCF path -- preferring a basename containing ``sid``
    when multiple are listed -- or ``None`` if the file is absent/empty/only
    contains paths that no longer exist.
    """
    path = os.path.join(odir, 'ATCF_FILES.dat')
    if not os.path.isfile(path):
        return None
    try:
        with open(path) as fh:
            lines = [ln.strip() for ln in fh
                     if ln.strip() and ln.strip().upper() != 'NONE']
    except OSError as exc:
        logger.warning(f"atcf_from_listfile: could not read {path}: {exc}")
        return None
    if not lines:
        return None
    # Prefer a listed ATCF whose basename matches the requested storm.
    if sid:
        sid_lc = sid.lower()
        for ln in lines:
            if sid_lc in os.path.basename(ln).lower() and os.path.isfile(ln):
                return ln
    for ln in lines:
        if os.path.isfile(ln):
            return ln
    return None


def walk_files_depth_limited(dirs, max_depth=4):
    """Yield (fullpath, basename) for files within ``max_depth`` levels below
    each directory in ``dirs``.

    Used as a *bounded* recursive ATCF search: it lets find_atcf_file descend
    into nested layouts (e.g. ``com/<cycle>/<storm>/``) when the flat glob over
    ATCF*_DIR misses, while the depth cap guarantees a large ATCF*_DIR (an
    experiment / scrub root) never triggers an unbounded filesystem walk. The
    walk is pruned -- it does not merely filter -- so subtrees below the cap are
    never descended into.
    """
    for top in dirs:
        if not top or not os.path.isdir(top):
            continue
        base_depth = top.rstrip(os.sep).count(os.sep)
        for root, subdirs, files in os.walk(top):
            for fn in files:
                yield os.path.join(root, fn), fn
            # Prune: stop descending once we're max_depth levels below `top`.
            if root.rstrip(os.sep).count(os.sep) - base_depth >= max_depth:
                subdirs[:] = []


def resolve_atcf_fallback(atcf_dirs, sid, idate, tags=None, max_depth=4):
    """Locate an ATCF for ``(sid, idate)`` under ``atcf_dirs``.

    Flat non-recursive glob first, then a depth-capped recursive walk (so a
    nested ``com/<cycle>/<storm>/`` layout is found without risking an
    unbounded walk of a large ATCF*_DIR). Returns a path or None.

    This is a standalone-run safety net for polar/airsea, which normally read
    the spawn-written ``ATCF_FILES.dat``; if that file is absent (e.g. the
    module is run by hand), they fall back to this. maps/ships have the
    equivalent logic inline in their own ``find_atcf_file``.
    """
    import glob
    import re as _re

    dirs = [d for d in (atcf_dirs or []) if d and str(d).strip()]
    if not dirs:
        return None
    sid_lc = (sid or '').lower()
    tags = [t for t in (tags or []) if t and str(t).strip()]
    _BAD = ('.grb2', '.grb', '.idx', '.grib2', '.orig', '.nc')
    _FHR = _re.compile(r'\.f\d{3,4}$')

    def _ok(full, bn):
        bl = bn.lower()
        return (idate in bn and sid_lc in bl and 'atcf' in bl
                and not full.endswith(_BAD)
                and not bn.endswith('.all')
                and not _FHR.search(bn))

    def _rank(path):
        bn = os.path.basename(path)
        tag_match = 0 if any(t in bn for t in tags) else 1
        parent_penalty = 1 if '.parent.' in bn else 0
        return (tag_match, parent_penalty, bn)

    # Tier 1: flat, non-recursive glob (fast, the common case).
    for d in dirs:
        if not os.path.isdir(d):
            continue
        cand = [f for f in glob.glob(os.path.join(d, f'*{idate}*'))
                if _ok(f, os.path.basename(f))]
        if cand:
            cand.sort(key=_rank)
            return cand[0]

    # Tier 2: bounded recursive walk (handles nested layouts).
    walked = [full for full, bn in walk_files_depth_limited(dirs, max_depth)
              if _ok(full, bn)]
    if walked:
        walked.sort(key=_rank)
        return walked[0]

    return None


def find_atcf_file(search_dirs, sid, idate, tags=None):
    """
    Search for an ATCF file matching the storm ID and cycle.

    Parameters
    ----------
    search_dirs : list[str]
        Directories to search, in order of priority.
    sid : str
        Storm ID (e.g., '13L', '04W').
    idate : str
        Forecast cycle (YYYYMMDDHH).
    tags : list[str], optional
        File name tags to look for (e.g., ['trak.hfsb.atcfunix']).

    Returns
    -------
    str or None
        Path to the found ATCF file, or None.
    """
    import glob

    sid_lower = sid.lower()
    for search_dir in search_dirs:
        if not os.path.isdir(search_dir):
            continue

        # Try with tags
        if tags:
            for tag in tags:
                patterns = [
                    os.path.join(search_dir, f"*{sid_lower}*{idate}*{tag}*"),
                    os.path.join(search_dir, f"*{idate}*{sid_lower}*{tag}*"),
                    os.path.join(search_dir, f"*{sid_lower}*{tag}*"),
                ]
                for pat in patterns:
                    matches = glob.glob(pat)
                    if matches:
                        logger.info(f"Found ATCF: {matches[0]}")
                        return matches[0]

        # Try without tags
        patterns = [
            os.path.join(search_dir, f"*{sid_lower}*{idate}*"),
            os.path.join(search_dir, f"*{idate}*{sid_lower}*"),
        ]
        for pat in patterns:
            matches = glob.glob(pat)
            if matches:
                logger.info(f"Found ATCF: {matches[0]}")
                return matches[0]

    logger.warning(f"ATCF file not found for SID={sid}, IDATE={idate}")
    return None
