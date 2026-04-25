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
