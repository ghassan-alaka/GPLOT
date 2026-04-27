"""
Namelist parsing for GPLOT.

Replaces Read_Master_Namelist() from GPLOT_main.ncl and nml_read() from
GPLOT_func.ncl. Handles the semicolon-commented KEY = VALUE format used
by GPLOT master namelists, the tab-delimited maps/ships namelists, and
the stats namelist.
"""

import os
import re
import logging

logger = logging.getLogger(__name__)

# Default values for master namelist keys (matches NCL defaults)
_MASTER_DEFAULTS = {
    'DOMAIN': 'atl',
    'TIER': 'Tier1',
    'DSOURCE': 'HAFS',
    'MACHINE': 'JET',
    'SYS_ENV': 'JET',
    'IDATE': '',
    'SID': '',
    'ENSID': '',
    'MODELID': '',
    'MID': '',
    'ATCF_REQD': True,
    'EXPT': 'HWRF_Forecast',
    'IDIR': '.',
    'ITAG': '',
    'EXT': '',
    'ODIR': '.',
    'ODIR_TYPE': 0,
    'INIT_HR': 0,
    'FNL_HR': 126,
    'FMT_HR': 3,
    'DT': 3,
    'IS_MSTORM': False,
    'DO_RMWHITE': False,
    'DO_SRCLBL': False,
    'PIV': 180.0,
    'DO_CONVERTGIF': False,
    'NMAX_MAPS': 100,
    'DO_TITLES': True,
    'DO_DISCLAIMER': True,
    'ATCF1_DIR': 'MISSING',
    'ATCF1_TAG': 'MISSING',
    'ATCF2_DIR': 'MISSING',
    'ATCF2_TAG': 'MISSING',
    'ADECK_DIR': 'MISSING',
    'BDECK_DIR': 'MISSING',
    'MCODE': 'HWRF',
    'MCODEI': 'MISSING',
    'MCODE12': 'MISSING',
    'MORIG': None,  # defaults to MCODE
    'DO_PDF': False,
    'FORCE': False,
    'DO_INTERP': False,
    'DO_MARKERS': True,
    'MAX_FHR': 180,
    'DO_FHRLABELS': True,
    'NTREND': 6,
    'LEAD_TIMES': [0, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156, 168],
    'DO_MAPS': False,
    'DO_STATS': False,
    'DO_SHIPS': False,
    'DO_POLAR': False,
    'DO_AIRSEA': False,
    'DO_OCEAN_MAPS': False,
    'DO_OCEAN_OBS': False,
    'ENSMEM': 0,
    'DO_TOPOMASK': False,
    'BATCH_MODE': 'SBatch',
    'CPU_ACCT': '',
}

# Keys that should be parsed as booleans
_BOOL_KEYS = {
    'IS_MSTORM', 'DO_RMWHITE', 'DO_SRCLBL', 'DO_CONVERTGIF',
    'DO_TITLES', 'DO_DISCLAIMER', 'DO_PDF', 'FORCE', 'DO_INTERP',
    'DO_MARKERS', 'DO_FHRLABELS', 'ATCF_REQD', 'DO_TOPOMASK',
    'DO_MAPS', 'DO_STATS', 'DO_SHIPS', 'DO_POLAR',
    'DO_AIRSEA', 'DO_OCEAN_MAPS', 'DO_OCEAN_OBS',
    'DO_DBZ', 'OCEAN_WRAP_LON',
}

# Keys that should be parsed as integers
_INT_KEYS = {
    'ODIR_TYPE', 'INIT_HR', 'FNL_HR', 'FMT_HR', 'DT',
    'NMAX_MAPS', 'MAX_FHR', 'NTREND', 'ENSMEM',
}

# Keys that should be parsed as floats
_FLOAT_KEYS = {
    'PIV', 'RESOLUTION', 'RMAX',
}

# Keys that should be parsed as space-separated lists
_LIST_KEYS = {
    'DOMAIN', 'TIER', 'ITAG', 'EXT',
    'TRKM', 'TRKIM', 'INTM', 'PRSM',
    'TRKMI', 'INTMI',
    'TRKMT', 'INTMT', 'PRSMT',
    'ETM', 'EIM', 'LTM', 'LIM',
    'LEAD_TIMES',
}


def _parse_value(key, raw_value):
    """Convert a raw string value to the appropriate Python type."""
    val = raw_value.strip()

    if key in _BOOL_KEYS:
        return val == 'True'

    if key in _INT_KEYS:
        try:
            return int(float(val))
        except (ValueError, TypeError):
            return _MASTER_DEFAULTS.get(key, 0)

    if key in _FLOAT_KEYS:
        try:
            return float(val)
        except (ValueError, TypeError):
            return _MASTER_DEFAULTS.get(key, 0.0)

    if key in _LIST_KEYS:
        parts = val.split()
        if key == 'LEAD_TIMES':
            try:
                return [int(x) for x in parts]
            except ValueError:
                return parts
        return parts

    return val


def read_master_namelist(nml_path):
    """
    Parse a GPLOT master namelist file.

    The format uses semicolon comments and KEY = VALUE pairs:
      - Lines starting with ';' are comments
      - Lines like ';KEY; = VALUE' are commented-out settings (skipped)
      - Lines like 'KEY = VALUE' are active settings
      - Empty values (e.g., 'IDATE =') are preserved as empty strings
      - Space-separated values (e.g., 'TRKM = HFXM HFXB') become lists
        for keys in _LIST_KEYS

    Parameters
    ----------
    nml_path : str
        Path to the master namelist file.

    Returns
    -------
    dict
        Dictionary of namelist key-value pairs with appropriate types.
    """
    if not os.path.isfile(nml_path):
        raise FileNotFoundError(f"Master namelist not found: {nml_path}")

    nml = dict(_MASTER_DEFAULTS)

    with open(nml_path, 'r') as f:
        for line in f:
            line = line.strip()

            # Skip empty lines
            if not line:
                continue

            # Skip comment lines (start with ';')
            if line.startswith(';'):
                continue

            # Skip lines without '='
            if '=' not in line:
                continue

            # Split on first '=' only
            parts = line.split('=', 1)
            key = parts[0].strip()
            raw_value = parts[1].strip() if len(parts) > 1 else ''

            # Strip any trailing inline comments (text after ; not in a value)
            # But be careful: semicolons can appear in paths, so only strip
            # if the semicolon is preceded by whitespace
            if ';' in raw_value:
                # Only treat as comment if preceded by whitespace
                match = re.match(r'^(.*?)\s+;.*$', raw_value)
                if match:
                    raw_value = match.group(1).strip()

            # Skip commented-out keys (e.g., ;KEY; = VALUE parsed as KEY; key)
            if key.endswith(';') or ';' in key:
                continue

            # Parse the value
            nml[key] = _parse_value(key, raw_value)

    # Apply derived defaults
    if nml.get('MORIG') is None:
        nml['MORIG'] = nml.get('MCODE', 'HWRF')

    # Derive MCODEV (verification model code)
    if nml.get('MCODE') == 'AVNO':
        nml['MCODEV'] = 'GFSO'
    else:
        nml['MCODEV'] = nml.get('MCODE', '')

    if nml.get('MCODEI') == 'AVNI':
        nml['MCODEVI'] = 'GFSI'
    else:
        nml['MCODEVI'] = nml.get('MCODEI', '')

    # Handle MACHINE / SYS_ENV fallback
    if 'MACHINE' not in nml or nml['MACHINE'] == 'JET':
        if 'SYS_ENV' in nml and nml['SYS_ENV'] != 'JET':
            nml['MACHINE'] = nml['SYS_ENV']

    # If DO_PDF is True, override conversion settings
    if nml.get('DO_PDF'):
        nml['DO_CONVERTGIF'] = False
        nml['DO_RMWHITE'] = False

    logger.info(f"Parsed master namelist: {nml_path} ({len(nml)} keys)")
    return nml


def read_maps_namelist(nml_path):
    """
    Parse a GPLOT maps module namelist (tab/whitespace-delimited table).

    The first line is a header row with column names. Subsequent lines
    define one plot recipe each. Example columns:
      PLOT_ON, BASE_CN_FILL, LEV1, OV_CN_LINE, LEV2, OV_CN_LINE2, LEV3,
      OV_VC_WIND, LEV4, OV_MAX_MIN, LEV5, OV_STLINE, LEV6, FILE_NAME

    Parameters
    ----------
    nml_path : str
        Path to the maps namelist file.

    Returns
    -------
    list[dict]
        List of plot recipe dictionaries, one per enabled row.
    """
    if not os.path.isfile(nml_path):
        raise FileNotFoundError(f"Maps namelist not found: {nml_path}")

    with open(nml_path, 'r') as f:
        lines = [l.strip() for l in f if l.strip() and not l.strip().startswith(';')]

    if not lines:
        return []

    # Parse header row
    header = lines[0].split()
    recipes = []

    for line in lines[1:]:
        fields = line.split()
        if len(fields) < len(header):
            # Pad with N/A
            fields.extend(['N/A'] * (len(header) - len(fields)))

        recipe = {}
        for i, col in enumerate(header):
            val = fields[i] if i < len(fields) else 'N/A'
            recipe[col] = val

        # Only include enabled recipes
        if recipe.get('PLOT_ON', 'False') == 'True':
            recipes.append(recipe)

    logger.info(f"Parsed maps namelist: {nml_path} ({len(recipes)} enabled plots)")
    return recipes


def read_ships_namelist(nml_path):
    """
    Parse a GPLOT ships module namelist.

    Similar tab-delimited format to maps namelist but with columns for
    DATA_ON, PLOT_ON, and diagnostic variable specifications.

    Parameters
    ----------
    nml_path : str
        Path to the ships namelist file.

    Returns
    -------
    list[dict]
        List of diagnostic recipe dictionaries.
    """
    if not os.path.isfile(nml_path):
        raise FileNotFoundError(f"Ships namelist not found: {nml_path}")

    with open(nml_path, 'r') as f:
        lines = [l.strip() for l in f if l.strip() and not l.strip().startswith(';')]

    if not lines:
        return []

    header = lines[0].split()
    recipes = []

    for line in lines[1:]:
        fields = line.split()
        if len(fields) < len(header):
            fields.extend(['N/A'] * (len(header) - len(fields)))

        recipe = {}
        for i, col in enumerate(header):
            val = fields[i] if i < len(fields) else 'N/A'
            recipe[col] = val

        recipes.append(recipe)

    logger.info(f"Parsed ships namelist: {nml_path} ({len(recipes)} diagnostics)")
    return recipes


def read_stats_namelist(nml_path):
    """
    Parse a GPLOT stats module namelist.

    Two-column NAME/CHOICE format controlling which stat plot types
    are enabled.

    Parameters
    ----------
    nml_path : str
        Path to the stats namelist file.

    Returns
    -------
    dict
        Dictionary mapping stat option names to their choices (True/False or values).
    """
    if not os.path.isfile(nml_path):
        raise FileNotFoundError(f"Stats namelist not found: {nml_path}")

    with open(nml_path, 'r') as f:
        lines = [l.strip() for l in f if l.strip() and not l.strip().startswith(';')]

    if not lines:
        return {}

    # Check if it has a header
    result = {}
    start = 0
    if lines[0].split()[0].upper() in ('NAME', 'OPTION', 'PLOT_ON'):
        start = 1

    for line in lines[start:]:
        parts = line.split()
        if len(parts) >= 2:
            key = parts[0]
            val = parts[1]
            if val in ('True', 'False'):
                result[key] = val == 'True'
            else:
                result[key] = val

    logger.info(f"Parsed stats namelist: {nml_path} ({len(result)} options)")
    return result


def _read_flag_namelist(nml_path, defaults=None):
    """
    Parse a key,Y/N module namelist.

    Each line has the form ``key_name,Y`` or ``key_name,N``.
    Lines starting with ';' or '#' are treated as comments.

    Parameters
    ----------
    nml_path : str
        Path to the namelist file.
    defaults : dict, optional
        Default values returned for keys not present in the file.

    Returns
    -------
    dict
        ``{key: bool}`` mapping.
    """
    if not os.path.isfile(nml_path):
        raise FileNotFoundError(f"Namelist not found: {nml_path}")

    result = dict(defaults or {})
    with open(nml_path, 'r') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith(';') or line.startswith('#'):
                continue
            parts = line.split(',', 1)
            if len(parts) == 2:
                key = parts[0].strip()
                val = parts[1].strip().upper()
                result[key] = (val == 'Y')
    return result


def read_ocean_maps_namelist(nml_path):
    """
    Parse a GPLOT ocean_maps module namelist.

    Format: one ``key,Y/N`` line per plot type flag.

    Parameters
    ----------
    nml_path : str
        Path to the namelist file.

    Returns
    -------
    dict
        ``{flag_name: bool}`` mapping.
    """
    defaults = {
        'do_iso_26': True,
        'do_iso_20': True,
        'do_ohc': True,
        'do_sfc_conv': True,
        'do_sfc_vort': True,
        'do_ml_conv': True,
        'do_ml_vort': True,
        'do_ssh': True,
        'do_shf': True,
        'do_dpi': False,
        'do_mld': True,
        'do_sss': True,
        'do_mlt': True,
        'do_mls': True,
        'do_ssh_tendency': True,
        'do_ships_output': True,
        'do_sst_tendency': True,
        'do_ohc_tendency': True,
        'do_obs_profiles': False,
    }
    nml = _read_flag_namelist(nml_path, defaults)
    logger.info(f"Parsed ocean_maps namelist: {nml_path}")
    return nml


def read_ocean_obs_namelist(nml_path):
    """
    Parse a GPLOT ocean_obs module namelist.

    Parameters
    ----------
    nml_path : str
        Path to the namelist file.

    Returns
    -------
    dict
        ``{flag_name: bool}`` mapping.
    """
    defaults = {
        'do_ships_output': True,
        'do_sst_ohc_profiles': True,
        'do_dsst_dohc_profiles': False,
        'do_ssh_ssh_tendency_fields': False,
        'do_iso_26_ohc_profiles': False,
        'do_iso_20_mld_profiles': False,
        'do_ssh_tendency_iso_26_tendency': False,
        'do_delta_t_delta_q_shf_fields': False,
        'do_dpi_profiles': False,
        'do_sss_mls_profiles': True,
        'do_sst_mlt_profiles': True,
        'do_iso_26_mlt_profiles': False,
        'do_mlt_mld_profiles': True,
    }
    nml = _read_flag_namelist(nml_path, defaults)
    logger.info(f"Parsed ocean_obs namelist: {nml_path}")
    return nml


def read_airsea_namelist(nml_path):
    """
    Parse a GPLOT airsea.pbl module namelist.

    Parameters
    ----------
    nml_path : str
        Path to the namelist file.

    Returns
    -------
    dict
        ``{flag_name: bool}`` mapping.
    """
    defaults = {
        'do_turb_flux': True,
        'do_total_flux': True,
        'do_theta_e_550': True,
        'do_theta_e_700': True,
        'do_theta_e_850': True,
        'do_delta_t': True,
        'do_delta_q': True,
        'do_gusts': True,
    }
    nml = _read_flag_namelist(nml_path, defaults)
    logger.info(f"Parsed airsea namelist: {nml_path}")
    return nml


def read_polar_namelist(nml_path):
    """
    Parse a GPLOT polar.structure module namelist.

    Parameters
    ----------
    nml_path : str
        Path to the namelist file.

    Returns
    -------
    dict
        ``{flag_name: bool}`` mapping.
    """
    defaults = {
        'do_ur_mean': True,
        'do_vt_mean': True,
        'do_w_mean': True,
        'do_dbz_mean': True,
        'do_rh_mean': True,
        'do_dbz_alongshear': True,
        'do_ur_alongshear': True,
        'do_w_alongshear': True,
        'do_rh_alongshear': True,
        'do_dbz_acrosshear': True,
        'do_ur_acrosshear': True,
        'do_w_acrosshear': True,
        'do_rh_acrosshear': True,
        'do_dbz5km_wavenumber': True,
        'do_rh5km_wavenumber': True,
        'do_vt10_wavenumber': True,
        'do_vt_tendency': False,
        'do_vort_tendency': False,
        'do_ur_pbl_p_mean': True,
        'do_radar_plots': True,
        'do_soundings': True,
        'do_shear_and_rh_plots': True,
        'do_write_netcdf': True,
        'do_tdr_recentering': False,
    }
    nml = _read_flag_namelist(nml_path, defaults)
    logger.info(f"Parsed polar namelist: {nml_path}")
    return nml


def resolve_namelist_path(gplot_dir, module, expt, domain=None, tier=None):
    """
    Resolve the path to a module-specific namelist using the GPLOT cascade.

    Search order (first found wins):
      1. namelist.{module}.{expt}.{domain}.{tier}
      2. namelist.{module}.{expt}.{tier}
      3. namelist.{module}.{domain}.{tier}
      4. namelist.{module}.default.{domain}.{tier}
      5. namelist.{module}.default.{tier}
      6. namelist.{module}.default

    Parameters
    ----------
    gplot_dir : str
        GPLOT root directory.
    module : str
        Module name ('maps', 'ships', 'stats').
    expt : str
        Experiment name.
    domain : str, optional
        Domain name (e.g., 'd03', 'd01').
    tier : str, optional
        Tier name (e.g., 'Tier1').

    Returns
    -------
    str
        Path to the resolved namelist file.

    Raises
    ------
    FileNotFoundError
        If no matching namelist is found.
    """
    parm_dir = os.path.join(gplot_dir, 'parm')

    candidates = []
    if domain and tier:
        candidates.append(f"namelist.{module}.{expt}.{domain}.{tier}")
    if tier:
        candidates.append(f"namelist.{module}.{expt}.{tier}")
    if domain and tier:
        candidates.append(f"namelist.{module}.{domain}.{tier}")
        candidates.append(f"namelist.{module}.default.{domain}.{tier}")
    if tier:
        candidates.append(f"namelist.{module}.default.{tier}")
    candidates.append(f"namelist.{module}.default")

    for name in candidates:
        path = os.path.join(parm_dir, name)
        if os.path.isfile(path):
            logger.info(f"Resolved {module} namelist: {path}")
            return path

    raise FileNotFoundError(
        f"No {module} namelist found for expt={expt}, domain={domain}, tier={tier} "
        f"in {parm_dir}. Tried: {candidates}"
    )
