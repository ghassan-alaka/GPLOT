"""
Variable table (Vtable) lookup for GPLOT.

Replaces findVarName() from GPLOT_util.ncl. Maps abstract variable names
(e.g., 'MSLP', 'U', 'T') to model-specific GRIB2 field names
(e.g., 'MSLET_P0_L101_GLL0') via the two-level Vtable system:

  Vtable.master: abstract_name -> catalog_number
  Vtable.{MODEL}: grib2_field_name -> catalog_number

The lookup joins on catalog_number to find the GRIB2 field name for
a given abstract variable and model data source.
"""

import os
import re
import logging

logger = logging.getLogger(__name__)

# Cache loaded vtables to avoid re-reading files
_vtable_cache = {}


def _load_vtable(vtable_path):
    """Load a Vtable file into a dict mapping catalog_number -> field_name."""
    if vtable_path in _vtable_cache:
        return _vtable_cache[vtable_path]

    if not os.path.isfile(vtable_path):
        raise FileNotFoundError(f"Vtable not found: {vtable_path}")

    entries = {}
    with open(vtable_path, 'r') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith(';') or line.startswith('#'):
                continue

            parts = line.split()
            if len(parts) < 2:
                continue

            field_name = parts[0]
            try:
                catalog_num = int(parts[1])
            except ValueError:
                continue

            entries[catalog_num] = field_name

    _vtable_cache[vtable_path] = entries
    return entries


def _load_master_vtable(vtable_path):
    """Load the master Vtable into a dict mapping abstract_name -> catalog_number."""
    if vtable_path in _vtable_cache:
        return _vtable_cache[vtable_path]

    if not os.path.isfile(vtable_path):
        raise FileNotFoundError(f"Master Vtable not found: {vtable_path}")

    entries = {}
    with open(vtable_path, 'r') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith(';') or line.startswith('#'):
                continue

            parts = line.split()
            if len(parts) < 2:
                continue

            abstract_name = parts[0]
            try:
                catalog_num = int(parts[1])
            except ValueError:
                continue

            entries[abstract_name] = catalog_num

    _vtable_cache[vtable_path] = entries
    return entries


def _resolve_dsource_vtable(dsource):
    """
    Map a DSOURCE value to the corresponding Vtable name.

    Handles variations like 'HFSB_MULTISTORM' -> 'HAFS'.
    """
    dsource_upper = dsource.upper()

    # Common mappings
    if 'HFS' in dsource_upper or 'HAFS' in dsource_upper:
        return 'HAFS'
    if 'HWRF' in dsource_upper:
        return 'HWRF'
    if 'GFS' in dsource_upper or 'AVNO' in dsource_upper:
        return 'GFS'
    if 'HMON' in dsource_upper:
        return 'HMON'
    if 'ECMWF' in dsource_upper or 'EGRR' in dsource_upper:
        return 'ECMWF'

    return dsource_upper


def find_var_name(dsource, var, level='', gplot_dir=None):
    """
    Look up the GRIB2 field name for an abstract variable.

    Implements the Vtable lookup chain:
      1. Look up abstract var in Vtable.master -> catalog_number
      2. Handle level suffix (e.g., var='U', level='10' -> 'U10' -> catalog 201)
      3. Look up catalog_number in Vtable.{MODEL} -> grib2_field_name

    Parameters
    ----------
    dsource : str
        Data source (e.g., 'HAFS', 'GFS', 'HFSB_MULTISTORM').
    var : str
        Abstract variable name (e.g., 'MSLP', 'T', 'U', 'RH').
    level : str, optional
        Level code (e.g., '850', '10', '02000850d' for difference).
    gplot_dir : str, optional
        GPLOT root directory. If None, uses $GPLOT_DIR environment variable.

    Returns
    -------
    str
        GRIB2 field name, or empty string if not found.
    """
    if gplot_dir is None:
        gplot_dir = os.environ.get('GPLOT_DIR', '.')

    tbl_dir = os.path.join(gplot_dir, 'tbl')
    master_path = os.path.join(tbl_dir, 'Vtable.master')
    model_name = _resolve_dsource_vtable(dsource)
    model_path = os.path.join(tbl_dir, f'Vtable.{model_name}')

    # Load tables
    try:
        master = _load_master_vtable(master_path)
        model = _load_vtable(model_path)
    except FileNotFoundError as e:
        logger.warning(str(e))
        return ''

    # Try with level suffix first (e.g., U + 10 -> U10)
    # Level codes like '850', '200' are pressure levels (3D var, no suffix needed)
    # Level codes like '10' for surface (U10, V10) need suffix
    level_str = str(level).strip()

    # Build candidate abstract names
    candidates = []
    if level_str and level_str != 'N/A':
        # Check for surface-level indicators
        combined = f"{var}{level_str}"
        candidates.append(combined)

    candidates.append(var)

    catalog_num = None
    for name in candidates:
        if name in master:
            catalog_num = master[name]
            break

    if catalog_num is None:
        logger.debug(f"Variable '{var}' (level='{level}') not found in master Vtable")
        return ''

    # Look up in model-specific Vtable
    if catalog_num in model:
        result = model[catalog_num]
        logger.debug(f"Vtable lookup: {var}(level={level}) -> {catalog_num} -> {result}")
        return result

    logger.debug(f"Catalog #{catalog_num} for '{var}' not in {model_name} Vtable")
    return ''


def get_abstract_var_info(gplot_dir=None):
    """
    Get the full list of abstract variables from Vtable.master.

    Returns
    -------
    dict
        Mapping of abstract variable name -> (catalog_number, description).
    """
    if gplot_dir is None:
        gplot_dir = os.environ.get('GPLOT_DIR', '.')

    master_path = os.path.join(gplot_dir, 'tbl', 'Vtable.master')

    if not os.path.isfile(master_path):
        return {}

    result = {}
    with open(master_path, 'r') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith(';') or line.startswith('#'):
                continue

            parts = line.split('\t')
            if len(parts) < 3:
                parts = line.split(None, 2)

            if len(parts) >= 3:
                name = parts[0].strip()
                try:
                    num = int(parts[1].strip())
                except ValueError:
                    continue
                desc = parts[2].strip().strip('"')
                result[name] = (num, desc)

    return result


def clear_cache():
    """Clear the Vtable cache (useful for testing)."""
    _vtable_cache.clear()
