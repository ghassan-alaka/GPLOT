"""
Ocean model data reader for GPLOT.

Abstracts HYCOM vs MOM6 differences so the calling scripts see a
uniform interface regardless of the underlying ocean model source.

Supported sources
-----------------
HYCOM : 3D + 2D separate netCDF files; Z-level depths; bathymetry
        read from binary .a/.b fix files.
MOM6  : Single netCDF file on z_l / z_i depth coordinates; U/V on a
        B-grid (xq, yq) that is interpolated to the scalar grid (xh, yh).

Variable abstraction
--------------------
Use ``get_ocean_var(ds, var, ocean_source)`` to fetch any supported field
by its abstract name.  The function handles unit conversions (e.g., SSH
m→cm, OHC integration) and returns the same dict schema used by
``grib_reader.get_var_2d`` (keys: data, lat, lon, units).
"""

import os
import struct
import logging

import numpy as np
import xarray as xr

logger = logging.getLogger(__name__)

# Seawater physical constants (shared with colormaps, ocean_maps DPI calc)
CP_SW_HYCOM = 3990.0   # J kg⁻¹ K⁻¹ -- HYCOM default seawater specific heat
CP_SW_MOM6  = 3990.0   # J kg⁻¹ K⁻¹ -- same for MOM6
RHO_SW      = 1025.0   # kg m⁻³ -- reference seawater density
KJ_CM2_PER_J_M2 = 1e-7  # unit conversion: J/m² → kJ/cm²

# Temperature threshold for OHC integration (°C)
OHC_T_THRESHOLD = 26.0


# ---------------------------------------------------------------------------
# HYCOM binary bathymetry reader
# ---------------------------------------------------------------------------

def read_hycom_depth(fix_dir, ocean_dsource, ocean_cfg):
    """
    Read the HYCOM binary bathymetry fix file.

    Looks for ``{fix_dir}/{ocean_dsource}_hycom_{ocean_cfg}.basin.regional.depth``
    (two files: ``.a`` binary + ``.b`` header).

    Parameters
    ----------
    fix_dir : str
        Directory containing HYCOM fix files.
    ocean_dsource : str
        Ocean data source tag (e.g., 'HWRF', 'HAFS').
    ocean_cfg : str
        Ocean configuration tag (e.g., 'NHC').

    Returns
    -------
    numpy.ndarray
        2D masked depth array (m); land masked as NaN.
    """
    base = os.path.join(fix_dir, f"{ocean_dsource}_hycom_{ocean_cfg}.basin.regional.depth")
    a_file = base + '.a'
    b_file = base + '.b'

    if not os.path.isfile(a_file):
        raise FileNotFoundError(f"HYCOM depth .a file not found: {a_file}")
    if not os.path.isfile(b_file):
        raise FileNotFoundError(f"HYCOM depth .b file not found: {b_file}")

    # Parse grid dimensions from .b header
    idm, jdm = None, None
    with open(b_file, 'r') as f:
        for line in f:
            line = line.strip()
            if 'idm' in line.lower():
                try:
                    idm = int(line.split('=')[-1].split()[0])
                except (ValueError, IndexError):
                    pass
            if 'jdm' in line.lower():
                try:
                    jdm = int(line.split('=')[-1].split()[0])
                except (ValueError, IndexError):
                    pass

    if idm is None or jdm is None:
        raise ValueError(f"Could not parse idm/jdm from {b_file}")

    # Read binary field: big-endian 32-bit floats, padded to 4096-word records
    pad = ((idm * jdm + 4095) // 4096) * 4096
    with open(a_file, 'rb') as f:
        raw = f.read(pad * 4)

    data = np.array(struct.unpack(f'>{pad}f', raw), dtype=np.float32)
    depth = data[:jdm * idm].reshape(jdm, idm).astype(float)

    # HYCOM convention: land = 0.0
    depth[depth == 0.0] = np.nan

    logger.debug(f"Read HYCOM depth: {jdm}×{idm}, range {np.nanmin(depth):.0f}–{np.nanmax(depth):.0f} m")
    return depth


# ---------------------------------------------------------------------------
# Dataset openers
# ---------------------------------------------------------------------------

def open_hycom(filepath_3d, filepath_2d=None, bounds=None, wrap_lon=False):
    """
    Open HYCOM netCDF output.

    Parameters
    ----------
    filepath_3d : str
        Path to the 3D HYCOM file (temperature, salinity, currents, depth
        variables on Z levels).
    filepath_2d : str, optional
        Path to the 2D HYCOM file (SSH, SHF, etc.).  If None, derived as
        ``filepath_3d.replace('3z', '2d')``.
    bounds : tuple, optional
        ``(lon_min, lon_max, lat_min, lat_max)`` to subset.
    wrap_lon : bool, optional
        If True, add 360 to longitudes ≤ 0 so the grid spans 0–360.

    Returns
    -------
    tuple[xr.Dataset, xr.Dataset or None]
        ``(ds3d, ds2d)`` — 2D dataset is None if the file does not exist.
    """
    if filepath_2d is None:
        filepath_2d = filepath_3d.replace('3z', '2d')

    ds3d = xr.open_dataset(filepath_3d)
    ds2d = xr.open_dataset(filepath_2d) if os.path.isfile(filepath_2d) else None

    # HYCOM longitude convention: may be in 0-360 or -180-180
    for ds in [ds3d, ds2d]:
        if ds is None:
            continue
        lon_name = _find_coord(ds, ('Longitude', 'lon', 'longitude', 'XLONG'))
        if lon_name and wrap_lon:
            ds[lon_name] = xr.where(ds[lon_name] <= 0, ds[lon_name] + 360, ds[lon_name])

    if bounds is not None:
        ds3d = _subset_bounds(ds3d, bounds)
        if ds2d is not None:
            ds2d = _subset_bounds(ds2d, bounds)

    logger.info(f"Opened HYCOM 3D: {os.path.basename(filepath_3d)}")
    return ds3d, ds2d


def open_mom6(filepath, bounds=None):
    """
    Open MOM6 netCDF output.

    Interpolates U/V from the B-grid stagger (xq, yq) to the scalar grid
    (xh, yh) so all variables share the same horizontal coordinates.

    Parameters
    ----------
    filepath : str
        Path to the MOM6 netCDF file.
    bounds : tuple, optional
        ``(lon_min, lon_max, lat_min, lat_max)`` to subset.

    Returns
    -------
    xr.Dataset
        Dataset with all variables on the scalar (xh, yh) grid.
    """
    ds = xr.open_dataset(filepath)

    # Interpolate staggered velocity grid → scalar grid if both exist
    if 'xq' in ds.coords and 'yq' in ds.coords:
        if 'xh' in ds.coords and 'yh' in ds.coords:
            ds = ds.interp({'xq': ds.coords['xh'], 'yq': ds.coords['yh']})

    if bounds is not None:
        ds = _subset_bounds(ds, bounds)

    logger.info(f"Opened MOM6: {os.path.basename(filepath)}")
    return ds


# ---------------------------------------------------------------------------
# Abstract variable accessor
# ---------------------------------------------------------------------------

_OCEAN_VAR_MAP = {
    # Abstract var: {source: (dataset_key, unit_info)}
    # 'dataset_key' is the variable name inside the netCDF file.
    # unit_info is a string describing what get_ocean_var returns.
    'SST':  {'HYCOM': ('temperature', '°C'),  'MOM6': ('temp', '°C')},
    'SSS':  {'HYCOM': ('salinity',    'psu'), 'MOM6': ('so',   'psu')},
    'SSH':  {'HYCOM': ('ssh',         'cm'),  'MOM6': ('SSH',  'cm')},
    'MLD':  {'HYCOM': ('mixed_layer_thickness', 'm'), 'MOM6': ('MLD_0125', 'm')},
    'ISO26':{'HYCOM': ('depth of 26C isotherm', 'm'), 'MOM6': None},
    'ISO20':{'HYCOM': ('depth of 20C isotherm', 'm'), 'MOM6': None},
    'OHC':  {'HYCOM': ('ocean_heat_content', 'kJ/cm²'), 'MOM6': None},
    'SHF':  {'HYCOM': ('surface_heat_flux', 'W/m²'), 'MOM6': ('LwLatSens', 'W/m²')},
    'DSST': {'HYCOM': None, 'MOM6': None},   # tendencies computed externally
    'DSSH': {'HYCOM': None, 'MOM6': None},
    'DOHC': {'HYCOM': None, 'MOM6': None},
    'MLT':  {'HYCOM': None, 'MOM6': ('MLD_0125', 'm')},  # MOM6: reuse MLD var; HYCOM: compute from T profile
    'MLS':  {'HYCOM': None, 'MOM6': ('so',       'psu')},
    'CONV': {'HYCOM': None, 'MOM6': None},    # computed from u/v in plot_ocean_maps
    'VORT_OCN': {'HYCOM': None, 'MOM6': None},
    'DPI':  {'HYCOM': None, 'MOM6': None},    # computed analytically
}


def get_ocean_var(ds, var, ocean_source, bounds=None):
    """
    Fetch an ocean variable by abstract name.

    Handles unit conversions (SSH m→cm, OHC integration) and returns a
    dict with the same schema as ``grib_reader.get_var_2d``.

    Parameters
    ----------
    ds : xr.Dataset or tuple
        For HYCOM, pass ``(ds3d, ds2d)`` tuple.  For MOM6, pass the
        single dataset returned by ``open_mom6()``.
    var : str
        Abstract variable name (e.g., ``'SST'``, ``'OHC'``, ``'SSH'``).
    ocean_source : str
        ``'HYCOM'`` or ``'MOM6'``.
    bounds : tuple, optional
        ``(lon_min, lon_max, lat_min, lat_max)`` — applied if not already
        subsetted at open time.

    Returns
    -------
    dict
        ``{data, lat, lon, units, var}`` — ``data`` is a 2D numpy array.
        Returns ``None`` if the variable cannot be computed from the
        provided dataset.
    """
    ocean_source = ocean_source.upper()

    if isinstance(ds, tuple):
        ds3d, ds2d = ds
    else:
        ds3d = ds
        ds2d = ds

    mapping = _OCEAN_VAR_MAP.get(var, {}).get(ocean_source)

    # --- OHC: integrate T profile (same formula for HYCOM & MOM6) ---
    if var == 'OHC':
        return _compute_ohc(ds3d, ocean_source, bounds)

    # --- SSH: convert m → cm ---
    if var == 'SSH':
        src_ds = ds2d if ocean_source == 'HYCOM' else ds3d
        arr, lat, lon = _extract_2d(src_ds, mapping[0] if mapping else 'SSH', bounds)
        if arr is None:
            return None
        return _make_result(arr * 100.0, lat, lon, 'cm', var)

    # --- SHF: MOM6 sums LwLatSens + SW ---
    if var == 'SHF' and ocean_source == 'MOM6':
        return _compute_mom6_shf(ds3d, bounds)

    # --- MLT for HYCOM: extract SST at surface ---
    if var == 'MLT' and ocean_source == 'HYCOM':
        return get_ocean_var(ds, 'SST', ocean_source, bounds)

    # --- MLS for HYCOM: extract surface salinity ---
    if var == 'MLS' and ocean_source == 'HYCOM':
        arr, lat, lon = _extract_surface(ds3d, 'salinity', bounds)
        if arr is None:
            return None
        return _make_result(arr, lat, lon, 'psu', var)

    # --- ISO26 / ISO20 for MOM6: compute from temperature profile ---
    if var in ('ISO26', 'ISO20') and ocean_source == 'MOM6':
        return _compute_mom6_isotherm(ds3d, var, bounds)

    # Generic lookup
    if mapping is None:
        logger.debug(f"No mapping for ({var}, {ocean_source})")
        return None

    field_name, units = mapping
    src_ds = ds2d if (ocean_source == 'HYCOM' and ds2d is not None and field_name in ds2d) else ds3d
    arr, lat, lon = _extract_2d(src_ds, field_name, bounds)
    if arr is None:
        return None
    return _make_result(arr, lat, lon, units, var)


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

def _make_result(data, lat, lon, units, var):
    return {'data': np.asarray(data), 'lat': np.asarray(lat),
            'lon': np.asarray(lon), 'units': units, 'var': var}


def _find_coord(ds, candidates):
    for name in candidates:
        if name in ds.coords or name in ds.data_vars:
            return name
    return None


def _subset_bounds(ds, bounds):
    lon_min, lon_max, lat_min, lat_max = bounds
    lon_name = _find_coord(ds, ('Longitude', 'lon', 'longitude', 'xh', 'XLONG'))
    lat_name = _find_coord(ds, ('Latitude', 'lat', 'latitude', 'yh', 'XLAT'))
    if lon_name and lat_name:
        lon_vals = ds[lon_name]
        lat_vals = ds[lat_name]
        lon_mask = (lon_vals >= lon_min) & (lon_vals <= lon_max)
        lat_mask = (lat_vals >= lat_min) & (lat_vals <= lat_max)
        ds = ds.where(lon_mask & lat_mask, drop=True)
    return ds


def _extract_2d(ds, field_name, bounds=None):
    """Extract a 2D field from a dataset, returning (array, lat, lon)."""
    if ds is None or field_name not in ds:
        return None, None, None
    da = ds[field_name]
    # Drop time / Z dimensions if present, take first slice
    for dim in list(da.dims):
        if dim not in ('Latitude', 'Longitude', 'lat', 'latitude', 'lon',
                       'longitude', 'xh', 'yh', 'x', 'y'):
            da = da.isel({dim: 0})
    lat_name = _find_coord(ds, ('Latitude', 'lat', 'latitude', 'yh'))
    lon_name = _find_coord(ds, ('Longitude', 'lon', 'longitude', 'xh'))
    lat = ds[lat_name].values if lat_name else np.arange(da.shape[0])
    lon = ds[lon_name].values if lon_name else np.arange(da.shape[1])
    return da.values, lat, lon


def _extract_surface(ds, field_name, bounds=None):
    """Extract the shallowest level of a 3D field."""
    if ds is None or field_name not in ds:
        return None, None, None
    da = ds[field_name]
    # Take the first (shallowest) depth level
    z_name = _find_coord(ds, ('Z', 'z', 'z_l', 'depth', 'lev', 'level'))
    if z_name and z_name in da.dims:
        da = da.isel({z_name: 0})
    # Drop time dimension
    for dim in list(da.dims):
        if dim in ('MT', 'Time', 'time', 't'):
            da = da.isel({dim: 0})
    lat_name = _find_coord(ds, ('Latitude', 'lat', 'latitude', 'yh'))
    lon_name = _find_coord(ds, ('Longitude', 'lon', 'longitude', 'xh'))
    lat = ds[lat_name].values if lat_name else np.arange(da.shape[0])
    lon = ds[lon_name].values if lon_name else np.arange(da.shape[1])
    return da.values, lat, lon


def _compute_ohc(ds3d, ocean_source, bounds):
    """Integrate ocean heat content above the 26°C isotherm."""
    temp_name = 'temperature' if ocean_source == 'HYCOM' else 'temp'
    z_name = _find_coord(ds3d, ('Z', 'z', 'z_l', 'depth'))
    if temp_name not in ds3d or z_name is None:
        return None

    T = ds3d[temp_name]
    # Drop time dimension
    for dim in list(T.dims):
        if dim in ('MT', 'Time', 'time', 't'):
            T = T.isel({dim: 0})

    z = ds3d[z_name].values  # positive downward (m)

    lat_name = _find_coord(ds3d, ('Latitude', 'lat', 'latitude', 'yh'))
    lon_name = _find_coord(ds3d, ('Longitude', 'lon', 'longitude', 'xh'))
    lat = ds3d[lat_name].values if lat_name else np.arange(T.shape[-2])
    lon = ds3d[lon_name].values if lon_name else np.arange(T.shape[-1])

    T_vals = T.values  # (nz, ny, nx)

    # dz at each level (use midpoint differences)
    dz = np.abs(np.gradient(z))

    # Clip temperature anomaly above threshold: max(T - 26, 0)
    dT = np.maximum(T_vals - OHC_T_THRESHOLD, 0.0)

    # OHC = rho * cp * sum(dT * dz) * kJ/cm² conversion
    ohc = RHO_SW * CP_SW_MOM6 * np.nansum(dT * dz[:, None, None], axis=0) * KJ_CM2_PER_J_M2

    return _make_result(ohc, lat, lon, 'kJ/cm²', 'OHC')


def _compute_mom6_shf(ds, bounds):
    """Compute MOM6 net surface heat flux: LwLatSens + SW."""
    shf = None
    for rad_key in ('LwLatSens', 'SW'):
        if rad_key in ds:
            da = ds[rad_key]
            for dim in list(da.dims):
                if dim in ('Time', 'time', 't'):
                    da = da.isel({dim: 0})
            shf = da.values if shf is None else shf + da.values

    if shf is None:
        return None

    lat_name = _find_coord(ds, ('Latitude', 'lat', 'latitude', 'yh'))
    lon_name = _find_coord(ds, ('Longitude', 'lon', 'longitude', 'xh'))
    lat = ds[lat_name].values if lat_name else np.arange(shf.shape[-2])
    lon = ds[lon_name].values if lon_name else np.arange(shf.shape[-1])
    return _make_result(shf, lat, lon, 'W/m²', 'SHF')


def _compute_mom6_isotherm(ds, var, bounds):
    """Compute depth of 26°C (ISO26) or 20°C (ISO20) isotherm from MOM6 T profile."""
    thresh = 26.0 if var == 'ISO26' else 20.0
    if 'temp' not in ds:
        return None

    T = ds['temp']
    for dim in list(T.dims):
        if dim in ('Time', 'time', 't'):
            T = T.isel({dim: 0})

    z_name = _find_coord(ds, ('z_l', 'Z', 'z', 'depth'))
    if z_name is None:
        return None
    z = ds[z_name].values  # (nz,)

    T_vals = T.values  # (nz, ny, nx)
    nz, ny, nx = T_vals.shape

    # At each column find the deepest level where T >= threshold
    iso = np.full((ny, nx), np.nan)
    for k in range(nz):
        mask = T_vals[k] >= thresh
        iso = np.where(mask, z[k], iso)

    lat_name = _find_coord(ds, ('Latitude', 'lat', 'latitude', 'yh'))
    lon_name = _find_coord(ds, ('Longitude', 'lon', 'longitude', 'xh'))
    lat = ds[lat_name].values if lat_name else np.arange(ny)
    lon = ds[lon_name].values if lon_name else np.arange(nx)
    return _make_result(iso, lat, lon, 'm', var)
