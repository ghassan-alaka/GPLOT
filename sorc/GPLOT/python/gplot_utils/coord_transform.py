"""
Coordinate transform utilities for GPLOT.

Replaces the Fortran sph2cart.f90 subroutine with a vectorized
NumPy/SciPy implementation. Provides:

  - Spherical (lat/lon) to Cartesian (x, y in km) transformation
    centered on a tropical cyclone position
  - Bilinear interpolation from regular lat/lon grids to Cartesian grids
  - Annular and circular averaging for SHIPS-style diagnostics
"""

import numpy as np
from scipy.interpolate import RegularGridInterpolator

from . import constants as C


def sph2cart(data, lat, lon, tc_lat, tc_lon, x_km, y_km, fill_value=np.nan):
    """
    Transform 2D data from spherical (lat/lon) to Cartesian (x, y) km
    coordinates centered on a tropical cyclone.

    Replaces the Fortran SPH2CART::sph2cart() subroutine using vectorized
    scipy interpolation instead of manual 4-nearest-neighbor lookup.

    Parameters
    ----------
    data : np.ndarray
        2D input data on a regular lat/lon grid, shape (nlat, nlon).
    lat : np.ndarray
        1D latitude array (degrees), must be monotonically sorted.
    lon : np.ndarray
        1D longitude array (degrees), must be monotonically sorted.
    tc_lat : float
        TC center latitude (degrees).
    tc_lon : float
        TC center longitude (degrees).
    x_km : np.ndarray
        1D array of x-coordinates in km (relative to TC center).
    y_km : np.ndarray
        1D array of y-coordinates in km (relative to TC center).
    fill_value : float, optional
        Value to use for points outside the grid (default: NaN).

    Returns
    -------
    np.ndarray
        2D array of shape (len(y_km), len(x_km)) with data interpolated
        to the Cartesian grid.
    """
    R = C.r_earth / 1000.0  # Earth radius in km
    tc_lat_rad = np.radians(tc_lat)
    tc_lon_rad = np.radians(tc_lon)

    # Build 2D meshgrids of x and y in km
    xx, yy = np.meshgrid(x_km, y_km)

    # Convert Cartesian (km) back to lat/lon
    # Latitude: straightforward arc distance
    target_lat = np.degrees(yy / R + tc_lat_rad)

    # Longitude: account for cos(lat) convergence
    cos_lat = np.cos(np.radians(target_lat))
    cos_lat = np.where(np.abs(cos_lat) < 1e-10, 1e-10, cos_lat)
    target_lon = np.degrees(xx / (R * cos_lat) + tc_lon_rad)

    # Ensure latitude is increasing for interpolation
    if lat[0] > lat[-1]:
        lat = lat[::-1]
        data = data[::-1, :]

    # Build interpolator
    interp = RegularGridInterpolator(
        (lat, lon), data,
        method='linear',
        bounds_error=False,
        fill_value=fill_value
    )

    # Flatten, interpolate, reshape
    points = np.column_stack([target_lat.ravel(), target_lon.ravel()])
    result = interp(points).reshape(xx.shape)

    return result


def sph2cart_3d(data_3d, lat, lon, tc_lat, tc_lon, x_km, y_km,
                fill_value=np.nan):
    """
    Transform 3D data (lev, lat, lon) to Cartesian coordinates.

    Applies sph2cart independently at each vertical level.

    Parameters
    ----------
    data_3d : np.ndarray
        3D input data, shape (nlev, nlat, nlon).
    lat, lon, tc_lat, tc_lon, x_km, y_km, fill_value
        Same as sph2cart.

    Returns
    -------
    np.ndarray
        3D array of shape (nlev, len(y_km), len(x_km)).
    """
    nlev = data_3d.shape[0]
    ny, nx = len(y_km), len(x_km)
    result = np.full((nlev, ny, nx), fill_value)

    for k in range(nlev):
        result[k] = sph2cart(data_3d[k], lat, lon, tc_lat, tc_lon,
                             x_km, y_km, fill_value)

    return result


def make_cartesian_grid(radius_km=800, spacing_km=10):
    """
    Create a symmetric Cartesian grid centered on the origin.

    Parameters
    ----------
    radius_km : float
        Half-width of the grid in km (default 800 for SHIPS diagnostics).
    spacing_km : float
        Grid spacing in km (default 10).

    Returns
    -------
    tuple
        (x_km, y_km) 1D arrays in km.
    """
    x_km = np.arange(-radius_km, radius_km + spacing_km, spacing_km,
                      dtype=np.float64)
    y_km = np.arange(-radius_km, radius_km + spacing_km, spacing_km,
                      dtype=np.float64)
    return x_km, y_km


def radial_distance(x_km, y_km):
    """
    Compute the radial distance grid from origin.

    Parameters
    ----------
    x_km : np.ndarray
        1D x-coordinates in km.
    y_km : np.ndarray
        1D y-coordinates in km.

    Returns
    -------
    np.ndarray
        2D array of radial distances (km), shape (len(y_km), len(x_km)).
    """
    xx, yy = np.meshgrid(x_km, y_km)
    return np.sqrt(xx ** 2 + yy ** 2)


def annular_mean(data, x_km, y_km, r_inner, r_outer, min_coverage=0.50):
    """
    Compute the mean of data within an annular region.

    Used for SHIPS diagnostics like SHRD (200-800 km annulus).

    When the storm is near the edge of the GRIB extent (e.g. d03
    moving nest has fallen behind the storm), a large portion of the
    annulus may be NaN. Averaging over only the covered sliver would
    silently write a biased value into the per-FHR DAT file --
    visible downstream in the guidance time series as a "real"
    signal when it's actually just partial-coverage bias. Require at
    least ``min_coverage`` of the annulus area to be finite before
    reporting a value; below that, return NaN so the FHR shows up as
    a gap in the time series rather than a misleading point.

    Parameters
    ----------
    data : np.ndarray
        2D data on the Cartesian grid, shape (len(y_km), len(x_km)).
    x_km : np.ndarray
        1D x-coordinates in km.
    y_km : np.ndarray
        1D y-coordinates in km.
    r_inner : float
        Inner radius of the annulus (km).
    r_outer : float
        Outer radius of the annulus (km).
    min_coverage : float, optional
        Fraction (0..1) of the annulus area that must be finite for a
        value to be returned. Default 0.50. Set to 0.0 to disable the
        coverage gate and recover the legacy "any-points-suffice"
        behavior.

    Returns
    -------
    float
        Mean value within the annulus, or NaN if coverage is below
        ``min_coverage`` (or no annulus cells exist at all).
    """
    r = radial_distance(x_km, y_km)
    in_annulus = (r >= r_inner) & (r <= r_outer)
    n_annulus = int(in_annulus.sum())
    if n_annulus == 0:
        return np.nan

    valid = in_annulus & np.isfinite(data)
    coverage = valid.sum() / float(n_annulus)
    if coverage < min_coverage:
        return np.nan

    return np.nanmean(data[valid])


def circular_mean(data, x_km, y_km, radius, min_coverage=0.50):
    """
    Compute the mean of data within a circular region.

    Thin wrapper around :func:`annular_mean` with ``r_inner=0``. The
    same ``min_coverage`` gate applies; see the docstring on
    ``annular_mean`` for the rationale (avoid silently biased per-FHR
    DAT values when the storm is near the GRIB edge).

    Parameters
    ----------
    data : np.ndarray
        2D data on the Cartesian grid.
    x_km, y_km : np.ndarray
        1D coordinate arrays.
    radius : float
        Radius of the circle (km).
    min_coverage : float, optional
        Pass-through to ``annular_mean``. Default 0.50.

    Returns
    -------
    float
        Mean value within the circle, or NaN if coverage is below
        ``min_coverage``.
    """
    return annular_mean(data, x_km, y_km, 0, radius,
                        min_coverage=min_coverage)


def azimuthal_mean(data, x_km, y_km, radii):
    """
    Compute azimuthally averaged radial profile.

    Parameters
    ----------
    data : np.ndarray
        2D data on the Cartesian grid.
    x_km, y_km : np.ndarray
        1D coordinate arrays.
    radii : np.ndarray
        1D array of radial bin edges (km).

    Returns
    -------
    np.ndarray
        Azimuthal mean at each radial bin center.
    """
    r = radial_distance(x_km, y_km)
    result = np.full(len(radii) - 1, np.nan)

    for i in range(len(radii) - 1):
        mask = (r >= radii[i]) & (r < radii[i + 1]) & np.isfinite(data)
        if np.any(mask):
            result[i] = np.nanmean(data[mask])

    return result


def cart2polar(data, x_km, y_km, r_edges, theta_edges):
    """
    Transform Cartesian grid data to polar (r, theta) coordinates.

    Parameters
    ----------
    data : np.ndarray
        2D data on Cartesian grid, shape (ny, nx).
    x_km, y_km : np.ndarray
        1D Cartesian coordinate arrays.
    r_edges : np.ndarray
        Radial bin edges (km).
    theta_edges : np.ndarray
        Azimuthal bin edges (degrees, 0 = North, clockwise).

    Returns
    -------
    np.ndarray
        2D array of shape (len(r_edges)-1, len(theta_edges)-1).
    """
    xx, yy = np.meshgrid(x_km, y_km)
    r = np.sqrt(xx ** 2 + yy ** 2)
    theta = np.degrees(np.arctan2(xx, yy)) % 360  # 0=N, CW

    nr = len(r_edges) - 1
    nt = len(theta_edges) - 1
    result = np.full((nr, nt), np.nan)

    for i in range(nr):
        for j in range(nt):
            if theta_edges[j] < theta_edges[j + 1]:
                mask = ((r >= r_edges[i]) & (r < r_edges[i + 1]) &
                        (theta >= theta_edges[j]) & (theta < theta_edges[j + 1]) &
                        np.isfinite(data))
            else:
                # Wrapping around 360/0
                mask = ((r >= r_edges[i]) & (r < r_edges[i + 1]) &
                        ((theta >= theta_edges[j]) | (theta < theta_edges[j + 1])) &
                        np.isfinite(data))
            if np.any(mask):
                result[i, j] = np.nanmean(data[mask])

    return result


def interp_to_polar_cylindrical(data, lat, lon, y_coord, x_coord, yi, xi):
    """
    Interpolate 2D data from Cartesian to polar-cylindrical coordinates.

    Wraps scipy RegularGridInterpolator, matching the pattern in
    modules/interp.py.

    Parameters
    ----------
    data : np.ndarray
        2D input data.
    lat : np.ndarray or y_coord
        Y-axis coordinates (could be lat or y_km).
    lon : np.ndarray or x_coord
        X-axis coordinates (could be lon or x_km).
    y_coord : np.ndarray
        Source y-coordinates.
    x_coord : np.ndarray
        Source x-coordinates.
    yi : np.ndarray
        Target y-coordinates (2D).
    xi : np.ndarray
        Target x-coordinates (2D).

    Returns
    -------
    np.ndarray
        Interpolated data at target points.
    """
    interp = RegularGridInterpolator(
        (y_coord, x_coord), data,
        method='linear',
        bounds_error=False,
        fill_value=np.nan
    )
    return interp((yi, xi))


def compute_wind_shear(u_cart, v_cart, x_km, y_km, r_inner=200, r_outer=800):
    """
    Compute wind shear magnitude and heading from U/V on a Cartesian grid.

    Used for SHIPS SHRD/SHTD, SHRS/SHTS diagnostics.

    Parameters
    ----------
    u_cart : np.ndarray
        U-wind (shear) on Cartesian grid.
    v_cart : np.ndarray
        V-wind (shear) on Cartesian grid.
    x_km, y_km : np.ndarray
        1D Cartesian coordinate arrays.
    r_inner, r_outer : float
        Annular averaging radii (km).

    Returns
    -------
    tuple
        (shear_magnitude, shear_heading_deg) where heading is in
        meteorological degrees (0=N, 90=E).
    """
    u_mean = annular_mean(u_cart, x_km, y_km, r_inner, r_outer)
    v_mean = annular_mean(v_cart, x_km, y_km, r_inner, r_outer)

    shear_mag = np.sqrt(u_mean ** 2 + v_mean ** 2)
    shear_dir = np.degrees(np.arctan2(u_mean, v_mean)) % 360

    return shear_mag, shear_dir
