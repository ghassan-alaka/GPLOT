"""
Wavenumber-0 (axisymmetric) vortex-removal filter for steering-flow
diagnostics.

Given a TC position and a horizontal U/V wind field, this module removes
the azimuthally-symmetric component (the "wavenumber-0" part in polar
coordinates about the storm center) *only within the vortex boundary*.
Asymmetric wind structure and flow outside the vortex pass through
unchanged.

This is a Python port of the approach used in the reference
``calc_steering`` routine, adapted to GPLOT's lat/lon grids without
requiring a full polar resampling round-trip. The algorithm:

1. Map each lat/lon grid point to (r, theta) about the TC center using
   a local-flat (equirectangular) approximation -- fine for TC scales
   (<1000 km).
2. Decompose the wind into (u_r, v_t) -- radial (outward positive) and
   tangential (counterclockwise positive) components.
3. Bin grid points by radius and compute, per bin, the azimuthal mean
   of u_r and v_t (that *is* the wavenumber-0 component at that radius)
   and the fractional azimuthal coverage of cyclonic tangential flow.
4. Locate the vortex boundary at each vertical level: start from the
   radius of maximum azimuthally-mean tangential wind (RMW) and search
   outward for the first radius where either the mean tangential wind
   weakens below 5 kt or where cyclonic coverage drops below 75 %.
5. Subtract the wavenumber-0 profile from each grid point whose radius
   lies inside the level's vortex boundary, then rotate (u_r, v_t) back
   to (u, v).

The result is a "detangled" wind field: the solid-body rotation of the
TC is gone, but asymmetries (troughs, ridges, shear tilt) and the
environmental steering flow remain intact.

Public API
----------
``remove_wavenumber0(u, v, lats, lons, tc_lat, tc_lon, ...)`` -- filter
U and V (2D or 3D) in place-compatible fashion. Returns a new pair of
arrays with the same shape as the inputs.

Notes
-----
* Hemispheric sign is handled via ``np.sign(tc_lat)``: "cyclonic" means
  CCW in the Northern Hemisphere and CW in the Southern Hemisphere,
  and the boundary-detection thresholds flip accordingly.
* When ``tc_lat`` is at or very near the equator, the hemisphere sign
  defaults to +1 (NH convention).
* If the vortex-maximum tangential mean at a level is below 10 kt the
  vortex boundary is set to 0 -- i.e., no filtering is done at that
  level. This is the same "no vortex present" escape hatch as in the
  reference implementation.
"""

from __future__ import annotations

import logging

import numpy as np

logger = logging.getLogger(__name__)


# Earth radius in km (consistent with gplot_utils.coord_transform).
_R_EARTH_KM = 6371.0088


def remove_wavenumber0(u, v, lats, lons, tc_lat, tc_lon,
                       rmax_km=None, dr_km=None,
                       vt_rmw_min_kt=10.0, vt_stop_kt=5.0,
                       coverage_stop=0.75, rmw_search_km=200.0):
    """
    Subtract the axisymmetric vortex component from a horizontal wind.

    Parameters
    ----------
    u, v : np.ndarray
        Wind components in the lat/lon basis. Same shape, either 2D
        ``(nlat, nlon)`` or 3D ``(nlev, nlat, nlon)``. Units are
        arbitrary but the thresholds (``vt_rmw_min_kt``, ``vt_stop_kt``)
        are in knots, so pass kt in.
    lats, lons : np.ndarray
        1D latitude and longitude arrays in degrees. Must match the
        last two axes of ``u`` / ``v``.
    tc_lat, tc_lon : float
        TC center position (degrees).
    rmax_km : float, optional
        Outer radius (km) to bin to. Defaults to 800 km, which
        comfortably contains any hurricane-scale vortex.
    dr_km : float, optional
        Radial bin width (km). Defaults to ``max(5 km, 2 * dx_km)``
        where ``dx_km`` is inferred from the grid spacing, giving
        ~5-15 samples per ring for high-res nests.
    vt_rmw_min_kt : float
        If the azimuthal-mean tangential wind at the RMW is weaker than
        this, we treat the level as having no vortex and skip filtering.
    vt_stop_kt : float
        Vortex boundary is the first radius (outward from RMW) where
        the azimuthal-mean tangential wind drops below this threshold.
    coverage_stop : float
        Alternate boundary criterion: fractional azimuthal coverage of
        cyclonic flow drops below this fraction. Catches asymmetric
        vortices where one side has already transitioned to the
        environment.
    rmw_search_km : float
        Maximum radius (km) to consider when locating the RMW. Prevents
        the argmax from landing on a distant environmental wind peak.

    Returns
    -------
    u_filt, v_filt : np.ndarray
        Same shape as ``u``/``v`` with the wavenumber-0 component
        subtracted inside the per-level vortex boundary.
    """
    u = np.asarray(u)
    v = np.asarray(v)
    if u.shape != v.shape:
        raise ValueError(
            f"remove_wavenumber0: u and v must have the same shape "
            f"(got {u.shape} vs {v.shape})")
    if u.ndim not in (2, 3):
        raise ValueError(
            f"remove_wavenumber0: expected 2D or 3D arrays, got ndim={u.ndim}")

    if tc_lat is None or tc_lon is None or not (np.isfinite(tc_lat)
                                                and np.isfinite(tc_lon)):
        logger.warning(
            "remove_wavenumber0: tc_lat/tc_lon not finite; returning "
            "input unchanged")
        return u.copy(), v.copy()

    lats = np.asarray(lats, dtype=float)
    lons = np.asarray(lons, dtype=float)
    if lats.ndim == 2 and lons.ndim == 2:
        lat2d, lon2d = lats, lons
    elif lats.ndim == 1 and lons.ndim == 1:
        lat2d, lon2d = np.meshgrid(lats, lons, indexing='ij')
    else:
        raise ValueError(
            "remove_wavenumber0: lats/lons must both be 1D or both be 2D")

    # Longitude wrap: put the grid in the same +/- convention as tc_lon
    lon_adj = _match_lon_convention(lon2d, tc_lon)

    r_km, theta = _compute_polar(lat2d, lon_adj, tc_lat, tc_lon)

    # Derive a default dr from the grid spacing so we get a few samples
    # per ring even on very high-res nests.
    if dr_km is None:
        if lats.ndim == 1 and len(lats) > 1:
            dlat = float(abs(lats[1] - lats[0]))
            dx_km_est = dlat * 111.0
        else:
            dx_km_est = 5.0
        dr_km = max(5.0, 2.0 * dx_km_est)
    if rmax_km is None:
        rmax_km = 800.0

    r_edges = np.arange(0.0, rmax_km + dr_km, dr_km)
    r_centers = 0.5 * (r_edges[:-1] + r_edges[1:])
    nr = r_centers.size

    cos_t = np.cos(theta)
    sin_t = np.sin(theta)

    # Hemispheric sign: +1 NH (CCW cyclonic), -1 SH (CW cyclonic).
    # Equatorial TCs are rare; default to NH if tc_lat == 0.
    hemi = 1.0 if tc_lat >= 0.0 else -1.0

    # Flatten to (nlev, npts) for vectorized bincount
    if u.ndim == 2:
        u3 = u[np.newaxis, ...]
        v3 = v[np.newaxis, ...]
        squeeze = True
    else:
        u3 = u
        v3 = v
        squeeze = False
    nlev, ny, nx = u3.shape
    npts = ny * nx

    # Radial components: ur = u cos(theta) + v sin(theta)
    # Tangential (CCW):  vt = -u sin(theta) + v cos(theta)
    ur = u3 * cos_t[np.newaxis, ...] + v3 * sin_t[np.newaxis, ...]
    vt = -u3 * sin_t[np.newaxis, ...] + v3 * cos_t[np.newaxis, ...]

    # Bin indices (flat)
    r_flat = r_km.ravel()
    bin_idx = np.digitize(r_flat, r_edges) - 1  # 0..nr-1 or out of range
    valid_bin = (bin_idx >= 0) & (bin_idx < nr)

    # Per-level: compute azimuthal means and cyclonic coverage
    ur_mean = np.full((nlev, nr), np.nan)
    vt_mean = np.full((nlev, nr), np.nan)
    cyc_cov = np.full((nlev, nr), np.nan)

    bi = bin_idx[valid_bin]
    for k in range(nlev):
        ur_k = ur[k].ravel()[valid_bin]
        vt_k = vt[k].ravel()[valid_bin]
        finite = np.isfinite(ur_k) & np.isfinite(vt_k)
        if not finite.any():
            continue
        bi_f = bi[finite]
        ur_f = ur_k[finite]
        vt_f = vt_k[finite]
        counts = np.bincount(bi_f, minlength=nr).astype(float)
        ur_sum = np.bincount(bi_f, weights=ur_f, minlength=nr)
        vt_sum = np.bincount(bi_f, weights=vt_f, minlength=nr)
        pos_count = np.bincount(bi_f,
                                weights=(hemi * vt_f > 0).astype(float),
                                minlength=nr)
        nonzero = counts > 0
        ur_mean[k, nonzero] = ur_sum[nonzero] / counts[nonzero]
        vt_mean[k, nonzero] = vt_sum[nonzero] / counts[nonzero]
        cyc_cov[k, nonzero] = pos_count[nonzero] / counts[nonzero]

    # Find vortex boundary per level
    rstops = _find_vortex_boundary(
        r_centers, vt_mean, cyc_cov, hemi,
        vt_rmw_min_kt=vt_rmw_min_kt,
        vt_stop_kt=vt_stop_kt,
        coverage_stop=coverage_stop,
        rmw_search_km=rmw_search_km,
    )
    logger.debug("remove_wavenumber0: rstops (km) per level = %s",
                 np.round(rstops, 1).tolist())

    # Build per-level wn0 fields on the 2D grid and subtract inside the
    # vortex boundary. bin_idx_2d lets us broadcast the 1D profile back.
    bin_idx_2d = bin_idx.reshape(r_km.shape)
    # Clamp to [0, nr-1] for safe indexing; out-of-range points are
    # masked out below.
    bi_clip = np.clip(bin_idx_2d, 0, nr - 1)
    in_grid = valid_bin.reshape(r_km.shape)

    ur_filt = ur.copy()
    vt_filt = vt.copy()
    for k in range(nlev):
        rstop = rstops[k]
        if not np.isfinite(rstop) or rstop <= 0.0:
            continue
        inside = in_grid & (r_km <= rstop)
        ur_wn0 = ur_mean[k, bi_clip]
        vt_wn0 = vt_mean[k, bi_clip]
        # Where profile is NaN (empty bin), don't subtract anything.
        ur_wn0 = np.where(np.isfinite(ur_wn0), ur_wn0, 0.0)
        vt_wn0 = np.where(np.isfinite(vt_wn0), vt_wn0, 0.0)
        ur_filt[k][inside] = ur[k][inside] - ur_wn0[inside]
        vt_filt[k][inside] = vt[k][inside] - vt_wn0[inside]

    # Rotate (ur, vt) back to (u, v):
    #   u = ur cos(theta) - vt sin(theta)
    #   v = ur sin(theta) + vt cos(theta)
    u_filt = ur_filt * cos_t[np.newaxis, ...] - vt_filt * sin_t[np.newaxis, ...]
    v_filt = ur_filt * sin_t[np.newaxis, ...] + vt_filt * cos_t[np.newaxis, ...]

    # Preserve original NaNs.
    u_nan = ~np.isfinite(u3)
    v_nan = ~np.isfinite(v3)
    if u_nan.any():
        u_filt[u_nan] = np.nan
    if v_nan.any():
        v_filt[v_nan] = np.nan

    if squeeze:
        return u_filt[0], v_filt[0]
    return u_filt, v_filt


# ---------------------------------------------------------------------------
# Internals
# ---------------------------------------------------------------------------

def _match_lon_convention(lon2d, tc_lon):
    """
    Shift ``lon2d`` into the same +/- convention as ``tc_lon`` so the
    dx = (lon - tc_lon) subtraction stays small and doesn't wrap the
    dateline.
    """
    lon = lon2d.copy()
    if tc_lon > 180.0 and np.any(lon < 0):
        lon = np.where(lon < 0, lon + 360.0, lon)
    elif tc_lon < 0 and np.any(lon > 180):
        lon = np.where(lon > 180, lon - 360.0, lon)
    # Handle straggling dateline crossings
    dlon = lon - tc_lon
    lon = np.where(dlon > 180, lon - 360.0, lon)
    lon = np.where(dlon < -180, lon + 360.0, lon)
    return lon


def _compute_polar(lat2d, lon2d, tc_lat, tc_lon):
    """
    Map (lat, lon) to (r_km, theta) about (tc_lat, tc_lon) using a
    local-flat (equirectangular) approximation. Theta is measured CCW
    from east (standard math convention).

    Good to <1 % for distances up to ~1000 km at mid-latitudes, which is
    well beyond any realistic TC vortex boundary.
    """
    deg2rad = np.pi / 180.0
    cos_ref = np.cos(deg2rad * tc_lat)
    dx = (lon2d - tc_lon) * cos_ref * deg2rad * _R_EARTH_KM  # east
    dy = (lat2d - tc_lat) * deg2rad * _R_EARTH_KM             # north
    r_km = np.sqrt(dx * dx + dy * dy)
    theta = np.arctan2(dy, dx)
    return r_km, theta


def _find_vortex_boundary(r_centers, vt_mean, cyc_cov, hemi,
                          vt_rmw_min_kt, vt_stop_kt, coverage_stop,
                          rmw_search_km):
    """
    Return the per-level vortex outer radius (km).

    Algorithm per level:
      1. Find j_rmw = argmax (over r < rmw_search_km) of hemi*vt_mean.
         If the maximum is below vt_rmw_min_kt, treat as "no vortex"
         (j_rmw = 0 -> rstop = 0).
      2. Starting outward from j_rmw, return the first radius where
         hemi*vt_mean < vt_stop_kt OR cyc_cov < coverage_stop. If
         neither triggers, return r_centers[-1].
    """
    nlev, nr = vt_mean.shape
    rstops = np.zeros(nlev)
    j_search_max = max(1, int(np.searchsorted(r_centers, rmw_search_km)))

    for k in range(nlev):
        vt_signed = hemi * vt_mean[k]
        cov = cyc_cov[k]

        window = vt_signed[:j_search_max]
        if not np.any(np.isfinite(window)):
            rstops[k] = 0.0
            continue
        j_rmw = int(np.nanargmax(window))
        if (not np.isfinite(vt_signed[j_rmw])
                or vt_signed[j_rmw] < vt_rmw_min_kt):
            # No identifiable vortex at this level.
            rstops[k] = 0.0
            continue

        rstop = r_centers[-1]
        for j in range(j_rmw + 1, nr):
            vj = vt_signed[j]
            cj = cov[j]
            if not np.isfinite(vj):
                continue
            if vj < vt_stop_kt or (np.isfinite(cj) and cj < coverage_stop):
                rstop = r_centers[j]
                break
        rstops[k] = rstop

    return rstops
