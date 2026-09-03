#!/work/noaa/aoml-hafs1/galaka/anaconda3/envs/GPLOT/bin/python3

#------------------------------------------------------------------------------------------------------------------------
# Compute storm-centered Skew-T soundings at the standard GPLOT
# (radius x quadrant) sample points and dispatch each to
# plotting.skewplot() / plotting.skewplot_blank(). Sounding panels
# produced per FHR:
#   Center                                                      (1)
#   {NE,NW,SW,SE} x {RMW, RMW-50nmi, 50-100, 100-150, 150-200}  (20)
#   = 21 figures total.
#
# Required inputs:
#   r_grid             : Radius (km), shape (nr,)
#   theta_grid         : Azimuth (rad), shape (ntheta,)
#   p_sounding_polar   : Pressure (Pa), shape (ntheta, nr, nz)
#   u_sounding_polar   : U wind (m/s), same shape
#   v_sounding_polar   : V wind (m/s), same shape
#   temp_sounding_polar: Temperature (K), same shape
#   rh_sounding_polar  : Relative humidity (%), same shape
#   rmw                : Radius of maximum winds (nautical miles)
#
# Optional kwarg:
#   dbz_2km_polar : 2-km reflectivity slice (dBZ), shape (ntheta, nr).
#                   When supplied, every Skew-T figure also gets a
#                   small reflectivity inset in the lower-left with
#                   the source region (point/swath/sector) shaded.
#------------------------------------------------------------------------------------------------------------------------

import numpy as np
import metpy.calc as mpcalc
from metpy.units import units
import os
import modules.plotting as plotting


# Mapping from compass quadrant -> (theta1, theta2) in matplotlib
# CCW-from-east convention used by patches.Wedge. Matches the
# index_theta_E/N/W/S layout (E=0, N=90, W=180, S=270).
_QUAD_DEG = {
    'NE': (0, 90),
    'NW': (90, 180),
    'SW': (180, 270),
    'SE': (270, 360),
}


def _quadrant_slice(quadrant, theta_grid,
                    index_theta_E, index_theta_N,
                    index_theta_W, index_theta_S):
    """Return the (start, stop) theta-index slice for a quadrant.
    Matches the legacy slicing convention (inclusive end via +1, with
    the SE quadrant wrapping to the end of the theta grid)."""
    if quadrant == 'NE':
        return index_theta_E, index_theta_N + 1
    if quadrant == 'NW':
        return index_theta_N, index_theta_W + 1
    if quadrant == 'SW':
        return index_theta_W, index_theta_S + 1
    if quadrant == 'SE':
        return index_theta_S, np.shape(theta_grid)[0]
    raise ValueError(f"Unknown quadrant {quadrant!r}")


def _build_inset_payload(dbz_2km_polar, r_grid, theta_grid):
    """Build the per-figure inset kwargs dict, or return None when no
    reflectivity data was supplied (legacy no-inset path)."""
    if dbz_2km_polar is None:
        return None
    return dict(
        dbz_polar=dbz_2km_polar,
        r_km=r_grid,
        theta_rad=theta_grid,
        # Show 0 .. 200 nmi window so the outermost annular sounding
        # (150-200 nmi) still has its highlight inside the inset.
        rmax_km=200.0 * 1.852,
    )


def skewTmodelTCpolar(r_grid, theta_grid, p_sounding_polar,
                      u_sounding_polar, v_sounding_polar,
                      temp_sounding_polar, rh_sounding_polar,
                      rmw, GPLOT_DIR, EXPT, FHR, maxwind, minpressure,
                      LONGSID, ODIR, forecastinit, DO_CONVERTGIF,
                      *,
                      dbz_2km_polar=None):
    """Produce all 21 Skew-T figures for one forecast hour."""

    # Unit / variable preprocessing identical to the legacy code.
    p_sounding_polar = p_sounding_polar / 100        # Pa -> hPa
    u_sounding_polar = u_sounding_polar * 1.94       # m/s -> kt
    v_sounding_polar = v_sounding_polar * 1.94
    temp_sounding_polar = temp_sounding_polar - 273.15  # K -> degC
    td_sounding_polar = mpcalc.dewpoint_from_relative_humidity(
        temp_sounding_polar * units.celsius,
        rh_sounding_polar * units.percent,
    )

    # Radial sample-point indices (km / nmi conversion factor 1.852).
    index_r_rmw = np.argmin(np.abs(r_grid - 1.852 * rmw))
    index_r_050 = np.argmin(np.abs(r_grid - 1.852 * 50))
    index_r_100 = np.argmin(np.abs(r_grid - 1.852 * 100))
    index_r_150 = np.argmin(np.abs(r_grid - 1.852 * 150))
    index_r_200 = np.argmin(np.abs(r_grid - 1.852 * 200))

    # Quadrant azimuth indices.
    index_theta_E = np.argmin(np.abs(theta_grid * 180 / np.pi - 0))
    index_theta_N = np.argmin(np.abs(theta_grid * 180 / np.pi - 90))
    index_theta_W = np.argmin(np.abs(theta_grid * 180 / np.pi - 180))
    index_theta_S = np.argmin(np.abs(theta_grid * 180 / np.pi - 270))

    inset_payload = _build_inset_payload(
        dbz_2km_polar, r_grid, theta_grid)

    # ----- Center sounding -----
    p_c   = p_sounding_polar[0, 0, :]
    u_c   = u_sounding_polar[0, 0, :]
    v_c   = v_sounding_polar[0, 0, :]
    t_c   = temp_sounding_polar[0, 0, :]
    td_c  = td_sounding_polar[0, 0, :]
    plotting.skewplot(
        p_c, t_c, td_c, u_c, v_c, 'Center',
        GPLOT_DIR, EXPT, FHR, maxwind, minpressure, LONGSID, ODIR,
        forecastinit, DO_CONVERTGIF,
        inset=inset_payload,
        region={'kind': 'center'})

    # ----- Quadrant soundings -----
    # The 5 radial bands. Each entry is:
    #   (label_suffix, r_inner_idx, r_outer_idx, region_kind,
    #    r_inner_km_for_highlight, r_outer_km_for_highlight)
    # RMW is a single radial slice (inner == outer index); the
    # highlight rendered on the inset is a 2-km-wide swath at that
    # radius, drawn by skewplot via region.kind == 'rmw_quadrant'.
    rmw_km = float(r_grid[index_r_rmw])
    rmw_in_range = (rmw < 50) and not np.isnan(rmw)

    radial_bands = [
        ('RMW',        index_r_rmw, index_r_rmw, 'rmw_quadrant',
         rmw_km, rmw_km),
        ('RMW-50nmi',  index_r_rmw, index_r_050, 'annulus_quadrant',
         rmw_km, float(r_grid[index_r_050])),
        ('50-100nmi',  index_r_050, index_r_100, 'annulus_quadrant',
         float(r_grid[index_r_050]), float(r_grid[index_r_100])),
        ('100-150nmi', index_r_100, index_r_150, 'annulus_quadrant',
         float(r_grid[index_r_100]), float(r_grid[index_r_150])),
        ('150-200nmi', index_r_150, index_r_200, 'annulus_quadrant',
         float(r_grid[index_r_150]), float(r_grid[index_r_200])),
    ]

    for quadrant in ('NE', 'NW', 'SW', 'SE'):
        t_lo, t_hi = _quadrant_slice(
            quadrant, theta_grid,
            index_theta_E, index_theta_N,
            index_theta_W, index_theta_S)

        for (suffix, r_lo, r_hi, kind,
             r_inner_km, r_outer_km) in radial_bands:
            location = f'{quadrant} {suffix}'

            # The first two bands (RMW and RMW-50nmi) are skipped
            # whenever the storm has no usable RMW; render a blank
            # placeholder figure instead so the file pattern stays
            # complete.
            if suffix in ('RMW', 'RMW-50nmi') and not rmw_in_range:
                # Blank case still gets the inset for spatial
                # context, but no region highlight (we don't know
                # where the RMW would be).
                plotting.skewplot_blank(
                    location, GPLOT_DIR, EXPT, FHR, maxwind,
                    minpressure, LONGSID, ODIR, forecastinit,
                    DO_CONVERTGIF,
                    inset=inset_payload,
                    region=None)
                continue

            # Slice + nanmean over the (theta, r) sub-block.
            if r_lo == r_hi:
                # Single radial line (RMW): mean over theta only.
                def _band_mean(arr):
                    return np.nanmean(arr[t_lo:t_hi, r_lo, :], axis=0)
            else:
                # Annular band: mean over theta then radius.
                def _band_mean(arr):
                    return np.nanmean(np.nanmean(
                        arr[t_lo:t_hi, r_lo:r_hi + 1, :], axis=0),
                        axis=0)

            p_band  = _band_mean(p_sounding_polar)
            u_band  = _band_mean(u_sounding_polar)
            v_band  = _band_mean(v_sounding_polar)
            t_band  = _band_mean(temp_sounding_polar)
            td_band = _band_mean(td_sounding_polar)

            if kind == 'rmw_quadrant':
                region = {
                    'kind': 'rmw_quadrant',
                    'quadrant': quadrant,
                    'rmw_km': r_inner_km,
                    'swath_km': 2.0,
                }
            else:
                region = {
                    'kind': 'annulus_quadrant',
                    'quadrant': quadrant,
                    'r_inner_km': r_inner_km,
                    'r_outer_km': r_outer_km,
                }

            plotting.skewplot(
                p_band, t_band, td_band, u_band, v_band, location,
                GPLOT_DIR, EXPT, FHR, maxwind, minpressure, LONGSID,
                ODIR, forecastinit, DO_CONVERTGIF,
                inset=inset_payload,
                region=region)
