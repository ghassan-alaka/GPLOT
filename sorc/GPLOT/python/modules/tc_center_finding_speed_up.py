### Optimized version of TC center determination using weighted circulation maximization ###
### Original by Michael Fischer on 6/7/2023 ###
### Optimized version created on 5/6/2025 ###

# Import libraries:
import numpy as np
from scipy import ndimage
import warnings
warnings.filterwarnings("ignore")

# Try to import numba if available, otherwise use non-numba versions
try:
    import numba as nb
    HAS_NUMBA = True
except ImportError:
    HAS_NUMBA = False
    print("Numba not found. Using non-compiled versions of functions.")

### Define optimized utility functions ###

# Define numba versions if available, otherwise fall back to non-compiled versions
if HAS_NUMBA:
    @nb.njit
    def distance_vectorized(center_lat, center_lng, lats, lons):
        """
        Vectorized and JIT-compiled function to compute distances from a center point to all grid points.
        
        Args:
            center_lat: Center latitude in degrees
            center_lng: Center longitude in degrees
            lats: 2D array of latitudes in degrees
            lons: 2D array of longitudes in degrees
        
        Returns:
            2D array of distances in km
        """
        # approximate radius of earth in km
        R = 6373.0
        
        # Pre-allocate output array
        result = np.zeros(lats.shape, dtype=np.float64)
        
        for i in range(lats.shape[0]):
            for j in range(lats.shape[1]):
                # Convert to radians
                s_lat = center_lat * np.pi/180.0
                s_lng = center_lng * np.pi/180.0
                e_lat = lats[i, j] * np.pi/180.0
                e_lng = lons[i, j] * np.pi/180.0
                
                d = np.sin((e_lat - s_lat)/2)**2 + np.cos(s_lat)*np.cos(e_lat) * np.sin((e_lng - s_lng)/2)**2
                result[i, j] = 2 * R * np.arcsin(np.sqrt(d))
                
        return result

    @nb.njit
    def bearing_vectorized(center_lat, center_lng, lats, lons):
        """
        Vectorized and JIT-compiled function to compute bearings from a center point to all grid points.
        
        Args:
            center_lat: Center latitude in degrees
            center_lng: Center longitude in degrees
            lats: 2D array of latitudes in degrees
            lons: 2D array of longitudes in degrees
        
        Returns:
            2D array of bearings in radians
        """
        # Pre-allocate output array
        result = np.zeros(lats.shape, dtype=np.float64)
        
        for i in range(lats.shape[0]):
            for j in range(lats.shape[1]):
                # Convert to radians
                s_lat = center_lat * np.pi/180.0
                s_lng = center_lng * np.pi/180.0
                e_lat = lats[i, j] * np.pi/180.0
                e_lng = lons[i, j] * np.pi/180.0
                
                # meridional distance:
                md_raw = np.sin((e_lat - s_lat)/2)**2
                md_sign = np.sign(e_lat - s_lat) # Acquire integer sign of distance (i.e., -1, 0, or 1)
                
                # zonal distance:
                zd_raw = np.cos(s_lat)*np.cos(e_lat) * np.sin((e_lng - s_lng)/2)**2
                zd_sign = np.sign(e_lng - s_lng) # Acquire integer sign of distance
                
                # Calculate bearing
                bearing = np.arctan2(md_sign * np.sqrt(md_raw), zd_sign * np.sqrt(zd_raw))
                result[i, j] = bearing
                
        return result

    @nb.njit
    def correct_angle_differences_numba(angle_diff):
        """
        Corrects angle differences to fall within -π to π
        Uses explicit looping for Numba compatibility
        """
        result = np.copy(angle_diff)
        
        for i in range(result.shape[0]):
            for j in range(result.shape[1]):
                if np.isfinite(result[i, j]):
                    # Correct angles less than -π
                    if result[i, j] < -np.pi:
                        result[i, j] = 2.0 * np.pi + result[i, j]
                    # Correct angles greater than π
                    elif result[i, j] > np.pi:
                        result[i, j] = result[i, j] - 2.0 * np.pi
                        
        return result
else:
    # Non-numba versions
    def distance_vectorized(center_lat, center_lng, lats, lons):
        """Non-JIT version of distance calculation"""
        # approximate radius of earth in km
        R = 6373.0
        
        # Convert to radians
        s_lat = center_lat * np.pi/180.0
        s_lng = center_lng * np.pi/180.0
        e_lat = lats * np.pi/180.0
        e_lng = lons * np.pi/180.0
        
        d = np.sin((e_lat - s_lat)/2)**2 + np.cos(s_lat)*np.cos(e_lat) * np.sin((e_lng - s_lng)/2)**2
        
        return 2 * R * np.arcsin(np.sqrt(d))

    def bearing_vectorized(center_lat, center_lng, lats, lons):
        """Non-JIT version of bearing calculation"""
        # Convert to radians
        s_lat = center_lat * np.pi/180.0
        s_lng = center_lng * np.pi/180.0
        e_lat = lats * np.pi/180.0
        e_lng = lons * np.pi/180.0
        
        # meridional distance calculation
        md_raw = np.sin((e_lat - s_lat)/2)**2
        md_sign = np.sign(e_lat - s_lat)
        
        # zonal distance calculation
        zd_raw = np.cos(s_lat)*np.cos(e_lat) * np.sin((e_lng - s_lng)/2)**2
        zd_sign = np.sign(e_lng - s_lng)
        
        # Calculate bearing
        bearing = np.arctan2(md_sign * np.sqrt(md_raw), zd_sign * np.sqrt(zd_raw))
        
        return bearing

# Common functions that don't need numba
def correct_angle_differences(angle_diff):
    """
    Non-JIT version that corrects angle differences to fall within -π to π
    """
    corrected = angle_diff.copy()
    
    # Correct angles less than -π
    mask_low = corrected < -np.pi
    corrected[mask_low] = 2.0 * np.pi + corrected[mask_low]
    
    # Correct angles greater than π
    mask_high = corrected > np.pi
    corrected[mask_high] = corrected[mask_high] - 2.0 * np.pi
    
    return corrected

# Function to find value of array closest to specified value
def find_nearest(array, value):
    array = np.asarray(array)
    X = np.abs(array - value)
    idx = np.where(X == np.nanmin(X))
    return array[idx]

# Function to find index of array closest to specified value
def find_nearest_ind(array, value):
    array = np.asarray(array)
    X = np.abs(array - value)
    idx = np.where(X == np.nanmin(X))
    return idx

### Optimized TC center finding function ###
def recenter_tc(uwind, vwind, lons, lats, num_sectors, spad, num_iterations, olon=None, olat=None):
    """
    Optimized function to re-center TDR merged analyses and/or model analyses.
    
    Parameters remain the same as the original function for compatibility:
    1) uwind is the storm-relative zonal component of the TC flow. Should be 2D array ([lat,lon]).
    2) vwind is the storm-relative meridional component of the TC flow. Should be 2D array ([lat,lon]).
    3) lons is a two-dimensional array of the longitudinal coordinates
    4) lats is a two-dimensional array of the latitudinal coordinates
    5) num_sectors is the number (integer) of azimuthal sectors used to compute the mean error.
    6) spad is the number of gridpoints to search from center (a value of 6 seems to work well).
    7) num_iterations is the amount of times to loop to try to find a center that converges
    8) olon is the first-guess center longitude (float). Default is None, uses max of smoothed vorticity.
    9) olat is the first-guess center latitude (float). Default is None, uses max of smoothed vorticity.
    """
    
    # Precompute constants and initialize parameters
    angle_thresh = np.linspace(-1.*np.pi, np.pi, num_sectors+1)  # Bounds of azimuthal sectors
    
    # Parameters for RMW search
    gr0 = 2  # Starting distance
    grf = 175  # Ending distance
    curr_delta = 2.0  # Annulus width (km)
    rad_gaussian = np.arange(gr0, grf+curr_delta, curr_delta)  # Array of radii for RMW calculations
    
    # Define core distances
    search_radius = 150.  # distance (km) to consider grid points for error calculations
    core_radius = 50.  # distance (km) from hypothetical TC center to weight Gaussian errors
    coverage_radius = 100.  # distance (km) from hypothetical TC center to consider for data coverage
    coverage_radius_inner = 50.  # inner distance (km) to consider for data coverage
    
    # Define min_dist_weight
    min_dist_weight = 1.E-6  # Minimum value for distance weighting
    
    # Define required amount of data within core radius
    min_data_frac = 0.02
    
    # Precompute wind speed once (used repeatedly)
    ws = np.sqrt(uwind**2 + vwind**2)
    
    # Prepare wind angle in advance (used repeatedly)
    wind_angle = np.arctan2(vwind, uwind)
    
    # Initialize parameters for iteration
    yloc = np.nan  # Meridional index of TC center
    xloc = np.nan  # Zonal index of TC center

    # smooth_vort is only assigned in the olon-is-None branch below, but the
    # function returns it unconditionally. Initialize to None so the return
    # statement does not raise UnboundLocalError when a first-guess center
    # is supplied (the typical caller path).
    smooth_vort = None

    # Determine grid indices closest to first-guess TC center, if provided
    if olon is not None:
        try:
            # Find closest lat/lon index to real-time center
            lat_close = find_nearest(lats, olat)[0]
            pnyi = np.where(lats == lat_close)[0][0]  # meridional index of prior guess center
            lon_close = find_nearest(lons[pnyi,:], olon)[0]
            pnxi = np.where(lons[pnyi,:] == lon_close)[0][0]  # zonal index of prior guess center
        except IndexError:
            # If unable to identify nearest grid point, use midpoint of grid
            pnyi = int(round(lons.shape[0]/2., 0))
            pnxi = int(round(lons.shape[1]/2., 0))
    
    # If no first-guess center provided, identify grid point with largest smoothed relative vorticity
    if olon is None:
        dx = 2000.  # Zonal grid spacing (m)
        dy = 2000.  # Meridional grid spacing (m)

        # Calculate vorticity components more efficiently using numpy gradient
        dvdx = np.gradient(vwind, dx, axis=1)  # Zonal gradient of meridional wind
        dudy = np.gradient(uwind, dy, axis=0)  # Meridional gradient of zonal wind
        relvort = dvdx - dudy  # Relative vorticity

        vpad = 10  # Number of indices for smoothing vorticity
        
        print('Smoothing vorticity field...')
        
        # Create a more efficient smoothing method using convolution
        # Create smoothing kernel
        kernel_size = 2*vpad + 1
        kernel = np.ones((kernel_size, kernel_size)) / (kernel_size * kernel_size)
        
        # Apply convolution for smoothing, handling NaNs properly
        mask = np.isfinite(relvort)
        smooth_vort = np.full_like(relvort, np.nan)
        
        # Replace NaNs with zeros for convolution
        relvort_filled = np.copy(relvort)
        relvort_filled[~mask] = 0
        
        # Apply convolution
        smooth_data = ndimage.convolve(relvort_filled, kernel, mode='reflect')
        
        # Create weight map (1 for valid data, 0 for NaN)
        weight_map = ndimage.convolve(mask.astype(float), kernel, mode='reflect')
        
        # Normalize by weight map (only where we have at least 10% data coverage)
        valid_idx = weight_map >= 0.25
        smooth_vort[valid_idx] = smooth_data[valid_idx] / weight_map[valid_idx]
        
        # Find maximum vorticity location
        max_vort = np.nanmax(smooth_vort)
        
        # Ensure positive vorticity was identified
        if max_vort > 0:
            max_indices = np.where(smooth_vort == max_vort)
            pnyi = max_indices[0][0]
            pnxi = max_indices[1][0]
            print('Vort centroid at:',lons[pnyi,pnxi],lats[pnyi,pnxi])
        else:
            pnyi = int(round(lons.shape[0]/2., 0))
            pnxi = int(round(lons.shape[1]/2., 0))
    
    # Initialize arrays for sector mean errors
    sector_mean_error = np.full((num_sectors, uwind.shape[0], uwind.shape[1]), np.nan, dtype='f8')

    # Set assumed previous error difference to infinity
    prev_mean_dif = np.inf

    # Initialize outputs
    vt_azi_max = np.nan
    data_cov = np.nan
    tc_rmw = np.nan
    tc_center_lon = np.nan
    tc_center_lat = np.nan

    # ----- Crop-window precompute (bit-exact speedup vs. full-grid loop) -----
    # Cells more than ~263 km from a candidate satisfy
    #   exp(-d^2 / (2 * core_radius^2)) <= min_dist_weight,
    # so the np.maximum(dist_weight_raw, min_dist_weight) floor below clamps
    # them to *exactly* min_dist_weight. We compute distance / bearing /
    # weighting only on a window that contains every cell whose contribution
    # to dist_weight_raw exceeds the floor for any candidate in the spad-box,
    # then analytically correct the np.nanmean(dist_weight_raw) denominator
    # by adding (n_outside_finite * min_dist_weight) to the numerator and
    # using the full-grid finite count as the denominator. This preserves
    # the original arithmetic identity to machine precision; the only
    # difference is which cells are summed first, which doesn't change the
    # result for the nanmean (sum is over identical values).
    crop_radius_km = core_radius * np.sqrt(-2.0 * np.log(min_dist_weight))

    # Estimate grid spacing (km) from the lat/lon arrays near the first guess.
    ny_grid, nx_grid = lats.shape
    iy = min(max(int(pnyi), 1), ny_grid - 2)
    ix = min(max(int(pnxi), 1), nx_grid - 2)
    dlat_deg = abs(float(lats[iy + 1, ix]) - float(lats[iy - 1, ix])) / 2.0
    dlon_deg = abs(float(lons[iy, ix + 1]) - float(lons[iy, ix - 1])) / 2.0
    coslat = np.cos(np.radians(float(lats[iy, ix])))
    dy_km = max(dlat_deg * 111.32, 1e-6)
    dx_km = max(dlon_deg * 111.32 * coslat, 1e-6)

    npy_half = int(np.ceil(crop_radius_km / dy_km)) + int(spad) + 2
    npx_half = int(np.ceil(crop_radius_km / dx_km)) + int(spad) + 2

    # Total finite ws cells over the full grid (denominator for the analytic
    # nanmean correction). nanmean = sum_finite / count_finite, so we need
    # the global finite count once.
    total_finite_full = int(np.count_nonzero(np.isfinite(ws)))

    ### Begin TC center search ###
    for n in range(num_iterations):
        print('current iteration is:', n)

        # If first iteration completed, copy center estimate from previous iteration
        if n >= 1:
            if prev_mean_dif < np.inf:
                pnyi = int(yloc)
                pnxi = int(xloc)
            else:
                break

        # Establish the per-iteration crop window (centered on current best
        # guess; spad-box of candidates fits inside the inner part of the
        # window with crop_radius_km of margin in each direction).
        ymin = max(0, int(pnyi) - npy_half)
        ymax = min(ny_grid, int(pnyi) + npy_half + 1)
        xmin = max(0, int(pnxi) - npx_half)
        xmax = min(nx_grid, int(pnxi) + npx_half + 1)
        ws_crop = ws[ymin:ymax, xmin:xmax]
        wa_crop = wind_angle[ymin:ymax, xmin:xmax]
        lats_crop = lats[ymin:ymax, xmin:xmax]
        lons_crop = lons[ymin:ymax, xmin:xmax]

        # Counts for the analytic nanmean correction.
        n_inside_finite = int(np.count_nonzero(np.isfinite(ws_crop)))
        n_outside_finite = total_finite_full - n_inside_finite

        # Hoist candidate-invariant work out of the inner loop.
        # wind_weight does not depend on the candidate center; only ws does,
        # and ws is fixed for the whole iteration.
        wind_weight_crop = np.sqrt(ws_crop + 1)
        data_mask_crop = np.isfinite(ws_crop)

        # Establish range of candidate grid points
        range_y = np.arange(pnyi - spad, pnyi + spad + 1, 1)
        range_x = np.arange(pnxi - spad, pnxi + spad + 1, 1)
        range_y = range_y[(range_y >= 0) & (range_y < uwind.shape[0])]
        range_x = range_x[(range_x >= 0) & (range_x < uwind.shape[1])]

        # Loop over grid points
        for ybi in range_y:
            for xbi in range_x:
                # Skip if already computed in previous iteration
                if n >= 1 and np.isfinite(np.nanmean(sector_mean_error[:, ybi, xbi])):
                    continue

                # Get lat/lon of current center guess
                center_lat = lats[ybi, xbi]
                center_lon = lons[ybi, xbi]

                # Compute distances and angles on the crop only.
                curr_dist = distance_vectorized(center_lat, center_lon, lats_crop, lons_crop)
                angle = bearing_vectorized(center_lat, center_lon, lats_crop, lons_crop)

                # Distance weighting (crop only)
                dist_weight_raw = np.full_like(ws_crop, np.nan)
                dist_weight_raw[data_mask_crop] = np.exp(
                    -1. * (((curr_dist[data_mask_crop] - 0.) ** 2) / (2. * ((core_radius) ** 2)))
                )
                dist_weight_raw = np.maximum(dist_weight_raw, min_dist_weight)

                # Analytic nanmean correction: cells outside the crop are all
                # finite-ws (if they were finite at all) and clamped to
                # exactly min_dist_weight, so they contribute
                # n_outside_finite * min_dist_weight to the sum and
                # n_outside_finite to the count, both of which we know.
                sum_in = np.nansum(dist_weight_raw)
                mean_full = (sum_in + n_outside_finite * min_dist_weight) / total_finite_full
                dist_weight = dist_weight_raw / mean_full

                # Total weighting
                curr_weight = dist_weight * wind_weight_crop

                # Compute ideal vortex angle (tangential)
                ideal_angle = angle + np.pi / 2.
                ideal_angle[ideal_angle > np.pi] -= 2.0 * np.pi

                # Compute angle difference
                curr_angle_dif = wa_crop - ideal_angle
                if HAS_NUMBA:
                    curr_angle_dif = correct_angle_differences_numba(curr_angle_dif)
                else:
                    curr_angle_dif = correct_angle_differences(curr_angle_dif)

                # Compute weighted differences
                curr_weighted_dif = curr_weight * curr_angle_dif

                # Mask points outside search radius
                curr_weighted_dif[curr_dist > search_radius] = np.nan

                # Calculate data coverage metrics
                coverage_mask = curr_dist <= coverage_radius
                coverage_mask_inner = curr_dist <= coverage_radius_inner

                curr_nf = np.count_nonzero(np.isfinite(curr_weighted_dif[coverage_mask]))
                curr_nf_inner = np.count_nonzero(np.isfinite(curr_weighted_dif[coverage_mask_inner]))

                curr_nt = np.count_nonzero(coverage_mask)
                curr_nt_inner = np.count_nonzero(coverage_mask_inner)

                # Check data coverage
                try:
                    if float(curr_nf / curr_nt) < min_data_frac:
                        continue
                except ZeroDivisionError:
                    continue

                # Vectorized azimuthal-sector mean error (replaces num_sectors-step
                # Python loop). np.digitize bucketizes by the same bin edges as
                # the original loop: bin i contains angles in
                # [angle_thresh[i], angle_thresh[i+1]) for i in 0..num_sectors-1,
                # so the assignment is bit-equivalent. Summation order over
                # each bin matches the original C-flattening of the boolean mask.
                abs_data = np.abs(curr_weighted_dif)
                finite_mask = np.isfinite(abs_data)
                sector_id = np.digitize(angle, angle_thresh) - 1
                valid = finite_mask & (sector_id >= 0) & (sector_id < num_sectors)
                if valid.any():
                    sid_flat = sector_id[valid]
                    data_flat = abs_data[valid]
                    sums = np.bincount(sid_flat, weights=data_flat, minlength=num_sectors)
                    counts = np.bincount(sid_flat, minlength=num_sectors)
                    sec_means = np.full(num_sectors, np.nan, dtype='f8')
                    nonzero = counts[:num_sectors] > 0
                    sec_means[nonzero] = sums[:num_sectors][nonzero] / counts[:num_sectors][nonzero]
                    sector_mean_error[:, ybi, xbi] = sec_means

                # Compute mean error across all sectors
                curr_mean_dif = np.nanmean(sector_mean_error[:, ybi, xbi])

                # Update if current location gives better center estimate
                if curr_mean_dif < prev_mean_dif:
                    prev_mean_dif = curr_mean_dif

                    # Store TC location estimate
                    tc_center_lon = center_lon
                    tc_center_lat = center_lat

                    # Store data coverage
                    data_cov = min(curr_nf / curr_nt, curr_nf_inner / curr_nt_inner)

                    # Store indices of estimated TC center
                    yloc = ybi
                    xloc = xbi
        
        # Check for convergence
        if n >= 1 and yloc == pnyi and xloc == pnxi:
            # Compute RMW using location of estimated TC center
            center_lat = lats[int(yloc), int(xloc)]
            center_lon = lons[int(yloc), int(xloc)]
            
            # Compute distance and angle relative to TC center
            curr_dist = distance_vectorized(center_lat, center_lon, lats, lons)
            angle = bearing_vectorized(center_lat, center_lon, lats, lons)
            
            # Calculate tangential wind about TC center
            curr_vt = -1.*(uwind*np.sin(angle) - vwind*np.cos(angle))
            
            # Initialize array for tangential wind in each annulus
            vt_ann_azi = np.full(len(rad_gaussian), np.nan, dtype='f4')
            
            # Process all radii at once using vectorization where possible
            for ri, radius in enumerate(rad_gaussian):
                # Create annulus mask
                annulus_mask = (curr_dist >= radius - 0.5*curr_delta) & (curr_dist < radius + 0.5*curr_delta)
                
                # Compute mean tangential wind in annulus
                if np.any(annulus_mask):
                    vt_ann_azi[ri] = np.nanmean(curr_vt[annulus_mask])
            
            # Find maximum tangential wind and corresponding radius
            vt_azi_max = np.nanmax(vt_ann_azi)
            
            try:
                tc_rmw = rad_gaussian[np.nanargmax(vt_ann_azi)]
            except (IndexError, ValueError):
                tc_rmw = np.nan
                
            break
    
    return tc_center_lon, tc_center_lat, vt_azi_max, tc_rmw, data_cov, smooth_vort

### END ###
