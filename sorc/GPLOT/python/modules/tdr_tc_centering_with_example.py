### Functions used to determine TC center using vector differencing technique ###
### Created by Michael Fischer on 5/20/2021 ###


# Import libraries:

import numpy as np
import sys
import warnings
warnings.filterwarnings("ignore")


### Define useful functions ###

# Function to find value of array closest to specified value:
def find_nearest(array, value):
    
    array = np.asarray(array)
    X = np.abs(array - value)
    idx = np.where( X == np.nanmin(X) )
    
    return array[idx]

# Function to find index of array closest to specified value:
def find_nearest_ind(array, value):
    
    array = np.asarray(array)
    X = np.abs(array - value)
    idx = np.where( X == np.nanmin(X) )

    return idx

# Function to compute distance:
def distance(s_lat, s_lng, e_lat, e_lng):
    
    """
    Where s_lat and s_lng are the starting latitudes/longitudes
    And e_lat and e_lng are the ending latitudes/longitudes.
    These should be individual floats or numpy arrays.
    """


    # approximate radius of earth in km
    R = 6373.0

    s_lat = s_lat*np.pi/180.0                      
    s_lng = np.deg2rad(s_lng)     
    e_lat = np.deg2rad(e_lat)                       
    e_lng = np.deg2rad(e_lng)  

    d = np.sin((e_lat - s_lat)/2)**2 + np.cos(s_lat)*np.cos(e_lat) * np.sin((e_lng - s_lng)/2)**2

    return 2 * R * np.arcsin(np.sqrt(d))

# Function to compute the angle of two grid points:
def get_bearing(s_lat, s_lng, e_lat, e_lng):
    
    """
    Where s_lat and s_lng are the starting latitudes/longitudes
    And e_lat and e_lng are the ending latitudes/longitudes.
    These should be individual floats or numpy arrays.
    """

    # approximate radius of earth in km
    R = 6373.0

    s_lat = s_lat*np.pi/180.0                      
    s_lng = np.deg2rad(s_lng)     
    e_lat = np.deg2rad(e_lat)                       
    e_lng = np.deg2rad(e_lng)  

    # meridional distance:
    md_raw = np.sin((e_lat - s_lat)/2)**2
    md_sign = np.sign(e_lat - s_lat) # Acquire integer sign of distance (i.e., -1, 0, or 1)
    md = md_sign * (2 * R * np.arcsin(np.sqrt(md_raw)))
    
    # zonal distance:
    zd_raw = np.cos(s_lat)*np.cos(e_lat) * np.sin((e_lng - s_lng)/2)**2
    # Identify the sign of the zonal distance:
    zd_sign = np.sign(e_lng - s_lng) # Acquire integer sign of distance (i.e., -1, 0, or 1)
    zd = zd_sign * (2 * R * np.arcsin(np.sqrt(zd_raw)))
    
    bearing = np.arctan2(md,zd)
    
    return bearing
        
        
### Establish the function used to determine the TC center for the vector differencing technique:

def recenter_tc(uwind,vwind,lons,lats,num_sectors,spad,num_iterations,olon=None,olat=None):
    
    """
    This function is optimized to re-center TDR merged analyses and/or (hopefully) model analyses
    
    ***
    NOTE: For this function to work, the "ideal_angle" variable should have already been created.
    Here, we assume ideal_angle is a four-dimensional array, shaped proportionally to uwind/vwind.
    
    Furthermore, this function assumes data is on an evenly-spaced grid of value delta_grid.
    ***
    
    Inputs variables are the following:
    
    1) uwind is the storm-relative zonal component of the TC flow. Should be two-dimensional array ([lat,lon]).
    2) vwind is the storm-relative meridional component of the TC flow. Should be two-dimensional array ([lat,lon]).
    3) lons is a two-dimensional array of the longitudinal coordinates
    4) lats is a two-dimensional array of the latitudinal coordinates
    5) num_sectors is the number (integer) of azimuthal sectors used to compute the mean error (e.g., quadrants = 4).
    6) spad is the number of gridpoints in each direction from the center to search for maximizing weighted 
       tangential wind difference
    7) num_iterations is the amount of times to loop to try to find a center that converges
    8) olon is the first-guess center longitude (float). Default is None, and midpoint of grid will be used.
    9) olat is the first-guess center latitude (float). Default is None, and midpoint of grid will be used.
    
    """
    
    # Sectors to process:
    angle_thresh = np.linspace(-1.*np.pi,np.pi,num_sectors+1) # Bounds of azimuthal sectors to average angle errors
    
    # Tyler changed rad_gaussian creation, look for RMW every "curr_delta" km:
    gr0=2 # Starting distance to search for RMW
    grf=175 # Ending distance to search for RMW
    curr_delta=2.0 # Annulus width (km) used to search for RMW
    rad_gaussian = np.arange(gr0,grf+curr_delta,curr_delta) # Array of radii to use for radius of maximum wind computations
    
    # Define core distance:
    search_radius = 150. # distance (km) to consider grid points for error calculations
    core_radius = 100. # distance (km) from hypothetical TC center to weight Gaussian errors
    coverage_radius = 100. # distance (km) from hypothetical TC center to consider for data coverage
    coverage_radius_inner = 50. # inner distance (km) from hypothetical TC center to consider for data coverage
    
    # Define initial search range:
    ipad = 6 # The first search iteration will search grid points of this interval
    
    # Define min_dist_weight (in case values get really small for truncation issues):
    min_dist_weight = 1.E-6 # Minimum value to use for distance weighting
    
    # Definite required amount of data within core radius to even attempt to compute a center:
    min_data_frac = 0.02

    ####################################
    #Start recentering
    ####################################

    # Prepare to iterate to find array indices of TC center; initialize as NaNs:
    yloc = np.nan # Meridional index of where TC center was found
    xloc = np.nan # Zonal index of where TC center was found

    # Determine grid indices closest to first-guess TC center, if provided:
    if olon != None:
        try:
            # Find closest lat/lon index to real-time center (olon_np/olat_np):
            lat_close = find_nearest(lats,olat)[0]
            pnyi = np.where(lats == lat_close)[0][0] # meridional index of prior guess center
            lon_close = find_nearest(lons[pnyi,:],olon)[0]
            pnxi = np.where(lons[pnyi,:] == lon_close)[0][0] # zonal index of prior guess center
        except IndexError:
            # If I cannot identify the nearest grid point for whatever reason, just use midpoint of grid:
            pnyi = int(round(lons.shape[0]/2.,0))
            pnxi = int(round(lons.shape[1]/2.,0))
    
    # Use midpoint of grid if no first-guess center is provided:
    if olon == None:
        pnyi = int(round(lons.shape[0]/2.,0))
        pnxi = int(round(lons.shape[1]/2.,0))
    
    # Compute wind speed:
    ws = np.sqrt(uwind**2 + vwind**2)
    
    # Wind angle difference between observed and idealized vortex:
    angle_dif = np.full((uwind.shape[0],uwind.shape[1]),np.nan,dtype='f8')
    
    # Weighted difference of angle_dif:
    weighted_dif = np.full((uwind.shape[0],uwind.shape[1]),np.nan,dtype='f8')
    
    # Observed wind angle:
    obs_angle = np.full((uwind.shape[0],uwind.shape[1]),np.nan,dtype='f8')
    
    # Create an array to be filled with mean errors:
    sector_mean_error = np.full((num_sectors,uwind.shape[0],uwind.shape[1]),np.nan,dtype='f8')
    
    # Set assumed previous error difference to infinity; will be replaced in loop:
    prev_mean_dif =  np.inf
    
    # Set maximum azimuthally-averaged tangential wind equal to NaN:
    vt_azi_max = np.nan
    data_cov = np.nan
    tc_rmw = np.nan
    
    ### Begin TC center search ###
    
    for n in range(num_iterations):

        print('current iteration is:',n)
        
        # If first iteration has been completed, copy center estimate from previous iteration:
        if n >= 1:
            if prev_mean_dif < np.inf:
                pnyi = np.copy(yloc)
                pnxi = np.copy(xloc)
            else:
                tc_center_lon = np.nan
                tc_center_lat = np.nan
                break
            
        # Establish range of grid points to search for TC center:
        # For first sweep through, use a coarser grid of interval "ipad"
        if n == 0:
            range_y = np.arange(pnyi - ipad*ipad, pnyi + (ipad*ipad)+1,ipad)
            range_x = np.arange(pnxi - ipad*ipad, pnxi + (ipad*ipad)+1,ipad)
        # For the remaining iterations, use original grid spacing to search:
        if n > 0:
            range_y = np.arange(pnyi - spad,pnyi + spad+1,1)
            range_x = np.arange(pnxi - spad,pnxi + spad+1,1)
        
        # Loop over meridional grid points:
        for ybi in range_y:
            
            # Ensure the algorithm isn't searching outside the bounds of the domain:
            if ybi >= int(uwind.shape[0]):
                continue
            if ybi < 0:
                continue

            # Loop over zonal grid points:
            for xbi in range_x:
                
                # Ensure the algorithm isn't searching outside the bounds of the domain:
                if xbi >= int(uwind.shape[1]):
                    continue
                if xbi < 0:
                    continue
                
                # Ensure the mean error at this grid point hasn't been computed already:
                if n >= 1:
                    if np.isfinite(np.nanmean(sector_mean_error[:,ybi,xbi])):
                        continue


                ### Establish error weighting ###
                
                # First determine the radial distance weighting for errors:
                # Make sure distance errors are normalized so that hypothetical TC centers
                # Located far from data aren't weighted more favorably
                
                # Compute the distance from each grid point relative to hypothetic TC center:
                curr_dist = distance(lats[ybi,xbi],lons[ybi,xbi],lats,lons)
                
                # Compute the angle from each grid point relative to hypothetic TC center:
                angle = get_bearing(lats[ybi,xbi],lons[ybi,xbi],lats,lons) #angle (radians) of each grid point from TC center
                
                # Identify grid points with finite data:
                yf = np.where(ws >= 0.)[0] # meridional indices with finite data
                xf = np.where(ws >= 0.)[1] # zonal indices with finite data
                
                dist_weight_raw = np.full((ws.shape[0],ws.shape[1]),np.nan) # Non-normalized distance weight
                
                # Apply the distance weighting for grid points with data:
                dist_weight_raw[yf,xf] = np.exp(-1.*(((curr_dist[yf,xf] - 0.)**2)/(2.*((0.25*core_radius)**2))))
                
                # Impose minimum bound of distance weighting:
                dist_low_inds = np.where(dist_weight_raw < min_dist_weight) #Two-dimensional indices
                
                # Replace distance weights deemed to be too small relative to lower bound threshold:
                dist_weight_raw[dist_low_inds] = min_dist_weight
                
                # Normalize distance weights:
                dist_weight = dist_weight_raw/np.nanmean(dist_weight_raw)
                
                # Compute the wind weights, so stronger wind speeds are weighted more heavily:
                wind_weight = np.sqrt(ws+1)

                # Compute the total weighting (in this case, multiply the distance and wind weighting):
                curr_weight = dist_weight*wind_weight
                
                ### Compute the angle difference between idealized vortex and observed flow ###
                
                # Compute the angle of wind in the idealized vortex (completely tangential):
                ideal_angle = np.copy(angle) + np.pi/2.

                # Correct for instances where the angle exceeds pi:
                highy = np.where(ideal_angle > np.pi)[0]
                highx = np.where(ideal_angle > np.pi)[1]

                ideal_angle[highy,highx] = ideal_angle[highy,highx] - 2.*np.pi
                
                curr_angle_dif = np.arctan2(vwind,uwind) - ideal_angle[:,:]

                # Correct for angle differences outside of specified range:
                low_angle_inds = np.where(curr_angle_dif < -1.*np.pi) #Two-dimensional indices

                curr_angle_dif[low_angle_inds] = 2.*np.pi + curr_angle_dif[low_angle_inds]

                high_angle_inds = np.where(curr_angle_dif > np.pi) #Two-dimensional indices

                curr_angle_dif[high_angle_inds] = curr_angle_dif[high_angle_inds] - 2.*np.pi
                
                # Compute the finalized errors at each grid point:
                curr_weighted_dif = curr_weight*curr_angle_dif
                
                # Only compute errors within core radius; exclude outer points:
                outer_inds = np.where(curr_dist > search_radius) #Two-dimensional indices
                
                curr_weighted_dif[outer_inds] = np.nan
                
                # Calculate the number of finite data points within outer coverage radius:
                coverage_y = np.where(curr_dist <= coverage_radius)[0]
                coverage_x = np.where(curr_dist <= coverage_radius)[1]
                curr_nf = float(np.size(np.where(np.isfinite(curr_weighted_dif[coverage_y,coverage_x]))))
                
                # Calculate the number of finite data points within inner coverage radius:
                inner_y = np.where(curr_dist <= coverage_radius_inner)[0]
                inner_x = np.where(curr_dist <= coverage_radius_inner)[1]
                curr_nf_inner = float(np.size(np.where(np.isfinite(curr_weighted_dif[inner_y,inner_x]))))
                
                # Calculate the number of total data points:
                curr_nt = float(np.size(curr_weighted_dif[coverage_y,coverage_x]))
                curr_nt_inner = float(np.size(curr_weighted_dif[inner_y,inner_x]))
                
                # Ensure a sufficient amount of data is available:
                try: 
                    if float(curr_nf/curr_nt) < min_data_frac:
                        # If not enough data, skip to next point
                        continue
                except ZeroDivisionError:
                    continue
                
                # Separate grid into azimuthal sectors:                    
                for thi in range(np.size(angle_thresh) - 1):
                    angle_upper = angle_thresh[thi+1]
                    angle_lower = angle_thresh[thi]
                    
                    ythi = np.where(np.logical_and(angle >= angle_lower, angle < angle_upper))[0]
                    xthi = np.where(np.logical_and(angle >= angle_lower, angle < angle_upper))[1]
                                        
                    sector_mean_error[thi,ybi,xbi] = np.nanmean(np.abs(curr_weighted_dif[ythi,xthi]))

                # Compute mean error of each quadrant:
                curr_mean_dif = np.nanmean(sector_mean_error[:,ybi,xbi])

                # Check to see if the current location yields a better center estimate than the previous iteration:
                if curr_mean_dif < prev_mean_dif:

                    # Set previous error equal to current error for next iteration:
                    prev_mean_dif = np.copy(curr_mean_dif)

                    # Store previous angle differences:
                    angle_dif[:,:]    = curr_angle_dif
                    weighted_dif[:,:] = curr_weighted_dif
                    obs_angle[:,:]    = np.arctan2(vwind[:,:],uwind[:,:])

                    # Store TC location estimate:
                    tc_center_lon = lons[ybi,xbi]
                    tc_center_lat = lats[ybi,xbi]
                        
                    # Store the minimum data coverage within the two coverage domains:
                    data_cov = np.nanmin([curr_nf/curr_nt,curr_nf_inner/curr_nt_inner])

                    # Store the indices of the estimated TC center:
                    yloc = np.copy(ybi)
                    xloc = np.copy(xbi)
                        
        
        # If TC center estimate converges, break from loop and keep estimate:
        if n >= 1:
            if np.logical_and(yloc == pnyi, xloc == pnxi):
                
                ### Compute RMW using location of estimated TC center ###
                
                # Compute distance relative to TC center:
                curr_dist = distance(lats[yloc,xloc],lons[yloc,xloc],lats,lons)
                
                # Compute angle relative to TC center:
                angle = get_bearing(lats[yloc,xloc],lons[yloc,xloc],lats,lons) #angle (radians) of each grid point from TC center

                # Calculate tangential wind about TC center:
                curr_vt = -1.*(uwind*np.sin(angle) - vwind*np.cos(angle))

                # Identify the annulus where the tangential wind is maximized:
                vt_ann_azi = np.full((np.size(rad_gaussian)),np.nan,dtype='f4')
                
                # Loop over radii:
                for ri in range(np.size(rad_gaussian)):
                    rmw_ann_xy = np.logical_and(curr_dist >= rad_gaussian[ri] - 0.5*curr_delta,\
                                                curr_dist < rad_gaussian[ri] + 0.5*curr_delta)
                    
                    # Compute the maximum value from all annuli:
                    vt_ann_azi[ri] = np.nanmean(curr_vt[rmw_ann_xy])

                # Identify the maximum azimuthally-averaged tangential wind:
                vt_azi_max = np.nanmax(vt_ann_azi)

                try:
                    tc_rmw = rad_gaussian[np.where(vt_ann_azi == vt_azi_max)[0][0]]
                except IndexError:
                    tc_rmw = np.nan
                        
                # Delete old variables to conserve memory:
                del curr_dist
                del angle
                    
                break
            
    # Delete old variables to conserve memory:
    del sector_mean_error
    del yloc
    del xloc
    del pnyi
    del pnxi
    
    
    return tc_center_lon, tc_center_lat, vt_azi_max, tc_rmw, data_cov


### Example application ###
    
# levv is the vertical index of the desired height (I typically use 2 km to-recenter swath data)
# pi is the desired swath index
# lons_np is a 3D longitudinal array [swath_index,latitude,longitude]
# lats_np is a 3D latitudinal array [swath_index,latitude,longitude]
# I've found the algorithm to work well without dividing the errors into quadrants; use tc_sectors = 1
# Typically, I use spad=3
# Typically, I use num_iters=50
    
#tc_center_coords = recenter_tc(u_np[pi,:,:,levv],v_np[pi,:,:,levv],\
#                               lons_np[pi,:,:],lats_np[pi,:,:],tc_sectors,\
#                               spad,num_iters,None,None)
    
# Ensure sufficient data coverage:
#swath_cov = tc_center_coords[4] # Swath data coverage within search domain
#print('Current data coverage is:',swath_cov)
#swath_rmw = tc_center_coords[3]
#print('Current RMW is:',swath_rmw)
#vt_azi_max = tc_center_coords[2]
#print('Current vt_azi_max is:',vt_azi_max)

# Select some coverage threshold needed to grab center coordinates.
# (I found a value of 0.15 to be good enough for most swaths; but 0.2 is more conservative).
# Larger values require more coverage to get a center estimate but provide higher confidence estimates.
# Theoretically, you could use a very small value (like 0.001) to make sure you get some center estimate,
# But this could be one of large uncertainty.
#if swath_cov >= 0.2:
#    swath_tc_lon = tc_center_coords[0]
#    swath_tc_lat = tc_center_coords[1]


### END ###
