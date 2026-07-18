"""
Name: HepTools (Hurricane Ensemble Products - Tools)
Author: Nikhil Trivedi, Matt Donahue
Description:
This script simply contains a list of functions that are repeatedly used by many of the other scripts in this "library". They are stored in here
so they can just be written once, rather than being repeated and cluttering the other scripts. A list of these functions and a brief description of
each of them will now be provided:
1) getAtcfData: retrieves raw ATCF data for the given model and processes it into a Pandas DataFrame for each member, returning a list of these DataFrames
2) getClusterRanks: ranks the members within the processed list of ATCF data based on a provided attribute
3) getMemberData: returns an averaged xarray DataArray for the provided members, using the specified model, variable, and pressure level
4) getRadAvgWinds: converts a cartesian coordinate system centered on a TC to a radial-averaged system, returning an xarray DataArray with this data
5) getDynamicVortex: uses a radial-averaged DataArray to objectively estimate both the width and depth of a TC's vortex, returning its bounds
Last modified Oct 17 2025
"""

"""
1/13/2026 - Meeting to consolidate some functions. Going to exclude getAtcfData() in favor of process_atcf.
"""

import pandas as pd
import xarray as xr
import cfgrib
import numpy as np
from pyproj import Proj
from datetime import datetime, timedelta
import os
import subprocess
import glob
import warnings

#this script relies on GPLOT_DIR existing in the environment
GPLOT_DIR = os.environ['GPLOT_DIR']

# dictionaries for conversions
keysDict = {"mslp": {'typeOfLevel': 'meanSea', "shortName": "prmsl"}, 
            "height": {'typeOfLevel': 'isobaricInhPa', "shortName": "gh"},
            "shum": {'typeOfLevel': 'isobaricInhPa', "shortName": "q"},
            "refl": {'stepType': 'instant', 'typeOfLevel': 'atmosphereSingleLayer', "shortName": "refc"},
            "u-wind": {'typeOfLevel': 'isobaricInhPa', "shortName": "u"},
            "v-wind": {'typeOfLevel': 'isobaricInhPa', "shortName": "v"},
            "w-wind": {'typeOfLevel': 'isobaricInhPa', "shortName": "w"},
            "temp": {'typeOfLevel': 'isobaricInhPa', "shortName": "t"}}


def getTrackSpeedData(atcfData, fHour):
    """
    Calculates along-track (called ltrack) and across-track (called xtrack) deviations relative 
    to the ensemble mean storm center.
    args:
        atcfData: DataFrame, must contain 'TAU', 'latitude', and 'longitude' columns.
        fHour: int, the forecast hour to analyze.
    returns:
        DataFrame for the specific hour with added columns:
            'xtrack': float, across-track distance from center mean in km (positive = right).
            'ltrack': float, along-track distance from center mean in km (positive = ahead).
    """
    relevantHours = atcfData[atcfData['TAU'].isin([fHour, fHour - 6, fHour + 6])].copy()
    hourData = relevantHours[relevantHours["TAU"] == fHour].copy()

    # project to Cartesian coordinates that are centered on mean
    meanCoords = relevantHours.groupby('TAU')[['longitude', 'latitude']].mean()
    meanLon, meanLat = meanCoords.loc[fHour].values
    proj = Proj(proj='aeqd', lat_0=meanLat, lon_0=meanLon, datum='WGS84', units='m')
    x, y = proj(hourData['longitude'].values, hourData['latitude'].values)

    # calculate storm direction of motion (and resolve edge cases)
    if fHour == atcfData['TAU'].min():
        prevLon, prevLat = meanCoords.loc[fHour].values
        nextLon, nextLat = meanCoords.loc[fHour + 6].values
    elif fHour == atcfData['TAU'].max():
        prevLon, prevLat = meanCoords.loc[fHour - 6].values
        nextLon, nextLat = meanCoords.loc[fHour].values
    else:
        prevLon, prevLat = meanCoords.loc[fHour - 6].values
        nextLon, nextLat = meanCoords.loc[fHour + 6].values
    dx = (nextLon - prevLon) * np.cos(np.deg2rad(meanLat))
    dy = (nextLat - prevLat)
    direction = np.arctan2(dy, dx)

    # rotate coordinates to make storm north-moving
    theta = np.pi/2 - direction
    x_rot = x * np.cos(theta) - y * np.sin(theta)
    y_rot = x * np.sin(theta) + y * np.cos(theta)

    # add ltrack and xtrack data to hourData
    hourData.loc[:, 'xtrack'] = np.round(x_rot / 1000, 2)
    hourData.loc[:, 'ltrack'] = np.round(y_rot / 1000, 2)
    return hourData
    

def getGribData(basePath, bounds, members=range(0, 21), 
                initDate="all", variable="u-wind", fHour="all", level=None):
    """
    Optimized GRIB2 data fetcher using wgrib2 to pre-slice the needed data. Determines 
    extraction method (Earth vs Storm relative) based on `bounds` input. Uses helper
    functions defined directly below (helper names start with _).
    args:
        basePath: str, path to data directory.
        bounds: List or Dict defining the extraction region:
            List: [[lat_min, lat_max], [lon_min, lon_max]] for Earth-Relative.
            Dict: {member_id: (center_lat, center_lon)} for Storm-Relative.
        members: List, list of member IDs to fetch.
        initDate: str, specific model run to use.
        variable: str or List, variable shortname or list of variables.
        fHour: int, the forecast hour to analyze.
        level: int or None, pressure level (e.g., 500). If None, gets all levels.
    returns:
        xarray.Dataset containing the requested ensemble data.
    """

    # External shell script that runs the wgrib2 command
    script_path = GPLOT_DIR + "/ush/extract_box.sh"
    
    # Build the regex string to pass to wgrib2 later
    match_str = _build_match_string(variable, level)
    
    print(f"Fetching data for members {list(members)} using wgrib2 optimization...")
    
    # If 'bounds' is a list, get a fixed geographical box (Earth-Relative plot)
    if isinstance(bounds, list):
        return _process_earth_relative(basePath, bounds, members, initDate, 
                                       fHour, match_str, script_path)
    
    # If 'bounds is a dict, get a storm-centered box for each member (Storm-Relative plot)
    else:
        return _process_storm_relative(basePath, bounds, members, initDate, 
                                       fHour, match_str, script_path, variable)


def _build_match_string(variable, level):
    """
    Constructs the regex match string used by wgrib2 to filter variables and levels.
    returns:
        String containing the formatted regex (e.g. "(:HGT:)(.*:)?500 mb:").
    """
    
    # Handle list of variables (e.g. ['UGRD', 'VRGD']) or single string
    if isinstance(variable, list):
        parts = [f":{v}:" for v in variable]
        match_str = f"({'|'.join(parts)})" # Join with OR operator. Ex: (:UGRD:|:VGRD:)
    else:
        match_str = f"(:{variable}:)"
    
    # Handle level (both ":HGT:500 mb:" (adjacent) and ":HGT:...:500 mb:" (separated))
    if level:
        match_str += f"(.*:)?{level} mb:"
    
    return match_str


def _process_earth_relative(basePath, bounds, members, initDate, fHour, 
                            match_str, script_path):
    """
    Fetches data for a fixed Lat/Lon box across all members.
    returns:
        xarray.Dataset (averaged data across members).
    """
    lat_b, lon_b = bounds  # Unpack bounds [[lat_min, lat_max], [lon_min, lon_max]]
    datasets = []
    
    for member in members:
        # Construct path to source GRIB file, skip member if it doesn't exist
        grib_path = f"{basePath}/{initDate}/{member:02}/00l.{initDate}.hfsa.parent.atm.f{fHour:03}.grb2"
        if not os.path.exists(grib_path):
            print(f"Skipping missing member {member}")
            continue
        
        temp_file = f"/dev/shm/temp_mem{member:02}_f{fHour}.grb2"  # Define temp file name for this member
        
        try:
            # Call the shell script to extract the specific lat/lon box and variable data
            _run_wgrib2_subset(script_path, grib_path, temp_file, 
                               lon_b[0], lon_b[1], lat_b[0], lat_b[1], match_str)
            
            # Open data with xarray (ensuring we only get pressure levels) and load into RAM
            ds = xr.open_dataset(temp_file, engine='cfgrib', decode_timedelta=False,
                                 backend_kwargs={'filter_by_keys': {'typeOfLevel': 'isobaricInhPa'}, 'indexpath': ''})
            datasets.append(ds.load())
            
        except Exception as e:
            print(f"Error processing member {member}: {e}")
            
        finally:
            for f in glob.glob(f"{temp_file}*"):
                os.remove(f)  # Remove file once we've loaded its contents in

    if not datasets:
        raise ValueError("No data found for any members.")

    # Average all members along a new 'member' dimension and return
    combined = xr.concat(datasets, dim='member')
    return combined.mean(dim='member')


def _process_storm_relative(basePath, bounds, members, initDate, fHour, 
                            match_str, script_path, variable):
    """
    Fetches data centered on a storm for each member (with bounds being a fixed distance 
    from the member's storm center), transforming coordinates from degrees to km.
    returns:
        xarray.Dataset (averaged across members, coordinates in km centered at 0).
    """
    TARGET_BOX_SIZE = 5.0  # The final box size in degrees (e.g., 5x5 degree box)
    BUFFER = 1.0           # Extra padding for the initial cut to ensure we don't clip edges
    member_datasets = []
    
    for member in members:    
        center_lat, center_lon = bounds[member]
        
        # Define a rough box around the center with some buffer room
        lon_min = center_lon - (TARGET_BOX_SIZE/2 + BUFFER)
        lon_max = center_lon + (TARGET_BOX_SIZE/2 + BUFFER)
        lat_min = center_lat - (TARGET_BOX_SIZE/2 + BUFFER)
        lat_max = center_lat + (TARGET_BOX_SIZE/2 + BUFFER)

        # Construct path to source GRIB file, skip member if it doesn't exist
        grib_path = f"{basePath}/{initDate}/{member:02}/00l.{initDate}.hfsa.parent.atm.f{fHour:03}.grb2"
        if not os.path.exists(grib_path):
            print(f"Skipping missing member {member}")
            continue
        
        temp_file = f"/dev/shm/temp_centered_mem{member:02}_f{fHour}.grb2"  # Define temp file name for this member
        
        try:
            # Extract rough box using shell script
            _run_wgrib2_subset(script_path, grib_path, temp_file, 
                               lon_min, lon_max, lat_min, lat_max, match_str)
            
            # Open data and load into RAM
            ds = xr.open_dataset(temp_file, engine='cfgrib', decode_timedelta=False,
                                 backend_kwargs={'filter_by_keys': {'typeOfLevel': 'isobaricInhPa'}, 'indexpath': ''})
            ds = ds.load()
            
            # Trim rough box to be exactly the size defined by TARGET_BOX_SIZE
            centered_box = _dynamic_slice_and_center(ds, center_lat, center_lon, TARGET_BOX_SIZE)
            member_datasets.append(centered_box)
            
        except Exception as e:
            print(f"Error processing member {member}: {e}")
            
        finally:
            for f in glob.glob(f"{temp_file}*"):
                os.remove(f)

    # Average members and return Dataset with coordinates in KM, centered at (0,0)
    combined = xr.concat(member_datasets, dim="member")
    return combined.mean(dim="member")


def _run_wgrib2_subset(script, infile, outfile, lon_min, lon_max, lat_min, lat_max, match):
    """Executes the external shell script to slice GRIB2 files via subprocess."""
    cmd = [script, infile, outfile, str(lon_min), str(lon_max), 
           str(lat_min), str(lat_max), match]
    
    # Run command and wait for it to finish, silence terminal output
    subprocess.check_call(cmd, stdout=subprocess.DEVNULL)


def _dynamic_slice_and_center(ds, clat, clon, target_size_deg):
    """
    Refines a rough GRIB slice into a precise box centered on the storm and converts axes to km.
    returns:
        xarray.Dataset with 'latitude' and 'longitude' coordinates converted to km offset.
    """
    # Get resolution (to determine how many pixels equals 5 degrees)
    var_name = list(ds.data_vars)[0]
    lats = ds[var_name].latitude.values
    res = abs(lats[1] - lats[0]) # e.g., 0.03 degrees
    
    # Calculate box size in pixels (get num of grid points needed)
    n_pixels = int(round(target_size_deg / res))
    
    # Force odd number of pixels so there is a perfect center pixel
    if n_pixels % 2 == 0:
        n_pixels += 1
    half_grid = n_pixels // 2
    
    # Snap ATCF center to the closest lat/lon indices in the model grid
    ilat_c = int(np.argmin(np.abs(ds.latitude.values - clat)))
    ilon_c = int(np.argmin(np.abs(ds.longitude.values - clon)))
    
    # Slice the dataset around the model center
    sub_ds = ds.isel(
        latitude=slice(ilat_c - half_grid, ilat_c + half_grid + 1),
        longitude=slice(ilon_c - half_grid, ilon_c + half_grid + 1)
    )
    
    # Replace lat/lon coords with storm-relative km coords
    km_axis = (np.arange(n_pixels) - half_grid) * res * 111.0
    sub_ds = sub_ds.assign_coords(latitude=km_axis, longitude=km_axis)
    
    return sub_ds
    

def getStormName(storm, initDate):
    """
    Retrieves storm name from bdeck and formats it into a string to be used in plot titles.
    returns:
        str formatted as: {NAME}-{number}L
    """
    bTrack = pd.read_csv(f'/work/noaa/hwrf/noscrub/input/abdeck/btk/b{storm.lower()}.dat', usecols=range(35), header=None)
    bTrack = bTrack[bTrack[2] == initDate]
    name = f"{bTrack.iloc[0, 27].strip()}-{bTrack.iloc[0, 1]:02d}L"
    return name




"""
Matt's added functions
1. process_atcf_files - analog to Nikhil's getAtcfData
2. gather_polar_structure_data - get polar structure statistics from GPLOT polar module outputs
3. tilt_displacement - convert tilt magnitude (km) and direction to lat and lon displacements (for plotting a tilt vector easily on a lat-lon grid)
4. combine_polar_and_atcf - put together polar shear and tilt data with atcf data (used for tilt plots) NOTE - WILL PROBABLY NEED TO MAKE SUBSTANTIAL CHANGES AFTER COMBINING ATCF PROCESSING FUNCTIONS
"""

# CREDIT: TAKEN FROM /work2/noaa/aoml-hafs1/lgramer/ocean/HEP/gp_tools.py
def str2latlon(s,div=1.0):
    if ( isinstance(s,str) ):
        s = s.strip();
        if ( s[:1].isdigit() or (s[:1]=='-' and s[1:2].isdigit()) ):
            if ( s.endswith('S') or s.endswith('W') ):
                s = '-' + s;
            try:
                s = np.double(s.strip('NSEW'))/div;
            except:
                pass;
    return(s);


# CREDIT: TAKEN FROM /work2/noaa/aoml-hafs1/lgramer/ocean/HEP/gp_tools.py
def read_atcf(fname):
    '''Read ATCF file for an individual TC forecast model block, and return it as a pandas DataFrame. Merges all records for a given IDate and FHr into a single record, adding Series "r50_rad[1234]" and "r64_rad[1234]". NOTE: Currently assumes there are seven (7) User Defined Data columns in the ATCF file.'''
# BASIN, CY, YYYYMMDDHH, TECHNUM/MIN, TECH, TAU, LatN/S, LonE/W, VMAX, MSLP, TY, RAD, WINDCODE, RAD1, RAD2, RAD3, RAD4, POUTER, ROUTER, RMW, GUSTS, EYE, SUBREGION, MAXSEAS, INITIALS, DIR, SPEED, STORMNAME, DEPTH, SEAS, SEASCODE, SEAS1, SEAS2, SEAS3, SEAS4, USERDEFINED, userdata
# BA, CY, YYYYMMDDHH, TN, TECH, TAU, LATI,  LONG, VMX, MSLP, TY, RAD, WCD, RAD1, RAD2, RAD3, RAD4, POCI, ROCI, RMW,GUST, EYE,SUBR,MXSE,INIS, DIR, SPD, STORM NAME,DP,SEA,SCOD,SEA1,SEA2,SEA3,SEA4,         USERDEFINED,userdat1,userdat2,userdat3,u4,ud5,ud6,udat7
# AL, 06, 2018091206, 03, HWRF, 000, 285N,  695W, 120,  945, XX,  34, NEQ, 0228, 0196, 0077, 0137, 1013,  199,  19,   0,   0,    ,   0,    ,   0,   0,           ,  ,   ,    ,   0,   0,   0,   0,       THERMO PARAMS,      11,    2336,    4584, Y, 10, DT, -999
# AL, 06, 2018091206, 03, HWRF, 000, 285N,  695W, 120,  945, XX,  50, NEQ, 0072, 0067, 0036, 0058, 1013,  199,  19,   0,   0,    ,   0,    ,   0,   0,           ,  ,   ,    ,   0,   0,   0,   0,       THERMO PARAMS,      11,    2336,    4584, Y, 10, DT, -999
# AL, 06, 2018091206, 03, HWRF, 000, 285N,  695W, 120,  945, XX,  64, NEQ, 0044, 0036, 0028, 0037, 1013,  199,  19,   0,   0,    ,   0,    ,   0,   0,           ,  ,   ,    ,   0,   0,   0,   0,       THERMO PARAMS,      11,    2336,    4584, Y, 10, DT, -999
# AL, 06, 2018091206, 03, HWRF, 003, 290N,  701W, 116,  943, XX,  34, NEQ, 0144, 0142, 0094, 0116, 1011,  154,  20,   0,   0,    ,   0,    ,   0,   0,           ,  ,   ,    ,   0,   0,   0,   0,       THERMO PARAMS,     -15,    2162,    4115, Y, 10, DT, -999
# AL, 06, 2018091206, 03, HWRF, 003, 290N,  701W, 116,  943, XX,  50, NEQ, 0066, 0062, 0043, 0052, 1011,  154,  20,   0,   0,    ,   0,    ,   0,   0,           ,  ,   ,    ,   0,   0,   0,   0,       THERMO PARAMS,     -15,    2162,    4115, Y, 10, DT, -999
# AL, 06, 2018091206, 03, HWRF, 003, 290N,  701W, 116,  943, XX,  64, NEQ, 0045, 0034, 0032, 0038, 1011,  154,  20,   0,   0,    ,   0,    ,   0,   0,           ,  ,   ,    ,   0,   0,   0,   0,       THERMO PARAMS,     -15,    2162,    4115, Y, 10, DT, -999
# ADDITIONAL (NEW?) FORMAT:
# AL, 15, 2021091818, 03, HAFS, 000, 387N,  657W,  45,  998, XX,  34, NEQ, 0185, 0170, 0000, 0100,  -99,  -99,  53,   0,   0,    ,   0,    , ,  71, 142,           ,  ,   ,    ,   0,   0,   0,   0,       THERMO PARAMS,     297,     963,    -776, N, 10, DT, -999, SHR82,  -99,   0, SST,  -99, ARMW,  55,  28
    
    data = pd.read_csv(fname,header=None);
    #data = data.applymap(lambda x: str2latlon(x,10));
    data = data.map(lambda x: str2latlon(x,10));
    if ( data.shape[1] == 52 ):
        # NOTE: TDIR and TSPEED appear to be shifted one field to the right vs. documentation
        data.columns = ['basin','stno','idtstr','mdlno','mdl','fhr','lat','lon','vmax','mslp','typ','rad','windcode','rad1','rad2','rad3','rad4','poci','roci','rmw','gusts','eye','subregion','maxseas','initials','userdata16','tdir','tspeed','stnm','depth','seas','seascode','seas1','seas2','seas3','seas4','userdef','userdata1','userdata2','userdata3','userdata4','userdata5','userdata6','userdata7','userdata8','userdata9','userdata10','userdata11','userdata12','userdata13','userdata14','userdata15'];
    elif ( data.shape[1] == 51 ):
        # Annoying: 2023 real-time experiments (HAFS v1.1.0) randomly dropped the extra field for short ATCF records...
        data.columns = ['basin','stno','idtstr','mdlno','mdl','fhr','lat','lon','vmax','mslp','typ','rad','windcode','rad1','rad2','rad3','rad4','poci','roci','rmw','gusts','eye','subregion','maxseas','initials','tdir','tspeed','stnm','depth','seas','seascode','seas1','seas2','seas3','seas4','userdef','userdata1','userdata2','userdata3','userdata4','userdata5','userdata6','userdata7','userdata8','userdata9','userdata10','userdata11','userdata12','userdata13','userdata14','userdata15'];
    ######################################################### Matt 10/21/2025 - HERC atcf dump from 2023 - ensemble mean has 17 columns and control has 20 ##########################################
    elif ( data.shape[1] == 17 ):
        data.columns = ['basin','stno','idtstr','mdlno','mdl','fhr','lat','lon','vmax','mslp','typ','rad','windcode','rad1','rad2','rad3','rad4']
    elif ( data.shape[1] == 20 ):
        data.columns = ['basin','stno','idtstr','mdlno','mdl','fhr','lat','lon','vmax','mslp','typ','rad','windcode','rad1','rad2','rad3','rad4','unknown_to_matt_1','unknown_to_matt_2','unknown_to_matt_3']
    else:
        data.columns = ['basin','stno','idtstr','mdlno','mdl','fhr','lat','lon','vmax','mslp','typ','rad','windcode','rad1','rad2','rad3','rad4','poci','roci','rmw','gusts','eye','subregion','maxseas','initials','tdir','tspeed','stnm','depth','seas','seascode','seas1','seas2','seas3','seas4','userdef','userdata1','userdata2','userdata3','userdata4','userdata5','userdata6','userdata7'];
    data['idt'] = pd.to_datetime(data['idtstr'],format='%Y%m%d%H');
    data['vdt'] = data['idt'] + pd.to_timedelta(data['fhr'],'hours');
    data['vdtstr'] = data['vdt'].dt.strftime('%Y%m%d%H');
    # Process ugliness with wind radii...
    data64 = data.loc[data.rad == 64].copy();
    #data64 = data64.rename(columns={'rad1':'r64_rad1','rad2':'r64_rad2','rad3':'r64_rad3','rad4':'r64_rad4',});
    data50 = data.loc[data.rad == 50].copy();
    #data50 = data50.rename(columns={'rad1':'r50_rad1','rad2':'r50_rad2','rad3':'r50_rad3','rad4':'r50_rad4',});
    #orgdata = data.copy();
    data = data.loc[data.rad == 34].copy();
    for fx,f in data50.iterrows(): 
        data.loc[(data.basin==f.basin)&(data.stno==f.stno)&(data.mdl==f.mdl)&(data.fhr==f.fhr),'r50_rad1'] = f['rad1'];
        data.loc[(data.basin==f.basin)&(data.stno==f.stno)&(data.mdl==f.mdl)&(data.fhr==f.fhr),'r50_rad2'] = f['rad2'];
        data.loc[(data.basin==f.basin)&(data.stno==f.stno)&(data.mdl==f.mdl)&(data.fhr==f.fhr),'r50_rad3'] = f['rad3'];
        data.loc[(data.basin==f.basin)&(data.stno==f.stno)&(data.mdl==f.mdl)&(data.fhr==f.fhr),'r50_rad4'] = f['rad4'];
    for fx,f in data64.iterrows(): 
        data.loc[(data.basin==f.basin)&(data.stno==f.stno)&(data.mdl==f.mdl)&(data.fhr==f.fhr),'r64_rad1'] = f['rad1'];
        data.loc[(data.basin==f.basin)&(data.stno==f.stno)&(data.mdl==f.mdl)&(data.fhr==f.fhr),'r64_rad2'] = f['rad2'];
        data.loc[(data.basin==f.basin)&(data.stno==f.stno)&(data.mdl==f.mdl)&(data.fhr==f.fhr),'r64_rad3'] = f['rad3'];
        data.loc[(data.basin==f.basin)&(data.stno==f.stno)&(data.mdl==f.mdl)&(data.fhr==f.fhr),'r64_rad4'] = f['rad4'];
    warnings.filterwarnings('ignore');
    data['r34'] = np.nanmax(data[['rad1','rad2','rad3','rad4']],axis=1);
    if ( 'r50_rad1' not in data.keys() ):
        data['r50_rad1'] = np.nan;
        data['r50_rad2'] = np.nan;
        data['r50_rad3'] = np.nan;
        data['r50_rad4'] = np.nan;
    if ( 'r64_rad1' not in data.keys() ):
        data['r64_rad1'] = np.nan;
        data['r64_rad2'] = np.nan;
        data['r64_rad3'] = np.nan;
        data['r64_rad4'] = np.nan;
    data['r50'] = np.nanmax(data[['r50_rad1','r50_rad2','r50_rad3','r50_rad4']],axis=1);
    data['r64'] = np.nanmax(data[['r64_rad1','r64_rad2','r64_rad3','r64_rad4']],axis=1);
    warnings.resetwarnings();
    return(data);


def process_atcf_files(cycle_path, timestamp, storm_id, begin_hour = 0, end_hour=126, 
                       members=np.arange(0,21)):
    """
    read in ensemble atcf files, combine, and save. Optionally, add error information (from best track or official forecast)
    args:
        cycle_path: str, path to ensemble forecast cycle of interest.
            example: "/work/noaa/aoml-hafs1/lgramer/staging/data/HERC_2023/2023082618"
        timestamp: str, cycle init time. could probably .split('/')[-1] on previous argument instead...
        storm_id: str, like AL10 for the 10th Atlantic storm
        begin_hour: int, start of atcf window (if only interested in 96-120hr forecast, set to 96)
        end_hour: int, end of atcf window. Should probably be greater than begin_hour, but I am not going to add
            input validation because if you put an end hour that is less than your begin hour, you deserve to
            stack trace your error.
        members=iterable, integer ensemble member IDs

    returns:
        combined dataframe with all atcf files, and optionally benchmark dataframe if benchmark != None
    """
    #get num of members depending on year
    year = timestamp[:4]
    n_mems = len(members)
    
    #turn timestamp into datetime type
    idt = datetime.strptime(timestamp,'%Y%m%d%H')

    #same with begin and end hours
    bdt = idt+timedelta(hours=begin_hour)
    edt = idt+timedelta(hours=end_hour)

    #read ensemble member TC data
    processed_atcf_list = []
    for emem in members:
        atcfall = read_atcf(f'{cycle_path}/{emem:02}/00l.{timestamp}.hfsa.trak.atcfunix.all')
        atcfall = atcfall[(begin_hour <= atcfall.fhr) & (atcfall.fhr <= end_hour)]
        
        #add stid to full df and filter on that
        atcfall['stid'] = list(map(lambda x,y: f'{x}{y:02}', atcfall['basin'], atcfall['stno']))
        atcf = atcfall[atcfall['stid']==storm_id].copy()   
        
        atcf['emem'] = emem
        
        atcf.reset_index(inplace=True)
        #atcf.set_index(['basin','stno','emem','idtstr','fhr'],drop=False,inplace=True);
        
        atcf.set_index(['basin','stno','emem','idtstr','fhr'],inplace=True)
        #put current atcf at the end of the list
        processed_atcf_list.append(atcf)
    
    
    #concat them all at once in the end
    atcfs2 = pd.concat(processed_atcf_list)
    del processed_atcf_list

    atcf_reset = atcfs2.copy().reset_index()

    return atcf_reset

# def gather_structure_data(gpout_path='/work/noaa/aoml-hafs1/lgramer/GPOUT/HERC', cycle='2025081600'):
#     """
#     DEPRECATED! Going to comment out for now.
#     read in polar structure data from GPLOT output directory
#     args:
#         gpout_path: str, path to GPLOT output directory of interest.
#             example: "/work/noaa/aoml-hafs1/lgramer/GPOUT/HERC"
#         cycle: str, forecast cycle
#     returns:
#         combined pandas dataframe with all ensemble members' polar structure data
#     """


    
#     polar_columns_path = "/work2/noaa/aoml-hafs1/donahue/explore_polar_outputs/polarcols.txt"
    
#     with open(polar_columns_path, 'r') as f:
#         column_names = f.readline().split(',')
    
#     #insert ensemble member to start of column list
#     column_names.insert(0, 'emem')

#     #specify gplot cycle, get path and filename pattern (regex)
    
#     gpout_cycle_path = gpout_path + '/' + cycle
#     structure_file_pattern = re.compile(".structure_statistics.{}".format(cycle))

#     #start "outer" list for data - I find this faster than continuously adding to a pandas df
#     outer_list = []
    
#     #loop through ensemble member subdirectories
#     for emem in os.listdir(gpout_cycle_path):
#         #skip cmp folder
#         if emem == "cmp":
#             continue
    
#         #get list of correct structure files
#         structure_files = [gpout_cycle_path+'/{}/polar/'.format(emem) + f for f in os.listdir(gpout_cycle_path+'/{}/polar/'.format(emem)) if structure_file_pattern.search(f)]
    
#         #loop through structure files
#         for file in structure_files:
            
#             #read data (one line, comma separated)
#             with open(file, 'r') as polar_data_file:
#                 polar_data = polar_data_file.readline().split(',')
    
#             #insert ensemble member into front of list - for later compilation
#             polar_data.insert(0,emem)
    
#             #add current list to outer list, will concat at the end
#             outer_list.append(polar_data)
    
#     all_polar_data = pd.DataFrame(outer_list)
#     all_polar_data.columns = column_names

#     return all_polar_data

def tilt_displacement(lat, lon, mag_km, dir_deg):
    """
    calculate new vortex center from tilt magnitude and direction
    POSSIBLY PHASING OUT - if SHIPS centers provide much better graphics, this function will not be necessary
    args:
        lat, lon: position of lower level center
        mag_km: tilt magnitude in kilometers
        dir_deg: tilt direction in degrees
    returns:
        tuple, position of upper center
    """
    #chatgpt assist
    
    #conver lowlevel center + tilt vector to other centers
    #keep nans!
    if pd.isna(mag_km) or pd.isna(dir_deg):
        return (np.nan, np.nan)
    
    # convert degrees to radians
    theta = np.deg2rad(dir_deg)

    #rcostheta
    dlat = (mag_km * np.cos(theta)) / 111.0
    dlon = (mag_km * np.sin(theta)) / (111.0 * np.cos(np.deg2rad(lat)))
    
    return (lat + dlat, lon + dlon)

def combine_polar_and_atcf(polar_data, atcf_data):
    """
    combine processed polar structure data with atcf data
        also perform some calculations to get tilt vectors for later
    args:
        polar_data: dataframe containing polar structure info
        atcf_data: dataframe containing atcf data
    returns:
        dataframe with all polar and atcf data
    """
    #specify columns we need from polar and atcf output - don't need all of them, and this makes it easier to look at
    polar_col_select = ['emem',
                      'fhr',
                      'vmax',
                      'tiltmag_mid_pressure',
                     'tiltdir_mid_pressure',
                      'tiltmag_deep_pressure',
                     'tiltdir_deep_pressure',
                     'shearmag_2km_5km_local',
                     'sheardir_2km_5km_local',
                     'shearmag_2km_8km_local',
                     'sheardir_2km_8km_local',
                     'shearmag_2km_10km_local',
                     'sheardir_2km_10km_local']
    
    atcf_col_select = ['emem','fhr','lat','lon','tdir','tspeed','mslp']

    polar_data = polar_data[polar_col_select].copy().reset_index(drop=True)
    atcf_data = atcf_data[atcf_col_select].copy().reset_index(drop=True)

    #typecast the polar data, it's all strings for some reason
    for col in polar_data.columns:
        if col == 'emem':
            polar_data[col] = polar_data[col].astype(str)
        elif col == 'fhr':
            polar_data[col] = polar_data[col].astype(int)
        else:
            polar_data[col] = polar_data[col].astype(float)

    #merge data on ensemble member, forecast hour
    #outer merge - some of the tilts will be NA and I won't want to skip those, just note at a later point
    all_data = atcf_data.merge(polar_data, on = ['emem','fhr'], how='outer')

    #Get low, mid, and high vortex locations, with chatgpt assisted function
    all_data=all_data.rename(columns={'lon':'lon_850','lat':'lat_850'})

    # 500 mb centers
    all_data[["lat_500", "lon_500"]] = all_data.apply(
        lambda row: tilt_displacement(row.lat_850, row.lon_850,
                                      row.tiltmag_mid_pressure, row.tiltdir_mid_pressure),
        axis=1, result_type="expand"
    )
    
    # 350 mb centers
    all_data[["lat_350", "lon_350"]] = all_data.apply(
        lambda row: tilt_displacement(row.lat_850, row.lon_850,
                                      row.tiltmag_deep_pressure, row.tiltdir_deep_pressure),
        axis=1, result_type="expand"
    )

    #get tilt vectors in u,v sense, by latitude and longitude
    all_data['u1'] = all_data['lon_500']-all_data['lon_850']
    all_data['v1'] = all_data['lat_500']-all_data['lat_850']
    all_data['u2'] = all_data['lon_350']-all_data['lon_500']
    all_data['v2'] = all_data['lat_350']-all_data['lat_500']
    all_data['u3'] = all_data['lon_350']-all_data['lon_850']
    all_data['v3'] = all_data['lat_350']-all_data['lat_850']

    #tilt magnitudes in degrees (useful for defining plot extent)
    all_data['mag1'] = np.sqrt(all_data['u1']**2 + all_data['v1']**2)
    all_data['mag2'] = np.sqrt(all_data['u2']**2 + all_data['v2']**2)
    all_data['mag3'] = np.sqrt(all_data['u3']**2 + all_data['v3']**2)
    all_data['max_mag'] = [np.nanmax([all_data['mag1'][x],all_data['mag2'][x],all_data['mag3'][x]]) for x in range(len(all_data))]

    #some lines have no good tilts at all - set max mag to 0
    all_data['max_mag']=all_data['max_mag'].fillna(0.0)
    

    return all_data

def get_tilt_from_ships_data(gpout_path, cycle, storm_id, levs = [1000,500,350], pass_data = None, fhr = None):
    ######################## DEPRECATED ######################################################################
    """
    Get vortex tilts from SHIPS output
    args:
        #NEED TO ACCOUNT FOR WHETHER IT ALREADY INCLUDES CYCLE
        gpout_path: str, GPLOT output directory + forecast cycle
            example: /work/noaa/aoml-hafs1/lgramer/GPOUT/HERC
        cycle: str, forecast cycle datetime
            example: 2025081600
        levs: list, 3 pressure levels for calculating tilts. Default is 1000, 500, 350
    returns:
        tilt_data: combined dataframe with 3 level tilts
    
    #MD 6/18/2026 - trying to remove SHIPS dependency!!!!!
    """

    if len(levs)!=3:
        print('{} pressure levels passed! 3 pressure levels needed.'.format(len(levs)))
        return False
        
    #empty list for dataframes
    centers_data_list = []
    tilt_cols = ['fhr','lev','lat','lon','vtx']

    #loop through gpout cycle subdirectories!
    yr = cycle[:4]
    memlist = [f'{x:02}' for x in range(21)] if yr == '2025' or yr == '2023' else [f'{x:02}' for x in range(31)]

    # for emem in os.listdir(f'{gpout_path}/{cycle}'):

    #     #skip cmp directory and other problem ones
    #     if emem == "cmp" or emem == '00-20' or emem == 'guidance' or emem == 'ensembleComparison':
    #         continue

    #     storm_tag = storm_id[2:] + 'l'
        
    #     #path to ships data file - CURRENTLY ONLY PROGRAMMED FOR ATLANTIC STORMS
    #     ships_centers_path = f'{gpout_path}/{cycle}/{emem}/ships/{storm_tag}.TCCEN.{cycle}.ships.dat'
    
    #     #read in file, it looks tab-separated, but I think it's actually just multiple whitespaces
    #     try:
    #         ships_centers = pd.read_csv(ships_centers_path, sep='\\s+',header=None,names=tilt_cols)
    #         ships_centers['emem']=emem
        
    #         centers_data_list.append(ships_centers)
    #     except:
    #         print("Something went wrong trying to read in the following file:")
    #         print(ships_centers_path)
    #         print('Skipping for now')
    #         print()

    for emem in memlist:

        #skip cmp directory and other problem ones
        if emem == "cmp" or emem == '00-20' or emem == 'guidance' or emem == 'ensembleComparison':
            continue

        storm_tag = storm_id[2:] + 'l'
    
        #read in file, it looks tab-separated, but I think it's actually just multiple whitespaces
        try:
            ships_centers = pd.read_csv(ships_centers_path, sep='\\s+',header=None,names=tilt_cols)
            ships_centers = pd.DataFrame(pass_data, columns = ['lev'])
            ships_centers['emem']=emem

        
            centers_data_list.append(ships_centers)
        except:
            print("Something went wrong trying to read in the following file:")
            print(ships_centers_path)
            print('Skipping for now')
            print()


    #probably poor practice - re-using this variable name to overwrite previous and combine all data
    ships_centers = pd.concat(centers_data_list)
    del centers_data_list

    #grab levels of interest, rename columns for later merge
    data1000 = ships_centers[ships_centers['lev']==1000].copy().reset_index(drop=True).drop('lev',axis=1).rename(columns={'lat':'lat_1000','lon':'lon_1000','vtx':'vtx_1000'})
    data500 = ships_centers[ships_centers['lev']==500].copy().reset_index(drop=True).drop('lev',axis=1).rename(columns={'lat':'lat_500','lon':'lon_500','vtx':'vtx_500'})
    data350 = ships_centers[ships_centers['lev']==350].copy().reset_index(drop=True).drop('lev',axis=1).rename(columns={'lat':'lat_350','lon':'lon_350','vtx':'vtx_350'})
    
    #merge on forecast hour, maybe ensemble member too?
    tilt_data = data1000.merge(data500, on=['emem','fhr'],how='outer').merge(data350, on=['emem','fhr'],how='outer')

    #get tilt vectors in u,v sense, by latitude and longitude
    tilt_data['u1'] = tilt_data['lon_500']-tilt_data['lon_1000']
    tilt_data['v1'] = tilt_data['lat_500']-tilt_data['lat_1000']
    tilt_data['u2'] = tilt_data['lon_350']-tilt_data['lon_500']
    tilt_data['v2'] = tilt_data['lat_350']-tilt_data['lat_500']
    tilt_data['u3'] = tilt_data['lon_350']-tilt_data['lon_1000']
    tilt_data['v3'] = tilt_data['lat_350']-tilt_data['lat_1000']
    
    #tilt magnitudes in degrees (useful for defining plot extent)
    tilt_data['mag1'] = np.sqrt(tilt_data['u1']**2 + tilt_data['v1']**2)
    tilt_data['mag2'] = np.sqrt(tilt_data['u2']**2 + tilt_data['v2']**2)
    tilt_data['mag3'] = np.sqrt(tilt_data['u3']**2 + tilt_data['v3']**2)
    tilt_data['max_mag'] = [np.nanmax([tilt_data['mag1'][x],tilt_data['mag2'][x],tilt_data['mag3'][x]]) for x in range(len(tilt_data))]
    
    #some lines have no good tilts at all - set max mag to 0
    tilt_data['max_mag']=tilt_data['max_mag'].fillna(0.0)
    return tilt_data

def get_shear_from_ships_data(gpout_path, cycle, storm_id):
    """
    Get shear from SHIPS output
    args:
    NEED TO ACCOUNT FOR WHETHER IT ALREADY INCLUDES CYCLE
        gpout_path: str, GPLOT output directory + forecast cycle
            example: /work/noaa/aoml-hafs1/lgramer/GPOUT/HERC
        cycle: str, forecast cycle datetime
            example: 2025081600
    returns:
        all_shear_data: combined dataframe with deep and shallow shear direction and magnitude
    """
    #empty list for dataframes
    shear_data_list = []

    #STORM TAG - only works for atlantic right now
    storm_tag = storm_id[2:] + 'l'

    #loop through GPOUT subdirectores
    for emem in os.listdir(f'{gpout_path}/{cycle}'):

        #skip cmp directory and other problem ones
        if emem == "cmp" or emem == '00-20' or emem == 'guidance' or emem == 'ensembleComparison':
            continue

        #four different paths - 
        low_shear_mag_path = f'{gpout_path}/{cycle}/{emem}/ships/{storm_tag}.SHRS.{cycle}.ships.dat'
        deep_shear_mag_path = f'{gpout_path}/{cycle}/{emem}/ships/{storm_tag}.SHRD.{cycle}.ships.dat'
        
        low_shear_dir_path = f'{gpout_path}/{cycle}/{emem}/ships/{storm_tag}.SHTS.{cycle}.ships.dat'
        deep_shear_dir_path = f'{gpout_path}/{cycle}/{emem}/ships/{storm_tag}.SHTD.{cycle}.ships.dat'
    
        low_shear_mag = pd.read_csv(low_shear_mag_path, sep='\\s+',header=None,names=['fhr','shear_mag_shallow'])
        deep_shear_mag = pd.read_csv(deep_shear_mag_path, sep='\\s+',header=None,names=['fhr','shear_mag_deep'])
        
        low_shear_dir = pd.read_csv(low_shear_dir_path, sep='\\s+',header=None,names=['fhr','shear_dir_shallow'])
        deep_shear_dir = pd.read_csv(deep_shear_dir_path, sep='\\s+',header=None,names=['fhr','shear_dir_deep'])
    
        all_shear_data = low_shear_mag.merge(deep_shear_mag, on='fhr').merge(low_shear_dir, on='fhr').merge(deep_shear_dir, on='fhr')
    
        all_shear_data['emem'] = emem

        shear_data_list.append(all_shear_data)

    all_shear_data = pd.concat(shear_data_list)
    return all_shear_data