"""
Name: HAFS Ensemble Plotting Script for GPLOT
Author: Nikhil Trivedi and Matt Donahue
Description:
This script reads HAFS ensemble ATCF data, computes member statistics and rankings, and generates a series of 
diagnostic plots for HAFS TC ensemble forecasts.

TO DO:
- Remove Forecast hour loop (NIKHIL TO MATT: Are you sure we want to do this? All the other modules run serial from forecast hour
        to forecast hour as far as I know. I think it may be better to optimize within each forecast hour rather the parallelize across
        all hours, given that the bottleneck is grb2 file reading anyways.)

- Add logic to check "already plotted" forecast hours, skip function call if plot is already there

- Add logic to check if new ensemble members have been produced?

- Switch from using print to using logger.info, and generate a more useful set of print/debug statements. Still not
        exactly sure how this works but I did a brief search and it seems more useful. I assume thats what the other parts
        of GPLOT currently use?

- Stop making all of Nikhil's functions rely on the storm variable (of format AL132025); instead make them build up from the
        GPLOT SID format (13L) as needed

- Potentially add ATCF path as a command line argument (probably should do it but discuss it first)

ANSWERS TO MATTS QUESTIONS:
"do we need borders and coastlines if land==False?", in function plotCartopyFigure():
        Yes, the land=False is just there because shading the land in is unnecessary if we have a background field like 500mb height
        because it is not seen anyways. Cartopy operations tend to be expensive which is why I have that toggle. We still need borders
        and coastlines in that plot though, because those are still visible.

"NIKHIL - are these hard-coded for geopotential heights?" in function plotTrackClustering():
    Yes unfortunately, I still need to make it so this works for a variety of background fields, I just haven't gotten around to it.


Plot types (more description within functions):
1. Ensemble Line Plots:      MSLP vs. forecast hour for all members, colored by rank for a user-chosen metric.
2. Ensemble Tracks Colored:  Storm tracks on a map for all members, colored by rank for a user-chosen metric.
3. Ensemble Wind Radii:      Wind radii (R34/R50/R64) member quartiles plotted for the forecast hour.
4. Ensemble Clustering:      Extreme members for a user-chosen metric grouped into clusters and plotted over 
                             a user-chosen background atmospheric field (averaged over members).
5. Vortex Average Steering:  Extreme members for a user-chosen metric grouped into clusters and averaged into
                             a 2-panel vortex structure plot with shear/motion diagnostics.
6. Ensemble Tilt:            Overlays mid- and deep-layer vortex tilt, along with shear/motion rose

COMMON ARGUMENT GLOSSARY:
(Trying this out, I think it'll make the function docstrings less bloated but let me know if you don't like it)
(Nikhil's functions only for now, will try to unify with Matt's functions)

The arguments below recur across several functions with identical defintions. Thus,
individual function docstrings will name them but not describe them; they will instead
be described here. Only function-specific arguments or common arguments used in an unusual 
way will be described in each function.

    adeckData     : DataFrame, full multi-hour ATCF data for all members + mean
    hourData      : DataFrame, adeckData sliced to a single forecast hours
    members       : list of ints, member IDs to plot; members[-1] is the ensemble mean
    
    clusterType   : str, ranking metric, options are: MSLP, RadMean, ltrack, xtrack
    clusterMembers : int, number of members in each extreme cluster
    allClusterMems : list of 2 lists of ints, member IDs per cluster
    
    radius       : int, wind radius (34/50/64) baked into RadMean
    fHour        : int, forecast hour
    storm        : str, e.g. 'AL132025' (NOTE: might try to get rid of this since it doesn't align with SID)
    initDate     : int, model initialization date, YYYYMMDDHH
    savePath     : str, output directory for the figure
    year, month, day, hour : ints, init-date components (used in titles)

Last modified July 14, 2026
"""

import time
import calendar
import os
import sys
import argparse
import concurrent.futures
import logging

#needs to point to a sorc/GPLOT/python which contains gplot_utils directory
#This is not present in the current branch, but will be present once merged!
# Make gplot_utils / modules importable regardless of CWD (this file lives in
# sorc/GPLOT/python/). Mirrors GPLOT_maps.py:35 -- no hardcoded user paths.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from gplot_utils.namelist import read_master_namelist
from gplot_utils.grib_reader import (get_var_2d, get_grid_info)
from gplot_utils.plot_utils import configure_cartopy
from gplot_utils.coord_transform import (sph2cart,
                                          make_cartesian_grid,
                                          compute_wind_shear)
from gplot_utils.atcf import (walk_files_depth_limited, read_atcf, atcf_from_listfile) 
#atcf from listfile will hopefully be used later once we produce list of atcf files to process

import glob
import re
logger = logging.getLogger('__main__')

import pandas as pd
import numpy as np
import xarray as xr

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from matplotlib.colors import LinearSegmentedColormap
from matplotlib.lines import Line2D
from matplotlib.ticker import LogLocator
from mpl_toolkits.axes_grid1.inset_locator import inset_axes
from matplotlib.patches import Rectangle
from matplotlib.collections import PatchCollection

import cartopy.crs as ccrs
import cartopy.feature as cf
import cartopy.io.shapereader as shpreader

import modules.HepTools as uf


# initialize data ----------------------------------------------------------------------------------------


def modifyAdeckData(members, idir, initDate, storm, clusterMembers, fHours):
    """
    Load ATCF data from all members once (via HepTools.process_atcf_files), 
    build per-radius columns, drop members with incomplete tracks, and append 
    ensemble mean as the final "member".
    
    POTENTIAL CHANGE: Currently changes the DataFrame into expected format for plotting functions,
    but I should probably keep as is and change the plotting function variable names for consistency.

    Common args (members, idir, initDate, storm): see glossary
    Function-specific:
        clusterMembers : int, minimum members required to continue, otherwise the script exits

    dependencies: 
        HepTools.process_atcf_files(), sys, logging, pandas as pd

    returns: tuple (adeckData, members)
        adeckData: DataFrame, all members + ensemble mean, sorted by member/TAU
        members: list of surviving member IDs, with the mean's ID appended last
    """

    #read data or exit if no data is found
    try:
        adeckData = uf.process_atcf_files(cycle_path=f'{idir}/{initDate}', timestamp=str(initDate),
                                          storm_id=f"{storm[:2].upper()}{storm[2:4]}",
                                          members=members)
        if adeckData.empty:
            print(f"ATCF file exists but contains no data for storm {storm}")
            sys.exit(1)
    except Exception as e:
        print(f"Error reading ATCF data at {idir} for storm {storm}, init {initDate}: {e}")
        sys.exit(1)

    # Source columns for every radius, each tuple is (mean, quad1..4) in the 
    # raw ATCF column names produced by uf.process_atcf_files
    rad_col_map = {
        34: ('r34', 'rad1', 'rad2', 'rad3', 'rad4'),
        50: ('r50', 'r50_rad1', 'r50_rad2', 'r50_rad3', 'r50_rad4'),
        64: ('r64', 'r64_rad1', 'r64_rad2', 'r64_rad3', 'r64_rad4'),
    }

    # rename to match names in remainder of script
    adeckData = adeckData.rename(columns={'fhr': 'TAU', 'emem': 'member', 'lon': 'longitude', 'lat': 'latitude', 
                                          'mslp': 'MSLP', 'mdl': 'TECH', 'tdir': 'DIR', 'tspeed': 'SPEED'})

    # convert longitude from -180/180 to 0-360 to match GRIB files
    adeckData['longitude'] = adeckData['longitude'] % 360

    # Build per-radius columns for ALL radii (R34/R50/R64 (mean) and R{n}_RAD1..4 (quadrants))
    # NOTE: No single "RadMean" column exists anymore so a run can rank/cluster/plot by any of them
    for _rad, (_mean, _q1, _q2, _q3, _q4) in rad_col_map.items():
        adeckData[f'R{_rad}']      = adeckData[_mean]
        adeckData[f'R{_rad}_RAD1'] = adeckData[_q1]
        adeckData[f'R{_rad}_RAD2'] = adeckData[_q2]
        adeckData[f'R{_rad}_RAD3'] = adeckData[_q3]
        adeckData[f'R{_rad}_RAD4'] = adeckData[_q4]

        # R50/R64 rows are often missing when R34 exists, so zero-fill those as needed
        if _rad != 34:
            missing = adeckData['r34'].notna() & (adeckData[_mean].isna() | (adeckData[_mean] == 0))
            if missing.any():
                print(f"Zero-filling {missing.sum()} row(s) with missing {_rad}kt radii")
                adeckData.loc[missing, [f'R{_rad}', f'R{_rad}_RAD1', f'R{_rad}_RAD2',
                                        f'R{_rad}_RAD3', f'R{_rad}_RAD4']] = 0

    # update members to only those present in the data
    members = [m for m in members if m in adeckData['member'].unique()]

    # select final columns
    finalCols = ['TECH', 'TAU', 'latitude', 'longitude', 'MSLP', 'DIR', 'SPEED', 'member']
    finalCols += [c for _rad in (34, 50, 64)
                    for c in (f'R{_rad}', f'R{_rad}_RAD1', f'R{_rad}_RAD2', f'R{_rad}_RAD3', f'R{_rad}_RAD4')]
    adeckData = adeckData[finalCols].sort_values(by=["member", "TAU"]).reset_index(drop=True)

    # filter out members with incomplete track data, exit if not enough are present (len(members) < clusterMembers)
    # complete is defined as the member covering every REQUESTED forecast hour
    requestedTaus = set(fHours)
    memberTaus = adeckData.groupby('member')['TAU'].apply(set)
    incompleteMembers = memberTaus[
        memberTaus.apply(lambda taus: not requestedTaus.issubset(taus))
    ].index.tolist()
    
    if incompleteMembers:
        print(f"Skipping {len(incompleteMembers)} member(s) missing one or more "
              f"requested forecast hours {sorted(requestedTaus)}: {incompleteMembers}")
        adeckData = adeckData[~adeckData['member'].isin(incompleteMembers)]
        members = [m for m in members if m not in incompleteMembers]
    else:
        print(f"All members cover the requested forecast hours {sorted(requestedTaus)}.")

    if len(members) < clusterMembers:
        print(f"Only {len(members)} member(s) available after filtering, but clusterMembers={clusterMembers}. Skipping forecast hour.")
        sys.exit(0)

    # calculate ensemble mean
    meanData = adeckData.groupby('TAU').mean(numeric_only=True).reset_index()
    meanData['TECH'] = 'mean'
    meanData['member'] = len(members)
    adeckData = pd.concat([adeckData, meanData], ignore_index=True)
    members = list(members) + [len(members)]

    return adeckData, members


def getHourData(fHour, adeckData):
    """ 
    Slice adeck data to a single forecast hour and attach mean-relative along/across
    track deviation columns.

    Common args (fHour, adeckData): see glossary

    dependencies: 
        HepTools.getTrackSpeedData()

    returns: hourData (see glossary)
    """

    hourData = uf.getTrackSpeedData(adeckData, fHour)
    print(hourData)
    return hourData


def getClusterMems(clusterType, hourData, clusterMembers):
    """ 
    Split members into two extreme clusters by clusterType (clusterMembers lowest-valued
    members and clusterMembers highest-valued members).

    Common args (clusterType, hourData, clusterMembers): see glossary

    returns: allClusterMems (see glossary), [0] is the low cluster and [1] is the high cluster
    """
    allClusterMems = []
    allClusterMems.append(hourData.nsmallest(clusterMembers, clusterType)["member"].tolist())
    allClusterMems.append(hourData.nlargest(clusterMembers, clusterType)["member"].tolist())
    return allClusterMems


# calculate graphic-specific data -----------------------------------------------------------------------


def sortedColoringData(clusterType, hourData, members):
    """ 
    Rank members by clusterType for the rank-colored line/track plots. MSLP is ranked
    in ascending order (rank 1 = lowest), all other metrics are descending. Ensemble mean
    is excluded, along with members with a zero radius (if applicable).

    Common args (clusterType, hourData, members): see glossary

    returns: avgVarTypes, DataFrame with columns ['member', clusterType, 'rank'], 
        where rank is from 1 to N over the ranked members and NaN for unranked members.
    """
    # Exclude the ensemble mean, it is drawn in black, not colored by rank
    realMembers = members[:-1]
    avgVarTypes = hourData[hourData['member'].isin(realMembers)][['member', clusterType]].copy()

    # Only rank members that actually have winds at this radius (value > 0)
    if clusterType in ('R34', 'R50', 'R64'):
        rankable = avgVarTypes[clusterType] > 0
    else:
        rankable = pd.Series(True, index=avgVarTypes.index)

    # Rank the members by clusterType
    ascendingOrder = True if clusterType == 'MSLP' else False
    avgVarTypes['rank'] = np.nan  # unranked members stay NaN
    avgVarTypes.loc[rankable, 'rank'] = (
        avgVarTypes.loc[rankable, clusterType]
        .rank(method='min', ascending=ascendingOrder)
        .astype(int)
    )

    return avgVarTypes


def windRadiiData(hourData, radius):
    """ 
    Select the five members closest to the ensembles quartiles (min/25th/median/75th/max), 
    then build the per-quadrant wind radii arc coordinates for those five members.

    Common args (hourData): see glossary
    Function-specific:
        radius: int (34/50/64), which wind radius to build quartiles/arcs for

    dependencies: 
        numpy as np, pandas as pd

    returns: tuple (quartileData, radData)
        quartileData: DataFrame with 5 rows (min, 25th, median, 75th, max), which is
            each member closest to that percentile, and all ATCF columns
        radData: DataFrame with columns ['lat', 'lon', 'percentile', 'quadrant']; one
            row per point along each quadrant's radius arc, for all 5 members
    """
    meanCol = f'R{radius}'
    percentiles = hourData[meanCol].quantile([0, 0.25, 0.5, 0.75, 1.0]).to_numpy()
    radiiArray = hourData[meanCol].to_numpy()[:, None]
    percentileArray = percentiles
    diffMatrix = np.abs(radiiArray - percentileArray)

    # select the closest member to each percentile value 
    closestRowIdxs = np.argmin(diffMatrix, axis=0)
    quartileData = hourData.iloc[closestRowIdxs]
    quartileData.index = ["min", "25th", "med", "75th", "max"]
    print(quartileData)

    # loop through each member in quartile data
    allLatPoints, allLonPoints, allPercentiles, allQuadrants = [], [], [], []
    for percentile, memberData in quartileData.iterrows():
        centerLat, centerLon = memberData['latitude'], memberData['longitude']
        radii = [f'R{radius}_RAD1', f'R{radius}_RAD2', f'R{radius}_RAD3', f'R{radius}_RAD4']

        # generate radii arc coords for each quadrant
        for idx, startAngle in enumerate([0, 90, 180, 270]):
            currentRad = memberData[radii[idx]]
            angles = np.radians(np.arange(startAngle, startAngle + 91, 3))
            sinVals, cosVals = np.sin(angles), np.cos(angles)

            latPoints = centerLat + currentRad * sinVals / 60
            lonPoints = centerLon + currentRad * cosVals / (60 * np.cos(np.radians(centerLat)))
            
            allLatPoints.extend(latPoints)
            allLonPoints.extend(lonPoints)
            allPercentiles.extend(np.repeat([percentile], len(latPoints)))
            allQuadrants.extend(np.repeat([f'RAD{idx + 1}'], len(latPoints)))
            
    # combine all data into a single DataFrame
    radData = pd.DataFrame({'lat': allLatPoints, 'lon': allLonPoints, 'percentile': allPercentiles, 'quadrant': allQuadrants})  
    return quartileData, radData


def trackClusteringData(clusterType, variable, level, fHour, adeckData, 
                        allClusterMems, idir, initDate, hourData):
    """ 
    For each of the two clusters, reads in the cluster-averaged background GRIB field and
    the cluster's full ATCF tracks. Map bounds are dynamically computed to fit the full ATCF
    tracks. The two clusters are fetched concurrently via ThreadPoolExecutor.

    Common args (clusterType, fHour, adeckData, allClusterMems, idir, 
        initDate, storm, hourData): see glossary
    Function specific:
        variable : str, background GRIB field to plot under tracks (e.g. 'HGT')
        level : int, pressure level (hPa) for the background field
        
    dependencies: 
        pandas as pd, sys, HepTools.getGribData(), concurrent.futures.ThreadPoolExecutor()

    returns: tuple (atcfClusters, gribClusters, clusterAvgs)
        atcfClusters: list of 2 DataFrames, each containing one cluster's ATCF member tracks
        gribClusters: list of 2 xarray Datasets, each containing cluster-averaged background field
        clusterAvgs: list of 2 floats, contains average MSLP for each cluster type
    """
    memberDataList = [adeckData[adeckData["member"].isin(mems)] for mems in allClusterMems]
    combinedMembersDf = pd.concat(memberDataList, ignore_index=True)

    # dynamically calculate bounds for .nc data
    min_lat, max_lat = combinedMembersDf['latitude'].min(), combinedMembersDf['latitude'].max()
    min_lon, max_lon = combinedMembersDf['longitude'].min(), combinedMembersDf['longitude'].max()
    mean_lat = combinedMembersDf['latitude'].mean()
    mean_lon = combinedMembersDf['longitude'].mean()

    if max_lat - min_lat > max_lon - min_lon:
        dist = ((max_lat - min_lat) / 2) + 1
        bounds = [(min_lat-1, max_lat+1), (mean_lon - dist, mean_lon + dist)]
    else:
        dist = (max_lon - min_lon) / 2
        bounds = [(mean_lat - dist, mean_lat + dist), (min_lon-1, max_lon+1)]
    

    def _fetch_cluster(idx, clusterMems):
        """ 
        Fetch and return all data for one cluster; wrapped in function so that it can
        be invoked concurrently using multithreading. This speeds things up since the
        bottleneck is the grb2 reads.

        Reads idir, bounds, initDate, variable, fHour, level, hourData, 
        clusterType, storm, and memberDataList from enclosing function.
        NOTE TO MATT: I think this is good enough reasoning to not include all the args again in this internal function

        args:
            idx: 0 or 1, which cluster we are processing (index into memberDataList)
            clusterMems: list of ints, the member IDs in this particular cluster

        dependencies: 
            HepTools.getGribData(), sys

        returns: tuple (memberDataList[idx], gribData, clusterAvg)
            memberDataList[idx]: DataFrame, this cluster's member ATCF data across all hours
            gribData: xarray Dataset, cluster-averaged background field
            clusterAvg: float, the average value of MSLP for the cluster
        """
        # get GRIB data for the members in the cluster, exit if this does not work
        try:
            gribData = uf.getGribData(f'{idir}', bounds, members=clusterMems, initDate=initDate, 
                                      variable=variable, fHour=fHour, level=level)
            if gribData is None or len(gribData.data_vars) == 0:
                print(f"No GRIB data returned for cluster {idx}, init {initDate}")
                sys.exit(1)

        except FileNotFoundError as e:
            print(f"GRIB file not found at {idir} for init {initDate}: {e}")
            sys.exit(1)
        except Exception as e:
            print(f"Error reading GRIB data  at {idir} for init {initDate}: {e}")
            sys.exit(1)
       
        # get cluster-averaged MSLP or radius at fHour
        clusterHourData = hourData[hourData["member"].isin(clusterMems)]
        clusterAvg = clusterHourData['MSLP'].mean()
            
        return memberDataList[idx], gribData, clusterAvg

    
    # Run the cluster extractions concurrently using threads
    with concurrent.futures.ThreadPoolExecutor(max_workers=len(allClusterMems)) as executor:
        futures = [executor.submit(_fetch_cluster, idx, clusterMems) for idx, clusterMems in enumerate(allClusterMems)]
        results = [f.result() for f in futures]

    # Unpack the concurrent results into the expected lists
    atcfClusters = [r[0] for r in results]
    gribClusters = [r[1] for r in results]
    clusterAvgs  = [r[2] for r in results]

    return atcfClusters, gribClusters, clusterAvgs
    

def vortexAvgSteerData(fHour, idir, initDate, hourData, storm, 
                       adeckData, allClusterMems):
    """ 
    For of the two clusters, load storm-centered u/v data, project to radial/tangential components, 
    dynamically estimate vortex depth and width (NOTE: STILL WORKING ON THIS PART), then compute 
    mass-weighted vortex-averaged steering flow, vertical shear, and actual ATCF storm motion for 
    each cluster.

    Common args (fHour, idir, initDate, hourData, storm, adeckData,
        allClusterMems): see glossary
    Notes: hourData supplies ATCF centers, adeckData supplies speed/direction for storm motion

    dependencies: numpy as np, xarray as xr, HepTools.getGribData()
        concurrent.futures.ThreadPoolExecutor(), sys

    returns: clusterDicts, list of 2 dicts (one per cluster), each with keys: 
        {'radAvgData', 'uSteer', 'vSteer', 'uShear', 'vShear', 'uMotion', 'vMotion', 
         'vortexWidth', 'vortexDepth', 'presLevData'}
    """
    
    def _process_single_vortex(cluster_idx, clusterMems):
        """ 
        Fetch and return all data for one cluster; wrapped in function so that it can
        be invoked concurrently using multithreading. This speeds things up since the
        bottleneck is the grb2 reads.

        Reads idir, initDate, fHour, hourData, storm, and adeckData from enclosing function.
        NOTE TO MATT: I think this is good enough reasoning to not include all the args again in this internal function

        args:
            idx: 0 or 1, which cluster we are processing (index into memberDataList)
            clusterMems: list of ints, the member IDs in this particular cluster

        dependencies: 
            numpy as np, xarray as xr, HepTools.getGribData(), sys

        returns: dict with keys: {'radAvgData', 'uSteer', 'vSteer', 'uShear', 'vShear', 'uMotion', 'vMotion',
                                  'vortexWidth', 'vortexDepth', 'presLevData'}
        """


        # get ATCF center data
        centerData = hourData[hourData["member"].isin(clusterMems)]
        centers = dict(zip(centerData["member"], zip(centerData["latitude"], centerData["longitude"])))

        # load 5x5 degree centered wind data into memory, handle errors
        try:
            windData_xy = uf.getGribData(f'{idir}', centers, variable=['UGRD', 'VGRD'], members=clusterMems, 
                                            initDate=initDate, fHour=fHour)
            if windData_xy is None or len(windData_xy.data_vars) == 0:
                print(f"No GRIB data returned for cluster {cluster_idx}, storm {storm}")
                sys.exit(1)
            
        except FileNotFoundError as e:
            print(f"GRIB file not found at {idir} for storm {storm}, init {initDate}: {e}")
            sys.exit(1)
        except Exception as e:
            print(f"Error reading GRIB data at {idir} for storm {storm}, init {initDate}: {e}")
            sys.exit(1)
        
        windData_xy = windData_xy.rename({"u": "uWind", "v": "vWind", "longitude": "x", "latitude": "y", "isobaricInhPa": "level"})

        # compute r and theta
        x, y = np.meshgrid(windData_xy.x.values, windData_xy.y.values)
        r = xr.DataArray(np.sqrt(x**2 + y**2), dims=("y", "x"), coords={"y": windData_xy.y, "x": windData_xy.x})
        theta = xr.DataArray(np.arctan2(y, x), dims=("y", "x"), coords={"y": windData_xy.y, "x": windData_xy.x})
    
        # project u and v into radial and tangential winds
        rWind = windData_xy.uWind * np.cos(theta) + windData_xy.vWind * np.sin(theta)
        tanWind = -windData_xy.uWind * np.sin(theta) + windData_xy.vWind * np.cos(theta)
    
        # define bins and bin centers
        rMax = float(r.max())
        radialBins = np.arange(0, rMax + 5, 5)
        radialBinCenters = (radialBins[:-1] + radialBins[1:]) / 2

        # Use groupby_bins for efficient radial averaging
        radAvgData = xr.Dataset(
            data_vars=dict(
                radial_wind=(["level", "radius"], rWind.groupby_bins(r, bins=radialBins).mean(dim='stacked_y_x').data),
                tangential_wind=(["level", "radius"], tanWind.groupby_bins(r, bins=radialBins).mean(dim='stacked_y_x').data)
            ),
            coords=dict(
                level=windData_xy.level.values,
                radius=radialBinCenters
            )
        )

        # calculate RMW wind value and radius at each pressure level
        tanWindRmw = radAvgData.tangential_wind.max(dim='radius')
        rmwRadius_vals = radAvgData.radius.values[radAvgData.tangential_wind.argmax(dim='radius').values]
        rmwRadius = xr.DataArray(rmwRadius_vals, dims=['level'], coords={'level': radAvgData.level})
        rmwData = xr.Dataset({'tanWindRmw': tanWindRmw, 'rmwRadius': rmwRadius})

        # get wind data from rmw to outer edge of box at level of rmw
        maxRmwLevelIdx = rmwData.tanWindRmw.sel(level=slice(1000, 800)).argmax(dim='level').item()
        rmw = rmwData.rmwRadius.isel(level=maxRmwLevelIdx)
        rmwWind = rmwData.tanWindRmw.isel(level=maxRmwLevelIdx)
        rmwLevel = radAvgData.tangential_wind.sel(radius=slice(rmw, None)).isel(level=maxRmwLevelIdx)

        # calculate vortex depth using wind decay and tilt with height methods
        decayThresh = rmwWind * 0.75 if rmwWind < 33 else rmwWind * 0.5
        vortexHgtDecay = radAvgData.level.where(rmwData.tanWindRmw < decayThresh, drop=True).max().item()
        vortexHgtTilt = radAvgData.level.where(
            np.abs(rmwData.rmwRadius.diff('level')) > np.abs(rmwData.level.diff('level')), drop=True).max().item()
        
        # calculate vortex depth as the average and find the nearest level
        vortexDepth_computed = (vortexHgtDecay + vortexHgtTilt) / 2
        vortexDepth = windData_xy.level.sel(level=vortexDepth_computed, method='nearest').item()
        
        # calculate vortex width using TS wind radius for hurricanes, 50% of RMW for TS
        isBelowThresh = rmwLevel < (rmwWind * 0.5 if rmwWind < 33 else 18)
        if isBelowThresh.any():
            vortexWidth = rmwLevel.radius.where(isBelowThresh, drop=True).min().item()
        else:
            vortexWidth = rmwLevel.radius.max().item()
            
        print(f"Vortex Depth:({vortexHgtDecay} + {vortexHgtTilt}) / 2 = {vortexDepth}")
        print(f"Vortex Width: {vortexWidth}")

        # slice Cartesian wind data to only include estimated vortex
        windData_xy = windData_xy.sel(level=slice(1000, vortexDepth))
        rData = xr.DataArray(r, dims=("x", "y"), coords={"x": windData_xy.x, "y": windData_xy.y})
        windData_xy = windData_xy.where(rData <= vortexWidth)
    
        # calculate mass-weighted vortex-averaged steering
        weights = xr.DataArray(windData_xy.level.values / 1000, dims=["level"], coords={"level": windData_xy.level})
        presLevData = windData_xy.mean(dim=['x', 'y'])
        steeringData = presLevData.weighted(weights).mean(dim='level')
    
        # calculate shear from vortex bottom to vortex top
        bottomData = windData_xy.sel(level=slice(950, 850)).mean(dim=['x', 'y'])
        bottomData = bottomData.weighted(weights).mean(dim='level')
        topData = windData_xy.sel(level=slice(vortexDepth + 100, vortexDepth)).mean(dim=['x', 'y'])
        topData = topData.weighted(weights).mean(dim='level')
        shearData = (topData - bottomData)

        # extract computed scalar values
        uSteer, vSteer = steeringData.uWind.item(), steeringData.vWind.item()
        uShear, vShear = shearData.uWind.item(), shearData.vWind.item()

        presLevData['uWind'] = presLevData['uWind'] - uSteer 
        presLevData['vWind'] = presLevData['vWind'] - vSteer

        # calculate actual storm motion
        membersData = adeckData[adeckData["member"].isin(clusterMems)]
        stormSpeedDir = membersData[membersData["TAU"] == fHour][['SPEED', 'DIR']].mean()
        theta = np.deg2rad(stormSpeedDir['DIR'])
        uMotion = stormSpeedDir['SPEED'] * np.sin(theta) / 1.94384
        vMotion = stormSpeedDir['SPEED'] * np.cos(theta) / 1.94384

        return {'radAvgData': radAvgData, 'uSteer': uSteer, 'vSteer': vSteer, 'uShear': uShear, 'vShear': vShear,
                             'uMotion': uMotion, 'vMotion': vMotion, 'vortexWidth': vortexWidth, 'vortexDepth': vortexDepth,
                             'presLevData': presLevData}

    # Parallel submission block
    with concurrent.futures.ThreadPoolExecutor(max_workers=len(allClusterMems)) as executor:
        futures = [executor.submit(_process_single_vortex, cluster_idx, clusterMems) for cluster_idx, clusterMems in enumerate(allClusterMems)]
        clusterDicts = [f.result() for f in futures]
    
    return clusterDicts
    

# plotting code ----------------------------------------------------------------------------------------


def plotCartopyFigure(ax, plotLand=True):
    """
    Add map features to cartopy axes. Plot land (optional), political borders, coastlines, 
    US state outlines, and gridlines on a cartopy axes object.

    args:
        ax: cartopy GeoAxes to draw on
        plotLand: bool, whether to fill land (False when a background field is drawn)

    dependencies: 
        cartopy.crs as ccrs, cartopy.feature as cf, cartopy.io.shapereader as shpreader

    returns: axes object with the specified features added.
    """
    if plotLand:
        ax.add_feature(cf.LAND.with_scale('50m'), rasterized=True)
    # ax.add_feature(cf.STATES, linewidth=0.2, edgecolor="gray")
    ax.add_feature(cf.BORDERS, linewidth=0.3)
    ax.coastlines(linewidth=0.5, resolution='50m')

    shpfilename = shpreader.natural_earth(resolution='50m', category='cultural', name='admin_1_states_provinces')
    reader = shpreader.Reader(shpfilename)
    us_states = [geom for geom in reader.records() if geom.attributes['admin'] == 'United States of America']
    ax.add_geometries([s.geometry for s in us_states], ccrs.PlateCarree(), 
                      facecolor='none', edgecolor='gray', linewidth=0.2)

    # plot gridlines
    gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True, linewidth=1, color='gray', alpha=0.5, linestyle='--')
    gl.top_labels = gl.right_labels = False
    gl.xlabel_style = {'size': 7, 'weight': 'bold', 'color': 'gray'}
    gl.ylabel_style = {'size': 7, 'weight': 'bold', 'color': 'gray'}
    
    return ax


def computeTrackBounds(lons, lats):
    """
    Set a lon/lat box for the track map to 3:2 (wider than tall) or 2:3 (taller than wide).

    args:
        lons, lats: array-like longitudes (0-360) and latitudes in the ATCF file
    returns: (lonMin, lonMax, latMin, latMax), orientation (horizontal/vertical)
    """
    # Long/short ratio, fractional padding, and a floor to prevent overzooming
    targetRatio, padFrac, minSpan = 1.5, 0.08, 2.0

    # Get bounding box purely based on ensemble tracks
    lon = ((np.asarray(lons) + 180) % 360) - 180  # 0-360 to -180..180 for a PlateCarree extent
    lat = np.asarray(lats)
    lonMin, lonMax, latMin, latMax = lon.min(), lon.max(), lat.min(), lat.max()

    # Get original ratio (width and height) based on those bounds
    w, h = lonMax - lonMin, latMax - latMin
    padX, padY = max(w * padFrac, minSpan / 2), max(h * padFrac, minSpan / 2)
    lonMin, lonMax, latMin, latMax = lonMin - padX, lonMax + padX, latMin - padY, latMax + padY
    w, h = lonMax - lonMin, latMax - latMin

    if w >= h:  # wider than tall, lock to 3:2 by growing whichever axis is short
        orientation = 'horizontal'
        if w / h > targetRatio:
            cy = (latMin + latMax) / 2; h = w / targetRatio
            latMin, latMax = cy - h / 2, cy + h / 2
        else:
            cx = (lonMin + lonMax) / 2; w = h * targetRatio
            lonMin, lonMax = cx - w / 2, cx + w / 2
    else:  # taller than wide, lock to 2:3
        orientation = 'vertical'
        if h / w > targetRatio:
            cx = (lonMin + lonMax) / 2; w = h / targetRatio
            lonMin, lonMax = cx - w / 2, cx + w / 2
        else:
            cy = (latMin + latMax) / 2; h = w * targetRatio
            latMin, latMax = cy - h / 2, cy + h / 2

    return lonMin, lonMax, latMin, latMax, orientation


def addRankColorbar(ax, sortTitle, clusterType, nColors, isTrack):
    """
    Add the vertical rank colorbar (rank 1 at top), with end labels showing what the extremes
    mean (e.g. Strong/Weak).
    """
    sm = plt.cm.ScalarMappable(cmap=plt.cm.viridis, norm=plt.Normalize(vmin=1, vmax=nColors))
    sm.set_array([])  # avoids a warning
    tickStep = max(1, nColors // 10)  # keep to ~10 ticks regardless of member count

    # put low MSLP at rank 1 (low=stronger), otherwise high value at rank 1
    ascendingOrder = (clusterType == 'MSLP')
    lowLabel, highLabel = clusterTypeDict[clusterType]
    rank1Label = lowLabel if ascendingOrder else highLabel  # rank 1
    rankNLabel = highLabel if ascendingOrder else lowLabel  # rank nColors

    # generate colorbar and label text at ends
    cbar = plt.colorbar(sm, ax=ax, pad=0.015, aspect=27)
    cbar.ax.invert_yaxis()  # rank 1 at top
    cbar.set_ticks(range(1, nColors + 1, tickStep))
    cbar.ax.tick_params(labelsize=8)
    cbar.set_label(f'Member Mean {sortTitle} Rank', fontsize=9, weight='bold')
    cbar.ax.text(0.5, 1.02, rank1Label, transform=cbar.ax.transAxes,
                 ha='center', va='bottom', fontsize=8, weight='bold')
    cbar.ax.text(0.5, -0.02, rankNLabel, transform=cbar.ax.transAxes,
                 ha='center', va='top', fontsize=8, weight='bold')

    # For an equal-aspect track map, ax=ax sizes the bar to the larger allocated box, so snap its
    # height to the map's real drawn rectangle. isTrack is False for line plots (axes fill their box).
    if isTrack:
        fig = ax.get_figure(); fig.canvas.draw()
        mapPos, cbPos = ax.get_position(), cbar.ax.get_position()
        cbar.ax.set_position([cbPos.x0, mapPos.y0, cbPos.width, mapPos.height])
    return cbar


def plotSortedLines(ax, avgVar, plotType, members, adeckData, typeDict, fHour,
                    clusterType, titleLine):
    """
    Draw a rank-colored ensemble figure onto an existing axes: MSLP vs. forecast hour
    (plotType='line') or spatial tracks (plotType='track'), lines colored by clusterType rank. 
    Adds the colorbar, ranking-hour emphasis, title, and legend.

    Common args (members, adeckData, typeDict, clusterType, fHour): see glossary
    Function-specific:
        ax: plain axes for 'line', cartopy GeoAxes for 'track'
        avgVar: DataFrame ['member', clusterType, 'rank']; 'rank' sets the color (NaN = gray)
        titleLine: common second title line (date/init/storm info)

    dependencies: matplotlib.pyplot as plt, numpy as np, pandas as pd, 
        cartopy.crs as ccrs, matplotlib.lines.Line2D

    returns: axes with data, colorbar, title, and legend plotted.
    """
    # Generate ranking colormap
    nColors = int(np.nanmax(avgVar['rank'])) if avgVar['rank'].notna().any() else 1
    colors = plt.cm.viridis(np.linspace(0, 1, nColors))
    grayColor = '0.6'  # gray for members with no winds at this radius
    isTrack = (plotType == "track")

    for member in members:
        memberData = adeckData[adeckData['member'] == member]
        
        # Color lines accordingly (different for mean, zero-radius, regular)
        if member == members[-1]:
            color = 'black'
        else:
            rank = avgVar.loc[avgVar['member'] == member, 'rank'].iloc[0]
            color = grayColor if pd.isna(rank) else colors[int(rank) - 1]
        isMean = (member == members[-1])

        # Specifiy dot and line characteristics (different for mean vs regular members)
        if isTrack:
            style = (25, 2.5, 1) if isMean else (5, 1.2, 0.7)
            xCol, yCol, kw = 'longitude', 'latitude', {'transform': ccrs.PlateCarree()}
        else:
            style = (50, 3, 1) if isMean else (15, 0.8, 0.5)
            xCol, yCol, kw = 'TAU', 'MSLP', {}
        dotSize, lineThickness, opacity = style
        zorder = member * 2 + (2 if isMean else 3)

        # Plot lines and dots
        ax.plot(memberData[xCol], memberData[yCol], color=color, linewidth=lineThickness,
                alpha=opacity, zorder=zorder, **kw)
        ax.scatter(memberData[xCol], memberData[yCol], color=color, s=dotSize,
                   alpha=opacity, zorder=zorder, **kw)

        # Emphasize the ranking hour dots (fHour) with a larger, black-edged marker
        rankPoint = memberData[memberData['TAU'] == fHour]
        if not rankPoint.empty:
            fHourDotSize = dotSize * 3 if isMean else dotSize * 4
            ax.scatter(rankPoint[xCol], rankPoint[yCol], color=color, s=fHourDotSize,
                       edgecolors='black', linewidths=1, zorder=zorder + 100, **kw)

    # Generate title and colorbar
    sortTitle = typeDict[clusterType][0]
    addRankColorbar(ax, sortTitle, clusterType, nColors, isTrack)
    fixedVar = "Track" if isTrack else "MSLP"
    plt.title(f"HAFS Ensemble {fixedVar} Colored by {sortTitle}\n{titleLine}",
              fontsize=9, weight='bold', loc='left')

    # Legend for the lines not colored by rank: the black mean, plus gray "no winds" members
    legendHandles = [Line2D([0], [0], color='black', lw=2, label='Ensemble Mean')]
    if avgVar['rank'].isna().any():
        legendHandles.append(Line2D([0], [0], color=grayColor, lw=1.2,
                                    label=f'No {clusterType[1:]}kt winds'))
    ax.legend(handles=legendHandles, loc='upper right', fontsize=8, framealpha=0.9)
    return ax


def plotLinePlots(avgVarTypes, members, savePath, clusterType, fHour, storm,
                  radius, initDate, titleLine):
    """
    MSLP vs. forecast-hour figure. The drawing is done by plotSortedLines(plotType='line'),
    with a few plot-specific customizations done in here (labels, ticks).
    
    Common args (members, savePath, clusterType, fHour, storm, radius, initDate): see glossary
        avgVarTypes: same as avgVar in plotSortedLines().

    returns: None (Writes a PNG).
    """
    # Build figure and set axis labels
    plt.close('all')
    plt.figure(figsize=(10, 6))
    ax = plt.gca()
    ax = plotSortedLines(ax, avgVarTypes, 'line', members, adeckData, typeDict, fHour,
                         clusterType, titleLine)
    ax.set_xlabel('Time in Hours', fontsize=9, weight='bold')
    ax.set_ylabel('MSLP', fontsize=9, weight='bold')

    # Set x ticks every 12 hours and gridlines
    tauMin, tauMax = adeckData['TAU'].min(), adeckData['TAU'].max()
    ax.set_xticks(np.arange(tauMin, tauMax , 12))
    ax.grid(True, linewidth=1, color='gray', alpha=0.3, linestyle='--')
    ax.tick_params(axis='both', labelsize=9, labelcolor='gray')
    for label in ax.get_xticklabels() + ax.get_yticklabels():
        label.set_fontweight('bold')

    plt.savefig(rf"{savePath}/{storm[2:4]}l.{initDate}.line_plot.{clusterType}.f{fHour:03d}.png",
                dpi=200, bbox_inches='tight')


def plotTracksColored(avgVarTypes, members, savePath, clusterType, fHour, storm,
                      radius, initDate, titleLine):
    """
    Spatial storm tracks plot. Map features via plotCartopyFigure(), drawing via
    plotSortedLines(plotType='track'). The extent is aspect-locked to 3:2 or 2:3 and the
    figure is sized to match.

    Common args (members, savePath, clusterType, fHour, storm, radius, initDate): see glossary
        avgVarTypes: same as avgVar in plotSortedLines().
    
    returns: None (Writes a PNG).
    """
    plt.close('all')

    # Determine and set figure aspect based on ATCF track points
    lonMin, lonMax, latMin, latMax, orientation = computeTrackBounds(
        adeckData['longitude'], adeckData['latitude'])
    figsize = (10, 6.67) if orientation == 'horizontal' else (6.67, 10)

    # Build figure and set extent based on ATCF lines
    plt.figure(figsize=figsize)
    ax = plt.axes(projection=ccrs.PlateCarree(central_longitude=180))
    ax.set_extent([lonMin, lonMax, latMin, latMax], crs=ccrs.PlateCarree())
    ax = plotCartopyFigure(ax)
    ax = plotSortedLines(ax, avgVarTypes, 'track', members, adeckData, typeDict, fHour,
                         clusterType, titleLine)

    plt.savefig(rf"{savePath}/{storm[2:4]}l.{initDate}.spatial_tracks.{clusterType}.f{fHour:03d}.png",
                dpi=200, bbox_inches='tight')


def plotWindRadii(quartileData, radData, savePath, fHour, storm, radius, 
                  initDate, adeckData, titleLine):
    """
    Plot the wind-radii quartile figure. For each of the five percentile members (from
    windRadiiData()), draw its track and its wind-radii arcs (potentially different per-quadrant)
    on a map. Color by percentile, with a legend of member IDs and MSLP.

    Common args (savePath, fHour, storm, radius, initDate, adeckData, year, month,
        day, hour): see glossary
    Function-specific:
        quartileData: DataFrame, ATCF data for the five quartile members (from windRadiiData)
        radData: DataFrame of arc coordinates (from windRadiiData):
            ['lat','lon','percentile','quadrant']

    dependencies: 
        matplotlib.pyplot as plt, numpy as np, plotCartopyFigure()
        cartopy.crs as ccrs, HepTools.getStormName()

    returns: None (writes a PNG).
    """
    plt.close('all')
    colors = plt.cm.viridis(np.linspace(0, 1, len(quartileData)))

    plt.figure(figsize=(10, 6))
    ax = plt.axes(projection=ccrs.PlateCarree(central_longitude=180))

    ax = plotCartopyFigure(ax)
    
    meanLon, meanLat = quartileData['longitude'].mean(), quartileData['latitude'].mean()
    ax.set_extent([meanLon-8, meanLon+8, meanLat-6, meanLat+6])

    ax.scatter(quartileData['longitude'], quartileData['latitude'], color=colors, zorder=100, s=20, transform=ccrs.PlateCarree())

    # add titling
    title = f"HAFS Ensemble {radius}kt Wind Radii Quartiles"
    ax.set_title(f"{title}\n{titleLine}", fontsize=9, weight='bold', loc='left')

    # add legend
    custom_markers = [
        Line2D([0], [0], marker='o', color=color, markersize=10, linestyle='None', markeredgecolor='black') 
        for color in colors
    ]
    formattedMems = [f"Mem {memberData['member']} ({memberIdx}): {memberData['MSLP']} hPa" for memberIdx, memberData in quartileData.iterrows()]
    ax.legend(custom_markers, formattedMems)

    for idx, (percentile, data) in enumerate(quartileData.iterrows()):
        memberData = adeckData[adeckData['member'] == data['member']]
        ax.plot(memberData['longitude'], memberData['latitude'], color=colors[idx], transform=ccrs.PlateCarree(), alpha=0.5, linewidth=1)
        ax.scatter(memberData['longitude'][::2], memberData['latitude'][::2], color=colors[idx], transform=ccrs.PlateCarree(), alpha=0.5, s=7)

        memRadData = radData[radData['percentile'] == data.name]
        for quad, quadrant in enumerate(['RAD1', 'RAD2', 'RAD3', 'RAD4']):
            memQuadData = memRadData[memRadData['quadrant'] == quadrant]
            ax.plot(memQuadData['lon'], memQuadData['lat'], color=colors[idx], transform=ccrs.PlateCarree(), zorder=10)
            
            memQuadNext = memRadData[memRadData['quadrant'] == f'RAD{(quad + 1) % 4 + 1}'].iloc[0]
            ax.plot([memQuadData.iloc[-1]['lon'], memQuadNext['lon']], [memQuadData.iloc[-1]['lat'], memQuadNext['lat']],
                    color=colors[idx], transform=ccrs.PlateCarree(), zorder=10)

    plt.savefig(rf"{savePath}/{storm[2:4]}l.{initDate}.wind_radii.R{radius}.f{fHour:03d}.png", dpi=200, bbox_inches='tight')

    
def plotTrackClustering(atcfClusters, gribClusters, clusterAvgs, savePath, allClusterMems, clusterType, 
                        clusterTypeDict, fHour, storm, variable, radius, titleLine):
    """
    Plot the two-panel ensemble-clustering figure. For each cluster, contour the cluster-averaged
    background field (currently only works for 500 hPa geopotential heights), with each member's
    track overlaid and the fHour position marked. 

    Common args (savePath, allClusterMems, clusterType, clusterTypeDict, fHour,
        storm, variable, radius, year, month, day, hour): see glossary
    Function-specific:
        - atcfClusters: list of 2 DataFrames, with each holding one cluster's ATCF track
        - gribClusters: list of 2 Datasets, cluster-averaged background field
        - clusterAvgs: list of 2 floats, average MSLP for each cluster

    POTENTIAL CHANGE: Contour levels and colormap are currently hard-coded for 500 hPa
    geopotential heights, but we should make this work for other background fields.

    dependencies: 
        cartopy.crs as ccrs, matplotlib.pyplot as plt, numpy as np
        matplotlib.colors.LinearSegmentedColormap, HepTools.getStormName()
        HepTools.getTitleDate()

    returns: None (writes a PNG).
    """
    plt.close('all')
    fig, axes = plt.subplots(2, 2, figsize=(9.5, 5.5), constrained_layout=True, subplot_kw={'projection': ccrs.PlateCarree()}, gridspec_kw={"height_ratios": [0.02, 1]})
    for ax in axes[0]:
        ax.axis('off')
    axes = axes[1]

    for idx, (clusterMems, atcfData, gribData, clusterAvg, ax) in enumerate(zip(allClusterMems, atcfClusters, gribClusters, clusterAvgs, axes)):
        ax = plotCartopyFigure(ax, plotLand=False)

        ax.set_extent([gribData.longitude.min(), gribData.longitude.max(), 
                       gribData.latitude.min(), gribData.latitude.max()], crs=ccrs.PlateCarree())

        def normalize(value):
            return (value - 540) / (600 - 540)
        
        gribData = gribData / 10
        gribData = gribData[list(gribData.data_vars)[0]]
        
        levelsContour = np.arange(540, 600, 2)
        levelsContourf = np.arange(540, 600, 2)
        colorscale_points = [ 
            (normalize(540), "#288DFF"),   # Blue (540)
            (normalize(552), "#029916"),   # Green (552)
            (normalize(570), "#e8d505"),   # Yellow (570)
            (normalize(582), "#e87802"),   # Orange (582)
            (normalize(588), "#e30202"),   # Red (588)
            (normalize(594), "#800000"),   # Deep maroon (594)
            (normalize(600), "#4B004B"),   # Purple-maroon (600)
        ]
        newcmp = LinearSegmentedColormap.from_list("", colorscale_points)


        contours = ax.contour(gribData.longitude, gribData.latitude, gribData, levelsContour, 
                              transform=ccrs.PlateCarree(), colors='black', linewidths=0.5) 
        ax.clabel(contours, levelsContour[::2], inline=True, fontsize=8)
    
        # plot subfigure title
        dataType = f"{clusterType}" if clusterType in ["MSLP", "R34", "R50", "R64"] else "Min MSLP"
        units = "nm" if clusterType in ["R34", "R50", "R64"] else "hPa"
        title = f"{clusterTypeDict[clusterType][idx]} (Cluster Avg {dataType}: {clusterAvg:.1f} {units})"
        ax.set_title(title, fontsize=9, weight='bold', loc='center')

        # plot grb2 data and colorbar
        contourf = ax.contourf(gribData.longitude, gribData.latitude, gribData, levelsContourf, extend='both',
                               transform=ccrs.PlateCarree(), cmap=newcmp)

        # plot ATCF data
        for clusterMem in clusterMems:
            memberData = atcfData[atcfData["member"] == clusterMem]
            ax.plot(memberData['longitude'], memberData['latitude'], transform=ccrs.PlateCarree(), color='black')
            
        hourData = atcfData.loc[atcfData['TAU'] == fHour]
        ax.scatter(hourData['longitude'], hourData['latitude'], transform=ccrs.PlateCarree(), color='blue', zorder=100, s=15)

    cbar = fig.colorbar(contourf, ax=axes, orientation='horizontal', pad=0.04, aspect=50)
    cbar.ax.tick_params(labelsize=8)

    # add titling
    titleDict = {"MSLP": "MSLP", "ltrack": "Along Track Variation", "xtrack": "Across Track Variation", 
                 "R34": "Radius of 34kt Winds", "R50": "Radius of 50kt Winds", "R64": "Radius of 64kt Winds", 
                 "vortexDepth": "Vortex Depth"}

    mainTitle = f"HAFS Ensemble 500mb Heights and Tracks Clustered By {titleDict[clusterType]}"
    fig.suptitle(f"{mainTitle}\n{titleLine}", fontsize=10, weight='bold')

    plt.savefig(rf"{savePath}/{storm[2:4]}l.{initDate}.{variable}.spatial_cluster.{clusterType}.f{fHour:03d}.png", dpi=200, bbox_inches='tight')


def plotVortexAvgSteer(clusterDicts, savePath, storm, initDate, clusterType, fHour, 
                       clusterTypeDict, radius, titleLine):
    """
    Plot the two-panel vortex-average steering figure. For each cluster, plot a radius 
    vs. pressure cross-section of radial/tangential wind. Additionally, outline the estimated
    vortex with a box, and add an inset hodograph with storm-motion, vortex-averaged steering,
    and shear vectors.

    Common args (savePath, storm, initDate, clusterType, fHour, clusterTypeDict,
        radius, year, month, day, hour): see glossary
    Function-specific:
        clusterDicts: list of 2 dicts from vortexAvgSteerData(), each with keys: 
            {'radAvgData', 'uSteer', 'vSteer', 'uShear', 'vShear', 'uMotion', 'vMotion',
             'vortexWidth', 'vortexDepth', 'presLevData'}

    POTENTIAL CHANGE: Improve the dynamic vortex estimation box.

    dependencies: 
        matplotlib.pyplot as plt, numpy as np,
        matplotlib.colors.LinearSegmentedColormap, matplotlib.ticker.LogLocator,
        matplotlib.lines.Line2D, HepTools.getStormName(), HepTools.getTitleDate()
        
    returns: None (writes a PNG).
    """
    plt.close('all')
    fig, axes = plt.subplots(2, 2, figsize=(10, 5.5), constrained_layout=True, gridspec_kw={"height_ratios": [0.02, 1]})
    for ax in axes[0]:
        ax.axis('off')
    axes = axes[1]

    def normalize(value):
        return (value - 0) / (140 - 0)

    newcmp = LinearSegmentedColormap.from_list("", [
        (normalize(0), "#FFFFFF"),
        (normalize(10), "#FFFFFF"),
        (normalize(10), "#65E8F7"),
        (normalize(34), "#0E97A7"),
        (normalize(34), "#14B937"),
        (normalize(50), "#E6EF23"),
        (normalize(64), "#B50204"),
        (normalize(64), "#C103C1"),
        (normalize(100), "#FEEFFE"),
        (normalize(140), "#FA8F83")])

    levels = np.arange(-30, 30, 6)
    levels = levels[levels != 0]

    for idx, (ax, data) in enumerate(zip(axes, clusterDicts)):
        radWindData = data['radAvgData'].radial_wind * 1.94384
        tanWindData = data['radAvgData'].tangential_wind  * 1.94384
        contourf = ax.contourf(tanWindData.radius, tanWindData.level, tanWindData, levels=np.arange(0, 140, 2), cmap=newcmp, 
                               extend='both')
        contour = ax.contour(radWindData.radius, radWindData.level, radWindData, levels, colors='black', linewidths=0.8)
        ax.clabel(contour, inline=True, fontsize=8)

        ax.plot([data['vortexWidth'], data['vortexWidth'], 0], [1000, data['vortexDepth'], data['vortexDepth']], color='black')

        ax.set_title(f"{clusterTypeDict[clusterType][idx]} (Vortex Depth: 1000-{int(data['vortexDepth'])} hPa)", fontsize=9, weight='bold')
    
        ax.set_yscale('log')
        ax.invert_yaxis()
        ax.set_xlim(left=0)
        pressure_levels = [1000, 925, 850, 700, 500, 300, 200, 100, 50]
        ax.set_yticks(pressure_levels, [str(p) for p in pressure_levels])
        ax.tick_params(axis='both', labelsize=9)
        ax.set_ylim(1000, 100)
        ax.yaxis.set_minor_locator(LogLocator(subs=[]))
    
        ax.set_xlabel("Distance from Center (km)", fontsize=9, weight='bold')
        ax.set_ylabel("Pressure Level (hPa)", fontsize=9, weight='bold')
    
        inset_ax = ax.inset_axes([0.70, 0.63, 0.30, 0.37], transform=ax.transAxes)
        inset_ax.set_xticks([])
        inset_ax.set_yticks([])
        inset_ax.set_xlim(0, 1)
        inset_ax.set_ylim(0, 1)
    
        # Convert u, v to inset coordinates using real speed scaling (inset radius / max speed)
        scale_factor = 0.5 / 25
        def scale(data):
            return data * scale_factor * 1.94384      
        uSteer, vSteer, uShear, vShear = scale(data['uSteer']), scale(data['vSteer']), scale(data['uShear']), scale(data['vShear'])
        uMotion, vMotion = scale(data['uMotion']), scale(data['vMotion'])
        presLevData = scale(data['presLevData']) + 0.5
        
        # Draw concentric circles every 10 knots
        for s in [5, 15, 25]:
            r = s * scale_factor
            circle = plt.Circle((0.5, 0.5), r, color='gray', fill=False, linewidth=0.8, alpha=0.5)
            inset_ax.add_patch(circle)
    
            label_x = 0.5 + r * np.cos(5 * np.pi / 4 )
            label_y = 0.5 + r * np.sin(5 * np.pi / 4 )
            inset_ax.text(label_x, label_y, f"{s}", fontsize=7, ha='center', va='center', alpha=0.7)
            
        inset_ax.plot([0, 1], [0.5, 0.5], color='gray', alpha=0.5, linewidth=1)
        inset_ax.plot([0.5, 0.5], [0, 1], color='gray', alpha=0.5, linewidth=1)

        # Draw hodograph
        hodoLevsColors = [(presLevData.sel(level=slice(1000, 850)), 'r'), (presLevData.sel(level=slice(850, 500)), 'g'), 
                          (presLevData.sel(level=slice(500, 100)), 'm')]
        for data, color in hodoLevsColors:
            if len(data.level.values) > 0:
                inset_ax.plot(data['uWind'], data['vWind'], color=color, zorder=2)
        
        # Draw the steering and shear vector arrows
        inset_ax.quiver(0.5, 0.5, uMotion, vMotion, angles='xy', scale_units='xy', scale=1, color='k', width=0.012, 
                        headwidth=3, headlength=4.5, zorder=3)
        inset_ax.quiver(0.5, 0.5, uSteer, vSteer, angles='xy', scale_units='xy', scale=1, color='#00AAFF', width=0.012, 
                        headwidth=3, headlength=4.5, zorder=3, edgecolors='black', linewidths=0.3)
        inset_ax.quiver(0.5, 0.5, uShear, vShear, angles='xy', scale_units='xy', scale=1, color='orange', width=0.012, 
                        headwidth=3, headlength=4.5, zorder=3, edgecolors='black', linewidths=0.3)

    cbar = fig.colorbar(contourf, ax=axes, pad=0.04, aspect=40, orientation='horizontal')
    cbar.ax.tick_params(labelsize=8)

    # add titling
    titleDict = {"MSLP": "MSLP", "ltrack": "Along Track Variation", "xtrack": "Across Track Variation", 
                 "R34": "Radius of 34kt Winds", "R50": "Radius of 50kt Winds", "R64": "Radius of 64kt Winds", 
                 "vortexDepth": "Vortex Depth"}

    mainTitle = f"HAFS Ensemble Rad Avg Wind (kts) Clustered By {titleDict[clusterType]}"
    fig.suptitle(f"{mainTitle}\n{titleLine}", fontsize=10, weight='bold')

    # proxy lines for hodograph segments
    hodograph_red    = Line2D([0], [0], color='r', lw=1.5)
    hodograph_green  = Line2D([0], [0], color='g', lw=1.5)
    hodograph_purple = Line2D([0], [0], color='m', lw=1.5)
    
    # proxy arrows for vectors
    motion_proxy = Line2D([0], [0], color='k', lw=0, marker=r'$\rightarrow$', markersize=10)
    steer_proxy  = Line2D([0], [0], color='#00AAFF', lw=0, marker=r'$\rightarrow$', markersize=10)
    shear_proxy  = Line2D([0], [0], color='orange', lw=0, marker=r'$\rightarrow$', markersize=10)

    linesList = [hodograph_red, motion_proxy, hodograph_green, steer_proxy, shear_proxy]
    labelsList = ["Hodograph (1000–850)", "Storm Motion", "Vortex-Averaged Steering Flow", "Vertical Shear (Top – Bottom of Vortex, 100 hPa Avg)"]
    if clusterDicts[0]['vortexDepth'] > 500 and clusterDicts[1]['vortexDepth'] > 500:
        labelsList.insert(2, "Hodograph (850–Vortex Top)")
    else:
        labelsList.insert(2, "Hodograph (850–500)")
        linesList.insert(4, hodograph_purple)
        labelsList.insert(4, "Hodograph (500–Vortex Top)")
    cbar.ax.legend(linesList, labelsList, loc="lower center", bbox_to_anchor=(0.5, -3), ncol=3, frameon=False, fontsize=8)

    plt.savefig(f"{savePath}/{storm[2:4]}l.{initDate}.wind.vortex_cluster.{clusterType}.f{fHour:03d}.png", dpi=200, bbox_inches='tight')


def call_iqr_calculation(df, metric='shear'):
    """
    Description: helper function to call the find_iqr_circle function and handle NaN values in the DataFrame.

    args: df, metric
        - df: DataFrame, the data to calculate IQR on, 
            currently must contain columns ['shear_dir_deep', 'shear_mag_deep'] or ['tdir', 'tspeed']
        - metric: str, either 'shear' or 'translation', determines which columns to use for IQR calculation

    dependencies: 
        numpy as np
        find_iqr_circle()

    returns: iqr_data: DataFrame, containing the IQR results for the specified metric
    """

    #WILL NEED TO CHANGE THESE IF KEEPING NIKHIL'S COLUMN NAMES
    if metric=='shear':
        metric_dir = 'shear_dir_deep'
        metric_mag = 'shear_mag_deep'
    elif metric == 'translation':
        metric_dir = 'tdir'
        metric_mag = 'tspeed'

    #Count how many nans there are
    nan_dir_count = sum(np.isnan(df[metric_dir]))
    nan_speed_count = sum(np.isnan(df[metric_mag]))

    # Warning for NAN translation speeds
    #if count does not match, there is a big problem. Never seen this happen, and hopefully never will
    if nan_dir_count != nan_speed_count:
        print("WARNING: Nan count mismatch:")
        print(f"Some ensemble member(s) have a {metric} with defined magnitude, undefined direction or vice-versa.")

    elif nan_dir_count != 0:
        print("WARNING: {} Ensemble members have undefined {} from GPLOT output".format(nan_dir_count, metric))
    
    #calculate iqr on circle
    iqr_data = find_iqr_circle(df, metric)
    return iqr_data

def find_iqr_circle(df_fhr, metric='shear'):
    """
    Description: Calculates the interquartile range (IQR) for circular data, 
        specifically for shear or translation metrics. 
        Looks at all vectors to find the largest gap in the circular data, shifts the data so 
        that the largest gap is at the end of the range,
        calculates the quantiles, and then shifts the quantiles back to the original circular space.

    args: df_fhr, metric
        - df_fhr:  DataFrame  for calculation 
            currently must contain columns ['shear_dir_deep', 'shear_mag_deep'] or ['tdir', 'tspeed']
        - metric: str, either 'shear' or 'translation', determines which columns to use for IQR calculation

    dependencies: 
        numpy as np
        pandas as pd

    returns: out_data: DataFrame, containing the IQR results for the specified metric
    """
    #WILL NEED TO CHANGE THESE IF KEEPING NIKHIL'S COLUMN NAMES
    if metric=='shear':
        metric_dir = 'shear_dir_deep'
        metric_mag = 'shear_mag_deep'
    elif metric == 'translation':
        metric_dir = 'tdir'
        metric_mag = 'tspeed'

    # Step 1: sort by angle
    sorted_angles_deep = np.sort(df_fhr[metric_dir].values)
    
    # Step 2: circular successive differences
    largest_gap_deep = np.max(np.diff(np.r_[sorted_angles_deep, sorted_angles_deep[0] + 360]))

    # Step 3: shift into continuous space
    shifted_deep = (sorted_angles_deep + largest_gap_deep) % 360
    
    # Step 4: quantiles
    q1_th_deep_shifted, med_th_deep_shifted, q3_th_deep_shifted = np.quantile(shifted_deep, [.25, .5, .75])
    
    # Step 5: shift  back
    # These are intentionally NOT wrapped to 0-360
    # so values may become negative or >360
    q1_th_deg_deep = q1_th_deep_shifted - largest_gap_deep
    med_th_deg_deep = med_th_deep_shifted - largest_gap_deep
    q3_th_deg_deep = q3_th_deep_shifted - largest_gap_deep
    
    q1_th_deep = np.deg2rad(q1_th_deg_deep)
    med_th_deep = np.deg2rad(med_th_deg_deep)                 # degrees -> radians
    q3_th_deep = np.deg2rad(q3_th_deg_deep)

    #get median magnitude
    v_deep = df_fhr[metric_mag]
    
    #get quantiles for magnitude
    q1_v_deep, med_v_deep, q3_v_deep = np.quantile(v_deep, [.25, .5, .75])

    #create output dataframe
    out_data = pd.DataFrame({'q1_th_deep':q1_th_deep,
                             'q1_v_deep':q1_v_deep,
                             'med_th_deep':med_th_deep,
                             'med_v_deep':med_v_deep,
                             'q3_th_deep':q3_th_deep,
                             'q3_v_deep':q3_v_deep},index=[0])

    return out_data

def add_scalebar(ax, location=(0.1, 0.05), length=100, linewidth=1, units='km', text_offset=0.01):
    """
    Adds a scale bar to a Cartopy map.

    args: ax, location, length, linewidth, units, text_offset
    - ax: The cartopy/matplotlib axes object.
    - location: Tuple (x, y) in axes fraction coordinates for scale bar origin.
    - length: Length of the scale bar in kilometers.
    - linewidth: Thickness of the scale bar.
    - units: Label units (e.g., 'km', 'mi').
    - text_offset: Vertical offset for the label text (in axes fraction).

    dependencies:
        cartopy.crs as ccrs
        numpy as np
    """

    #sometimes passing 0 in to scale - not allowed
    length = max(5,length)

    # Get map extent
    x0, x1, y0, y1 = ax.get_extent(crs=ccrs.PlateCarree())
    width = x1 - x0

    # Convert location (fraction) to lon/lat
    lon = x0 + location[0] * width
    lat = y0 + location[1] * (y1 - y0)

    # Compute degrees for scale length
    km_per_deg = 111.0  # rough average
    deg_length = length / (km_per_deg * np.cos(np.radians(lat)))

    # Draw scale line
    ax.plot([lon, lon + deg_length], [lat, lat],
            transform=ccrs.PlateCarree(), color='black', linewidth=linewidth)

    # Add text label
    ax.text(lon + deg_length / 2, lat + text_offset * (y1 - y0),
            f'{length} {units}', transform=ccrs.PlateCarree(),
            ha='center', va='bottom', fontsize=9)

def add_shear_and_translation_stats(data, ax_main, colormap, location=(0.3, 0.05), size=0.25, legend_fontsize=6, bar_hack=True, exclude_translation=True):
    """
    Add subplot for shear and translation spreads

    Args: data, ax_main, colormap, location, size, legend_fontsize, bar_hack, exclude_translation
        - data: single cycle dataframe with ships data and atcf data, merged on forecast hour and ensemble member
        - ax_main: matplotlib pyplot axis to add this subplot onto
        - location: main plot location for this subplot position
        - size: subplot size
        - legend_fontsize: size of text on subplot legend - will determine legend size, which can very easily be too big
        - bar_hack: There exists an issue with how matplotlib.collections.PatchCollection displays rectangles on polar plots.
            Sometimes, this "bar hack" is needed, and sometimes it is not, though I am yet unable to determine when and why
                (9/1/2025)
        exclude_translation: If True, do not display translation vectors and IQR. For when there are too many individual vectors to display and the graphic will be hard to read
    
    dependencies:
        matplotlib.pyplot as plt
        matplotlib.patches.Rectangle
        matplotlib.collections.PatchCollection
        numpy as np
        call_iqr_calculation()

    returns: ax: matplotlib pyplot axis with shear and translation stats added
    
    """

    """
    9/1/2025 - I am going to add different behavior depending on how many ensemble members are selected.
    If there are only a handful, we can display each vector individually instead of the IQR.
    """

    #get number of ensemble members to determine which behavior to use
    n_members = len(data['emem'].unique())

    mslps = data['mslp'].values
    mslp_max = mslps.max()

    ######################################################################## Calculate IQR for Shear and Translation - also check for missing values #####################
    # --- translation statistics -------------------------------------------------------------

    translation_iqr_data = call_iqr_calculation(data, 'translation')
    q1_th_translation, q1_v_translation, med_th_translation, med_v_translation, q3_th_translation, q3_v_translation = translation_iqr_data.iloc[0]
    
    # --- shear statistics -------------------------------------------------------------
  
    shear_iqr_data = call_iqr_calculation(data, 'shear')
    q1_th_shear, q1_v_shear, med_th_shear, med_v_shear, q3_th_shear, q3_v_shear = shear_iqr_data.iloc[0]

    #################################################################### END Calculate IQR for Shear and Translation - also check for missing values ######################


    
    # --- plot -------------------------------------------------------------------

    # arrows along median direction
    def arrow(r, th,**kw):
        ax.arrow(th, 0, 0, r, length_includes_head=True,
                 head_width=.2, head_length=min(2,r), **kw)

    
    ax = ax_main.inset_axes([location[0], location[1], size, size],
                            projection="polar")

    if n_members >= 7:
        ######################################################## BEHAVIOR FOR LARGE N_MEMBERS ###################################################
        p_translation = PatchCollection([Rectangle((q1_th_translation, q1_v_translation), q3_th_translation-q1_th_translation, q3_v_translation-q1_v_translation,)],color='blue',alpha=0.5)
        
        if not exclude_translation:
            ax.add_collection(p_translation)
            ax.set_rlim(0, 1)
        
        p_shear = PatchCollection([Rectangle((q1_th_shear, q1_v_shear), q3_th_shear-q1_th_shear, q3_v_shear-q1_v_shear,)],color='blue',alpha=0.5)
        ax.add_collection(p_shear)
        ax.set_rlim(0, 1)

        if not exclude_translation:
            arrow(med_v_translation,
                  med_th_translation,
                  color='#009E73',
                  linewidth=2, 
                  label = 'Motion')
        
        arrow(med_v_shear,
              med_th_shear,
              color='#D55E00',
              linewidth=2, 
              label = 'Shear')
    
        #I do not understand this piece - I found it on github discussions of how to make
        #rectangles render properly in polar coordinates. Seems that sometimes it is needed
        #and some times it is not.
        if bar_hack:
            ax.bar(0, 1).remove()
        
        
        ax.set_ylim(0, max(q3_v_shear ,q3_v_translation)*1.1)

    else:
        #MD 6/24/2026 - this capability has not been tested in GPLOT.
        ############################################################ BEHAVIOR FOR SMALL N_MEMBERS ####################################################
        theta_translation = data['tdir'].values
        v_translation = data['tspeed'].values
        theta_shear = data['shear_dir_deep'].values
        v_shear = data['shear_mag_deep'].values

        for indexnum in range(len(theta_translation)):
            #get color that corresponds to current intensity
            current_intensity_value = mslps[indexnum]
            current_intensity_colormap_index = mslp_max - current_intensity_value+1


            current_translation_angle = theta_translation.values[indexnum]
            current_translation_magnitude = v_translation.values[indexnum]

            current_shear_angle = theta_shear.values[indexnum]
            current_shear_magnitude = v_shear.values[indexnum]

            #add current arrows, and label on the first iteration
            if indexnum == 0:
                if not exclude_translation:
                    arrow(current_translation_magnitude,
                      current_translation_angle,
                      color=colormap[len(colormap)-current_intensity_colormap_index],
                      linewidth=1,
                      linestyle = '--',
                      label = 'Motion')

                arrow(current_shear_magnitude,
                      current_shear_angle,
                      color=colormap[len(colormap)-current_intensity_colormap_index],
                      linewidth=1, 
                      label = 'Shear')

            else:
                if not exclude_translation:
                    arrow(current_translation_magnitude,
                      current_translation_angle,
                      color=colormap[len(colormap)-current_intensity_colormap_index],
                      linestyle = '--',
                      linewidth=1)
                arrow(current_shear_magnitude,
                      current_shear_angle,
                      color=colormap[len(colormap)-current_intensity_colormap_index],
                      linewidth=2)

    #I want to have default radial ticks, only changed if necessary
    #Will allow to pass this as an argument in later version
    #need the ticks to go at least as far out as 75th percentile shear and translation
    if exclude_translation:
        min_tick_requirement = q3_v_shear
    else:
        min_tick_requirement = max(q3_v_shear, q3_v_translation)

    if min_tick_requirement <= 15:
        spec_yticks = ([5,10,15])
    else:
        #need to go to the next multiple of 5 after min_tick_requirement
        if min_tick_requirement % 5 == 0:
            upper_tick_lim = min_tick_requirement
        else:
            upper_tick_lim = ((min_tick_requirement+5)//5)*5
        spec_yticks = np.arange(5,int(upper_tick_lim)+5,5)

    ax.set_yticks(spec_yticks)
    ax.set_yticklabels(spec_yticks, fontsize=6)
    ax.set_xticklabels([])
    ax.set_theta_zero_location('N')            # put 0° at top
    ax.set_theta_direction(-1)                 # clockwise positive
    ax.legend(loc='best',fontsize=legend_fontsize)
    return ax

def plot_tilts(atcf_df,atcf_dirs,atcf_tag,idir,dsource,
               gpout_path = '/work/noaa/aoml-hafs1/lgramer/GPOUT/HERC', 
               cycle = '2025081600', 
               fhr=0,
               storm_id = 'AL05',
                members_to_plot = 'all',
               out_path = 'OUTPUT/PATH/NEEDED',
              show=False):
    
    """
    Main function to plot tilts and shears from ships data and atcf data. 
    This function reads in the necessary data, processes it, and generates plots for analysis.
    ALSO - Should I replace print with logger.info???????????

    Args: atcf_df, atcf_dirs, atcf_tag, idir, dsource, gpout_path, cycle, fhr, storm_id, members_to_plot, out_path, show
        - atcf_df: DataFrame containing ATCF data
        - atcf_dirs: list of directories containing ATCF data - from namelist or from command line
        - atcf_tag: string, tag for ATCF data - from namelist or from command line
        - idir: input directory for ATCF data - from namelist or from command line
        - dsource: string, data source for ATCF data - from namelist or from command line. Used for static data lookup
        - gpout_path: path of gplot output - duplicated I believe, but leaving for now
        - cycle: str, forecast cycle, YYYYMMDDHH
        - fhr: int, forecast hour
        - storm_id: str, BBNN, basin and number
        - members_to_plot: either 'all' or a list of specific ensemble members of interest like ['01','05','10']
        - out_path: path to save figures
        - show: bool, create plots in current operating environment, like when running in a notebook
    
    dependencies:
        find_centers_and_shear()
        calculate_tilt_vectors()
        numpy as np
        matplotlib
        matplotlib.cm as cm
        matplotlib.pyplot as plt
        cartopy.crs as ccrs
        add_shear_and_translation_stats()
        add_scalebar()
    
    """
    
    
    #should we add option to specify the pressure levels to use for tilt calculation?
    #For now, I will use 1000, 500, 350?
    #commented ships dependency out! Deprecating
    # tilt_data = uf.get_tilt_from_ships_data(gpout_path, cycle, storm_id)
    centers_data, shear_data = find_centers_and_shear(members_to_plot, fhr, atcf_dirs, atcf_tag, idir, cycle, out_path, dsource,
                 master_namelist_path=MASTER_NML)
    tilt_data = calculate_tilt_vectors(centers_data)

    #keep track of whether bad vortex centers exist anywhere in the ensemble
    any_bad_vortices = False

    #for shear, only need shear_mag_deep, shear_dir_deep
    #commented ships dependency out! Deprecating
    # shear_data = uf.get_shear_from_ships_data(gpout_path, cycle, storm_id)
    
    #combine
    shear_and_tilt = tilt_data.merge(shear_data, on = ['fhr','emem'], how='outer')

    #only use relevant columns for this analysis
    atcf_col_select = ['emem','fhr','lat','lon','tdir','tspeed','mslp']

    #don't do any inplace manipulation of df - just for now, until we land on how to process the atcf data
    atcf_data=atcf_df.copy()

    atcf_data = atcf_data.rename(columns={'TAU':'fhr',
                                         'member':'emem',
                                          'longitude':'lon',
                                          'latitude':'lat',
                                          'SPEED':'tspeed',
                                          'DIR':'tdir',
                                          'MSLP':'mslp'})
    atcf_data = atcf_data[atcf_col_select].copy().reset_index(drop=True)
    #uf.process_atcf_files reads in emem as int, so format as string to match other data
    atcf_data['emem'] = [f'{x:02}' for x in atcf_data['emem']]
    
    #merge data on ensemble member, forecast hour
    #INNER join for now. This will exclude any ensemble members missing from either dataset.
    #Can add test a later time to check that the expected number is here -
    all_data = atcf_data.merge(shear_and_tilt, on = ['emem','fhr'], how='inner')

    
    ##### Two different options - either select a handful of ensemble members to display, or display all
    ##### if displaying all, then text_labels and exclude_translation should be False
    if members_to_plot == 'all':
        select_emem = list(all_data['emem'].unique())
        text_labels=False
        exclude_translation = False
    else:
        select_emem = members_to_plot
        if len(select_emem) >= 7:
            text_labels=False
            exclude_translation = False
        else:
            text_labels=True
            exclude_translation = True

    #catch when a member is specified but does not exist in the data, don't break, but inform user
    exclude_members = []
    for emem in select_emem:
        if emem not in all_data['emem'].unique():
            print(f"WARNING: Ensemble member {emem} was passed as an argument, but is missing from current data. Excluding this member from analysis.")
            exclude_members.append(emem)
    
    for e in exclude_members:
        select_emem.remove(e)
    
    #plot all forecast hours by default. can add option to select a handful - will be obsolete in production version because will call with fhr specified
    #DEPRECATED
    #forecast_hours= list(all_data['fhr'].unique())
    
    intensity_colormap = cm.viridis
    # Define colormap for current emem selection based on intensity (mslp)
    # Define colormap extent as (min mslp minus 1, max mslp + 1), with one discrete color at each integer mslp value between
    mslp_min = int(all_data['mslp'].min())
    mslp_max = int(all_data['mslp'].max())
    colormap_norm = matplotlib.colors.Normalize(vmin=mslp_min-1, vmax=mslp_max+1)
    intensity_colors = intensity_colormap(np.linspace(0,1,mslp_max-mslp_min+3))
    
    
    ##################################### CORE PLOTTING LOGIC STARTS HERE ############################
        
    # Filter DataFrame for the current forecast hour and ensemble members
    df_fhr = all_data[(all_data['emem'].isin(select_emem))&(all_data['fhr'] == fhr)].copy()

    #window big enough to include all tilt vectors
    window_size = df_fhr['max_mag'].max()*2 + 0.25
    
    # Get the starting point (lon_1000 and lat_1000 of the first ensemble member in selection)
    # If all members selected, this should be control member
    start_lon = df_fhr.sort_values(by='emem').iloc[0]['lon_1000']
    start_lat = df_fhr.sort_values(by='emem').iloc[0]['lat_1000']
    

    # Set up map
    fig, ax = plt.subplots(figsize=(8, 6), subplot_kw={'projection': ccrs.PlateCarree()})
    
    #Use first member's low level center as plot center
    centerlon_ref = start_lon
    centerlat_ref = start_lat
    
    ax.set_extent([centerlon_ref - window_size/2, centerlon_ref + window_size/2,
                   centerlat_ref - window_size/2, centerlat_ref + window_size/2], crs=ccrs.PlateCarree())
    # ax.coastlines(resolution='10m')

    #loop through ensemble members to create each tilt vector
    excluded_emems = []
    for emem in select_emem:

        current_data = df_fhr[df_fhr['emem']==f'{emem:02}'].copy()
        
        #get color that corresponds to current intensity
        # ON 2/5/2026 - I ran into an error when some members dissipated. tracking this
        if len(current_data) == 0:
            excluded_emems.append(emem)
            continue
        current_intensity_value = int(current_data['mslp'].values[0])
        current_intensity_colormap_index = mslp_max - current_intensity_value+1
        
        #for storm-relative
        centerlon = centerlon_ref
        centerlat = centerlat_ref
        
        # #for earth-relative - maybe I should get rid of this capability, it has never been used or requested
        # centerlon = current_data['lon_1000'].values[0]
        # centerlat = current_data['lat_1000'].values[0]

        #plot star at low level center (storm-centered - will be same position for all storms)
        ax.plot(centerlon, centerlat, '*', transform=ccrs.PlateCarree(), color=intensity_colors[len(intensity_colors)-current_intensity_colormap_index], ms=5,alpha=0.5) # SKIPPING label for now, i am having trouble with it label='SFC_{}'.format(model),ms=2)
    
        
        #flag for 'bad' vortex center
        bad_value_mid = False
        bad_value_high = False

        if current_data['vtx_500'].values[0]==0:
            bad_value_mid = True
        if current_data['vtx_350'].values[0]==0:
            bad_value_high = True

        if bad_value_high or bad_value_mid:
            any_bad_vortices = True
        
        # # Compute end points of tilt vectors
        endlon = centerlon + current_data['u1'].values[0]
        endlat = centerlat + current_data['v1'].values[0]
    
        endlon2 = centerlon + current_data['u3'].values[0]
        endlat2 = centerlat + current_data['v3'].values[0]

        # Plot the low-level tilt vector        
        ax.plot([centerlon, endlon], [centerlat, endlat],
                color=intensity_colors[len(intensity_colors)-current_intensity_colormap_index], linestyle='--',linewidth=1, transform=ccrs.PlateCarree(),label=None)

        # Add mid level vortex center marker - X if no vortex, O if vortex
        mid_marker = 'x' if bad_value_mid else 'o'
        ax.plot(endlon,
                endlat,
                marker=mid_marker,
                color='k' if bad_value_mid else intensity_colors[len(intensity_colors)-current_intensity_colormap_index],
                transform=ccrs.PlateCarree(),
                ms=5,
                alpha=1 if bad_value_mid else 0.5)

        #add high level tilt vector
        ax.plot([endlon, endlon2], [endlat, endlat2],
            color=intensity_colors[len(intensity_colors)-current_intensity_colormap_index], linestyle='--',linewidth=1, transform=ccrs.PlateCarree(),label=None)

        #add high level vortex center marker - x is no vortex, star if vortex
        #also, color black if no vortex to make it stand out
        high_marker = 'x' if bad_value_high else 's'
        ax.plot(endlon2,
                endlat2,
                marker=high_marker,
                color='k' if bad_value_high else intensity_colors[len(intensity_colors)-current_intensity_colormap_index],
                transform=ccrs.PlateCarree(),
                ms=5,
                alpha=1 if bad_value_high else 0.5)

        if text_labels:
            #label ensemble member at outermost point
            ax.annotate(emem, xy = [endlon2,endlat2])

    # Create custom legend handles
    low_level_handle = plt.Line2D([], [], color='black', marker='*', linestyle='None', markersize=5, label='Low-level vortex')
    mid_level_handle = plt.Line2D([], [], color='black', marker='o', linestyle='None', markersize=5, label='Mid-level vortex')
    upper_level_handle = plt.Line2D([], [], color='black', marker='s', linestyle='None', markersize=5, label='Upper-level vortex')
    bad_vortex_handle = plt.Line2D([], [], color='black', marker='x', linestyle='None', markersize=5, label='Bad Vortex (1km per hPa)')

    
    # # Add dashed lat/lon gridlines - only when earth centered! again - maybe get rid of this
    # gl = ax.gridlines(draw_labels=True, linestyle='--', linewidth=0.8, color='gray', alpha=0)
    # gl.top_labels = False
    # gl.right_labels = False

    #window size is in degrees, make scale a multiple of 25km
    scale_length = np.round((np.floor(window_size/2 * 100)/2) /25 ,0) * 25

    #attempt to add shear/translation subplot - WARNING! Shear IQR broken for shear directions near southerly!
    add_shear_and_translation_stats(df_fhr, ax, colormap = intensity_colors, location=(0.1, 0.68), size=0.25, legend_fontsize=6,exclude_translation=exclude_translation)
    
    add_scalebar(ax, location=(0.1, 0.05), length=scale_length)

    #only display X legend if there are bad vortices:
    if any_bad_vortices:
        ax.legend(handles=[low_level_handle,mid_level_handle, upper_level_handle, bad_vortex_handle], loc='upper right')
    else:
        ax.legend(handles=[low_level_handle,mid_level_handle, upper_level_handle], loc='upper right')  

    fig.colorbar(matplotlib.cm.ScalarMappable(norm=colormap_norm, cmap=intensity_colormap),
             ax=ax, orientation='vertical', label='MSLP')

    
    stid = storm_id.replace('AL','')+'l'
    plt.title('{} {} Tilt at f{}'.format(storm_id, cycle,f"{fhr:03}"))
    if members_to_plot == 'all':
        plt.savefig('{}/{}.{}.tilt_plot.all_members.f{}.png'.format(out_path, stid, cycle,f"{fhr:03}"))
    else:
        plt.savefig('{}/{}.{}.tilt_plot.selected_members.f{}.png'.format(out_path, stid, cycle,f"{fhr:03}"))
    if show:
        plt.show()
        time.sleep(1)
    plt.close('all')

##### Only got to here on 6/24/2026 Docstring edit ###########################################################

#this functionality is used in SHIPS and similar ones are used throughout GPLOT - we should move this to an external script.
def find_grib_files(idir, itag, ext, idate, fhr_fmt, init_hr, fnl_hr, dt, ens_id,
                    prefer_nest=True):
    """
    Build list of (fhr, filepath) for available GRIB2 files.

    SHIPS diagnostics are storm-centric (shear annulus averages,
    vortex-centered fields, IKE integrals, etc.), so when multiple
    per-fhr GRIB2 files exist -- e.g. HAFS multistorm runs with both
    `.parent.atm.*.grb2` and `.storm2.atm.*.grb2` side by side -- we
    select the higher-resolution storm nest rather than the parent.
    The ``prefer_nest`` flag controls this; set False to take whatever
    ``sorted(glob)`` returns (legacy behavior).

    The ``.sat.`` files (simulated IR / microwave) are excluded here --
    they are picked up separately by ``open_sat_file()`` when needed.
    """
    files = []
    fhr = init_hr
    while fhr <= fnl_hr:
        fhr_str = fhr_fmt % fhr
        # Try common patterns
        patterns = [
            os.path.join(idir, f"{itag}*{fhr_str}*{ext}"),
            os.path.join(idir, f"*f{fhr_str}*{ext}"),
            os.path.join(idir, f"*{fhr_str}*{ext}"),
            os.path.join(idir+f'/{idate}/{ens_id}', f"*{fhr_str}*{ext}")  #this was added to find ens members
        ]
        found = False
        for pat in patterns:
            matches = sorted(glob.glob(pat))
            # Drop the companion ``*.sat.*`` satellite-IR bundle; it
            # has no atmospheric fields we need here, and mixing the
            # two would grab the wrong one alphabetically.
            matches = [m for m in matches
                       if '.sat.' not in os.path.basename(m)]
            if not matches:
                continue

            if prefer_nest:
                nest_matches = [m for m in matches
                                if _NEST_TOKEN_RE.search(
                                    os.path.basename(m))]
                if nest_matches:
                    files.append((fhr, nest_matches[0]))
                    found = True
                    break
                # No storm nest at this fhr - fall back to whatever
                # non-parent candidate exists.  This keeps runs where
                # only a single parent file is produced (non-multistorm
                # HAFS, legacy HWRF) working.
                non_parent = [m for m in matches
                              if not _PARENT_TOKEN_RE.search(
                                  os.path.basename(m))]
                chosen = non_parent[0] if non_parent else matches[0]
            else:
                chosen = matches[0]

            files.append((fhr, chosen))
            found = True
            break
        if not found:
            logger.debug(f"No GRIB2 file for fhr={fhr}")
        fhr += dt
    return files

#this functionality is used in SHIPS and similar ones are used throughout GPLOT - we should move this to an external script.
def find_atcf_file(atcf_dir, atcf_tag, idate, sid, ens_id):
    """Search for ATCF file matching storm and cycle.

    ``atcf_dir`` may be a single path or a list/tuple of paths to try
    in order -- typically [ATCF2_DIR, ATCF1_DIR] so the experiment's
    real track directory wins over a placeholder.  Files whose basename
    contains ``atcf_tag`` are preferred over siblings, and ``.parent.``
    variants are ranked below nest-merged ones.  Per-fhr splits
    (``.f000``, ``.f003``, ...) and ``.all`` / ``.orig`` are excluded.
    """
    if isinstance(atcf_dir, (list, tuple)):
        dirs = [d for d in atcf_dir if d]
    else:
        dirs = [atcf_dir] if atcf_dir else []
    if not dirs:
        return None

    basin = sid[-1].lower() if sid else ''
    storm_num = sid[:-1] if sid else ''

    pattern_tmpls = [
        f"{atcf_tag}*{sid.lower()}*{idate}*atcf*",
        f"*{sid.lower()}*{idate}*trak*atcf*",
        f"*{sid.lower()}*{idate}*atcf*",
        f"*{storm_num}{basin}*{idate}*atcf*",
        f"{idate}/{ens_id}/{sid.lower()}*{idate}*atcf*" #this was added to find ens member
        
    ]
    broader_tmpls = [
        f"*{sid.lower()}*{idate}*",
        f"*{storm_num}{basin}*{idate[:4]}*",
    ]

    def _rank(path):
        bn = os.path.basename(path)
        tag_match = 0 if (atcf_tag and atcf_tag in bn) else 1
        parent_penalty = 1 if '.parent.' in bn else 0
        return (tag_match, parent_penalty, bn)

    _FHR_RE = re.compile(r'\.f\d{3,4}$')

    def _filter(matches):
        return [m for m in matches
                if not m.endswith(('.grb2', '.grb', '.idx',
                                   '.grib2', '.orig', '.nc'))
                #and not os.path.basename(m).endswith('.all') this line prevents it from finding the ensembel atcf
                and not _FHR_RE.search(os.path.basename(m))]

    for adir in dirs:
        if not os.path.isdir(adir):
            continue
        for tmpl in pattern_tmpls:
            matches = _filter(glob.glob(os.path.join(adir, tmpl)))
            if matches:
                matches.sort(key=_rank)
                return matches[0]
        for tmpl in broader_tmpls:
            matches = _filter(glob.glob(os.path.join(adir, tmpl)))
            if matches:
                matches.sort(key=_rank)
                return matches[0]

    # Bounded recursive fallback: the flat globs above are non-recursive, so a
    # track nested under e.g. com/<cycle>/<storm>/ is missed when ATCF*_DIR
    # points higher. Walk up to 4 levels below each dir (depth-capped so a big
    # ATCF*_DIR can't trigger an unbounded walk) and match basename on
    # sid + idate + 'atcf'.
    sid_lc = sid.lower()
    walked = [full for full, bn in walk_files_depth_limited(dirs, max_depth=4)
              if sid_lc in bn.lower() and idate in bn
              and 'atcf' in bn.lower()]
    walked = _filter(walked)
    if walked:
        walked.sort(key=_rank)
        return walked[0]

    return None

#this comes from SHIPS and could go in a separate module
def compute_tccen(datasets, dsource, tc_lat, tc_lon, levels=None):
    """
    Compute TC centers at multiple pressure levels via the smoothed
    HGT centroid, then apply NCL's vortex-continuity check: a level's
    center is "in the vortex" iff it lies within 1 km per 1 hPa of the
    level immediately below it (or, for k>=2, also valid against k-2).
    Continuity propagates: a level fails if any of its anchor levels
    below failed.

    Returns dict: level -> (lat, lon, hgt_value, use_flag).
    """
    if levels is None:
        # 25 hPa increments from 200 to 1000, matching the NCL
        # coordinate-subscript LEV(:{200:1000}). Models that don't
        # provide every 25-hPa level will have those entries return
        # None from get_var_2d and be filtered out downstream.
        levels = list(range(200, 1001, 25))

    grid = get_grid_info(datasets, dsource)
    lat = grid['lat']
    lon = grid['lon']

    # Process surface->top so the continuity chain anchors at the
    # near-surface vortex (matches NCL: levels are flipped if
    # max(LEV) != LEV(0) before the use-flag pass).
    levs_asc = sorted(levels, reverse=True)

    raw = {}
    for lev in levs_asc:
        clat, clon, hgt, found = find_center_at_level(
            datasets, dsource, lev, tc_lat, tc_lon, lat, lon)
        raw[lev] = (clat, clon, hgt, found)
        #this really spams the log
        # if found:
        #     logger.debug(f"  TCCEN L={lev}: ({clat:.2f}, {clon:.2f})")

    use = {}
    for k, lev in enumerate(levs_asc):
        clat, clon, hgt, found = raw[lev]
        if not found or not np.isfinite(clat):
            use[lev] = False
            continue
        if k == 0:
            use[lev] = True
            continue
        prev_lev = levs_asc[k - 1]
        pclat, pclon, _phgt, pfound = raw[prev_lev]
        if not pfound or not use.get(prev_lev, False):
            use[lev] = False
            continue
        d1 = _haversine_km(clat, clon, pclat, pclon)
        lim1 = d1 / max(abs(lev - prev_lev), 1e-6)  # km per hPa
        ok = lim1 <= 1.0
        if not ok and k >= 2:
            prev2_lev = levs_asc[k - 2]
            p2clat, p2clon, _p2hgt, p2found = raw[prev2_lev]
            if p2found and use.get(prev2_lev, False):
                d2 = _haversine_km(clat, clon, p2clat, p2clon)
                lim2 = d2 / max(abs(lev - prev2_lev), 1e-6)
                ok = lim2 <= 1.0
        use[lev] = bool(ok)

    centers = {}
    for lev in levels:
        clat, clon, hgt, _ = raw[lev]
        centers[lev] = (clat, clon, hgt, 1 if use.get(lev, False) else 0)
    return centers

#this comes from SHIPS and could go in separate module
def find_center_at_level(datasets, dsource, level, tc_lat, tc_lon, lat, lon):
    """
    Find the TC center at a given pressure level via the geopotential-
    height centroid algorithm ported from NCL findCenter type=1:
    1) crop to ~5 degrees around the ATCF position;
    2) smooth with a 1-2-1 filter, 25 iterations;
    3) value-weighted centroid of the lower 20% of the smoothed field.

    Returns (center_lat, center_lon, hgt_value, found_flag).
    hgt_value is min(smoothed) at the center for downstream "lowest
    level" marker selection. found_flag is 1 if a center could be
    computed (vortex-continuity check happens later in compute_tccen).
    """
    result = get_var_2d(datasets, dsource, 'HGT', str(level))
    if result is None:
        return np.nan, np.nan, np.nan, 0

    data = result['data']
    rlat = result['lat']
    rlon = result['lon']

    tc_lon_data = _match_lon_convention(tc_lon, rlon)
    lat_mask = (rlat >= tc_lat - 5) & (rlat <= tc_lat + 5)
    lon_mask = (rlon >= tc_lon_data - 5) & (rlon <= tc_lon_data + 5)

    if not np.any(lat_mask) or not np.any(lon_mask):
        return np.nan, np.nan, np.nan, 0

    sub = data[np.ix_(lat_mask, lon_mask)]
    sub_lat = rlat[lat_mask]
    sub_lon = rlon[lon_mask]

    if sub.size == 0 or np.all(np.isnan(sub)):
        return np.nan, np.nan, np.nan, 0

    smoothed = _filter121_2d(sub, n_iter=25)
    i_c, j_c = _centroid_min(smoothed)
    if not (np.isfinite(i_c) and np.isfinite(j_c)):
        return np.nan, np.nan, np.nan, 0

    clat = float(np.interp(i_c, np.arange(len(sub_lat)), sub_lat))
    clon = float(np.interp(j_c, np.arange(len(sub_lon)), sub_lon))
    if clon > 180:
        clon -= 360

    hgt_val = float(np.nanmin(smoothed))
    return clat, clon, hgt_val, 1

#this comes from ships and really should be in a separate module
def _filter121_2d(arr, n_iter):
    """
    Apply a 1-2-1 smoothing filter (NCL FILTER121 equivalent) to a 2D
    field. Each iteration smooths along axis 0, then axis 1, on the
    interior; edges are left unchanged. NaNs are filled with the field
    mean before filtering so they don't propagate.
    """
    out = np.asarray(arr, dtype=float).copy()
    if not np.all(np.isfinite(out)):
        fill = np.nanmean(out)
        out = np.where(np.isfinite(out), out, fill)
    for _ in range(int(n_iter)):
        tmp = out.copy()
        tmp[1:-1, :] = 0.25 * (out[:-2, :] + 2.0 * out[1:-1, :] + out[2:, :])
        out[:, 1:-1] = 0.25 * (tmp[:, :-2] + 2.0 * tmp[:, 1:-1] + tmp[:, 2:])
        out[0, :] = tmp[0, :]
        out[-1, :] = tmp[-1, :]
    return out

#this comes from ships and should be in a separate module
def _match_lon_convention(lon_val, lon_array):
    """Ensure a single lon value matches the convention of the lon array."""
    if lon_array.min() >= 0 and lon_array.max() > 180:
        # Data in 0..360
        if lon_val < 0:
            return lon_val + 360
    elif lon_array.max() <= 180:
        # Data in -180..180
        if lon_val > 180:
            return lon_val - 360
    return lon_val

#this comes from ships and should be in a separate module
def _centroid_min(field):
    """
    Value-weighted centroid of the lower-tail of a 2D field. Replicates
    NCL findCenter type=1 with b=-1 (HGT min mode):
    threshold A = min + 0.20 * (max - min); centroid is the
    (A - field)-weighted mean of grid indices over points where
    field <= A (so the deepest part of the trough dominates).

    Returns (i_centroid, j_centroid) as float indices into field, or
    (nan, nan) if the field has no usable values.
    """
    if not np.isfinite(field).any():
        return np.nan, np.nan
    fmin = float(np.nanmin(field))
    fmax = float(np.nanmax(field))
    if fmax == fmin:
        return np.nan, np.nan
    A = fmin + 0.20 * (fmax - fmin)
    weights = np.maximum(A - field, 0.0)
    weights = np.nan_to_num(weights, nan=0.0)
    total = float(weights.sum())
    if total <= 0 or not np.isfinite(total):
        return np.nan, np.nan
    i_idx, j_idx = np.indices(field.shape)
    i_c = float((i_idx * weights).sum() / total)
    j_c = float((j_idx * weights).sum() / total)
    return i_c, j_c

#comes from SHIPS and should be in separate module
def _haversine_km(lat1, lon1, lat2, lon2):
    """Great-circle distance in km between two points (degrees)."""
    rlat1, rlat2 = np.radians(lat1), np.radians(lat2)
    dlat = np.radians(lat2 - lat1)
    dlon = np.radians(lon2 - lon1)
    a = np.sin(dlat / 2) ** 2 + np.cos(rlat1) * np.cos(rlat2) * np.sin(dlon / 2) ** 2
    return 2.0 * 6371.0 * np.arcsin(np.sqrt(a))

def find_centers_and_shear(members, fhr, atcf_dirs, atcf_tag, idir, idate, odir, dsource,
                 master_namelist_path=None):
    # Fall back to the module-level namelist path resolved from --master-nml.
    if master_namelist_path is None:
        master_namelist_path = MASTER_NML

    #SUGGESTIONS - PASS IN tc_lat, tc_lon!!!! rather than reading the atcf's here
    
    outer_dataframe_centers = pd.DataFrame()
    outer_dataframe_shear = pd.DataFrame()

    #this is duplicated from main code
    ##nml = read_master_namelist(args.master_nml)
    nml = read_master_namelist(master_namelist_path)
    gplot_dir = nml.get('GPLOT_DIR', os.environ.get('GPLOT_DIR', ''))
    #end duplicated code

    ships_nml_name = nml.get('SHIPS_NML', f'namelist.ships.default')
    ships_nml_path = os.path.join(gplot_dir, 'parm', ships_nml_name)

    yr = idate[:4]
    if members == 'all':
        memlist = [f'{x:02}' for x in range(21)] if yr == '2025' or yr == '2023' else [f'{x:02}' for x in range(31)]
    else:
        memlist = members


    for ensid in memlist:
        logger.info(f'starting loop with ensid={ensid}') #DEBUG

        ensemble_member_dataframe = pd.DataFrame()
        tccen_store = {}

        grib_files = None
        #this part is just included in case we want other grb discovery later
        if grib_files is not None:
            logger.info(f"  Using spawn-prepared file list: "
                        f"{len(grib_files)} FHRs")
        else:
            logger.info("  No spawn file list found; falling back to "
                         "find_grib_files() discovery")
            grib_files = find_grib_files(idir, itag, ext, idate, fhrfmt,
                                        fhr, fhr, dt, ensid) #called with init_hr and fnl_hr = fhr because only want one grib here
        if not grib_files:
            logger.error(f"No GRIB2 files found in {idir}")
            #_write_status(status_file, 'failed') #commenting out status writer because that does not exist in our current module MD 20260622
            return 1

        #in other modules, we iterate through grib_files because there are multiple forecast hours, but here, there should only be one!
        #but also note, by default, it returns [(fhr, filepath)]
        #ran into problem here on test.
        assert len(grib_files) == 1

        fhr_copy, grib_path = grib_files[0]

        #optimize this with Nikhil's grib reader!
        try:
            logging.info('In the GRIB read section')
            datasets=[]
            ds = xr.open_dataset(
                grib_path,
                engine="cfgrib",
                filter_by_keys={"typeOfLevel": "isobaricInhPa",
                                'level':[1000,500,350],
                               'shortName':['gh','u','v']},
                backend_kwargs={"indexpath":""},
            )
            datasets.append(ds)
        except Exception as e:
            logger.error(f"Failed to open {grib_path}: {e}")

        # ---- Find ATCF file ----
        ################ Could try to replace this with our heptools atcf reading, but also don't need to because this really does
        #function one-at-a-time. Could parallelize later too.
        atcf_file = find_atcf_file(atcf_dirs, atcf_tag, idate, '', ensid) #sid='' required for ensemble
        if atcf_file is None:
            # find_atcf_file globs ATCF*_DIR non-recursively; if those namelist
            # dirs sit above the actual file (e.g. ATCF under com/<cycle>/<storm>/)
            # it misses it. Fall back to the spawn's recursively-resolved path in
            # ATCF_FILES.dat -- the same source polar/airsea use.

            #MD 20260622 - atcf_from_listfile() not currently supported in plot_ens_compare because we are not producing an atcf files list
            # fallback = atcf_from_listfile(odir_path, sid)
            fallback = None
            if fallback is not None:
                logger.warning(f"find_atcf_file found nothing under {atcf_dirs}; "
                               f"using ATCF_FILES.dat fallback -> {fallback}")
                atcf_file = fallback
        if atcf_file is None:
            logger.error("No ATCF file found; find_centers requires ATCF data")
            _write_status(status_file, 'failed')
            return 1

        print(f"  ATCF: {atcf_file}")
        atcf_df = read_atcf(atcf_file)

        # Check ATCF availability for this hour
        atcf_row = atcf_df[atcf_df['fhr'] == fhr]
        if atcf_row.empty:
            logger.debug(f"No ATCF entry for fhr={fhr}, skipping")
            continue

        tc_lat = atcf_row.iloc[0]['lat']
        tc_lon = atcf_row.iloc[0]['lon']
        #GET SHEAR
        shear = compute_shear(datasets, dsource, tc_lat, tc_lon, lev_top = 350, lev_bot=1000, r_inner=200, r_outer=800)
        ensemble_member_dataframe_shear = pd.DataFrame([shear], columns = ['shear_mag_deep','shear_dir_deep'], index=[0])
        ensemble_member_dataframe_shear['emem'] = ensid
        ensemble_member_dataframe_shear['fhr'] = fhr
        outer_dataframe_shear = pd.concat([outer_dataframe_shear, ensemble_member_dataframe_shear])


        #GET TC CENTERS
        centers = None
        #the way this is stored is copied from SHIPS - I don't like it, but I want to leave it in the same format for code portability
        # if 'TCCEN' in active_diags:
        #MD 20260624 - should we be plotting the 1000hPa center, or should we be using the ATCF center? Hmm...
        #By default, I am calculating the 1000mb center, but not using it in favor of the ATCF center.
        centers = compute_tccen(datasets, dsource, tc_lat, tc_lon, levels = [1000,500,350])
        for lev, (clat, clon, _hgt, flag) in centers.items():
            if np.isfinite(clat):
                tccen_store[(fhr, lev)] = [clat, clon, flag]

        for key in sorted(tccen_store.keys()):
            vals = tccen_store[key]
            fhr, lev = key
            lat, lon, flag = vals
            data_row = pd.DataFrame([[fhr, lev, lat, lon, flag]], columns =['fhr','lev','lat','lon','vtx'], index=[0])
            ensemble_member_dataframe = pd.concat([ensemble_member_dataframe,data_row])
        
        ensemble_member_dataframe['emem'] = ensid
        outer_dataframe_centers = pd.concat([outer_dataframe_centers,ensemble_member_dataframe])

    outer_dataframe_centers = outer_dataframe_centers.reset_index(drop=True)
    outer_dataframe_shear = outer_dataframe_shear.reset_index(drop=True)

    return outer_dataframe_centers, outer_dataframe_shear

def calculate_tilt_vectors(centers_df):
    #grab levels of interest, rename columns for later merge
    data1000 = centers_df[centers_df['lev']==1000].copy().reset_index(drop=True).drop('lev',axis=1).rename(columns={'lat':'lat_1000','lon':'lon_1000','vtx':'vtx_1000'})
    data500 = centers_df[centers_df['lev']==500].copy().reset_index(drop=True).drop('lev',axis=1).rename(columns={'lat':'lat_500','lon':'lon_500','vtx':'vtx_500'})
    data350 = centers_df[centers_df['lev']==350].copy().reset_index(drop=True).drop('lev',axis=1).rename(columns={'lat':'lat_350','lon':'lon_350','vtx':'vtx_350'})
    
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

def compute_shear(datasets, dsource, tc_lat, tc_lon, lev_top, lev_bot,
                  r_inner=200, r_outer=800):
    """
    Compute vertical wind shear magnitude and heading.

    Parameters
    ----------
    lev_top, lev_bot : int
        Top/bottom pressure levels in hPa (e.g., 200, 850).

    Returns (shear_mag_kts, shear_heading_deg) or (nan, nan).
    """
    u_top = get_var_2d(datasets, dsource, 'U', str(lev_top))
    u_bot = get_var_2d(datasets, dsource, 'U', str(lev_bot))
    v_top = get_var_2d(datasets, dsource, 'V', str(lev_top))
    v_bot = get_var_2d(datasets, dsource, 'V', str(lev_bot))

    if any(x is None for x in [u_top, u_bot, v_top, v_bot]):
        return np.nan, np.nan

    # Shear = top - bottom
    du = u_top['data'] - u_bot['data']
    dv = v_top['data'] - v_bot['data']
    lat = u_top['lat']
    lon = u_top['lon']

    # Ensure tc_lon is in the data convention
    tc_lon_data = _match_lon_convention(tc_lon, lon)

    x_km, y_km = make_cartesian_grid(radius_km=r_outer, spacing_km=10)
    du_cart = sph2cart(du, lat, lon, tc_lat, tc_lon_data, x_km, y_km)
    dv_cart = sph2cart(dv, lat, lon, tc_lat, tc_lon_data, x_km, y_km)

    shear_mag, shear_dir = compute_wind_shear(du_cart, dv_cart, x_km, y_km,
                                               r_inner, r_outer)
    return shear_mag, shear_dir


###################################################################################################################################

# Parse command-line arguments -------------------------------------------------------------------------------
parser = argparse.ArgumentParser(description='GPLOT ens_compare: ensemble comparison plots')

# ALL configuration is read solely from the master namelist except for initialization date
# and SID, which are the per-invocation identity (which cycle, which storm)
parser.add_argument('--master-nml', dest='master_nml', required=True,
                    help='Path to the master namelist (carries all configuration)')
parser.add_argument('--idate', type=str, required=True, help='Forecast cycle YYYYMMDDHH')
parser.add_argument('--sid', type=str, required=True, help='Storm ID, e.g. 13L')

args = parser.parse_args()

# Read all configuration from the master namelist ---------------------------------------
MASTER_NML = args.master_nml
nml = read_master_namelist(MASTER_NML)

# Point cartopy at the local Natural Earth cache (CARTOPY_DIR) so the offline
# supercomputer does not try to download shapefiles. Must run before any
# cartopy / shpreader.natural_earth() call further below.
configure_cartopy(nml.get('CARTOPY_DIR'))

# General experiment configuration (invariant across many runs)
# For center finding logic from SHIPS, need a gplot_dir, idir, itag, ext, idate, fhrfmt_raw, init_hr, fnl_hr, dt, sid
gplot_dir = nml.get('GPLOT_DIR', os.environ.get('GPLOT_DIR', ''))
dsource = nml.get('DSOURCE', 'HAFS')
expt = nml.get('EXPT', '')
idir = nml.get('IDIR', '')
itag = nml.get('ITAG', '')
ext = nml.get('EXT', '.grb2')
init_hr = int(nml.get('INIT_HR', 0))
fnl_hr = int(nml.get('FNL_HR', 126))
fhrfmt_raw = nml.get('FMT_HR', 3)
dt = int(nml.get('DT', 3))

# Configuration for this specific run case
idate = str(args.idate or nml.get('IDATE', ''))
sid = args.sid or nml.get('SID', '')

# these come from args in SHIPS, but we currently don't take them as args - and probably don't need to - no inner domain in HERC, and only 1 tier
domain = nml.get('DOMAIN','')  # args.domain
tier = nml.get('TIER','')  # args.tier

# ATCF directory/tag: prefer the merged multistorm (ATCF2) over parent
# track (ATCF1), mirroring the selection logic in GPLOT_maps.py.

# don't currently have atcf_dir in args - could add later to match other modules. For now, default to nml.get
# if args.atcf_dir:
#     atcf_dirs = [args.atcf_dir]
#else:
atcf_dirs = [d for d in (nml.get('ATCF2_DIR', ''),
                            nml.get('ATCF1_DIR', '')) if d]
atcf_tag = nml.get('ATCF2_TAG', '') or nml.get('ATCF1_TAG', '')

# Handle list values from namelist (robustness check, in case any of these parameters got read in as a list)
if isinstance(ext, list):
    ext = ext[0] if ext else '.grb2'
if isinstance(itag, list):
    itag = itag[0] if itag else ''
if isinstance(idir, list):
    idir = idir[0] if idir else ''

# Build forecast hour format string
try:
    ndigits = int(fhrfmt_raw)
    fhrfmt = f'%0{ndigits}d'
except (ValueError, TypeError):
    fhrfmt = '%03d'

odir_type = int(nml.get('ODIR_TYPE', 0))

# Regex to classify nest vs. parent domain filenames
_NEST_TOKEN_RE = re.compile(
    r'(?:^|[._-])(storm\d*|nest\d*|moving|d03)(?:[._-]|$)',
    re.IGNORECASE,
)
_PARENT_TOKEN_RE = re.compile(
    r'(?:^|[._-])(parent|d01|hwrf)(?:[._-]|$)',
    re.IGNORECASE,
)

# Comparison plot toggles (read from the master namelist) ----------------------------
def _nml_bool(key, default='True'):
    return str(nml.get(key, default)).strip().lower() == 'true'

ensembleLinePlots = _nml_bool('ENSEMBLE_LINE_PLOTS')
ensembleTracksColored = _nml_bool('ENSEMBLE_TRACKS_COLORED')
ensembleWindRadii = _nml_bool('ENSEMBLE_WIND_RADII')
ensembleClustering = _nml_bool('ENSEMBLE_CLUSTERING')
vortexAvgSteer = _nml_bool('VORTEX_AVG_STEER')
tiltPlots = _nml_bool('TILT_PLOTS')

# parameter lists, derived from namelist
fHours = list(range(init_hr, fnl_hr + 1, dt))  # forecast hours from INIT_HR/FNL_HR/DT
variable = nml.get('BG_VARIABLE', 'HGT')  # variable to plot under ATCF tracks
level = int(nml.get('BG_LEVEL', 500))  # atmospheric level to plot for (if applicable)

# Cluster types to generate graphics for
ALLOWED_CLUSTER_TYPES = ["MSLP", "R34", "R50", "R64", "ltrack", "xtrack"]

# Normalize input into a clean list of clusterTypes
_ct_raw = nml.get('CLUSTER_TYPES', '')
if isinstance(_ct_raw, list):
    clusterTypes = [str(_c).strip() for _c in _ct_raw if str(_c).strip()]
else:
    clusterTypes = [_c for _c in re.split(r'[,\s]+', str(_ct_raw).strip()) if _c]

# Remove bad clusterType inputs and notify user
_badTypes = [_c for _c in clusterTypes if _c not in ALLOWED_CLUSTER_TYPES]
if _badTypes:
    print(f"WARNING: Ignoring unrecognized CLUSTER_TYPES value(s): {_badTypes}")
    clusterTypes = [_c for _c in clusterTypes if _c in ALLOWED_CLUSTER_TYPES]

clusterTypes = list(dict.fromkeys(clusterTypes))  # de-dup, preserve order
print(f"MSG: Cluster types to plot --> {clusterTypes}")

# All three radii are considered for wind-radii plots every run
requestedRadii = [34, 50, 64]

# static parameters
membersStart = int(nml.get('MEMBERS_START', 0))
membersEnd = int(nml.get('MEMBERS_END', 21))
members = range(membersStart, membersEnd)  # members to use
clusterMembers = int(nml.get('CLUSTER_MEMBERS', 4))  # number of members to include in each cluster

# Parse date string into components
year = int(idate[0:4])
month = int(idate[4:6])
day = int(idate[6:8])
hour = int(idate[8:10])

# nikhil's functions want "storm" to be in format AL132025, but sid is supposed to come in form "13l"
if sid[2].lower() == 'l':
    basin = 'AL'
elif sid[2].lower() == 'e':
    basin='EP'
else:
    raise ValueError(f"Unsupported basin in SID: {sid}")

storm = f'{basin}{sid[:2]}{year}'

# input-dependent variables
initDate = int(f"{year:04d}{month:02d}{day:02d}{hour:02d}")
ODIR = nml.get('ODIR', '')
ODIR_full = ODIR+'/ensembleComparison'
os.makedirs(ODIR_full, exist_ok=True)


def cluster_radius(ct):
    """Wind radius (int) implied by a cluster type; None for non-radius types."""
    return int(ct[1:]) if ct in ("R34", "R50", "R64") else None


def radius_is_plottable(hourData, radius):
    """
    Whether a wind-radii figure is worth making for this radius at this hour. Always makes R34, 
    but R50/R64 are skipped unless at least half of the ensemble members have a nonzero radius 
    there (so the low quartiles don't collapse onto identical zero-radius members).

    args:
        hourData: single-fHour ATCF data for all members (including the appended mean)
        radius: int, 34/50/ 64
    returns: bool
    """
    if radius == 34:
        return True

    # Exclude the appended ensemble mean (last member) from the member count
    nMembers = max(len(hourData) - 1, 1)
    nNonzero = int((hourData[f'R{radius}'] > 0).sum())
    return nNonzero >= (nMembers / 2)


# dictionaries for conversions and static variables
typeDict = {"MSLP":   ["MSLP", "MSLP (hPa)"],
            "ltrack": ["Along Track Deviation", "Distance (km)"],
            "xtrack": ["Across Track Deviation", "Distance (km)"]}
clusterTypeDict = {"MSLP":   ["Strong", "Weak"], "vortexDepth": ["Shallow", "Deep"],
                   "ltrack": ["Behind", "Ahead"], "xtrack": ["Left", "Right"]}

# Add a per-radius entry for each wind-radius cluster type (R34/R50/R64)
for _rad in (34, 50, 64):
    typeDict[f"R{_rad}"] = [f"{_rad}kt Avg Wind Radius", "Radius (km)"]
    clusterTypeDict[f"R{_rad}"] = [f"R{_rad} Small", f"R{_rad} Large"]

# Main execution --------------------------------------------------------------------------------------

t_script_start = time.perf_counter()  # Doing some timing for testing purposes, not necessary but helpful to quickly gauge speed issues

# MATT: Is this stuff actually getting output anywhere? I don't see it in the log file
logger.info(f"GPLOT Ens Comparison starting: {sid} {idate}")
logger.info(f"  DSOURCE={dsource} EXPT={expt}")
logger.info(f"  IDIR={idir}")
logger.info(f"  ODIR={ODIR_full}")

# Load ATCF data once for all forecast hours
adeckData, members = modifyAdeckData(members, idir, initDate, storm, clusterMembers, fHours)

# Storm name is constant across all hours
name = uf.getStormName(storm, initDate)

# Cumulative timing accumulators, summed across all forecast hours
timing_totals = {
    'ensembleLinePlots': 0.0,
    'ensembleTracksColored': 0.0,
    'ensembleWindRadii': 0.0,
    'ensembleClustering': 0.0,
    'vortexAvgSteer': 0.0,
    'tiltPlots': 0.0,
}

# Number of forecast hours each plot type actually ran for
timing_counts = {_k: 0 for _k in timing_totals}

# Loop over all requested forecast hours
for fHour in fHours:
    print(f"\n{'-'*60}\nProcessing forecast hour: {fHour}\n{'-'*60}\n")
    t_hour_start = time.perf_counter()

    hourData = getHourData(fHour, adeckData)

    # Shared second title line for every plot this hour 
    titleLine = (f"{name} | Forecast Hour {fHour} | "
                f"Initialized at {hour:02}Z {calendar.month_name[month]} {day:02} {year}")

    # Wind radii depends on radius, not clusterType, so it runs once per hour per radius
    if ensembleWindRadii:
        for _rad in requestedRadii:
            # Skip if less than half the available members have nonzero rXX values
            if not radius_is_plottable(hourData, _rad):
                print(f"MSG: fHour {fHour}: skipping R{_rad} wind-radii plot, too few members")
                continue
            
            t_step_start = time.perf_counter()
            adeckRadiiData, radData = windRadiiData(hourData, _rad)
            plotWindRadii(adeckRadiiData, radData, ODIR_full, fHour, storm, 
                          _rad, initDate, adeckData, titleLine)
            t_elapsed = time.perf_counter() - t_step_start
            timing_totals['ensembleWindRadii'] += t_elapsed
            timing_counts['ensembleWindRadii'] += 1

    for clusterType in clusterTypes:
        print(f"\nForecast hour {fHour}: cluster type {clusterType}")

        if ensembleClustering or vortexAvgSteer:
            allClusterMems = getClusterMems(clusterType, hourData, clusterMembers)

        if ensembleLinePlots:
            t_step_start = time.perf_counter()
            avgVarTypes = sortedColoringData(clusterType, hourData, members)
            plotLinePlots(avgVarTypes, members, ODIR_full, clusterType, 
                          fHour, storm, cluster_radius(clusterType), initDate, titleLine)
            t_elapsed = time.perf_counter() - t_step_start
            timing_totals['ensembleLinePlots'] += t_elapsed
            timing_counts['ensembleLinePlots'] += 1

        if ensembleTracksColored:
            t_step_start = time.perf_counter()
            avgVarTypes = sortedColoringData(clusterType, hourData, members)
            plotTracksColored(avgVarTypes, members, ODIR_full, clusterType, 
                              fHour, storm, cluster_radius(clusterType), initDate, titleLine)
            t_elapsed = time.perf_counter() - t_step_start
            timing_totals['ensembleTracksColored'] += t_elapsed
            timing_counts['ensembleTracksColored'] += 1

        if ensembleClustering:
            t_step_start = time.perf_counter()
            atcfClusters, gribClusters, clusterAvgs = trackClusteringData(
                clusterType, variable, level, fHour, adeckData, allClusterMems, 
                idir, initDate, hourData)
            plotTrackClustering(atcfClusters, gribClusters, clusterAvgs, ODIR_full, allClusterMems, clusterType, 
                                clusterTypeDict, fHour, storm, variable, cluster_radius(clusterType), titleLine)
            t_elapsed = time.perf_counter() - t_step_start
            timing_totals['ensembleClustering'] += t_elapsed
            timing_counts['ensembleClustering'] += 1

        if vortexAvgSteer:
            t_step_start = time.perf_counter()
            clusterDicts = vortexAvgSteerData(fHour, idir, initDate, hourData, 
                                              storm, adeckData, allClusterMems)
            plotVortexAvgSteer(clusterDicts, ODIR_full, storm, initDate, clusterType, fHour, 
                               clusterTypeDict, cluster_radius(clusterType), titleLine)
            t_elapsed = time.perf_counter() - t_step_start
            timing_totals['vortexAvgSteer'] += t_elapsed
            timing_counts['vortexAvgSteer'] += 1

    # Tilt plots do not depend on clusterType, so they run once per forecast hour
    if tiltPlots:
        t_step_start = time.perf_counter()
        plot_tilts(adeckData,atcf_dirs,atcf_tag,idir,dsource,
        gpout_path = ODIR, 
        cycle = idate, 
        fhr=int(fHour),
        storm_id = storm[:4].upper(),
        members_to_plot = [f'{x:02}' for x in members[:-1]],
        out_path = ODIR_full,
        show=False)
        t_elapsed = time.perf_counter() - t_step_start
        timing_totals['tiltPlots'] += t_elapsed
        timing_counts['tiltPlots'] += 1


print(f"\nTotal Python time for all hours: {time.perf_counter() - t_script_start:.4f}s")

print("Average time per forecast hour, by plot type:")
for _label, _seconds in timing_totals.items():
    _n = timing_counts[_label]
    if _n > 0:
        print(f"  {_label:<25} {_seconds / _n:>10.4f}s  (n={_n})")

# Mark this (cycle, storm) case complete so the HAFS workflow's status check
# (find -name 'status.*') sees ens_compare finish. Must match the path/key the
# spawn writes 'working' to: ODIR/ensembleComparison/status.ens_compare.<idate>.<sid>.log
try:
    _status_file = os.path.join(ODIR_full, f"status.ens_compare.{idate}.{sid.lower()}.log")
    with open(_status_file, 'w') as _sf:
        _sf.write("complete\n")
    print(f"MSG: Wrote status 'complete' --> {_status_file}")
except Exception as _status_err:
    print(f"WARNING: Could not write status file: {_status_err}")