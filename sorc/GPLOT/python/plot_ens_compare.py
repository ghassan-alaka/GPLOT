"""
Name: HAFS Ensemble Plotting Script for GPLOT
Author: Nikhil Trivedi and Matt Donahue
Description:
This script reads HAFS ensemble ATCF data, computes member statistics and rankings, and generates a series of 
diagnostic plots for HAFS TC ensemble forecasts.

TO DO:      
- Add logic to check "already plotted" forecast hours, skip function call if plot is already there

- Add logic to check if new ensemble members have been produced?

- Need a minimum number of ensembles present based on clusterSize preference - can't make clusters of 5 if there are only 9 members.
    Or, we can but there will be overlap. Options:
        - Warn user that there will be overlap
        - Warn user that the clusterSize is invalid, overwrite it with a smaller one (N_members//2)


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
from concurrent.futures import ProcessPoolExecutor, as_completed
import logging

logger = logging.getLogger('plot_ens_compare')

# Make gplot_utils / modules importable regardless of CWD (this file lives in
# sorc/GPLOT/python/). Mirrors GPLOT_maps.py:35 -- no hardcoded user paths.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from gplot_utils.namelist import read_master_namelist
from gplot_utils.grib_reader import get_var_2d
from gplot_utils.plot_utils import configure_cartopy
from gplot_utils.coord_transform import (sph2cart,
                                          make_cartesian_grid,
                                          compute_wind_shear)

import glob
import re

import pandas as pd
import numpy as np
import xarray as xr

import matplotlib

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


# Add functionality for flexible spatial cluster background field
def _cmap(stops, vmin, vmax):
    """Generate colormap from (value, color) stops given in the field's own units."""
    return LinearSegmentedColormap.from_list("", [((v - vmin) / (vmax - vmin), c) for v, c in stops])


# Define colormaps for HGT and RH
_HGT_STOPS = [(0.00, "#288DFF"), (0.20, "#029916"), (0.50, "#e8d505"),
              (0.70, "#e87802"), (0.80, "#e30202"), (0.90, "#800000"),
              (1.00, "#4B004B")]
_RH_STOPS = [(0, "#6E4B1F"), (30, "#C8A96E"), (50, "#FFFFFF"),
             (70, "#6FBF73"), (100, "#0B4D1E")]

# Background fields available for the spatial-clustering plot 
# Namelist inputs that aren't in this dict are rejected during namelist parsing
#   scale: scale raw data to correct units (HGT gpm to dam is 0.1, RH already in percent)
#   levels: filled-contour levels
#   lineLevels: black line-contour levels, or None to skip line contours (skipped for RH)
#   labelEvery: label every Nth line contour (unused when lineLevels is None)
#   cmap: LinearSegmentedColormap for the filled contours
#   cbarLabel: colorbar label including units
#   titleField: human-readable field name for the figure suptitle
BG_FIELD_SPECS = {
    ('HGT', 850): dict(scale=0.1, levels=np.arange(138, 163, 2),
                       lineLevels=np.arange(138, 163, 2), labelEvery=2,
                       cmap=_cmap(_HGT_STOPS, 0, 1),
                       cbarLabel='850 hPa Height (dam)', titleField='850mb Heights'),
    ('HGT', 500): dict(scale=0.1, levels=np.arange(540, 602, 2),
                       lineLevels=np.arange(540, 602, 2), labelEvery=2,
                       cmap=_cmap(_HGT_STOPS, 0, 1),
                       cbarLabel='500 hPa Height (dam)', titleField='500mb Heights'),
    ('HGT', 200): dict(scale=0.1, levels=np.arange(1215, 1263, 3),
                       lineLevels=np.arange(1215, 1263, 3), labelEvery=2,
                       cmap=_cmap(_HGT_STOPS, 0, 1),
                       cbarLabel='200 hPa Height (dam)', titleField='200mb Heights'),
    ('RH', 850):  dict(scale=1.0, levels=np.arange(0, 105, 5),
                       lineLevels=None, labelEvery=None, cmap=_cmap(_RH_STOPS, 0, 100),
                       cbarLabel='850 hPa Relative Humidity (%)', titleField='850mb RH'),
    ('RH', 700):  dict(scale=1.0, levels=np.arange(0, 105, 5),
                       lineLevels=None, labelEvery=None, cmap=_cmap(_RH_STOPS, 0, 100),
                       cbarLabel='700 hPa Relative Humidity (%)', titleField='700mb RH'),
    ('RH', 500):  dict(scale=1.0, levels=np.arange(0, 105, 5),
                       lineLevels=None, labelEvery=None, cmap=_cmap(_RH_STOPS, 0, 100),
                       cbarLabel='500 hPa Relative Humidity (%)', titleField='500mb RH'),
}

# Regex to classify nest vs. parent domain filenames
_NEST_TOKEN_RE = re.compile(
    r'(?:^|[._-])(storm\d*|nest\d*|moving|d03)(?:[._-]|$)',
    re.IGNORECASE,
)
_PARENT_TOKEN_RE = re.compile(
    r'(?:^|[._-])(parent|d01|hwrf)(?:[._-]|$)',
    re.IGNORECASE,
)


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
            logger.error(f"ATCF file exists but contains no data for storm {storm}")
            sys.exit(1)
    except Exception as e:
        logger.error(f"Error reading ATCF data at {idir} for storm {storm}, init {initDate}: {e}")
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
                logger.warning(f"Zero-filling {missing.sum()} row(s) with missing {_rad}kt radii")
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
        logger.warning(f"Skipping {len(incompleteMembers)} member(s) missing one or more "
              f"requested forecast hours {sorted(requestedTaus)}: {incompleteMembers}")
        adeckData = adeckData[~adeckData['member'].isin(incompleteMembers)]
        members = [m for m in members if m not in incompleteMembers]
    else:
        logger.info(f"All members cover the requested forecast hours {sorted(requestedTaus)}.")

    if len(members) < clusterMembers:
        logger.error(f"Only {len(members)} member(s) available after filtering, but clusterMembers={clusterMembers}. Skipping forecast hour.")
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
        HepTools.getTrackSpeedData(), logging

    returns: hourData (see glossary)
    """

    hourData = uf.getTrackSpeedData(adeckData, fHour)
    #recall - logger.debug will not execute unless debug mode is specified
    logger.debug(hourData)
    logger.debug(f'Filtered aDeck data to hour {fHour}')
    return hourData


def getClusterMems(clusterType, hourData, clusterMembers):
    """ 
    Split members into two extreme clusters by clusterType (clusterMembers lowest-valued
    members and clusterMembers highest-valued members).

    Common args (clusterType, hourData, clusterMembers): see glossary

    dependencies: logging

    returns: allClusterMems (see glossary), [0] is the low cluster and [1] is the high cluster
    """
    allClusterMems = []
    allClusterMems.append(hourData.nsmallest(clusterMembers, clusterType)["member"].tolist())
    allClusterMems.append(hourData.nlargest(clusterMembers, clusterType)["member"].tolist())
    logger.debug("getClusterMems() complete")
    return allClusterMems


# calculate graphic-specific data -----------------------------------------------------------------------


def sortedColoringData(clusterType, hourData, members):
    """ 
    Rank members by clusterType for the rank-colored line/track plots. MSLP is ranked
    in ascending order (rank 1 = lowest), all other metrics are descending. Ensemble mean
    is excluded, along with members with a zero radius (if applicable).

    Common args (clusterType, hourData, members): see glossary

    dependencies: pandas as pd, numpy as np, logging

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

    logger.debug("sortedColoringData() complete")
    return avgVarTypes


def windRadiiData(hourData, radius):
    """ 
    Select the five members closest to the ensembles quartiles (min/25th/median/75th/max), 
    then build the per-quadrant wind radii arc coordinates for those five members.

    Common args (hourData): see glossary
    Function-specific:
        radius: int (34/50/64), which wind radius to build quartiles/arcs for

    dependencies: 
        numpy as np, pandas as pd, logging

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
    logger.debug("Quartile data in windRadiiData():")
    logger.debug(quartileData)

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
    logger.debug('windRadiiData() complete')  
    return quartileData, radData


def trackClusteringData(clusterType, variable, level, fHour, adeckData, sid, expt,
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
        pandas as pd, sys, HepTools.getGribData(), concurrent.futures.ThreadPoolExecutor(), logging

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

        args:
            idx: 0 or 1, which cluster we are processing (index into memberDataList)
            clusterMems: list of ints, the member IDs in this particular cluster

        dependencies: 
            HepTools.getGribData(), sys, logging

        returns: tuple (memberDataList[idx], gribData, clusterAvg)
            memberDataList[idx]: DataFrame, this cluster's member ATCF data across all hours
            gribData: xarray Dataset, cluster-averaged background field
            clusterAvg: float, the average value of MSLP for the cluster
        """
        # get GRIB data for the members in the cluster, exit if this does not work
        try:
            gribData = uf.getGribData(f'{idir}', bounds, sid, expt, members=clusterMems, initDate=initDate, 
                                      variable=variable, fHour=fHour, level=level)
            if gribData is None or len(gribData.data_vars) == 0:
                logger.error(f"No GRIB data returned for cluster {idx}, init {initDate}")
                sys.exit(1)

        except FileNotFoundError as e:
            logger.error(f"GRIB file not found at {idir} for init {initDate}: {e}")
            sys.exit(1)
        except Exception as e:
            logger.error(f"Error reading GRIB data  at {idir} for init {initDate}: {e}")
            sys.exit(1)
       
        # get cluster-averaged MSLP or radius at fHour
        clusterHourData = hourData[hourData["member"].isin(clusterMems)]
        clusterAvg = clusterHourData['MSLP'].mean()
            
        logger.debug('_fetch_cluster() completed')
        return memberDataList[idx], gribData, clusterAvg

    
    # Run the cluster extractions concurrently using threads
    with concurrent.futures.ThreadPoolExecutor(max_workers=len(allClusterMems)) as executor:
        futures = [executor.submit(_fetch_cluster, idx, clusterMems) for idx, clusterMems in enumerate(allClusterMems)]
        results = [f.result() for f in futures]

    # Unpack the concurrent results into the expected lists
    atcfClusters = [r[0] for r in results]
    gribClusters = [r[1] for r in results]
    clusterAvgs  = [r[2] for r in results]

    logger.debug('trackClusteringData() completed')
    return atcfClusters, gribClusters, clusterAvgs
    

def vortexAvgSteerData(fHour, idir, initDate, hourData, storm, sid, expt,
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
        concurrent.futures.ThreadPoolExecutor(), sys, logging

    returns: clusterDicts, list of 2 dicts (one per cluster), each with keys: 
        {'radAvgData', 'uSteer', 'vSteer', 'uShear', 'vShear', 'uMotion', 'vMotion', 
         'vortexDepth', 'presLevData'}
    """
    
    def _process_single_vortex(cluster_idx, clusterMems):
        """ 
        Fetch and return all data for one cluster; wrapped in function so that it can
        be invoked concurrently using multithreading. This speeds things up since the
        bottleneck is the grb2 reads.

        Reads idir, initDate, fHour, hourData, storm, and adeckData from enclosing function.

        args:
            idx: 0 or 1, which cluster we are processing (index into memberDataList)
            clusterMems: list of ints, the member IDs in this particular cluster

        dependencies: 
            numpy as np, xarray as xr, HepTools.getGribData(), sys, logging

        returns: dict with keys: {'radAvgData', 'uSteer', 'vSteer', 'uShear', 'vShear', 'uMotion', 'vMotion',
                                  'vortexDepth', 'presLevData'}
        """

        def _massWeights(levelCoord):
            """Compute mass weighting using layer thickness (accounts for uneven vertical spacing)"""
            dp = np.abs(np.gradient(np.asarray(levelCoord.values, dtype=float)))
            logger.debug('_massWeights() completed')
            return xr.DataArray(dp, dims=["level"], coords={"level": levelCoord})

        # get ATCF center data
        centerData = hourData[hourData["member"].isin(clusterMems)]
        centers = dict(zip(centerData["member"], zip(centerData["latitude"], centerData["longitude"])))

        # load 5x5 degree centered wind data into memory, handle errors
        try:
            windData_xy = uf.getGribData(f'{idir}', centers, sid, expt, variable=['UGRD', 'VGRD'], members=clusterMems, 
                                            initDate=initDate, fHour=fHour)
            if windData_xy is None or len(windData_xy.data_vars) == 0:
                logger.error(f"No GRIB data returned for cluster {cluster_idx}, storm {storm}")
                sys.exit(1)
            
        except FileNotFoundError as e:
            logger.error(f"GRIB file not found at {idir} for storm {storm}, init {initDate}: {e}")
            sys.exit(1)
        except Exception as e:
            logger.error(f"Error reading GRIB data at {idir} for storm {storm}, init {initDate}: {e}")
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

        FRAC_DEPTH = 0.99  # fraction of column KE enclosed below the vortex top
        PEAK_FRAC = 0.50  # radius defined as where tangential wind falls to this fraction of its peak
        VT_MIN_DEPTH = 10 / 1.94384  # min wind that's allowed to count toward vortex depth (10 kts)
        DEPTH_FLOOR_HPA = 700.0  # last-resort floor if depth calc is too shallow

        # get radius coordinates and tangential wind averaged over low-levels, per radius
        rad = radAvgData.radius
        vtLow = radAvgData.tangential_wind.sel(level=slice(1000, 850)).mean(dim='level')

        # width stays the full 5x5 degree disk because steering is on the synoptic-scale
        vortexWidth = float(rad.max())

        # R_depth: largest radius where low-level winds exceed vtThresh
        vtPeak = float(vtLow.max())  # strongest low-level tangential wind
        vtThresh = max(PEAK_FRAC * vtPeak, VT_MIN_DEPTH)
        R_depth = float(rad.where(vtLow >= vtThresh).max())

        # compute core-mean tangential vertical wind profile over the width of the vortex
        mean_vt_core = radAvgData.tangential_wind.where(rad <= R_depth).mean(dim='radius')
        levs_desc = np.sort(radAvgData.level.values)[::-1]
        mean_vt_core = mean_vt_core.sel(level=levs_desc)  # reorder to integrate upward

        # check if any level in the core-mean profile has wind > 10kts
        hasVortexSignal = bool((mean_vt_core >= VT_MIN_DEPTH).any())
        if hasVortexSignal:
            # set vortex top as layer that encloses 99% of column KE
            mean_vt_depth = mean_vt_core.where(mean_vt_core >= VT_MIN_DEPTH)
            kePerLayer = (mean_vt_depth ** 2) * _massWeights(mean_vt_depth.level)  # apply mass weighting
            cumFracZ = kePerLayer.cumsum('level') / kePerLayer.sum()
            vortexDepth = float(mean_vt_core.level.where(cumFracZ >= FRAC_DEPTH).max())
        else:
            vortexDepth = np.nan  # no level clears 10kt at all, nothing meaningful to integrate

        # if vortex top is NaN or shallower than 700 hPa, set top to 700 hPa
        if np.isnan(vortexDepth) or vortexDepth > DEPTH_FLOOR_HPA:
            depthFloored = True
            vortexDepth = DEPTH_FLOOR_HPA
        else:
            depthFloored = False

        logger.debug(f"[{storm} f{fHour:03d}] width={vortexWidth:.0f}km (full box)  "
                     f"vtPeak={vtPeak*1.94384:.0f}kt  R_depth={R_depth:.0f}km  "
                     f"depthTop={vortexDepth:.0f}hPa  floored={depthFloored}")

        # full-column domain-averaged wind for the hodograph (before slicing to vortex depth)
        presLevData = windData_xy.where(r <= vortexWidth).mean(dim=['x', 'y'])

        # slice Cartesian wind data to only include estimated vortex (for steering/shear)
        windData_xy = windData_xy.sel(level=slice(1000, vortexDepth))
        windData_xy = windData_xy.where(r <= vortexWidth)

        # calculate mass-weighted domain-averaged steering (over the 5x5 degree circle)
        vortexColData = windData_xy.mean(dim=['x', 'y'])
        steeringData = vortexColData.weighted(_massWeights(vortexColData.level)).mean(dim='level')

        # calculate fixed deep-layer bulk shear (850-200 hPa)
        botDeep = presLevData.sel(level=850, method='nearest')
        topDeep = presLevData.sel(level=200, method='nearest')
        shearData = (topDeep - botDeep)

        # calculate max shear across every layer pair within the deep layer (200-850 hPa)
        deepLevels = presLevData.sel(level=slice(850, 200))
        top = deepLevels.rename({'level': 'levTop'})
        bot = deepLevels.rename({'level': 'levBot'})
        shearMagAll = np.hypot(top.uWind - bot.uWind, top.vWind - bot.vWind)  # dims (levTop, levBot)

        # pick the layer pair with the largest magnitude difference
        maxIdx = shearMagAll.argmax(dim=['levTop', 'levBot'])
        maxShearMag = float(shearMagAll.isel(maxIdx))
        maxShearTop = float(shearMagAll.levTop.isel(levTop=maxIdx['levTop']))
        maxShearBot = float(shearMagAll.levBot.isel(levBot=maxIdx['levBot']))

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

        logger.debug("_process_single_vortex() complete")
        return {'radAvgData': radAvgData, 'uSteer': uSteer, 'vSteer': vSteer, 'uShear': uShear, 'vShear': vShear,
                'uMotion': uMotion, 'vMotion': vMotion, 'vortexDepth': vortexDepth, 'presLevData': presLevData,
                'maxShearMag': maxShearMag, 'maxShearBot': maxShearBot, 'maxShearTop': maxShearTop}

    # Parallel submission block
    with concurrent.futures.ThreadPoolExecutor(max_workers=len(allClusterMems)) as executor:
        futures = [executor.submit(_process_single_vortex, cluster_idx, clusterMems) for cluster_idx, clusterMems in enumerate(allClusterMems)]
        clusterDicts = [f.result() for f in futures]
    
    logger.debug("vortexAvgSteerData() complete")
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
        cartopy.crs as ccrs, cartopy.feature as cf, cartopy.io.shapereader as shpreader,
        logging

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
    
    logger.debug("plotCartopyFigure() complete")
    return ax


def computeTrackBounds(lons, lats):
    """
    Set a lon/lat box for the track map to 3:2 (wider than tall) or 2:3 (taller than wide).

    args:
        lons, lats: array-like longitudes (0-360) and latitudes in the ATCF file

    dependencies: 
        numpy as np, logging

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

    logger.debug("computeTrackBounds() complete")
    return lonMin, lonMax, latMin, latMax, orientation


def addRankColorbar(ax, sortTitle, clusterType, nColors, isTrack):
    """
    Add the vertical rank colorbar (rank 1 at top), with end labels showing what the extremes
    mean (e.g. Strong/Weak).

    Common args (clusterType): see glossary
    Function specific:
        ax: plain axes for 'line', cartopy GeoAxes for 'track'
        sortTitle: puts ranking method (e.g. MSLP) in colorbar title
        nColors: number of colors in colormap
        isTrack: whether or not the plot type is 'track'

    dependencies:
        matplotlib.pyplot as plt, logging

    return: colorbar object with title and end labels

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

    logger.debug("addRankColorbar() complete")
    return cbar


def plotSortedLines(ax, avgVar, plotType, members, adeckData, fHour,
                    clusterType, titleLine):
    """
    Draw a rank-colored ensemble figure onto an existing axes: MSLP vs. forecast hour
    (plotType='line') or spatial tracks (plotType='track'), lines colored by clusterType rank. 
    Adds the colorbar, ranking-hour emphasis, title, and legend.

    Common args (members, adeckData, clusterType, fHour): see glossary
    Function-specific:
        ax: plain axes for 'line', cartopy GeoAxes for 'track'
        avgVar: DataFrame ['member', clusterType, 'rank']; 'rank' sets the color (NaN = gray)
        titleLine: common second title line (date/init/storm info)

    dependencies: 
        matplotlib.pyplot as plt, numpy as np, pandas as pd, 
        cartopy.crs as ccrs, matplotlib.lines.Line2D, logging
        typeDict: static dictionary defined in main namespace

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

    logger.debug("plotSortedLines() complete")
    return ax


def plotLinePlots(avgVarTypes, members, adeckData, savePath, clusterType, fHour, storm,
                  radius, initDate, titleLine):
    """
    MSLP vs. forecast-hour figure. The drawing is done by plotSortedLines(plotType='line'),
    with a few plot-specific customizations done in here (labels, ticks).
    
    Common args (members, savePath, clusterType, fHour, storm, radius, initDate): see glossary
        avgVarTypes: same as avgVar in plotSortedLines().

    dependencies:
        matplotlib.pyplot as plt, plotSortedLines(), numpy as np, logging

    returns: None (Writes a PNG).
    """
    # Build figure and set axis labels
    plt.close('all')
    plt.figure(figsize=(10, 6))
    ax = plt.gca()
    ax = plotSortedLines(ax, avgVarTypes, 'line', members, adeckData, fHour,
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
    logger.debug("plotLinePlots() complete")


def plotTracksColored(avgVarTypes, members, adeckData, savePath, clusterType, fHour, storm,
                      radius, initDate, titleLine):
    """
    Spatial storm tracks plot. Map features via plotCartopyFigure(), drawing via
    plotSortedLines(plotType='track'). The extent is aspect-locked to 3:2 or 2:3 and the
    figure is sized to match.

    Common args (members, adeckData, savePath, clusterType, fHour, storm, radius, initDate): see glossary
        avgVarTypes: same as avgVar in plotSortedLines().

    dependencies:
        matplotlib.pyplot as plt, cartopy.crs as ccrs, logging
    
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
    ax = plotSortedLines(ax, avgVarTypes, 'track', members, adeckData, fHour,
                         clusterType, titleLine)

    
    plt.savefig(rf"{savePath}/{storm[2:4]}l.{initDate}.spatial_tracks.{clusterType}.f{fHour:03d}.png",
                dpi=200, bbox_inches='tight')
    logger.debug("plotTracksColored() complete")


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
        cartopy.crs as ccrs, HepTools.getStormName(), logging,
        matplotlib.lines.Line2D

    returns: None (writes a PNG).
    """
    plt.close('all')
    colors = plt.cm.viridis(np.linspace(0, 1, len(quartileData)))

    plt.figure(figsize=(10, 6))
    ax = plt.axes(projection=ccrs.PlateCarree(central_longitude=180))

    ax = plotCartopyFigure(ax)
    
    meanLon, meanLat = quartileData['longitude'].mean(), quartileData['latitude'].mean()
    ax.set_extent([meanLon-8, meanLon+8, meanLat-6, meanLat+6])

    ax.scatter(quartileData['longitude'], quartileData['latitude'], color=colors, zorder=100, 
               s=25, transform=ccrs.PlateCarree(), edgecolors='black', linewidths=1)

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
        ax.scatter(memberData['longitude'][::2], memberData['latitude'][::2], color=colors[idx], transform=ccrs.PlateCarree(), alpha=0.5, s=10)

        memRadData = radData[radData['percentile'] == data.name]
        for quad, quadrant in enumerate(['RAD1', 'RAD2', 'RAD3', 'RAD4']):
            memQuadData = memRadData[memRadData['quadrant'] == quadrant]
            ax.plot(memQuadData['lon'], memQuadData['lat'], color=colors[idx], transform=ccrs.PlateCarree(), zorder=10)
            
            memQuadNext = memRadData[memRadData['quadrant'] == f'RAD{(quad + 1) % 4 + 1}'].iloc[0]
            ax.plot([memQuadData.iloc[-1]['lon'], memQuadNext['lon']], [memQuadData.iloc[-1]['lat'], memQuadNext['lat']],
                    color=colors[idx], transform=ccrs.PlateCarree(), zorder=10)

    
    plt.savefig(rf"{savePath}/{storm[2:4]}l.{initDate}.wind_radii.R{radius}.f{fHour:03d}.png", dpi=200, bbox_inches='tight')
    logger.debug("plotWindRadii() complete")

    
def plotTrackClustering(atcfClusters, gribClusters, clusterAvgs, savePath, allClusterMems, clusterType, 
                        clusterTypeDict, fHour, storm, level, variable, radius, initDate, titleLine):
    """
    Plot the two-panel ensemble-clustering figure. For each cluster, contour the cluster-averaged
    background field (currently only works for 500 hPa geopotential heights), with each member's
    track overlaid and the fHour position marked. 

    Common args (savePath, allClusterMems, clusterType, clusterTypeDict, fHour,
        storm, level, variable, radius, year, month, day, hour): see glossary
    Function-specific:
        - atcfClusters: list of 2 DataFrames, with each holding one cluster's ATCF track
        - gribClusters: list of 2 Datasets, cluster-averaged background field
        - clusterAvgs: list of 2 floats, average MSLP for each cluster

    POTENTIAL CHANGE: Contour levels and colormap are currently hard-coded for 500 hPa
    geopotential heights, but we should make this work for other background fields.

    dependencies: 
        cartopy.crs as ccrs, matplotlib.pyplot as plt, numpy as np
        matplotlib.colors.LinearSegmentedColormap, HepTools.getStormName()
        HepTools.getTitleDate(), plotCartopyFigure(), logging
        BG_FIELD_SPECS (static variable defined in main namespace)

    returns: None (writes a PNG).
    """
    plt.close('all')
    fig, axes = plt.subplots(2, 2, figsize=(9.5, 5.5), constrained_layout=True, subplot_kw={'projection': ccrs.PlateCarree()}, 
                             gridspec_kw={"height_ratios": [0.02, 1]})
    for ax in axes[0]:
        ax.axis('off')
    axes = axes[1]

    spec = BG_FIELD_SPECS[(variable, level)]

    for idx, (clusterMems, atcfData, gribData, clusterAvg, ax) in enumerate(zip(allClusterMems, atcfClusters, gribClusters, clusterAvgs, axes)):
        ax = plotCartopyFigure(ax, plotLand=False)

        ax.set_extent([gribData.longitude.min(), gribData.longitude.max(), 
                       gribData.latitude.min(), gribData.latitude.max()], crs=ccrs.PlateCarree())

        gribData = gribData[list(gribData.data_vars)[0]] * spec['scale']  # select and scale data

        contourf = ax.contourf(gribData.longitude, gribData.latitude, gribData,
                               spec['levels'], extend='both', transform=ccrs.PlateCarree(), cmap=spec['cmap'])

        # plot contours on top of shading if requested
        if spec['lineLevels'] is not None:
            contours = ax.contour(gribData.longitude, gribData.latitude, gribData,
                                spec['lineLevels'], transform=ccrs.PlateCarree(),
                                colors='black', linewidths=0.5)
            ax.clabel(contours, spec['lineLevels'][::spec['labelEvery']],
                    inline=True, fontsize=8)
    
        # plot subfigure title
        title = f"{clusterTypeDict[clusterType][idx]} (Cluster Avg Min MSLP: {clusterAvg:.1f} hPa)"
        ax.set_title(title, fontsize=9, weight='bold', loc='center')

        # plot ATCF data
        for clusterMem in clusterMems:
            memberData = atcfData[atcfData["member"] == clusterMem]
            ax.plot(memberData['longitude'], memberData['latitude'], transform=ccrs.PlateCarree(), color='black')
            
        hourData = atcfData.loc[atcfData['TAU'] == fHour]
        ax.scatter(hourData['longitude'], hourData['latitude'], transform=ccrs.PlateCarree(), color='blue', zorder=100, s=15)

    cbar = fig.colorbar(contourf, ax=axes, orientation='horizontal', pad=0.04, aspect=50)
    cbar.ax.tick_params(labelsize=8)
    cbar.set_label(spec['cbarLabel'], fontsize=9, weight='bold')

    # add titling
    titleDict = {"MSLP": "MSLP", "ltrack": "Along Track Variation", "xtrack": "Across Track Variation", 
                 "R34": "Radius of 34kt Winds", "R50": "Radius of 50kt Winds", "R64": "Radius of 64kt Winds", 
                 "vortexDepth": "Vortex Depth"}

    mainTitle = f"HAFS Ensemble {spec['titleField']} and Tracks Clustered By {titleDict[clusterType]}"
    fig.suptitle(f"{mainTitle}\n{titleLine}", fontsize=10, weight='bold')
    
    plt.savefig(rf"{savePath}/{storm[2:4]}l.{initDate}.{variable}{level}.spatial_cluster.{clusterType}.f{fHour:03d}.png", dpi=200, bbox_inches='tight')
    logger.debug("plotTrackClustering() complete")


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
             'vortexDepth', 'presLevData'}

    dependencies: 
        matplotlib.pyplot as plt, numpy as np,
        matplotlib.colors.LinearSegmentedColormap, matplotlib.ticker.LogLocator,
        matplotlib.lines.Line2D, HepTools.getStormName(), HepTools.getTitleDate(),
        logging
        
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

        # dynamic vortex top: horizontal dashed line across the full width, labeled
        ax.axhline(data['vortexDepth'], color='black', linestyle='--', linewidth=1.2)
        ax.text(0.5, data['vortexDepth'], f"Vortex Top ({int(data['vortexDepth'])} hPa)",
                transform=ax.get_yaxis_transform(), ha='center', va='bottom',
                fontsize=8, weight='bold',
                bbox=dict(boxstyle='round,pad=0.2', fc='white', ec='none', alpha=0.7))

        # add informational subtitle
        steerMag = np.hypot(data['uSteer'], data['vSteer']) * 1.94384
        bulkShearMag = np.hypot(data['uShear'], data['vShear']) * 1.94384
        maxShearMag = data['maxShearMag'] * 1.94384
        hiP = max(data['maxShearBot'], data['maxShearTop'])
        loP = min(data['maxShearBot'], data['maxShearTop'])
        maxShearLayer = f"{hiP:.0f}-{loP:.0f} hPa"
        ax.set_title(
            f"{clusterTypeDict[clusterType][idx]}  |  Steering {steerMag:.0f} kt\n"
            f"Bulk Shear {bulkShearMag:.0f} kt  |  Max Shear {maxShearMag:.0f} kt ({maxShearLayer})",
            fontsize=9, weight='bold')
    
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

    linesList = [hodograph_red, motion_proxy, hodograph_green, steer_proxy, hodograph_purple, shear_proxy]
    labelsList = ["Hodograph (1000–850)", "Storm Motion", "Hodograph (850–500)",
                  "Vortex-Averaged Steering Flow", "Hodograph (500–100)",
                  "Deep-Layer Bulk Shear (200-850 hPa)"]
    cbar.ax.legend(linesList, labelsList, loc="lower center", bbox_to_anchor=(0.5, -3), ncol=3, frameon=False, fontsize=8)

    plt.savefig(f"{savePath}/{storm[2:4]}l.{initDate}.wind.vortex_cluster.{clusterType}.f{fHour:03d}.png", dpi=200, bbox_inches='tight')
    logger.debug("plotVortexAvgSteer() complete")

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
        logging

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
        logger.warning("WARNING: Nan count mismatch:")
        logger.warning(f"Some ensemble member(s) have a {metric} with defined magnitude, undefined direction or vice-versa.")

    elif nan_dir_count != 0:
        logger.warning("WARNING: {} Ensemble members have undefined {} from GPLOT output".format(nan_dir_count, metric))
    
    #calculate iqr on circle
    iqr_data = find_iqr_circle(df, metric)
    logger.debug("call_iqr_calculation() complete")
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
        logging

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


    logger.debug("find_iqr_circle() complete")
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
        logging
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
    logger.debug("add_scalebar() complete")

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
        logging

    returns: ax: matplotlib pyplot axis with shear and translation stats added
    
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
    logger.debug("add_shear_and_translation_stats() complete")
    return ax

def plot_tilts(atcf_df, itag, idir,dsource, out_path, ext, fhrfmt, output_timestep,
               cycle = '2025081600', 
               fhr=0,
               storm_id = 'AL05',
                members_to_plot = 'all',
              show=False):
    
    """
    Main function to plot tilts and shears from ships data and atcf data. 
    This function reads in the necessary data, processes it, and generates plots for analysis.

    Args: atcf_df, idir, dsource, out_path, cycle, fhr, storm_id, members_to_plot, show
        - atcf_df: DataFrame containing ATCF data
        - itag: string, tag for grb2 file - from namelist or from command line
        - idir: input directory for ATCF data - from namelist or from command line
        - dsource: string, data source for ATCF data - from namelist or from command line. Used for static data lookup
        - out_path: full path for saving figures
        - ext: file extension substring for file search function
        - fhrfmt: probably '%03d' or whatever to get 3-digit formats, comes from namelist
        - output_timestep: create graphics for every {output_timestep} hours
        - cycle: str, forecast cycle, YYYYMMDDHH
        - fhr: int, forecast hour
        - storm_id: str, BBNN, basin and number
        - members_to_plot: either 'all' or a list of specific ensemble members of interest like ['01','05','10']
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
        logging
    
    """
    logger.info(f"Begin Plot Tilts for fhr {fhr:03}, storm {storm_id}")
    
    #should we add option to specify the pressure levels to use for tilt calculation?
    #For now, I will use 1000, 500, 350?
    #get atcf centers from hourData, pass into tilt plotting function
    atcf_centers = atcf_df[atcf_df['TAU']==fhr][['latitude','longitude','member']].copy()
    atcf_centers['member'] = [f'{x:02}' for x in atcf_centers['member']]

    centers_data, shear_data = find_centers_and_shear(members_to_plot, atcf_centers, fhr, itag, idir, cycle, out_path, dsource,
                                                    ext, fhrfmt, output_timestep)
    tilt_data = calculate_tilt_vectors(centers_data)

    #keep track of whether bad vortex centers exist anywhere in the ensemble
    any_bad_vortices = False

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
            logger.warning(f"WARNING: Ensemble member {emem} was passed as an argument, but is missing from current data. Excluding this member from analysis.")
            exclude_members.append(emem)
    
    for e in exclude_members:
        select_emem.remove(e)
    
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
    # ax.coastlines(resolution='10m') - left over from earth-centered plot. should probably deprecate - md 20260730

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
        
        # left over from earth-centered plot. should probably deprecate - md 20260730
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

    
    # # Add dashed lat/lon gridlines - left over from earth-centered plot. should probably deprecate - md 20260730
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
    # if members_to_plot == 'all':
    #     plt.savefig('{}/{}.{}.tilt_plot.all_members.f{}.png'.format(out_path, stid, cycle,f"{fhr:03}"))
    # else:
    #MD 20260810 - removing name flexibility, not currently ever passing "all" into function
    plt.savefig('{}/{}.{}.vortex_tilt.f{}.png'.format(out_path, stid, cycle,f"{fhr:03}"))
    if show:
        plt.show()
        time.sleep(1)
    # plt.close('all')
    logger.debug("plot_tilts() complete")


#this functionality is used in SHIPS and similar ones are used throughout GPLOT - we should move this to an external script.
def find_grib_files(idir, itag, ext, idate, fhr_fmt, init_hr, fnl_hr, dt, ens_id,
                    prefer_nest=True):
    """
    Build list of (fhr, filepath) for available GRIB2 files.

    Args: idir, itag, ext, idate, fhr_fmt, init_hr, fnl_hr, dt, ens_id, prefer_nest
        - idir: input directory for ATCF data - from namelist or from command line
        - ext: file extension substring for file search function
        - itag: string, tag for grb2 file - from namelist or from command line
        - idate: str, forecast cycle, YYYYMMDDHH
        - fhr_fmt: probably '%03d' or whatever to get 3-digit formats, comes from namelist
        - init_hr: int, first forecast hour in range
        - fnl_hr: int, last forecast hour in range. Note - in this use case, I think these will be the same hour
        - dt: int, output timestep
        - ens_id: string, ensemble member ID, 0-padded if necessary
        - prefer_nest: bool, default True, decide which grb to keep if there are two
            Not applicable in ensemble case as of 2026
        
    dependencies:
        os
        _NEST_TOKEN_RE   (regex pattern defined in main namespace)
        _PARENT_TOKEN_RE (regex pattern defined in main namespace)
        logging
        
    returns:
        files: list of (fhr, filepath)
    
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
            logger.warning(f"No GRIB2 file for fhr={fhr}")
        fhr += dt
    logger.debug("find_grib_files() complete")
    return files

#this comes from SHIPS and could go in a separate module
def compute_tccen(datasets, dsource, tc_lat, tc_lon, levels=None):
    """
    Compute TC centers at multiple pressure levels via the smoothed
    HGT centroid, then apply NCL's vortex-continuity check: a level's
    center is "in the vortex" iff it lies within 1 km per 1 hPa of the
    level immediately below it (or, for k>=2, also valid against k-2).
    Continuity propagates: a level fails if any of its anchor levels
    below failed.

    args: datasets, dsource, tc_lat, tc_lon, levels
        - datasets: list of xarray datasets. in this module, it is a single dataset, but it was left in
            list format to match SHIPS functionality prior to parallelization.
        - dsource: string, data source for ATCF data - from namelist or from command line. Used for static data lookup
        - tc_lat: float, storm center latitude
        - tc_lon: float, storm center longitude
        - levels: list of height levels for calculation, in mb

    dependencies:
        find_center_at_level()
        numpy as np
        _haversine_km()
        logging
    
    Returns dict: level -> (lat, lon, hgt_value, use_flag).
    """
    if levels is None:
        # 25 hPa increments from 200 to 1000, matching the NCL
        # coordinate-subscript LEV(:{200:1000}). Models that don't
        # provide every 25-hPa level will have those entries return
        # None from get_var_2d and be filtered out downstream.
        levels = list(range(200, 1001, 25))

    # Process surface->top so the continuity chain anchors at the
    # near-surface vortex (matches NCL: levels are flipped if
    # max(LEV) != LEV(0) before the use-flag pass).
    levs_asc = sorted(levels, reverse=True)

    raw = {}
    for lev in levs_asc:
        clat, clon, hgt, found = find_center_at_level(
            datasets, dsource, lev, tc_lat, tc_lon)
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

    logger.debug("compute_tccen() complete")
    return centers

#this comes from SHIPS and could go in separate module
def find_center_at_level(datasets, dsource, level, tc_lat, tc_lon):
    """
    Find the TC center at a given pressure level via the geopotential-
    height centroid algorithm ported from NCL findCenter type=1:
    1) crop to ~5 degrees around the ATCF position;
    2) smooth with a 1-2-1 filter, 25 iterations;
    3) value-weighted centroid of the lower 20% of the smoothed field.

    args: datasets, dsource, level, tc_lat, tc_lon
        - datasets: list of xarray datasets. in this module, it is a single dataset, but it was left in
            list format to match SHIPS functionality prior to parallelization.
        - dsource: string, data source for ATCF data - from namelist or from command line. Used for static data lookup
        - level: height level for calculation, in mb
        - tc_lat: float, storm center latitude
        - tc_lon: float, storm center longitude

    dependencies: 
        gplot_utils.grib_reader.get_var_2d()
        numpy as np
        _match_lon_convention()
        _filter121_2d()
        _centroid_min()
        logging

    Returns (center_lat, center_lon, hgt_value, found_flag).
    hgt_value is min(smoothed) at the center for downstream "lowest
    level" marker selection. found_flag is 1 if a center could be
    computed (vortex-continuity check happens later in compute_tccen).
    """
    result = get_var_2d(datasets, dsource, 'HGT', str(level))
    if result is None:
        logger.warning("get_var_2d() returned None, so find_center_at_level() is returning (nan, nan, nan, 0)")
        return np.nan, np.nan, np.nan, 0

    data = result['data']
    rlat = result['lat']
    rlon = result['lon']

    tc_lon_data = _match_lon_convention(tc_lon, rlon)
    lat_mask = (rlat >= tc_lat - 5) & (rlat <= tc_lat + 5)
    lon_mask = (rlon >= tc_lon_data - 5) & (rlon <= tc_lon_data + 5)

    if not np.any(lat_mask) or not np.any(lon_mask):
        logger.warning("found centers were more than 5 degrees away, so find_center_at_level() is returning (nan, nan, nan, 0)")
        return np.nan, np.nan, np.nan, 0

    sub = data[np.ix_(lat_mask, lon_mask)]
    sub_lat = rlat[lat_mask]
    sub_lon = rlon[lon_mask]

    if sub.size == 0 or np.all(np.isnan(sub)):
        logger.warning("found centers were more than 5 degrees away or nonexistent domain?, so find_center_at_level() is returning (nan, nan, nan, 0)")
        return np.nan, np.nan, np.nan, 0

    smoothed = _filter121_2d(sub, n_iter=25)
    i_c, j_c = _centroid_min(smoothed)
    if not (np.isfinite(i_c) and np.isfinite(j_c)):
        logger.warning("_centroid_min() result was not finite, so find_center_at_level() is returning (nan, nan, nan, 0)")
        return np.nan, np.nan, np.nan, 0

    clat = float(np.interp(i_c, np.arange(len(sub_lat)), sub_lat))
    clon = float(np.interp(j_c, np.arange(len(sub_lon)), sub_lon))
    if clon > 180:
        clon -= 360

    hgt_val = float(np.nanmin(smoothed))
    logger.debug("find_center_at_level() complete with valid entries")
    return clat, clon, hgt_val, 1

#this comes from ships and really should be in a separate module
def _filter121_2d(arr, n_iter):
    """
    Apply a 1-2-1 smoothing filter (NCL FILTER121 equivalent) to a 2D
    field. Each iteration smooths along axis 0, then axis 1, on the
    interior; edges are left unchanged. NaNs are filled with the field
    mean before filtering so they don't propagate.

    args: arr, n_iter
        - arr: data array on which to perform the 1-2-1 smoothing filter
        - n_iter: int, how many times to pass the filter

    dependencies: 
        numpy as np
        logging

    returns:
        smoothed array
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
    logger.debug("_filter121_2d() complete")
    return out

#this comes from ships and should be in a separate module
def _match_lon_convention(lon_val, lon_array):
    """Ensure a single lon value matches the convention of the lon array.

    args: lon_val, lon_array
        - lon_val: tc center longitude
        - lon_array: model longitude values from static data table

    dependencies: 
        logging

    returns:
        tc center longitude in correct convenction
    """
    if lon_array.min() >= 0 and lon_array.max() > 180:
        # Data in 0..360
        if lon_val < 0:
            logger.debug("_match_lon_convention() complete, added 360 to raw lon values")
            return lon_val + 360
    elif lon_array.max() <= 180:
        # Data in -180..180
        if lon_val > 180:
            logger.debug("_match_lon_convention() complete, subtracted 360 from raw lon values")
            return lon_val - 360
    logger.debug("_match_lon_convention() complete, raw lon values left alone")
    return lon_val

#this comes from ships and should be in a separate module
def _centroid_min(field):
    """
    Value-weighted centroid of the lower-tail of a 2D field. Replicates
    NCL findCenter type=1 with b=-1 (HGT min mode):
    threshold A = min + 0.20 * (max - min); centroid is the
    (A - field)-weighted mean of grid indices over points where
    field <= A (so the deepest part of the trough dominates).

    args: field
        - field: 2d field on which to perform the centroid algorithm

    dependencies:
        numpy as np 
        logging

    Returns (i_centroid, j_centroid) as float indices into field, or
    (nan, nan) if the field has no usable values.
    """

    if not np.isfinite(field).any():
        logger.warning("_centroid_min() returning nan due to non-finite field values")
        return np.nan, np.nan
    fmin = float(np.nanmin(field))
    fmax = float(np.nanmax(field))
    if fmax == fmin:
        logger.warning("_centroid_min() returning nan due field min == field max")
        return np.nan, np.nan
    A = fmin + 0.20 * (fmax - fmin)
    weights = np.maximum(A - field, 0.0)
    weights = np.nan_to_num(weights, nan=0.0)
    total = float(weights.sum())
    if total <= 0 or not np.isfinite(total):
        logger.warning("_centroid_min() returning nan due to bad weighted-min calculation")
        return np.nan, np.nan
    i_idx, j_idx = np.indices(field.shape)
    i_c = float((i_idx * weights).sum() / total)
    j_c = float((j_idx * weights).sum() / total)
    logger.debug("_centroid_min() returning centroid coordinates")
    return i_c, j_c

#comes from SHIPS and should be in separate module
def _haversine_km(lat1, lon1, lat2, lon2):
    """Great-circle distance in km between two points (degrees).

        args: lat1, lon1, lat2, lon2
        - floats indicating two points 

    dependencies:
        numpy as np 
        logging

    Returns (i_centroid, j_centroid) as float indices into field
    """

    rlat1, rlat2 = np.radians(lat1), np.radians(lat2)
    dlat = np.radians(lat2 - lat1)
    dlon = np.radians(lon2 - lon1)
    a = np.sin(dlat / 2) ** 2 + np.cos(rlat1) * np.cos(rlat2) * np.sin(dlon / 2) ** 2
    ret_val = 2.0 * 6371.0 * np.arcsin(np.sqrt(a))
    logger.debug(f"_haversine_km() returning ret_val: {ret_val}")
    return ret_val

def find_centers_and_shear(members, atcf_centers, fhr, itag,  idir, idate, odir, dsource, ext, fhrfmt, output_timestep):
    """
    Description: Shell function to execute center-finding and shear-calculating function. Summons functions in parallel
        using ProcessPoolExecutor


    args: members, atcf_centers, fhr, itag,  idir, idate, odir, dsource, ext, fhrfmt, output_timestep
        - members: list of 0-padded strings for member ID
        - atcf_centers: pandas DataFrame, subset of hourData containing atcf center coordinates for each member
        - fhr: int, forecast hour
        - itag: str, input file tag for grib2 files
        - idir: str, input data directory for grib2 files
        - idate: str, forecast cycle, YYYYMMDDHH
        - odir: directory for output folders, I think from legacy version probably can remove REMOVE
        - dsource: string, data source for ATCF data - from namelist or from command line. Used for static data lookup
        - ext: file extension substring for file search function
        - fhrfmt: probably '%03d' or whatever to get 3-digit formats, comes from namelist
        - output_timestep: create graphics for every {output_timestep} hours

    dependencies:
        pandas as pd
        concurrent.futures.ProcessPoolExecutor()
        concurrent.futures.as_completed()
        logging

    Returns (outer_dataframe_centers, outer_dataframe_shear)
        pandas dataframes containing tc center and shears at various heights
    """
    
    outer_dataframe_centers = pd.DataFrame()
    outer_dataframe_shear = pd.DataFrame()

    yr = idate[:4]
    if members == 'all':
        memlist = [f'{x:02}' for x in range(21)] if yr == '2025' or yr == '2023' else [f'{x:02}' for x in range(31)]
    else:
        memlist = members


    

    outer_dataframe_centers_list = []
    outer_dataframe_shear_list = []

    with ProcessPoolExecutor(max_workers=22) as executor:

        futures = {
            executor.submit(
                process_ensemble_member,
                ensid,
                atcf_centers,
                idir,
                itag,
                ext,
                idate,
                fhrfmt,
                fhr,
                output_timestep,
                dsource,
            ): ensid
            for ensid in memlist
        }

        for future in as_completed(futures):

            ensid = futures[future]

            try:
                dataframe_centers, dataframe_shear = future.result()

                outer_dataframe_centers_list.append(dataframe_centers)
                outer_dataframe_shear_list.append(dataframe_shear)

            except Exception as e:
                logger.error(
                    f"Failed processing ensemble member {ensid}: {e}"
                )
                raise


    outer_dataframe_centers = pd.concat(
        outer_dataframe_centers_list,
        ignore_index=True,
    )

    outer_dataframe_shear = pd.concat(
        outer_dataframe_shear_list,
        ignore_index=True,
    )

    logger.debug("find_centers_and_shear() completing successfully")
    return outer_dataframe_centers, outer_dataframe_shear

def calculate_tilt_vectors(centers_df):
    """
    Description: Calculate u,v vectors for vortex tilt given the vortex centers at various heights


    args: centers_df
        pandas dataframe with tc centers at various heights

    dependencies:
        numpy as np
        logging

    Returns tilt_data - pandas dataframe containing vortex tilt vectors
    """
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

    logger.debug("calculate_tilt_vectors() complete")
    return tilt_data

############################################### START Experimental parallelization #########################
def process_ensemble_member(ensid,
                            atcf_centers,
                            idir,
                            itag,
                            ext,
                            idate,
                            fhrfmt,
                            fhr,
                            output_timestep,
                            dsource):
    """
    Description: Read grb2 file and calculate centers, shear for one ensemble member.
        To be run in parallel for multiple ensemble members


    args: ensid, atcf_centers, idir, itag, ext, idate, fhrfmt, fhr, output_timestep, dsource
        - ensid: string, ensemble member ID, 0-padded if necessary
        - atcf_centers: pandas dataframe with surface TC centers from ATCF
        - idir: input directory for ATCF data - from namelist or from command line
        - itag: string, tag for grb2 file - from namelist or from command line
        - ext: file extension substring for file search function
        - idate: str, forecast cycle, YYYYMMDDHH
        - fhr_fmt: probably '%03d' or whatever to get 3-digit formats, comes from namelist
        - fhr: int, forecast hour
        - output_timestep: create graphics for every {output_timestep} hours
        - dsource: string, data source for ATCF data - from namelist or from command line. Used for static data lookup

    dependencies:
        logging
        find_grib_files()
        xarray as xr
        compute_shear()
        pandas as pd
        compute_tccen()
        numpy as np

    Returns dataframe_centers, dataframe_shear
        pandas dataframes containing tc centers and shear for one ensemble member
        """

    logger.debug(f"starting loop with ensid={ensid}")

    # TC center
    tc_lat = atcf_centers[
        atcf_centers["member"] == ensid
    ].iloc[0]["latitude"]

    tc_lon = atcf_centers[
        atcf_centers["member"] == ensid
    ].iloc[0]["longitude"]

    # Find GRIB
    grib_files = find_grib_files(
        idir,
        itag,
        ext,
        idate,
        fhrfmt,
        fhr,
        fhr,
        output_timestep,
        ensid,
    )

    if not grib_files:
        raise FileNotFoundError(
            f"No GRIB2 files found for ensid={ensid} in {idir}"
        )

    assert len(grib_files) == 1

    fhr_copy, grib_path = grib_files[0]

    # Read GRIB
    try:
        logger.debug(f"Reading GRIB for ensid={ensid}: {grib_path}")

        datasets = []

        ds = xr.open_dataset(
            grib_path,
            engine="cfgrib",
            filter_by_keys={
                "typeOfLevel": "isobaricInhPa",
                "level": [1000, 500, 350],
                "shortName": ["gh", "u", "v"],
            },
            backend_kwargs={"indexpath": ""},
            decode_timedelta=True,
        )

        datasets.append(ds)

    except Exception as e:
        raise RuntimeError(
            f"Failed to open {grib_path} for ensid={ensid}: {e}"
        ) from e

    # ---------------------------------------------------------
    # SHEAR
    # ---------------------------------------------------------

    shear = compute_shear(
        datasets,
        dsource,
        tc_lat,
        tc_lon,
        lev_top=350,
        lev_bot=1000,
        r_inner=200,
        r_outer=800,
    )

    dataframe_shear = pd.DataFrame(
        [shear],
        columns=["shear_mag_deep", "shear_dir_deep"],
        index=[0],
    )

    dataframe_shear["emem"] = ensid
    dataframe_shear["fhr"] = fhr

    # ---------------------------------------------------------
    # TC CENTERS
    # ---------------------------------------------------------

    centers = compute_tccen(
        datasets,
        dsource,
        tc_lat,
        tc_lon,
        levels=[1000, 500, 350],
    )

    center_rows = []

    for lev, (clat, clon, _hgt, flag) in centers.items():

        if np.isfinite(clat):
            center_rows.append(
                [fhr, lev, clat, clon, flag]
            )

    if center_rows:
        dataframe_centers = pd.DataFrame(
            center_rows,
            columns=["fhr", "lev", "lat", "lon", "vtx"],
        )
    else:
        dataframe_centers = pd.DataFrame(
            columns=["fhr", "lev", "lat", "lon", "vtx"]
        )

    dataframe_centers["emem"] = ensid

    # Close dataset
    ds.close()

    return dataframe_centers, dataframe_shear
    ##################################################################### END EXPERIMENT ###############

def compute_shear(datasets, dsource, tc_lat, tc_lon, lev_top, lev_bot,
                  r_inner=200, r_outer=800):
    """
    Compute vertical wind shear magnitude and heading.

    args: datasets, dsource, tc_lat, tc_lon, lev_top, lev_bot, r_inner, r_outer
        - datasets: list of xarray datasets for which to calculate shear. it is a single dataset, but it was left in
            list format to match SHIPS functionality
        - dsource: string, data source for ATCF data - from namelist or from command line. Used for static data lookup
        - tc_lat: float, storm center latitude
        - tc_lon: float, storm center longitude
        - lev_top: int, top of shear layer height
        - lev_bot: int, bottom of shear layer height
        - r_inner: int, distance to start of "environment" for calculating environmental wind shear. Default 200km
        - r_outer: int, distance to end of "environment" for calculating environmental wind shear. Default 800km

    dependencies:
        gplot_utils.grib_reader.get_var_2d()
        gplot_utils.coord_transform.sph2cart()
        gplot_utils.coord_transform.make_cartesian_grid()
        gplot_utils.coord_transform.compute_wind_shear()
        numpy as np
        logging
        _match_lon_convention()

    Returns (shear_mag_kts, shear_heading_deg) or (nan, nan).
    """
    u_top = get_var_2d(datasets, dsource, 'U', str(lev_top))
    u_bot = get_var_2d(datasets, dsource, 'U', str(lev_bot))
    v_top = get_var_2d(datasets, dsource, 'V', str(lev_top))
    v_bot = get_var_2d(datasets, dsource, 'V', str(lev_bot))

    if any(x is None for x in [u_top, u_bot, v_top, v_bot]):
        logger.error("one of 4 get_var_2d() calls returned None, exiting compute_shear() with (nan, nan)")
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

    logger.debug("compute_shear() complete successfully")
    return shear_mag, shear_dir

def cluster_radius(ct):
    """Wind radius (int) implied by a cluster type; None for non-radius types.
    dependencies:
        logging
    """
    ret_val = int(ct[1:]) if ct in ("R34", "R50", "R64") else None
    logger.debug("cluster_radius() complete")
    return ret_val


def radius_is_plottable(hourData, radius):
    """
    Whether a wind-radii figure is worth making for this radius at this hour. Always makes R34, 
    but R50/R64 are skipped unless at least half of the ensemble members have a nonzero radius 
    there (so the low quartiles don't collapse onto identical zero-radius members).

    args:
        hourData: single-fHour ATCF data for all members (including the appended mean)
        radius: int, 34/50/ 64

    dependencies:
        logging

    returns: bool
    """
    if radius == 34:
        logger.debug(f"radius_is_plottable() complete, returning True")
        return True

    # Exclude the appended ensemble mean (last member) from the member count
    nMembers = max(len(hourData) - 1, 1)
    nNonzero = int((hourData[f'R{radius}'] > 0).sum())
    ret_val = nNonzero >= (nMembers / 2)
    logger.debug(f"radius_is_plottable() complete, returning {ret_val}")
    return ret_val


def parse_bg_fields(nml):
    """
    Parse BG_FIELDS (in format 'VAR:LEVEL') into a list of (variable, level) tuples. 
    Unrecognized pairs are dropped with a warning.

    args:
        nml: master namelist

    depdendencies:
        logging
        regex as re
        BG_FIELD_SPECS: (variable defined in main namespace)

    returns: fields, list of tuples, each tuple containing (VAR, LEVEL)
    """
    # Get raw field and default to 500mb heights if it cannot be found
    raw = nml.get('BG_FIELDS', "HGT:500")
    if raw == "HGT:500":
        logger.warning(f"BG_FIELDS not set, falling back to default: {raw}")

    # Convert to list of VAR:LEVEL pairs if necessary
    tokens = raw if isinstance(raw, list) else re.split(r'[,\s]+', str(raw).strip())

    # Convert to list of (VAR, LEVEL) tuples, exclude any combos that aren't allowed
    fields, bad = [], []
    for tok in [t for t in tokens if t]:
        m = re.fullmatch(r'([A-Za-z_]+):(\d+)', tok)
        pair = (m.group(1).upper(), int(m.group(2))) if m else None
        if pair in BG_FIELD_SPECS:
            fields.append(pair)
        else:
            bad.append(tok)

    if bad:
        logger.warning(f"Ignoring unrecognized BG_FIELDS entry/entries: {bad}")

    fields = list(dict.fromkeys(fields))  # de-dup, preserve order
    logger.info(f"Background fields to plot --> {[f'{v}:{l}' for v, l in fields]}")
    return fields

def write_unplotted_file_list(file_path, file_list):
    with open(file_path, 'w') as f:
        for line in file_list:
            f.write(f"{line}\n")

def read_unplotted_file_list(file_path):
    with open(file_path, 'r') as f:
        out_list=[x.strip() for x in f.readlines()]
    return out_list
    


###################################################################################################################################

def main():



    matplotlib.use('Agg')

    # Parse command-line arguments -------------------------------------------------------------------------------
    parser = argparse.ArgumentParser(description='GPLOT ens_compare: ensemble comparison plots')

    # ALL configuration is read solely from the master namelist except for initialization date
    # and SID, which are the per-invocation identity (which cycle, which storm)
    parser.add_argument('--master-nml', dest='master_nml', required=True,
                        help='Path to the master namelist (carries all configuration)')
    parser.add_argument('--idate', type=str, required=True, help='Forecast cycle YYYYMMDDHH')
    parser.add_argument('--sid', type=str, required=True, help='Storm ID, e.g. 13L')
    parser.add_argument('-v', '--verbose', type=int, default=0,
                        help='Verbosity level')

    args = parser.parse_args()

    # Configure logging
    log_level = logging.DEBUG if args.verbose > 0 else logging.INFO

    logging.basicConfig(level=log_level,
                        format='%(name)s %(levelname)s: %(message)s')

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
    #DONT NEED ANYMORE - CAN REMOVE
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
    bgFields = parse_bg_fields(nml)  # [(variable, level), ...] for clustering plots
    #MD 20260818 - these two variabels below are legacy. 
    #including for now while I merge other changes
    variable = nml.get('BG_VARIABLE', 'HGT')  # variable to plot under ATCF tracks
    level = int(nml.get('BG_LEVEL', 500))  # atmospheric level to plot for (if applicable)

    # Cluster types to generate graphics for
    ALLOWED_CLUSTER_TYPES = ["MSLP", "R34", "R50", "R64", "ltrack", "xtrack"]

    # Normalize input into a clean list of clusterTypes, default to all
    _ct_raw = nml.get('CLUSTER_TYPES', 'all')
    if _ct_raw=='all':
        clusterTypes = ALLOWED_CLUSTER_TYPES
    else:
        if isinstance(_ct_raw, list):
            clusterTypes = [str(_c).strip() for _c in _ct_raw if str(_c).strip()]
        else:
            clusterTypes = [_c for _c in re.split(r'[,\s]+', str(_ct_raw).strip()) if _c]

    # Remove bad clusterType inputs and notify user
    _badTypes = [_c for _c in clusterTypes if _c not in ALLOWED_CLUSTER_TYPES]
    if _badTypes:
        logger.warning(f"Ignoring unrecognized CLUSTER_TYPES value(s): {_badTypes}")
        clusterTypes = [_c for _c in clusterTypes if _c in ALLOWED_CLUSTER_TYPES]

    clusterTypes = list(dict.fromkeys(clusterTypes))  # de-dup, preserve order
    logger.info(f"Cluster types to plot --> {clusterTypes}")

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


    #Define status file and lockfile names
    STATUS_FILE=os.path.join(ODIR_full, f"status.ens_compare.{idate}.{sid.lower()}.log")
    ST_LOCK_FILE = f'{STATUS_FILE}.lock'







    # Main execution --------------------------------------------------------------------------------------

    t_script_start = time.perf_counter()  # Doing some timing for testing purposes, not necessary but helpful to quickly gauge speed issues

    logger.info(f"GPLOT Ens Comparison starting: {sid} {idate}")
    logger.info(f"  DSOURCE={dsource} EXPT={expt}")
    logger.info(f"  IDIR={idir}")
    logger.info(f"  ODIR={ODIR_full}")

    #read in UnplottedFiles
    UnplottedFilePath = f"{ODIR_full}/UnplottedFiles.{expt}.{idate}.{sid.lower()}.dat"
    # with open(UnplottedFilePath, 'r') as f:
    #     UnplottedFilesList=[x.strip() for x in f.readlines()]
    UnplottedFilesList=read_unplotted_file_list(UnplottedFilePath)


    # Load ATCF data once for all forecast hours
    adeckData, members = modifyAdeckData(members, idir, initDate, storm, clusterMembers, fHours)
    
    #flag whether the atcf has all required forecast hours for requested plots. NOTE MD 20260819
    #Need to adjust how we check whether all expected members are present. For 2026, this will change per forecast hour, so need to think about it
    #leaving member_check as variable placeholder
    #also, current "fnl_hr" check is only checking whether ANY members have the final hour, and we should really check whether ALL members have the final hour,
    #but will leave that until after we decide how to check that lal members are present
    member_check = True
    if (fnl_hr in adeckData['TAU']) and member_check:
        ALL_DATA_PRESENT = True
    else:
        ALL_DATA_PRESENT = False

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
        logger.info(f"\n{'-'*60}\nProcessing forecast hour: {fHour}\n{'-'*60}\n")

        #check if there are any unplotted files at this hour
        if all(f'f{fHour:03}' not in x for x in UnplottedFilesList):
            logger.info(f"All files already processed for hour: {fHour}. Skipping...")
            continue

        t_hour_start = time.perf_counter()

        hourData = getHourData(fHour, adeckData)

        # Shared second title line for every plot this hour 
        titleLine = (f"{name} | Forecast Hour {fHour} | "
                    f"Initialized at {hour:02}Z {calendar.month_name[month]} {day:02} {year}")

        # Wind radii depends on radius, not clusterType, so it runs once per hour per radius
        if ensembleWindRadii:
            for _rad in requestedRadii:
                #check if plot is still needed - NOTE: if we change to .gif, this needs to update
                current_windRad_filename = f'{sid.lower()}.{initDate}.wind_radii.R{_rad}.f{fHour:03}.png'
                if current_windRad_filename not in UnplottedFilesList:
                    logger.debug(f'skipping plot: {current_windRad_filename} because it is not in unplottedfileslist.')
                    continue

                # Skip if less than half the available members have nonzero rXX values, and remove from UnplottedFilesList
                if not radius_is_plottable(hourData, _rad):
                    logger.warning(f"fHour {fHour}: skipping R{_rad} wind-radii plot, too few members")
                    UnplottedFilesList.remove(current_windRad_filename)
                    continue
                
                t_step_start = time.perf_counter()
                adeckRadiiData, radData = windRadiiData(hourData, _rad)
                plotWindRadii(adeckRadiiData, radData, ODIR_full, fHour, storm, 
                            _rad, initDate, adeckData, titleLine)
                #if plot contains all requested forecast hours, remove the file from the unplotted files list and overwrite the file
                if ALL_DATA_PRESENT:
                    UnplottedFilesList.remove(current_windRad_filename)
                    write_unplotted_file_list(UnplottedFilePath,UnplottedFilesList)
                t_elapsed = time.perf_counter() - t_step_start
                timing_totals['ensembleWindRadii'] += t_elapsed
                timing_counts['ensembleWindRadii'] += 1

        for clusterType in clusterTypes:
            logger.info(f"Forecast hour: {fHour}; cluster type: {clusterType}")

            #first, check if we can skip this cluster type because it is already plotted.
            if all(f'{clusterType}.f{fHour:03}' not in x for x in UnplottedFilesList):
                logger.info(f"All files already processed for cluster type {clusterType} for hour: {fHour}. Skipping...")
                continue

            if ensembleClustering or vortexAvgSteer:
                allClusterMems = getClusterMems(clusterType, hourData, clusterMembers)
                skipClustering = set(allClusterMems[0]) == set(allClusterMems[1])
                if skipClustering:
                    #skip these clusters and remove from unplotted files list
                    logger.warning(f"fHour {fHour}: {clusterType} produced identical clusters; "
                                   f"skipping clustering/vortex plots and removing from unplotted files list")
                    #make list of files to remove and remove them
                    #can't just iterate and find matching files then remove them because
                    #that has possibility to miss files because file indices change when
                    #a file is removed
                    files_to_remove = []
                    for file in UnplottedFilesList:
                        if f'{clusterType}.f{fHour:03}' in file:
                            files_to_remove.append(file)
                    for file in files_to_remove:
                        UnplottedFilesList.remove(file)
                    del files_to_remove
                    write_unplotted_file_list(UnplottedFilePath,UnplottedFilesList)

            if ensembleLinePlots:
                current_lineplot_filename = f'{sid.lower()}.{initDate}.line_plot.{clusterType}.f{fHour:03}.png'
                #skip if not in unplotted list
                if current_lineplot_filename not in UnplottedFilesList:
                    logger.debug(f'skipping plot: {current_lineplot_filename} because it is not in unplottedfileslist.')
                else:
                    t_step_start = time.perf_counter()
                    avgVarTypes = sortedColoringData(clusterType, hourData, members)
                    plotLinePlots(avgVarTypes, members, adeckData, ODIR_full, clusterType, 
                                fHour, storm, cluster_radius(clusterType), initDate, titleLine)
                    #if plot contains all required forecast hours, remove it from unplotted files list
                    if ALL_DATA_PRESENT:
                        UnplottedFilesList.remove(current_lineplot_filename)
                        write_unplotted_file_list(UnplottedFilePath,UnplottedFilesList)
                    t_elapsed = time.perf_counter() - t_step_start
                    timing_totals['ensembleLinePlots'] += t_elapsed
                    timing_counts['ensembleLinePlots'] += 1

            if ensembleTracksColored:
                current_trackplot_filename = f'{sid.lower()}.{initDate}.spatial_tracks.{clusterType}.f{fHour:03}.png'
                #skip if not in unplotted list
                if current_trackplot_filename not in UnplottedFilesList:
                    logger.debug(f'skipping plot: {current_trackplot_filename} because it is not in unplottedfileslist.')
                else:
                    t_step_start = time.perf_counter()
                    avgVarTypes = sortedColoringData(clusterType, hourData, members)
                    plotTracksColored(avgVarTypes, members, adeckData, ODIR_full, clusterType, 
                                    fHour, storm, cluster_radius(clusterType), initDate, titleLine)
                    #if plot contains all required forecast hours, remove it from unplotted files list
                    if ALL_DATA_PRESENT:
                        UnplottedFilesList.remove(current_trackplot_filename)
                        write_unplotted_file_list(UnplottedFilePath,UnplottedFilesList)
                    t_elapsed = time.perf_counter() - t_step_start
                    timing_totals['ensembleTracksColored'] += t_elapsed
                    timing_counts['ensembleTracksColored'] += 1

            if ensembleClustering and not skipClustering:
                for bgVariable, bgLevel in bgFields:
                    current_track_clustering_filename = f'{sid.lower()}.{initDate}.{bgVariable}{bgLevel}.spatial_cluster.{clusterType}.f{fHour:03}.png'
                    #skip if not in unplotted files list
                    if current_track_clustering_filename not in UnplottedFilesList:
                        logger.debug(f'skipping plot: {current_track_clustering_filename} because it is not in unplottedfileslist.')
                    else:
                        t_step_start = time.perf_counter()
                        atcfClusters, gribClusters, clusterAvgs = trackClusteringData(
                            clusterType, bgVariable, bgLevel, fHour, adeckData, sid, expt, allClusterMems, 
                            idir, initDate, hourData)
                        plotTrackClustering(atcfClusters, gribClusters, clusterAvgs, ODIR_full, allClusterMems, clusterType, 
                                            clusterTypeDict, fHour, storm, bgLevel, bgVariable, cluster_radius(clusterType), initDate, titleLine)
                        #if plot contains all required forecast hours, remove it from unplotted files list
                        if ALL_DATA_PRESENT:
                            UnplottedFilesList.remove(current_track_clustering_filename)
                            write_unplotted_file_list(UnplottedFilePath,UnplottedFilesList)
                        t_elapsed = time.perf_counter() - t_step_start
                        timing_totals['ensembleClustering'] += t_elapsed
                        timing_counts['ensembleClustering'] += 1

            if vortexAvgSteer and not skipClustering:
                current_vortex_clustering_filename = f'{sid.lower()}.{initDate}.wind.vortex_cluster.{clusterType}.f{fHour:03}.png'
                #skip if not in unplotted files list
                if current_vortex_clustering_filename not in UnplottedFilesList:
                    logger.debug(f'skipping plot: {current_vortex_clustering_filename} because it is not in unplottedfileslist.')
                else:
                    t_step_start = time.perf_counter()
                    clusterDicts = vortexAvgSteerData(fHour, idir, initDate, hourData, 
                                                    storm, sid, expt, adeckData, allClusterMems)
                    plotVortexAvgSteer(clusterDicts, ODIR_full, storm, initDate, clusterType, fHour, 
                                    clusterTypeDict, cluster_radius(clusterType), titleLine)
                    #plot does not depend on all required forecast hours, remove it from unplotted files list
                    UnplottedFilesList.remove(current_vortex_clustering_filename)
                    write_unplotted_file_list(UnplottedFilePath,UnplottedFilesList)
                    t_elapsed = time.perf_counter() - t_step_start
                    timing_totals['vortexAvgSteer'] += t_elapsed
                    timing_counts['vortexAvgSteer'] += 1

        # Tilt plots do not depend on clusterType, so they run once per forecast hour
        if tiltPlots:
            current_tilt_filename = f'{sid.lower()}.{initDate}.vortex_tilt.f{fHour:03}.png'
            #skip if not in unplotted files list
            if current_tilt_filename not in UnplottedFilesList:
                logger.debug(f'skipping plot: {current_tilt_filename} because it is not in unplottedfileslist.')
            else:
                t_step_start = time.perf_counter()

                plot_tilts(adeckData,itag,idir,dsource, ODIR_full, ext, fhrfmt, dt,
                cycle = idate, 
                fhr=int(fHour),
                storm_id = storm[:4].upper(),
                members_to_plot = [f'{x:02}' for x in members[:-1]],
                show=False)
                t_elapsed = time.perf_counter() - t_step_start
                timing_totals['tiltPlots'] += t_elapsed
                timing_counts['tiltPlots'] += 1
                #plot does not depend on all required forecast hours, remove it from unplotted files list
                UnplottedFilesList.remove(current_tilt_filename)
                write_unplotted_file_list(UnplottedFilePath,UnplottedFilesList)


    logger.info(f"\nTotal Python time for all hours: {time.perf_counter() - t_script_start:.4f}s")

    logger.info("Average time per forecast hour, by plot type:")
    for _label, _seconds in timing_totals.items():
        _n = timing_counts[_label]
        if _n > 0:
            logger.info(f"  {_label:<25} {_seconds / _n:>10.4f}s  (n={_n})")

    # Mark this (cycle, storm) case complete so the HAFS workflow's status check
    # (find -name 'status.*') sees ens_compare finish. Must match the path/key the
    # spawn writes 'working' to: ODIR/ensembleComparison/status.ens_compare.<idate>.<sid>.log
    try:
        # _status_file = os.path.join(ODIR_full, f"status.ens_compare.{idate}.{sid.lower()}.log")
        # with open(_status_file, 'w') as _sf:
        #     _sf.write("complete\n")
        #implementing lockfile from polar - MD 20260804
        os.system(f'lockfile -r-1 -l 180 {ST_LOCK_FILE}')
        os.system(f'echo "complete" > {STATUS_FILE}')
        os.system(f'rm -f {ST_LOCK_FILE}')
        logger.info(f"MSG: Wrote status 'complete' --> {STATUS_FILE}")
    except Exception as _status_err:
        logger.error(f"WARNING: Could not write status file: {_status_err}")

##############################
if __name__ == '__main__':
  main()