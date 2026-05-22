"""
Name: HAFS Ensemble Plotting Script for GPLOT
Author: Nikhil Trivedi
Description:
This script reads HAFS ensemble ATCF data, computes member statistics and rankings, and generates 
diagnostic plots for HAFS TC ensemble forecasts. All plot types and parameters are controlled via 
command-line arguments, allowing the script to be called in a loop over forecast hours from a shell 
script. Bad file paths and missing member data in the ATCF file is handled safely.

Plot types:
1. Ensemble Line Plots:      MSLP vs. forecast hour for all members, colored by rank for a user-chosen metric.
2. Ensemble Tracks Colored:  Storm tracks on a map for all members, colored by rank for a user-chosen metric.
3. Ensemble Wind Radii:      Wind radii (R34/R50/R64) member quartiles plotted for the forecast hour.
4. Ensemble Clustering:      Extreme members for a user-chosen metric grouped into clusters and plotted over 
                             a user-chosen background atmospheric field (averaged over members).
5. Vortex Average Steering:  Extreme members for a user-chosen metric grouped into clusters and averaged into
                             a 2-panel vortex structure plot with shear/motion diagnostics.

Last modified May 21, 2026
"""

import time
import subprocess
import os
import sys
import argparse
import concurrent.futures

import pandas as pd
import numpy as np
import xarray as xr
from pyproj import Proj, Transformer

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from matplotlib.colors import LinearSegmentedColormap
from matplotlib.lines import Line2D
from matplotlib.ticker import LogLocator
from mpl_toolkits.axes_grid1.inset_locator import inset_axes

import cartopy.crs as ccrs
import cartopy.feature as cf
import cartopy.io.shapereader as shpreader

import HepTools as uf

# ----------------------------------------------------------------------------------------------------
# initialize data
# ----------------------------------------------------------------------------------------------------

def modifyAdeckData(radius):
    global members

    try:
        adeckData = uf.process_atcf_files(cycle_path=f'{baseDataPath}/{initDate}', timestamp=str(initDate),
                                          storm_id=f"{storm[:2].upper()}{storm[2:4]}", benchmark=None)
        if adeckData.empty:
            print(f"ATCF file exists but contains no data for storm {storm}")
            sys.exit(1)
    except Exception as e:
        print(f"Error reading ATCF data at {baseDataPath} for storm {storm}, init {initDate}: {e}")
        sys.exit(1)

    # map radius to the correct columns
    rad_col_map = {
        34: ('r34',  'rad1',     'rad2',     'rad3',     'rad4'),
        50: ('r50',  'r50_rad1', 'r50_rad2', 'r50_rad3', 'r50_rad4'),
        64: ('r64',  'r64_rad1', 'r64_rad2', 'r64_rad3', 'r64_rad4'),
    }
    mean_col, q1_col, q2_col, q3_col, q4_col = rad_col_map[radius]

    # rename to match names in remainder of script
    adeckData = adeckData.rename(columns={'fhr': 'TAU', 'emem': 'member', 'lon': 'longitude', 'lat': 'latitude', 
                                          'mslp': 'MSLP', 'mdl': 'TECH', 'tdir': 'DIR', 'tspeed': 'SPEED'})

    # convert longitude from -180/180 to 0-360 to match GRIB files
    adeckData['longitude'] = adeckData['longitude'] % 360

    # build RAD1-4 and RadMean from the appropriate radius columns
    adeckData['RAD'] = radius
    adeckData['RAD1'] = adeckData[q1_col]
    adeckData['RAD2'] = adeckData[q2_col]
    adeckData['RAD3'] = adeckData[q3_col]
    adeckData['RAD4'] = adeckData[q4_col]
    adeckData['RadMean'] = adeckData[mean_col]

    # for non-34kt radii, fill missing rows (has r34 but no data for this radius) with zeros
    if radius != 34:
        missing_mask = adeckData['r34'].notna() & (adeckData[mean_col].isna() | (adeckData[mean_col] == 0))
        if missing_mask.any():
            print(f"Zero-filling {missing_mask.sum()} row(s) with missing {radius}kt radii")
            adeckData.loc[missing_mask, ['RAD1', 'RAD2', 'RAD3', 'RAD4', 'RadMean']] = 0

    # update members to only those present in the data
    members = [m for m in members if m in adeckData['member'].unique()]

    # select final columns
    finalCols = ['TECH', 'TAU', 'latitude', 'longitude', 'MSLP', 'RAD', 'RadMean',
                 'RAD1', 'RAD2', 'RAD3', 'RAD4', 'DIR', 'SPEED', 'member']
    adeckData = adeckData[finalCols].sort_values(by=["member", "TAU"]).reset_index(drop=True)

    # filter out members with incomplete track data
    allTaus = set(adeckData['TAU'].unique())
    memberTauCounts = adeckData.groupby('member')['TAU'].apply(set)
    incompleteMembers = memberTauCounts[memberTauCounts.apply(lambda x: x != allTaus)].index.tolist()
    if incompleteMembers:
        print(f"Skipping {len(incompleteMembers)} member(s) with incomplete track data: {incompleteMembers}")
        adeckData = adeckData[~adeckData['member'].isin(incompleteMembers)]
        members = [m for m in members if m not in incompleteMembers]
    else:
        print("All members have complete track data.")

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


def getHourData(fHour):
    # get ATCF data for specific forecastHour with mean-relative along/across track data 
    hourData = uf.getTrackSpeedData(adeckData, fHour)
    print(hourData)
    return hourData


def getClusterMems(clusterType):
    # get members in each cluster
    allClusterMems = []
    allClusterMems.append(hourData.nsmallest(clusterMembers, clusterType)["member"].tolist())
    allClusterMems.append(hourData.nlargest(clusterMembers, clusterType)["member"].tolist())
    return allClusterMems


# ----------------------------------------------------------------------------------------------------
# calculate graphic-specific data
# ----------------------------------------------------------------------------------------------------

def sortedColoringData(clusterType):
    # get sorted attribute data specified by clusterType
    avgVarTypes = hourData[['member', clusterType]].copy()
    ascendingOrder = True if clusterType == 'MSLP' else False
    avgVarTypes['rank'] = avgVarTypes[clusterType].rank(method='min', ascending=ascendingOrder).astype(int)
    
    return avgVarTypes


def windRadiiData():
    # get a difference matrix between every member in hourData and every quartile member
    percentiles = hourData['RadMean'].quantile([0, 0.25, 0.5, 0.75, 1.0]).to_numpy()
    radiiArray = hourData['RadMean'].to_numpy()[:, None]
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
        radii = ['RAD1', 'RAD2', 'RAD3', 'RAD4']

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
            allQuadrants.extend(np.repeat([radii[idx]], len(latPoints)))
            
    # combine all data into a single DataFrame
    radData = pd.DataFrame({'lat': allLatPoints, 'lon': allLonPoints, 'percentile': allPercentiles, 'quadrant': allQuadrants})  
    return quartileData, radData


def trackClusteringData(clusterType, variable, level, fHour):
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
        try:
            gribData = uf.getGribData(f'{baseDataPath}', bounds, members=clusterMems, initDate=initDate, 
                                      variable=variable, fHour=fHour, level=level)
            if gribData is None or len(gribData.data_vars) == 0:
                print(f"No GRIB data returned for cluster {idx}, storm {storm}")
                sys.exit(1)

        except FileNotFoundError as e:
            print(f"GRIB file not found at {baseDataPath} for storm {storm}, init {initDate}: {e}")
            sys.exit(1)
        except Exception as e:
            print(f"Error reading GRIB data  at {baseDataPath} for storm {storm}, init {initDate}: {e}")
            sys.exit(1)
       
        # get cluster-averaged MSLP or radius at fHour
        clusterHourData = hourData[hourData["member"].isin(clusterMems)]
        if clusterType in ["MSLP", "RadMean"]:
            clusterAvg = clusterHourData[clusterType].mean()
        else:
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
    

def vortexAvgSteerData(fHour):
    
    def _process_single_vortex(cluster_idx, clusterMems):
        # get ATCF center data
        centerData = hourData[hourData["member"].isin(clusterMems)]
        centers = dict(zip(centerData["member"], zip(centerData["latitude"], centerData["longitude"])))

        # load 5x5 degree centered wind data into memory, handle errors
        try:
            windData_xy = uf.getGribData(f'{baseDataPath}', centers, variable=['UGRD', 'VGRD'], members=clusterMems, 
                                            initDate=initDate, fHour=fHour)
            if windData_xy is None or len(windData_xy.data_vars) == 0:
                print(f"No GRIB data returned for cluster {cluster_idx}, storm {storm}")
                sys.exit(1)
            
        except FileNotFoundError as e:
            print(f"GRIB file not found at {baseDataPath} for storm {storm}, init {initDate}: {e}")
            sys.exit(1)
        except Exception as e:
            print(f"Error reading GRIB data at {baseDataPath} for storm {storm}, init {initDate}: {e}")
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
    

# ----------------------------------------------------------------------------------------------------
# plotting code
# ----------------------------------------------------------------------------------------------------

def plotCartopyFigure(ax, plotLand=True):
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


def plotSortedLines(ax, avgVar, plotType, members):
    colors = plt.cm.viridis(np.linspace(0, 1, len(members)))
    
    # plot lines based on property of interest
    for member in members:
        memberData = adeckData[adeckData['member'] == member]
        color = colors[avgVar[avgVar['member'] == member]['rank'] - 1]

        if plotType == "line":
            if member == members[-1]:
                dotSize, lineThickness, opacity, zorder = 50, 3, 1, member * 2 + 2
            else:
                dotSize, lineThickness, opacity, zorder = 15, 0.8, 0.5, member * 2 + 3
            
            ax.plot(memberData['TAU'], memberData['MSLP'], color=color, linewidth=lineThickness, 
                    alpha=opacity, zorder=zorder)
            ax.scatter(memberData['TAU'], memberData['MSLP'], color=color, s=dotSize, 
                       alpha=opacity, zorder=zorder)
        elif plotType == "track":
            if member == members[-1]:
                dotSize, lineThickness, opacity, zorder = 25, 2.5, 1, member * 2 + 2
            else:
                dotSize, lineThickness, opacity, zorder = 5, 1.2, 0.7, member * 2 + 3
                            
            ax.plot(memberData['longitude'] - 180, memberData['latitude'],
                    color=color, linewidth=lineThickness, alpha=opacity, zorder=zorder)
            ax.scatter(memberData['longitude'] - 180, memberData['latitude'],
                       color=color, s=dotSize, alpha=opacity, zorder=zorder)

    sortTitle = typeDict[clusterType][0] if 'RadMean' else clusterType
    
    sm = plt.cm.ScalarMappable(cmap=plt.cm.viridis, norm=plt.Normalize(vmin=1, vmax=len(members)))
    sm.set_array([])  # Needed to avoid warning
    cbar = plt.colorbar(sm, ax=plt.gca(), pad=0.015, aspect=27)
    cbar.ax.tick_params(labelsize=8)
    cbarTitle = 'MSLP' if plotType == 'track' else clusterType 
    cbar.set_label(f'Member Mean {sortTitle} Rank', fontsize=9, weight='bold')
    cbar.ax.invert_yaxis()
    cbar.set_ticks(range(1, 22, 2))

    fixedVar = "Track" if plotType == "track" else "MSLP"
    title = f"HAFS Ensemble {fixedVar} Colored by {sortTitle} at Forecast Hour {fHour}"
    subTitle = f"\nInitialized at {hour:02}Z {monthsDict[month]} {day:02} {year}"
    plt.title(title + subTitle, fontsize=9, weight='bold', loc='left')

    return ax
            

def plotLinePlots(avgVarTypes, members):
    # plot figure and title
    plt.close('all')
    plt.figure(figsize=(10, 6))
    ax = plt.gca()
    
    # plot lines based on property of interest
    ax = plotSortedLines(ax, avgVarTypes, 'line', members)
    
    ax.set_xlabel('Time in Hours', fontsize=9, weight='bold')
    ax.set_ylabel('MSLP', fontsize=9, weight='bold')

    sortSave = f"R{radius}" if clusterType == 'RadMean' else clusterType
    plt.savefig(rf"{savePath}/{storm[2:4]}l.{initDate}.line_plot.{sortSave}.f{fHour:03d}.png", dpi=200, bbox_inches='tight')


def plotTracksColored(avgVarTypes, members):
    plt.close('all')
    plt.figure(figsize=(10, 6))
    ax = plt.axes(projection=ccrs.PlateCarree(central_longitude=180))
    
    ax = plotCartopyFigure(ax)
    ax = plotSortedLines(ax, avgVarTypes, 'track', members)

    sortSave = f"R{radius}" if clusterType == 'RadMean' else clusterType
    plt.savefig(rf"{savePath}/{storm[2:4]}l.{initDate}.spatial_tracks.{sortSave}.f{fHour:03d}.png", dpi=200, bbox_inches='tight')


def plotWindRadii(quartileData, radData):
    plt.close('all')
    colors = plt.cm.viridis(np.linspace(0, 1, len(quartileData)))

    plt.figure(figsize=(10, 6))
    ax = plt.axes(projection=ccrs.PlateCarree(central_longitude=180))

    ax = plotCartopyFigure(ax)
    
    filename = ""
    
    meanLon, meanLat = quartileData['longitude'].mean(), quartileData['latitude'].mean()
    ax.set_extent([meanLon-8, meanLon+8, meanLat-6, meanLat+6])

    ax.scatter(quartileData['longitude'], quartileData['latitude'], color=colors, zorder=100, s=20, transform=ccrs.PlateCarree())

    # add titling
    name = uf.getStormName(storm, initDate)
    title = f"HAFS Ensemble {name} {radius}kt Wind Radii Quartiles at Forecast Hour {fHour}"
    subTitle = f"\nInitialized at {hour:02}Z {monthsDict[month]} {day:02} {year}"
    ax.set_title(title + subTitle, fontsize=9, weight='bold', loc='left')

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

    
def plotTrackClustering(atcfClusters, gribClusters, clusterAvgs):
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
    titleDict = {"MSLP": "MSLP", "RadMean": f"Radius of {radius}kt Winds", "ltrack": "Across Track Variation", 
                 "xtrack": "Along Track Variation", "vortexDepth": "Vortex Depth"}

    name = uf.getStormName(storm, initDate)
    mainTitle = f"HAFS Ensemble {name} 500mb Heights and Tracks Clustered By {titleDict[clusterType]}"
    subTitle = uf.getTitleDate(year, month, day, hour, fHour)
    fig.suptitle(mainTitle + subTitle, fontsize=10, weight='bold')

    plt.savefig(rf"{savePath}/{storm[2:4]}l.{initDate}.{variable}.spatial_cluster.{clusterType}.f{fHour:03d}.png", dpi=200, bbox_inches='tight')


def plotVortexAvgSteer(clusterDicts):
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
        motionVect = inset_ax.quiver(0.5, 0.5, uMotion, vMotion, angles='xy', scale_units='xy', scale=1, color='k', width=0.012, 
                                     headwidth=3, headlength=4.5, zorder=3)
        steerVect = inset_ax.quiver(0.5, 0.5, uSteer, vSteer, angles='xy', scale_units='xy', scale=1, color='#00AAFF', width=0.012, 
                                    headwidth=3, headlength=4.5, zorder=3, edgecolors='black', linewidths=0.3)
        shearVect = inset_ax.quiver(0.5, 0.5, uShear, vShear, angles='xy', scale_units='xy', scale=1, color='orange', width=0.012, 
                                    headwidth=3, headlength=4.5, zorder=3, edgecolors='black', linewidths=0.3)

    cbar = fig.colorbar(contourf, ax=axes, pad=0.04, aspect=40, orientation='horizontal')
    cbar.ax.tick_params(labelsize=8)

    # add titling
    titleDict = {"MSLP": "MSLP", "RadMean": f"Radius of {radius}kt Winds", "ltrack": "Across Track Variation", 
                 "xtrack": "Along Track Variation", "vortexDepth": "Vortex Depth"}

    name = uf.getStormName(storm, initDate)
    mainTitle = f"HAFS Ensemble {name} Rad Avg Wind (kts) Clustered By {titleDict[clusterType]}"
    subTitle = uf.getTitleDate(year, month, day, hour, fHour)
    fig.suptitle(mainTitle + subTitle, fontsize=10, weight='bold')

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


###################################################################################################################################

# Parse command-line arguments ------------------------------- 
parser = argparse.ArgumentParser(description='Ensemble plotting script')

# Toggle arguments
parser.add_argument('--ensembleLinePlots', type=lambda x: x.lower() == 'true', required=True)
parser.add_argument('--ensembleTracksColored', type=lambda x: x.lower() == 'true', required=True)
parser.add_argument('--ensembleWindRadii', type=lambda x: x.lower() == 'true', required=True)
parser.add_argument('--ensembleClustering', type=lambda x: x.lower() == 'true', required=True)
parser.add_argument('--vortexAvgSteer', type=lambda x: x.lower() == 'true', required=True)

# Parameter arguments
parser.add_argument('--fHours', type=int, nargs='+', required=True)
parser.add_argument('--clusterType', type=str, required=True)
parser.add_argument('--variable', type=str, required=True)
parser.add_argument('--level', type=int, required=True)
parser.add_argument('--membersStart', type=int, required=True)
parser.add_argument('--membersEnd', type=int, required=True)
parser.add_argument('--clusterMembers', type=int, required=True)

# Date/time arguments
parser.add_argument('--date', type=str, required=True)

# Storm and path arguments
parser.add_argument('--storm', type=str, required=True)
parser.add_argument('--baseDataPath', type=str, required=True)
parser.add_argument('--savePath', type=str, required=True)

args = parser.parse_args()

# Assign variables from arguments -------------------------------
baseDataPath = args.baseDataPath

# namelist section (toggles and input variables)
ensembleLinePlots = args.ensembleLinePlots
ensembleTracksColored = args.ensembleTracksColored
ensembleWindRadii = args.ensembleWindRadii
ensembleClustering = args.ensembleClustering
vortexAvgSteer = args.vortexAvgSteer

# parameter lists
fHours = args.fHours  # list of forecast hours to process
clusterType = args.clusterType  # MSLP, R34, R50, R64, ltrack, xtrack
variable = args.variable  # variable to plot under ATCF tracks
level = args.level  # atmospheric level to plot for (if applicable)

# static parameters
members = range(args.membersStart, args.membersEnd)  # members to use
clusterMembers = args.clusterMembers  # number of members to include in each cluster

# Parse date string into components
date_str = args.date
year = int(date_str[0:4])
month = int(date_str[4:6])
day = int(date_str[6:8])
hour = int(date_str[8:10])

storm = args.storm  # storm to plot data for

# input-dependent variables
initDate = int(f"{year:04d}{month:02d}{day:02d}{hour:02d}")
savePath = args.savePath
os.makedirs(savePath, exist_ok=True)

radius = int(clusterType[-2:]) if clusterType in ["R34", "R50", "R64"] else 34
clusterType = "RadMean" if clusterType in ["R34", "R50", "R64"] else clusterType

# dictionaries for conversions and static variables
monthsDict = {1: "Jan", 2: "Feb", 3: "Mar", 4: "Apr", 5: "May", 6: "Jun", 
              7: "Jul", 8: "Aug", 9: "Sep", 10: "Oct", 11: "Nov", 12: "Dec"}
typeDict = {"MSLP": ["Intensity", "MSLP (hPa)"], 
            "RadMean": [f"{radius}kt Avg Wind Radius", "Radius (km)"], 
            "ltrack": ["Across Track Deviation", "Distance (km)"], 
            "xtrack": ["Along Track Deviation", "Distance (km)"]}
clusterTypeDict = {"MSLP": ["Strong", "Weak"], 
                   "RadMean": [f"R{radius} Small", f"R{radius} Large"], 
                   "ltrack": ["Left of Track", "Right of Track"], 
                   "xtrack": ["Slow", "Fast"], 
                   "vortexDepth": ["Shallow", "Deep"]}

# ----------------------------------------------------------------------------------------------------
# Top-level execution
# ----------------------------------------------------------------------------------------------------

t_script_start = time.perf_counter()

# Load ATCF data once for all forecast hours
adeckData, members = modifyAdeckData(radius)

# Loop over all requested forecast hours
for fHour in fHours:
    print(f"\n{'='*60}")
    print(f"Processing forecast hour: {fHour}")
    print(f"{'='*60}\n")

    t_hour_start = time.perf_counter()

    hourData = getHourData(fHour)

    if ensembleClustering or vortexAvgSteer:
        allClusterMems = getClusterMems(clusterType)

    if ensembleLinePlots:
        avgVarTypes = sortedColoringData(clusterType)
        plotLinePlots(avgVarTypes, members)

    if ensembleTracksColored:
        avgVarTypes = sortedColoringData(clusterType)
        plotTracksColored(avgVarTypes, members)

    if ensembleWindRadii:
        adeckRadiiData, radData = windRadiiData()
        plotWindRadii(adeckRadiiData, radData)

    if ensembleClustering:
        atcfClusters, gribClusters, clusterAvgs = trackClusteringData(clusterType, variable, level, fHour)
        plotTrackClustering(atcfClusters, gribClusters, clusterAvgs)

    if vortexAvgSteer:
        clusterDicts = vortexAvgSteerData(fHour)
        plotVortexAvgSteer(clusterDicts)

    print(f"[TIMING] Total time for forecast hour {fHour}: {time.perf_counter() - t_hour_start:.4f}s")

print(f"\n[TIMING] Total Python time for all hours: {time.perf_counter() - t_script_start:.4f}s")