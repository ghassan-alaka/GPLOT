'''
Tools for use with pyGPLOT: lew.gramer@noaa.gov
'''

import cartopy.crs as ccrs;
import cartopy.feature as cfeature;
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
import matplotlib.pyplot as plt;
import matplotlib.ticker as mticker;
from mpl_toolkits.axes_grid1 import make_axes_locatable;
from matplotlib.collections import LineCollection, PatchCollection;
from matplotlib.colors import ListedColormap, BoundaryNorm;
from matplotlib.patches import Wedge;
import matplotlib.path as mpath;
#https://matplotlib.org/3.1.1/tutorials/advanced/patheffects_guide.html
import matplotlib.patheffects as path_effects;
import matplotlib.dates as pltdts;
from shapely.geometry import Point
from shapely.ops import nearest_points

import csv
import datetime
import glob
import io
import netCDF4 as nc4
import numpy as np
import numpy.ma as ma
import os
import pandas as pd
import pickle
#import pygrib
import pyproj
import re
import socket
import struct
import subprocess
import time
import xarray as xr
import warnings

default_mdl = 'HB19';
default_expt = 'HB19_v2_Forecast';

default_strm = 'irma11l';
default_begdtst = '2017090718';
default_INIT='20170907_180000';
default_begdt = None; # E.g., for begdtst='2019082906', this would be datetime(2019,8,29,6,0,0)
default_stid = None; # E.g., for strm=dorian05l, this would be 05L
default_bstn = None; # E.g., for strm=dorian05l, this would be AL05


def is_hera():
    #return (re.match('hfe.*',socket.gethostname()) is not None);
    # If we are in an SALLOC'd shell, the above would fail!
    return (re.match('h.*',socket.gethostname()) is not None);

def is_orion():
    return (re.match('[Oo]rion.*',socket.gethostname()) is not None);

def is_hercules():
    return (re.match('[Hh]ercules.*',socket.gethostname()) is not None);

if ( is_hera() ):
    default_idir = '/scratch2/AOML/aoml-hafs1/Lew.Gramer/ocean';
    default_ddir = '/scratch2/AOML/aoml-hafs1/Lew.Gramer/ocean/data';
    default_odir = '/scratch2/AOML/aoml-hafs1/Lew.Gramer/ocean/figs';
    bdeck_idir = '/scratch2/AOML/aoml-hafs1/Ghassan.Alaka/bdeck';
    adeck_idir = '/scratch2/AOML/aoml-hafs1/Ghassan.Alaka/adeck/GPLOT_2024';
    #default_fix_idir='/scratch2/AOML/aoml-hafs1/Lew.Gramer/src/multi_nests_nestcpl/fix/fix_hycom'
    default_fix_idir='/scratch2/AOML/aoml-hafs1/Lew.Gramer/staging/fix/fix_hycom'
    default_fix_mom6_idir='/scratch2/AOML/aoml-hafs1/Lew.Gramer/staging/fix/fix_mom6'
elif ( is_orion() or is_hercules() ):
    default_idir = '/work2/noaa/aoml-hafs1/lgramer/ocean';
    default_ddir = '/work2/noaa/aoml-hafs1/lgramer/ocean/data';
    default_odir = '/work2/noaa/aoml-hafs1/lgramer/ocean/figs';
    #bdeck_idir = '/work/noaa/aoml-hafs1/galaka/bdeck';
    bdeck_idir = '/work/noaa/hwrf/noscrub/input/abdeck/btk';
    #adeck_idir = '/work/noaa/aoml-hafs1/galaka/adeck/GPLOT_2024';
    adeck_idir = '/work/noaa/hwrf/noscrub/input/abdeck/aid';
    #default_fix_idir='/work2/noaa/aoml-hafs1/lgramer/src/multi_nests_nestcpl/fix/fix_hycom';
    default_fix_idir='/work2/noaa/aoml-hafs1/lgramer/staging/fix/fix_hycom';
    default_fix_mom6_idir='/work2/noaa/aoml-hafs1/lgramer/staging/fix/fix_mom6';
else:
    default_idir = '/lfs5/HFIP/hur-aoml/Lew.Gramer/ocean';
    default_ddir = '/lfs5/HFIP/hur-aoml/Lew.Gramer/ocean/data';
    default_odir = '/lfs5/HFIP/hur-aoml/Lew.Gramer/ocean/figs';
    bdeck_idir = '/lfs5/HFIP/hur-aoml/Ghassan.Alaka/bdeck';
    adeck_idir = '/lfs5/HFIP/hur-aoml/Ghassan.Alaka/adeck/GPLOT_2024';
    #default_fix_idir='/lfs4/HFIP/hur-aoml/Lew.Gramer/src/multi_nests_nestcpl/fix/fix_hycom';
    default_fix_idir='/lfs1/HFIP/hur-aoml/Lew.Gramer/staging/fix/fix_hycom';
    default_fix_mom6_idir='/lfs1/HFIP/hur-aoml/Lew.Gramer/staging/fix/fix_mom6';


def find_matching_filenames(rootdir,ext=None,patt=None):
  fnames = [];
  for root, dirs, files in os.walk(rootdir):
    for fname in files:
      if ( ext is not None ):
        if ( fname.endswith(f".{ext}") ):
          fnames.append(os.path.join(root, fname));
      else:
          raise ValueError('Sorry, I do not know how to handle file patterns yet');
  return(fnames);

def minutes2hours(mins):
  '''Convert a number of minutes into a string of the form "<nnn>h<nn>m"'''
  h,m = divmod(mins,60)
  return(f'{h:03}h{int(m):02}m');

#https://www.geeksforgeeks.org/matplotlib-colors-boundarynorm-class-in-python/
def NHC_vmax_ranges():
    return( np.array([    0, 34, 64, 83, 96,114,135,200]) );

def NHC_vmax_colors():
    return( ['c','b','g','y','r','m','pink',] );

def NHC_vmax_cmap():
    return( ListedColormap(NHC_vmax_colors()) );


def wrap_lons(lons):
    '''Take longitudes in range -180 to +180 and wrap to 0-360 range'''
    if ( hasattr(lons, 'copy') and callable(lons.copy) ):
        wrap_lons = lons.copy();
    else:
        wrap_lons = lons;
    if ( isinstance(wrap_lons, (list, tuple, np.ndarray, pd.Series)) ):
        wrap_lons[wrap_lons<0] = 360 + wrap_lons[wrap_lons<0];
    elif ( wrap_lons < 0 ):
        wrap_lons = 360 + wrap_lons;
    return(wrap_lons);

def unwrap_lons(wrap_lons):
    '''Take longitudes in range 0-360 and unwrap to -180 to +180'''
    if ( hasattr(wrap_lons,'copy') and callable(wrap_lons.copy) ):
        lons = wrap_lons.copy();
    else:
        lons = wrap_lons;
    if ( isinstance(lons, (list, tuple, np.ndarray, pd.Series)) ):
        lons[lons>180] = lons[lons>180] - 360;
    elif ( lons > 180 ):
        lons = lons - 360;
    return(lons);

def find_curvilinear_index(xs,ys,x,y):
    '''Find [ix,jx] for point [x,y] in curvilinear (2D) coordinate arrays xs and ys'''
    ix = np.argmin(np.abs(xs-x)+np.abs(ys-y));
    # jx,ix = np.unravel_index(ix,xs.shape)
    # return((ix,jx));
    return(np.unravel_index(ix,xs.shape));


_coast_10m = None;

def plot_coastline(ax,facecolor='0.8',**kwargs):
    '''Add a coastline contour and land-shading to plot in Axes ax'''
    global _coast_10m
    if ( _coast_10m is None ):
        _coast_10m = cfeature.NaturalEarthFeature('physical', 'land', '10m',
                                                  edgecolor='k', facecolor=facecolor);
    cf = ax.add_feature(_coast_10m);
    return(cf)

def latlon_gridlines(ax,xts=np.arange(-180.0,360.0,2.0),yts=np.arange(-90.0,+90.0,2.0),dlon=None,dlat=None):
    if ( dlon != None ): xts = np.arange(xts.min(),xts.max(),dlon);
    if ( dlat != None ): yts = np.arange(yts.min(),yts.max(),dlat);
    gl = ax.gridlines(crs=ccrs.PlateCarree(),draw_labels=True);
    gl.xlocator = mticker.FixedLocator(xts);
    gl.ylocator = mticker.FixedLocator(yts);
    #gl.ylabels_left = True; #DEPRECATED
    #gl.xlabels_bottom = True; #DEPRECATED
    #gl.xlabels_top = False; #DEPRECATED
    #gl.ylabels_right = False; #DEPRECATED
    gl.left_labels = True;
    gl.bottom_labels = True;
    gl.top_labels = False;
    gl.right_labels = False;
    gl.xformatter = LONGITUDE_FORMATTER;
    gl.yformatter = LATITUDE_FORMATTER;
    return(gl)

def nearest_coastal_point(lon,lat,maxdist=10):
    '''Find the coordinates of the point on a coastline (Cartopy Feature) nearest to point [lon,lat]'''
    global _coast_10m
    #https://programtalk.com/python-examples/cartopy.feature.NaturalEarthFeature/
    if ( _coast_10m is None ):
        _coast_10m = cfeature.NaturalEarthFeature('physical', 'land', '10m',
                                                  edgecolor='k', facecolor='0.8');
    # extnt = domain_corners(lons,lats);
    domain_coastline_list = list(_coast_10m.intersecting_geometries([lon-maxdist,lat-maxdist,lon+maxdist,lat+maxdist]));
    
    #https://stackoverflow.com/questions/33311616/find-coordinate-of-the-closest-point-on-polygon-in-shapely
    #poly = Polygon([(0, 0), (2, 8), (14, 10), (6, 1)])
    center_Point = Point(lon,lat)
    # The points are returned in the same order as the input geometries:
    mindist = np.inf;
    for ix,poly in enumerate(domain_coastline_list):
        p1, p2 = nearest_points(poly, center_Point)
        # print(p1.wkt,p1.distance(center_Point))
        # if ( p1.distance(center_Point) > 20 ):
        #   breakpoint();
        if ( p1.distance(center_Point) < mindist ):
            coast_Point = p1;
            mindist = p1.distance(center_Point);
    #print(coast_Point.wkt,coast_Point.distance(center_Point))
    if ( not np.isfinite(mindist) ):
        raise ValueError('No coastal points in domain!');
    coast_pt = [coast_Point.x,coast_Point.y];
    return(coast_pt);


# https://stackoverflow.com/questions/5067604/determine-function-name-from-within-that-function-without-using-traceback
current_function_name = lambda n=0: sys._getframe(n + 1).f_code.co_name;


##########
## Adapted from PyHurSea

def inside_bbox(bbox,pt):
    return(bbox[0] <= pt[0] <= bbox[0]+bbox[2] and bbox[1] <= pt[1] <= bbox[1]+bbox[3]);

def domain_corners(domain_lons,domain_lats):
    x1 = domain_lons.min();
    y1 = domain_lats.min();
    x2 = domain_lons.max();
    y2 = domain_lats.max();
    return(x1,y1,x2,y2);
#domain_corners(domain_lons,domain_lats):

def domain_bbox(domain_lons,domain_lats):
    x = domain_lons.min();
    y = domain_lats.min();
    w = domain_lons.max() - x;
    h = domain_lats.max() - y;
    return(x,y,w,h);
#domain_bbox(domain_lons,domain_lats):

def inside_domain(domain_lons,domain_lats,pt):
    return(inside_bbox(domain_bbox(domain_lons,domain_lats),pt));

def reorient_vectors(ori,u,v):
  '''Reorient U and V (x and y) field components to local isobath orientation ORI (deg T). Return matrices X of cross-shore, and L of long-shore components.'''
  # Cross-shore component
  x = (np.cos(np.radians(ori)) * u) - (np.sin(np.radians(ori)) * v);
  # Long-shore component
  l = (np.sin(np.radians(ori)) * u) + (np.cos(np.radians(ori)) * v);
  return(x,l);


geod = None;
def azimuth_distance_wgs84(lon1,lat1,lon2,lat2,**kwargs):
    '''Calculate distance in [m] and azimuth of line between two points (lat/lon coordinates). Uses WGS84 datum by default.'''
    global geod
    if ( geod is None ):
        geod = pyproj.Geod(ellps='WGS84')
    az12,az21,dst = geod.inv(lon1,lat1,lon2,lat2);
    return(az12,dst);

def distance_wgs84(lon1,lat1,lon2,lat2,**kwargs):
    '''Calculate distance in [m] between two points (lat/lon coordinates). Calls AZIMUTH_DISTANCE_WGS84.'''
    az,dst = azimuth_distance_wgs84(lon1,lat1,lon2,lat2,**kwargs);
    return(dst);

def translate_wgs84(lon1,lat1,dst,az,**kwargs):
    '''Calculate lon,lat of new location DST m along azimuth AZ from points lon1,lat1. Uses WGS84 datum by default.'''
    global geod
    if ( geod is None ):
        geod = pyproj.Geod(ellps='WGS84')
    lon2,lat2,backaz = geod.fwd(np.array(lon1),np.array(lat1),np.array(az),np.array(dst));
    lon2 = np.array(lon2); lat2 = np.array(lat2); 
    lon2[lon2<0] = lon2[lon2<0] + 360
    return(lon2,lat2,backaz);


#https://stackoverflow.com/questions/25837544/get-all-points-of-a-straight-line-in-python
def ray_trace_grid(x1,y1,x2,y2,doround=True):
  '''Apply Bresenham algorithm to find all 2D indices along the line between two grid points. If doround=False, return float indices'''
  def _cround(val):
    if ( doround ):
      return(int(round(val)))
    else:
      return(val)
  
  if x1==x2:			# Horizontal line
    return [(x1,i) for i in range(y1,y2+1)]
  elif y1==y2:		# Vertical line
    return [(i,y1) for i in range(x1,x2+1)]
  else:
    slope=(y2-y1)/(x2-x1)
    if ( abs(slope) < 1 ):
      if ( x1 < x2 ):	# Right-leaning track
        return [(x1+i,_cround(y1+slope*i)) for i in range(0,x2-x1+1)]
      else:		# Left-leaning track
        return [(x1-i,_cround(y1-slope*i)) for i in range(0,x1-x2+1)]
    else:
      slope = 1/slope;
      if ( y1 < y2 ):	# Up-lifting track
        return [(_cround(x1+slope*i),y1+i) for i in range(0,y2-y1+1)]
      else:		# Down-drifting track
        return [(_cround(x1-slope*i),y1-i) for i in range(0,y1-y2+1)]


##########
## Adapted from ECOFORECASTS::UTIL

def arrayize(val):
  '''Whenever possible, interpret VAL as an NP.NDARRAY of NP.DOUBLE with non-empty shape'''
  # EAFP is nice, but sometimes sequences of TRY's just get too darn
  # complicated! So what is the more "Pythonic" way of doing this??
  val = np.asanyarray(val,np.double);
  # Stupid NP.ASANYARRAY passes through SHAPE-less scalars??
  if ( not hasattr(val,'shape') or len(val.shape) == 0 ):
    vala = np.ndarray(1,np.double); 
    vala[0] = val; 
    val = vala; 
    del(vala);
  return(val);


def datetime2datenum(dt):
  '''Return MATLAB datenum from datetime.datetime tuple (or list). From: https://stackoverflow.com/questions/8776414/python-datetime-to-matlab-datenum'''
  mdn = dt + datetime.timedelta(days = 366);
  frac_seconds = (dt-datetime.datetime(dt.year,dt.month,dt.day,0,0,0)).seconds / (24.0 * 60.0 * 60.0);
  frac_microseconds = dt.microsecond / (24.0 * 60.0 * 60.0 * 1000000.0);
  dn = mdn.toordinal() + frac_seconds + frac_microseconds;
  return(dn);


def datenum2datetime(dn):
  '''Return datetime.datetime tuple (or list) from MATLAB datenum. From: https://stackoverflow.com/questions/13965740/converting-matlabs-datenum-format-to-python'''
  dy = datetime.datetime.fromordinal(int(dn));
  dyfrac = datetime.timedelta(days=dn%1) - datetime.timedelta(days = 366);
  dt = dy + dyfrac;
  return(dt);


def date_range(begdt,enddt):
  '''Return a range (ITERATOR) of dates'''
  for n in range( int((enddt - begdt).days) ):
    yield(begdt + datetime.timedelta(n));


def is_32bit():
  '''Is this a 32-bit Python implementation?'''
  return(sys.maxsize <= 2**32);


def latest_date():
  '''Most recent full day (YYYYMMDD) before today'''
  dt = datetime.date.today() - datetime.timedelta(days=1);
  return(int(dt.strftime('%Y%m%d')));


def matlab_slice(slicestr):
  '''Convert a MATLAB-like (but 0-based) slice string into a Python slice()'''
  sl = list(map(lambda x: int(x.strip()), slicestr.split(':')));
  if ( len(sl) == 1 ): 
    if ( sl[0] == -1 ):
      res = slice(sl[0],None,None);
    else:
      res = slice(sl[0],sl[0]+1,None);
  elif ( len(sl) == 2 ): 
    res = slice(sl[0],sl[1]+1,None);
  elif ( len(sl) == 3 ): 
    res = slice(sl[0],sl[2]+1,sl[1]);
  else:
    raise ValueError('Slice string should have one to three numerical elements');
  return(res);


def range_check(val,lo,hi=np.inf):
  '''Raise ValueError if VAL is outside (mathematical) range [LO,HI]'''
  if ( np.any(np.logical_or(val < np.double(lo),val > np.double(hi))) ):
    raise ValueError('Input(s) %s outside valid range (%s,%s)' %(repr(val),repr(lo),repr(hi)));


##########
## Adapted from ECOFORECASTS::WIND

def kts2mps(kts):
  '''Convert from Knots to [m/s]'''
  mps = kts * 0.5144444444;
  return(mps);


def mps2kts(mps):
  '''Convert from [m/s] to Knots'''
  kts = mps / 0.5144444444;
  return(kts);


def spddir_to_uv(wspd,wdir):
  '''Convert speed and WIND (from) direction array to array of u,v vector components'''
  wspd = arrayize(wspd);  wdir = arrayize(wdir);
  range_check(wdir,0,360);
  u = np.round( wspd * (-np.sin(np.radians(wdir))), 8);
  v = np.round( wspd * (-np.cos(np.radians(wdir))), 8);
  return(u,v);


def spddir_to_uv_curr(cspd,cdir):
  '''Convert speed and CURRENT (to) direction array to array of u,v vector components'''
  cspd = arrayize(cspd);  cdir = arrayize(cdir);
  range_check(cdir,0,360);
  cdir = cdir - 180;
  cdir[cdir < 0] = 360 + cdir[cdir < 0];

  u = np.round( cspd * (-np.sin(np.radians(cdir))), 8);
  v = np.round( cspd * (-np.cos(np.radians(cdir))), 8);
  return(u,v);


def uv_to_spd(u,v):
  '''Convert array of u,v vector components to speed'''
  u = arrayize(u);  v = arrayize(v);
  with np.errstate(all='raise'):
      spd = np.sqrt((u*u) + (v*v));
  return(spd);


# Convert U,V *wind* components to direction True
def uv_to_dir(u,v):
  '''Convert array of u,v vector components to WIND (from) direction'''
  u = arrayize(u);  v = arrayize(v);
  
  wdir = np.ndarray(u.shape,np.double);
  wdir[:] = np.nan;
  
  spd = uv_to_spd(u,v);
  
  # Handle direction for "zero wind" specially
  bpidx = np.logical_or( np.logical_and(u>0,v>=0) , np.logical_and(u>=0,v>0) );
  vpidx = np.logical_or( np.logical_and(u<0,v>=0) , np.logical_and(u<=0,v>0) );
  upidx = np.logical_or( np.logical_and(u>0,v<=0) , np.logical_and(u>=0,v<0) );
  npidx = np.logical_or( np.logical_and(u<0,v<=0) , np.logical_and(u<=0,v<0) );
  b0idx = np.logical_and(u==0,v==0);
  
  wdir[bpidx] = 180 + np.degrees( np.arcsin(u[bpidx] / spd[bpidx]) );
  wdir[vpidx] = 180 + np.degrees( np.arcsin(u[vpidx] / spd[vpidx]) );
  wdir[upidx] = 360 - np.degrees( np.arcsin(u[upidx] / spd[upidx]) );
  with np.errstate(all='raise'):
      wdir[npidx] =   0 - np.degrees( np.arcsin(u[npidx] / spd[npidx]) );
  # Make sure this one happens last
  wdir[b0idx] = 0;
  
  return(wdir);


def uv_to_dir_curr(u, v):
  '''Convert array of u,v vector components to CURRENT (toward) direction'''
  # Convert u and v vector components into a direction in degrees True.
  # NOTE: This version of 'uv-to-dir' is coded for OCEAN CURRENTS: "dir" here
  # means "target direction", NOT "source direction" as it would for winds.
  u = arrayize(u);  v = arrayize(v);
  cdir = uv_to_dir(u, v);

  cdir = cdir - 180;
  cdir[cdir < 0] = 360 + cdir[cdir < 0];
  return(cdir);

def wind_stress(u10,v10,rho_air=1.22):
  '''Calculate wind stress from 10 m wind speed using Large & Pond 1981'''
  sp = uv_to_spd(u10,v10);
  cd = (4.9e-4 + (6.5e-5 * sp))  # Compute cd(u10).
  cd[sp < 10.15385] = 1.15e-3
  taux = rho_air * (cd * (sp * u10))
  tauy = rho_air * (cd * (sp * v10))
  return(taux,tauy);

def inertial_period(lat):
  '''%function [IP_day,IP_hr,IP_sec,f] = inertial_period(lat)
     % Convenience function: returns local inertial period at latitude ABS(LAT) in
     % d, hours, and s. Local Coriolis frequency /f/ can also be returned.'''
  OMEGA = 7.292e-5;     #s-1   A.E.Gill p.597
  f = 2*OMEGA*np.sin(np.radians(lat));  #SW_F: Phil Morgan 93-04-20  (morgan@ml.csiro.au)
  IP_sec = np.abs( 2*np.pi/f );
  IP_hr = IP_sec/3600;
  IP_day = IP_sec/3600/24;
  return(IP_day,IP_hr,IP_sec,f);


########################################
## Storm-specific stuff
########################################

def id_from_storm(strm):
    '''From a storm name (e.g., "dorian05l"), return storm ID (e.g., "05L"): does simple string manipulation'''
    stid = re.sub('^[a-zA-Z][a-zA-Z]*','',strm).upper();
    #DEBUG:    print(f'STRM is {strm}, I guess STID is {stid}');
    return stid;
#id_from_storm(strm):

def name_from_storm(strm):
    '''From a storm name (e.g., "dorian05l"), return storm name (e.g., "DORIAN"): does simple string manipulation. See also ID_FROM_STORM.'''
    stnm = re.sub('[0-9][0-9][a-zA-Z]$','',strm).upper();
    return stnm;
#name_from_storm(strm):

def b_from_bsn(bsn):
    switcher = {
        'CP': 	"C",
        'EP': 	"E",
        'AL': 	"L",
        'WP': 	"W",
    };
    b = switcher.get(str.upper(bsn),None);
    return b;

def bsn_from_b(b):
    switcher = {
        'C': 	"CP",
        'E': 	"EP",
        'L': 	"AL",
        'W': 	"WP",
    };
    bsn = switcher.get(str.upper(b),None);
    return bsn;

def bstn_from_id(stid):
    '''From a storm ID (e.g., "05L"), return Basin-STorm-Num (e.g., "AL05"): does simple string manipulation'''
    #BASIN      - basin, e.g. WP, IO, SH, CP, EP, AL, LS
    # SUBREGION  - subregion code: W,A,B,S,P,C,E,L,Q.
    #              A - Arabian Sea
    #              B - Bay of Bengal
    #              C - Central Pacific
    #              E - Eastern Pacific
    #              L - Atlantic
    #              P - South Pacific (135E - 120W)
    #              Q - South Atlantic
    #              S - South IO (20E - 135E)
    #              W - Western Pacific
    bsn = bsn_from_b(stid[-1]);
    if ( bsn is None ):
        raise Exception(f'Unknown basin code in storm ID "{stid}"');
    bstn = f'{bsn}{stid[0:2]}';
    return bstn;
#bstn_from_id(strm):

def bstn_from_storm(strm):
    '''From a storm name (e.g., "dorian05l"), return Basin-STorm-Num (e.g., "AL05"): does simple string manipulation'''
    stid = id_from_storm(strm); #'05L'
    return(bstn_from_id(stid));
#bstn_from_storm(strm):

def id_from_bstn(bstn):
    '''From a Basin-STorm-Num (e.g., "AL05"), return storm ID (e.g., "05L"): does simple string manipulation'''
    b = b_from_bsn(bstn[0:2]);
    id = f'{bstn[2:4]}{b}';
    return id;
#def id_from_bstn(bstn):

def year_bstn_from_stid(mySTID='AL052019'):
    '''From STID (e.g., "AL052019"), return, e.g., year 2019 and Basin-Storm-num "AL05".'''
    myBSTN = mySTID[0:4];
    myYEAR = int(mySTID[4:]);
    return(myYEAR,myBSTN);
#year_bstn_from_stid(mySTID='AL052019'):

def stid_from_year_bstn(myYEAR=2019,myBSTN='AL05'):
    '''From, e.g., year 2019 and Basin-STorm-Num "AL05", return Storm ID (e.g., "AL052019").'''
    return(f'{myBSTN.upper()}{myYEAR}');
#stid_from_year_bstn(myYEAR=2019,myBSTN='AL05'):

def storm_num2word(stid):
    stid = re.sub('[a-zA-Z][a-zA-Z]*$','',stid);
    switcher = {
        '01': 	"one",
        '02': 	"two",
        '03': 	"three",
        '04': 	"four",
        '05': 	"five",
        '06': 	"six",
        '07': 	"seven",
        '08': 	"eight",
        '09': 	"nine",
        '10': 	"ten",
        '11': 	"eleven",
        '12': 	"twelve",
        '13': 	"thirteen",
        '14': 	"fourteen",
        '15': 	"fifteen",
        '16': 	"sixteen",
        '17': 	"seventeen",
        '18': 	"eighteen",
        '19': 	"nineteen",
        '20': 	"twenty",
        '21': 	"twenty-one",
        '22': 	"twenty-two",
        '23': 	"twenty-three",
        '24': 	"twenty-four",
        '25': 	"twenty-five",
        '26': 	"twenty-six",
        '27': 	"twenty-seven",
        '28': 	"twenty-eight",
        '29': 	"twenty-nine",
        '30': 	"thirty",
        '31': 	"thirty-one",
        '32': 	"thirty-two",
        '33': 	"thirty-three",
        '34': 	"thirty-four",
        '35': 	"thirty-five",
        '36': 	"thirty-six",
        '37': 	"thirty-seven",
        '38': 	"thirty-eight",
        '39': 	"thirty-nine", # God Forbid
    };
    numnm = switcher.get(stid,f'Unknown storm ID format "{stid}"');
    return(numnm);
#storm_num2word(stid):

def altstrm_from_storm(strm):
    '''Return, e.g., "five05l" for a storm name "dorian05l": does simple string manipulation'''
    stid = id_from_storm(strm);
    numnm = storm_num2word(stid);
    altstrm = re.sub('^[a-zA-Z][a-zA-Z]*',numnm,strm).lower();
    #DEBUG:    print(f'STRM is {strm}, I guess STID is {stid}');
    return altstrm;
#altid_from_storm(strm):

def expt_hours(expt,a_or_o='atm'):
    '''Return max forecast hour and forecast-hour timestep for model experiment EXPT'''
    if ( re.match(expt,'GFS.*') ):
        (maxhr,hrstep) = (384,3);
    elif ( re.match(expt,'EC.*') ):
        (maxhr,hrstep) = (240,24);
    else:
        (maxhr,hrstep) = (126,3);
    if ( re.match(a_or_o,'o.*') and hrstep == 3 ):
        hrstep = 6;
    return (maxhr,hrstep);
#expt_hours(expt,a_or_o='atm'):

def domain_bounds(domn,dslonlat=None):
    '''Return a coordinate 4-tuple (suitable for passing for an XARRAY subset, or to MATPLOTLIB.PYPLOT.AXIS) for the pyGPLOT domain "domn". This will ultimately be parameterized - e.g., with config files. If the string DOMN is not recognized currently, return a bbox for the entire dataset domain, DSLONLAT=(LONS,LATS). If not recognized and DSLONLAT is not specified, return None.'''
    switcher = {
        "atl":			[-110.,   0.,  0., 50.],
        "basin":		[-160.,   0.,-15., 50.],
        "bigd01":		[-200.,  40.,-40., 70.],
        "bob":			[  65., 100.,  5., 40.],
        "carolina":		[ -84., -74., 31., 41.],
        "cpac":			[-180.,-140.,  0., 35.],
        "elant":		[ -60.,   0.,  5., 35.],
        "epac":			[-140., -80.,  0., 35.],
        "eus":			[ -85., -60., 20., 45.],
        "fvgfs":		[-100.,   0.,  5., 55.],
        "gom":			[-105., -70., 10., 35.],
        "leeward":		[ -85., -70., 15., 28.],
        "nio":			[  50., 100.,  0., 30.],
        "wlant":		[-110., -50.,  5., 40.],
        #
        "irma_late":		[ -95., -65., 10., 30.],
        "straits":		[ -82., -72., 22., 34.],
        #
        "laura_inner":   	[ -97., -89., 22.5, 30.5 ],
        "isaias_inner":  	[ -83., -74., 29.5, 37.5 ],
        "dorian":       	[ -82., -72., 22., 34. ],
        "irma_early":       	[ -80., -50., 10., 25. ],
        "irma_inner":       	[ -85., -70., 15., 30. ],
        "irma":       		[ -90., -50., 10., 30. ],
    };
    if ( dslonlat is None ):
        return( switcher.get(domn,None) );
    else:
        return( switcher.get(domn,[np.min(dslonlat[0]),np.max(dslonlat[0]),np.min(dslonlat[1]),np.max(dslonlat[1])]) );


# def str2latlon(s):
#     if ( not isinstance(s,str) ):
#         return(s);
#     if ( s.endswith('S') or s.endswith('W') ):
#         s = '-' + s.strip();
#     return(np.double(s.strip('NSEW')));
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

def pres2hkm(P,T,P0=1013*100):
    '''
    Computes height using press and T using the equation:
    
        ((P0/P)^(1/5.257) - 1) x (T+273.15)
    h=_______________________________________
           0.0065
    
    where P, P0 in Pa, T in [C], and h in [m]
    
    -- Hyun-Sook.Kim@noaa.gov
    '''
    P0 = 1013*100
    upper = ( (P0/P)**(1/5.257) -1 )*(T+273.15)
    out = upper/0.0065/1000.0     # [m] to [km]
    return (out)

def seawater_surface_density(S,T):
    '''
    From "Sea Water Density According to UNESCO Formula": https://link.springer.com/content/pdf/bbm%3A978-3-319-18908-6%2F1.pdf
    '''
    a0 = 999.842594
    a1 = 6.793953e-2
    a2 = -9.095290e-3
    a3 = 1.001685e-4
    a4 = -1.120083e-6
    a5 = 6.536332e-9
    
    b0 = 8.2449e-1
    b1 = -4.0899e-3
    b2 = 7.6438e-5
    b3 = -8.2467e-7
    b4 = 5.3875e-9
    
    c0 = -5.7246e-3
    c1 = 1.0227e-4
    c2 = -1.6546e-6
    
    d0 = 4.8314e-4
    
    B1 = b0 + (b1*T) + (b2*(T**2)) + (b3*(T**3)) + (b4*(T**4))
    C1 = c0 + (c1*T) + (c2*(T**2))
    rho0 = a0 + (a1*T) + (a2*(T**2)) + (a3*(T**3)) + (a4*(T**4)) + (a5*(T**5)) + (B1*S) + (C1*(S**1.5)) + (d0*(S**2))
    return(rho0)

def seawater_compressibility(S,T,d):
    '''
    From "Sea Water Density According to UNESCO Formula": https://link.springer.com/content/pdf/bbm%3A978-3-319-18908-6%2F1.pdf
    '''
    p = d / 10 # Pressure in bar
    e0 = 19652.210000
    e1 = 148.420600
    e2 = -2.327105
    e3 = 1.360477e-2
    e4 = -5.155288e-5
    Kw = e0 + (e1*T) + (e2*(T**2)) + (e3*(T**3)) + (e4*(T**4))
    f0 = 54.674600
    f1 = -0.603459
    f2 = 1.099870e-2
    f3 = -6.167000e-5
    F1 = f0 + (f1*T) + (f2*(T**2)) + (f3*(T**3))
    g0 = 7.9440e-2
    g1 = 1.6483e-2
    g2 = -5.3009e-4
    G1 = g0 + (g1*T) + (g2*(T**2))
    K0 = Kw + (F1*S) + (G1*(S**1.5))
    h0 = 3.23990
    h1 = 1.43713e-3
    h2 = 1.16092e-4
    h3 = -5.77905e-7
    i0 = 2.28380e-3
    i1 = -1.09810e-5
    i2 = -1.60780e-6
    j0 = 1.91075e-4
    Aw = h0 + (h1*T) + (h2*(T**2)) + (h3*(T**3))
    #A1 = 3.28574 # HACK HACK HACK
    A1 = Aw + ((i0 + (i1*T) + (i2*(T**2)))*S) + (j0*(S**(1.5)))
    k0 = 8.50935e-5 
    k1 = -6.12293e-6 
    k2 = 5.27870e-8
    m0 = -9.9348e-7
    m1 = 2.0816e-8
    m2 = 9.1697e-10 
    Bw = k0 + (k1*T) + (k2*(T**2))
    #B2 = 0.00020 # HACK HACK HACK
    B2 = Bw + ((m0 + (m1*T) + (m2*(T**2)))*S)
    K = K0 + (A1*p) + (B2*(p**2))
    return(K)

def seawater_density(S,T,d):
    '''
    From "Sea Water Density According to UNESCO Formula": https://link.springer.com/content/pdf/bbm%3A978-3-319-18908-6%2F1.pdf
    '''
    p = d / 10 # Pressure in bar
    rho = seawater_surface_density(S,T) / (1 - (p/seawater_compressibility(S,T,d)))
    return(rho)


def load_axbts_TROPIC(ddir=default_ddir):
    axbts = xr.open_dataset(f'{ddir}/axbt/0209221/1.1/data/0-data/TROPIC_AXBTdata.nc');
    #axbts['sea_water_temperature_qc'] = axbts.where(axbts.sea_water_temperature>=-2)
    return(axbts);

def load_axbt_data(BASEFDT,ndays=(-3,-2,-1,0,1,2,3,4),ddir=default_ddir):
    '''Create XArray Dataset of AXBT float profiles for all flights during days in BASEFDT +/- NDAYS'''
    basefdt = datetime.datetime.strptime(BASEFDT,'%Y%m%d');
    for nday in ndays:
        fdt = basefdt + datetime.timedelta(days=nday);
        FDT = fdt.strftime('%Y%m%d');
        #/scratch2/AOML/aoml-hafs1/Lew.Gramer/ocean/data/axbts/20230910I1/20230910I1_BT_120527.dat
        fdfpatt = f'{ddir}/axbts/{FDT}*/{FDT}*.dat'
        for fdfname in sorted(glob.glob(f'{fdfpatt}')):
            try:
                fse = pd.read_csv(fdfname,r'\s+',low_memory=False,header=2)
            except:
                print(f'Failed read_csv: {fdfname}');
                breakpoint()
                continue;
            
            flight = os.path.basename(fdfname).split('_')[0]
            t = datetime.datetime.strptime(fse.columns[0]+fse.columns[1], '%Y%m%d%H%M%S')
            try:
                lat = np.double(fse.columns[2])
                lon = np.double(fse.columns[3])
            except:
                lat = np.nan
                lon = np.nan
            platform = fse.columns[4]
            storm = fse.columns[5]
            fse.dropna(axis=1,inplace=True)
            fse.columns = ['depth','temperature','ignore']
            T = fse.temperature.values.reshape((1,len(fse.temperature)))
            T[T<0] = np.nan
            z = fse.depth
            
            fds = xr.Dataset( { "T": (("t","z"),T), "lon": ("t",[lon]), "lat": ("t",[lat]), \
                                "flight": ("t",[flight]), "storm": ("t",[storm]), "platform": ("t",[platform]), }, \
                              coords={"t": [t], "z": (("z"), z) }, )
            if ( 'fdses' in locals() ):
                fdses = xr.merge([fdses,fds]);
            else:
                fdses = fds;
    return(fdses)

def load_argo_data(BASEFDT,ndays=(-3,-2,-1,0,1),ddir=default_ddir):
  '''Create DICT of Argo float profiles for all of the days in BASEFDT + NDAYS. DICT has one element (an XArray Dataset) for each day.'''
  fdses = []
  basefdt = datetime.datetime.strptime(BASEFDT,'%Y%m%d');
  for nday in ndays:
    fdt = basefdt + datetime.timedelta(days=nday);
    FDT = fdt.strftime('%Y%m%d');
    FDTy = f'{fdt.year}';
    FDTm = f'{fdt.month:02}'
    fdfname = f'{ddir}/argo/usgodae.org/pub/outgoing/argo/geo/atlantic_ocean/{FDTy}/{FDTm}/{FDT}_prof.nc'
    try:
      fds = xr.open_dataset(fdfname);
      fdses.append(fds);
    except:
      print(f'Not found: {fdfname}');
  return(fdses)

def find_domain_argos(fdses,corners):
  '''Return array of (subgroup-indexed) indices into FDSES with all Argo profiles inside domain corners==(LON1,LAT1,LON2,LAT2)'''
  argo_idxs = []
  search_start_idx = 0;
  for fds in fdses:
    idxs = np.where( (corners[0]<=fds.LONGITUDE) & (fds.LONGITUDE<=corners[2]) & (corners[1]<=fds.LATITUDE) & (fds.LATITUDE<=corners[3]) )
    if ( len(idxs) > 0 ):
        idxs[0][:] = idxs[0][:] + search_start_idx
        argo_idxs = np.unique(np.concatenate((argo_idxs,idxs[0]))).astype(int);
    search_start_idx = search_start_idx + len(fds.N_PROF)
  return(argo_idxs);

def calc_argo_dists(fdses,pt):
  '''Calculate distance from point PT (a (LON,LAT) tuple) to each Argo profile in DICT of XArray.Datasets FDSES'''
  argo_dists = []
  for fds in fdses:
    fdsts = np.sqrt( ((fds.LONGITUDE-pt[0])**2)  + ((fds.LATITUDE-pt[1])**2) ).values * 111
    argo_dists = np.concatenate((argo_dists,fdsts));
  return(argo_dists);

def get_argo_profile(fdses,fix):
  '''Retrieve an Argo float profile at index FIX from among DICT of multiple XArray.Datasets. NOTE: FIX must be <= sum(len(FDSES)).'''
  fix_checked = 0;
  fix_searched = 0;
  for fds in fdses:
    fix_checked = fix_searched + len(fds.N_PROF)
    if ( fix < fix_checked ):
      prof = fds.isel({'N_PROF':fix-fix_searched})
      break;
    fix_searched = fix_checked;
  return(prof);

def read_ndbc(stnid=None,begdt=None,enddt=None,ddir=default_ddir,url=None):
    url2 = None
    if ( url is None ):
        yyyy = begdt.year;
        url = f'{ddir}/ndbc/{stnid}h{yyyy}.txt';
        if ( not os.path.exists(url) ):
            Mmm = begdt.strftime('%b');
            url = f'{ddir}/ndbc/{stnid}h{yyyy}m{Mmm}.txt';
            if ( (datetime.datetime.now()-begdt).days <= 45 ):
                print(f'Attempting to read Real-Time NDBC data...');
                # EARLY RETURN
                return(read_ndbc_RT(stnid,ddir=ddir));
    
    if ( not os.path.exists(url) ):
        raise ValueError(f'NO DATA {url} (or alternatives)');
    stdmet = pd.read_csv(url,r'\s+',low_memory=False) #parse_dates=['#YY','MM','DD','hh']
    #DEBUG:    print(f'read_ndbc: {url}'); breakpoint();
    #stdmet = pd.read_fwf('data/ndbc/42012h2020.txt',header=0,delim_whitespace=True);
    if ( '#' in stdmet.keys()[0] ):
        stdmet.rename(columns={stdmet.keys()[0]:stdmet.keys()[0][1:]}, inplace=True);
    if ( '#' in stdmet.iloc[0,0] ):
        stdmet = stdmet.drop(stdmet.index[0])
    #dts = stdmet[['YY','MM','DD','hh','mm']].apply(pd.to_datetime,errors='coerce',format='%Y %m %d %H %M');
    dts = pd.to_datetime(stdmet[['YY','MM','DD','hh','mm']].astype(str).apply(' '.join, 1),format='%Y %m %d %H %M',errors='coerce')
    stdmet = stdmet.apply(pd.to_numeric, errors='coerce')
    stdmet.index = dts;
    
    # #YY  MM DD hh mm WDIR WSPD GST  WVHT   DPD   APD MWD   PRES  ATMP  WTMP  DEWP  VIS  TIDE
    # 2019 01 01 00 00 162  7.0  8.6 99.00 99.00 99.00 999 1017.0  20.1 999.0  19.4 99.0 99.00 
    magicvals = {
        'WDIR':	999,
        'WSPD':	99,
        'GST':	99,
        'WVHT':	99,
        'DPD':	99,
        'APD':	99,
        'MWD':	999,
        'PRES':	9999,
        'ATMP':	999,
        'WTMP':	999,
        'DEWP':	999,
        'VIS':	99,
        'TIDE':	99,
    };
    for fnm in magicvals:
        val = magicvals[fnm];
        stdmet.loc[stdmet[fnm]==val,fnm] = np.nan;
    
    if ( enddt is not None ):
        if ( enddt < begdt ):
            raise ValueError(f'ENDDT {enddt} <= BEGDT {begdt}');
        loops = 0;
        while ( stdmet.index[-1] < enddt ):
            try:
                stdmet2 = read_ndbc(stnid,stdmet.index[-1]+datetime.timedelta(days=1));
                if ( np.all(stdmet.index == stdmet2.index) ):
                    warnings.warn(f'WARNING:: NDBC data record ended prematurely? {url}');
                    break;
                stdmet = stdmet.merge(stdmet2);
            except:
                # EARLY LOOP TERMINATION - maybe we just ran out of available files!
                break;
            # Avoid infinite loops for weirdo ENDDTs
            loops = loops + 1;
            if ( loops > 10 ):
                raise ValueError(f'ENDDT {enddt} more than 10 files from BEGDT {begdt}??');
                break;
        # Let the caller do this for now...
        #stdmet = stdmet[(idt<=stdmet.index) & (stdmet.index<=edt)];
    return(stdmet);


def read_ndbc_RT(stnid=None,ddir=default_ddir,url=None):
    if ( url is None ):
        url = f'{ddir}/ndbc/{stnid}RT.txt';
    stdmet = pd.read_csv(url,r'\s+',low_memory=False) #parse_dates=['#YY','MM','DD','hh']
    #DEBUG:    print(f'read_ndbc_RT: {url}'); breakpoint();
    #stdmet = pd.read_fwf('data/ndbc/42012h2020.txt',header=0,delim_whitespace=True);
    if ( '#' in stdmet.keys()[0] ):
        stdmet.rename(columns={stdmet.keys()[0]:stdmet.keys()[0][1:]}, inplace=True);
    if ( '#' in stdmet.iloc[0,0] ):
        stdmet = stdmet.drop(stdmet.index[0])
    #dts = stdmet[['YY','MM','DD','hh','mm']].apply(pd.to_datetime,errors='coerce',format='%Y %m %d %H %M');
    dts = pd.to_datetime(stdmet[['YY','MM','DD','hh','mm']].astype(str).apply(' '.join, 1),format='%Y %m %d %H %M',errors='coerce')
    # 'coerce' forces, e.g., "MMM" values in RT files to NaNs.
    stdmet = stdmet.apply(pd.to_numeric, errors='coerce')
    stdmet.index = dts;
    
    # #YY  MM DD hh mm WDIR WSPD GST  WVHT   DPD   APD MWD   PRES  ATMP  WTMP  DEWP  VIS  TIDE
    # 2019 01 01 00 00 162  7.0  8.6 99.00 99.00 99.00 999 1017.0  20.1 999.0  19.4 99.0 99.00 
    magicvals = {
        'WDIR':	999,
        'WSPD':	99,
        'GST':	99,
        'WVHT':	99,
        'DPD':	99,
        'APD':	99,
        'MWD':	999,
        'PRES':	9999,
        'ATMP':	999,
        'WTMP':	999,
        'DEWP':	999,
        'VIS':	99,
        'TIDE':	99,
    };
    for fnm in magicvals:
        val = magicvals[fnm];
        stdmet.loc[stdmet[fnm]==val,fnm] = np.nan;
    # Reverse time order for RT files
    return(stdmet.iloc[::-1]);


def read_ndbc_adcp(stnid=None,begdt=None,enddt=None,ddir=default_ddir,url=None): #,maxbins=1):
    if ( url is None ):
        yyyy = begdt.year;
        url = f'{ddir}/ndbc/{stnid}h{yyyy}ADCP.txt';
        if ( not os.path.exists(url) ):
            Mmm = begdt.strftime('%b');
            #url = f'/scratch2/AOML/aoml-hafs1/Lew.Gramer/ocean/data/ndbc/{stnid}h2020mSepADCP.txt'
            url = f'{ddir}/ndbc/{stnid}h{yyyy}m{Mmm}ADCP.txt';
    #DEBUG:    print(url);
    try:
        #adcp = pd.read_csv(url,r'\s+',usecols=range(0,(5+(maxbins*3))));
        with open(url, 'r') as f:
            # Get column names (and so, total number of bins) from first line
            colnames = f.readline().split();
        adcp = pd.read_csv(url,r'\s+',names=colnames,low_memory=False);
    except:
        #DEBUG:        print(f'SKIPPING {url}');
        return(None);
    #DEBUG:    print(url);
    #DEBUG:    breakpoint();
    if ( '#' in adcp.keys()[0] ):
        adcp.rename(columns={adcp.keys()[0]:adcp.keys()[0][1:]}, inplace=True);
    if ( '#' in adcp.iloc[0,0] ):
        adcp = adcp.drop(adcp.index[0])
    if ( '#' in adcp.iloc[0,0] ):
        adcp = adcp.drop(adcp.index[0])
    dts = pd.to_datetime(adcp[['YY','MM','DD','hh','mm']].astype(str).apply(' '.join, 1),format='%Y %m %d %H %M',errors='coerce')
    adcp = adcp.apply(pd.to_numeric, errors='coerce')
    # Magic value for missing ADCP data (DEPth, DIRection, and SPeeD)
    adcp.replace(to_replace=(-999,999),value=np.nan,inplace=True)
    # Convert cm/s -> m/s
    adcp.iloc[:,7::3] = adcp.iloc[:,7::3] / 100.0;
    # #adcp.[...] = spddir_to_uv_curr(adcp.iloc[:,7::3],adcp.iloc[:,6::3]);
    # (adcp['U01'],adcp['V01']) = spddir_to_uv_curr(adcp.iloc[:,7],adcp.iloc[:,6]);
    spds = adcp.iloc[:,7::3].values;
    dirs = adcp.iloc[:,6::3].values;
    for ix in range(0,spds.shape[1]):
        (adcp[f'U{(ix+1):02}'],adcp[f'V{(ix+1):02}']) = spddir_to_uv_curr(spds[:,ix],dirs[:,ix]);
    adcp.index = dts;
    #breakpoint();
    
    if ( enddt is not None ):
        if ( enddt < begdt ):
            raise ValueError(f'ENDDT {enddt} <= BEGDT {begdt}');
        loops = 0;
        while ( adcp.index[-1] < enddt ):
            try:
                adcp2 = read_ndbc_adcp(stnid,adcp.index[-1]+datetime.timedelta(days=1));
                if ( np.all(adcp.index == adcp2.index) ):
                    warnings.warn(f'WARNING:: NDBC ADCP data record ended prematurely? {url}');
                    break;
                adcp = adcp.merge(adcp2);
            except:
                # EARLY LOOP TERMINATION - maybe we just ran out of available files!
                break;
            # Avoid infinite loops for weirdo ENDDTs
            loops = loops + 1;
            if ( loops > 10 ):
                raise ValueError(f'ENDDT {enddt} more than 10 ADCP files from BEGDT {begdt}??');
                break;
        # Let the caller do this for now...
        #adcp = adcp[(idt<=adcp.index) & (adcp.index<=edt)];
    return(adcp);


def read_ndbc_adcp_RT(stnid=None,ddir=default_ddir,url=None): #,maxbins=1):
    if ( url is None ):
        url = f'{ddir}/ndbc/{stnid}RTADCP.txt';
    #DEBUG:    print(url);
    try:
        #adcp = pd.read_csv(url,r'\s+',usecols=range(0,(5+(maxbins*3))));
        with open(url, 'r') as f:
            # Get column names (and so, total number of bins) from first line
            colnames = f.readline().split();
        adcp = pd.read_csv(url,r'\s+',names=colnames,low_memory=False);
    except:
        #DEBUG:        print(f'SKIPPING {url}');
        return(None);
    #DEBUG:    print(url);
    #DEBUG:    breakpoint();
    if ( '#' in adcp.keys()[0] ):
        adcp.rename(columns={adcp.keys()[0]:adcp.keys()[0][1:]}, inplace=True);
    if ( '#' in adcp.iloc[0,0] ):
        adcp = adcp.drop(adcp.index[0])
    if ( '#' in adcp.iloc[0,0] ):
        adcp = adcp.drop(adcp.index[0])
    dts = pd.to_datetime(adcp[['YY','MM','DD','hh','mm']].astype(str).apply(' '.join, 1),format='%Y %m %d %H %M',errors='coerce')
    adcp = adcp.apply(pd.to_numeric, errors='coerce')
    # Magic value for missing ADCP data (DEPth, DIRection, and SPeeD)
    adcp.replace(to_replace=(-999,999),value=np.nan,inplace=True)
    # Convert cm/s -> m/s
    adcp.iloc[:,7::3] = adcp.iloc[:,7::3] / 100.0;
    # #adcp.[...] = spddir_to_uv_curr(adcp.iloc[:,7::3],adcp.iloc[:,6::3]);
    # (adcp['U01'],adcp['V01']) = spddir_to_uv_curr(adcp.iloc[:,7],adcp.iloc[:,6]);
    spds = adcp.iloc[:,7::3].values;
    dirs = adcp.iloc[:,6::3].values;
    for ix in range(0,spds.shape[1]):
        (adcp[f'U{(ix+1):02}'],adcp[f'V{(ix+1):02}']) = spddir_to_uv_curr(spds[:,ix],dirs[:,ix]);
    adcp.index = dts;
    #breakpoint();
    # Reverse time order for RT files
    return(adcp.iloc[::-1]);


def read_bdeck(year=2019,stid='AL05',bdir=bdeck_idir):
    '''Load B-Deck (Best Track data) file for year YEAR (default: 2019) and storm STID (default: 'AL05' for Dorian): default BDIR is Gus's B-Deck directory. Dictionary RES is returned with fields dts,dtst,lat,lon,vmax,mslp,levl,rtyp,r34k,r50k,r64k,ocip,roci,rmw,stnm,stid.
    >>> b = read_bdeck()
    >>> b.keys()
    dict_keys(['dtst', 'lat', 'lon', 'vmax', 'mslp', 'levl', 'rtyp', 'r34k', 'r34k_1', 'r34k_2', 'r34k_3', 'r34k_4', 'r50k', 'r50k_1', 'r50k_2', 'r50k_3', 'r50k_4', 'r64k', 'r64k_1', 'r64k_2', 'r64k_3', 'r64k_4', 'ocip', 'roci', 'rmw', 'stnm', 'stid', 'dts'])
    '''
    res = {};
    pth = f'{bdir}/b{stid.lower()}{year}.dat';
    # DEPRECATED: https://stackoverflow.com/questions/38093326/whats-the-difference-between-rb-and-ru-in-the-open-function-for-csv
    #ifile = open(pth, "rU");
    ifile = open(pth, newline='');
    reader = csv.reader(ifile, delimiter=";");
    #/lfs1/HFIP/hur-aoml/Lew.Gramer/ocean/bdeck-format.txt:
    # Best Track/Objective Aid/Wind Radii Format: https://www.nrlmry.navy.mil/atcf_web/docs/database/new/abrdeck.html
    #AL, 05, 2019090118,   , BEST,   0, 265N,  771W, 160,  910, HU,  34, NEQ,  120,  110,   70,   90, 1011,  100,  10, 195,   0,   L,   0,    ,   0,   0,     DORIAN, D, 12, NEQ,  150,  120,   90,  180, 
    # BASIN - basin, e.g. WP, IO, SH, CP, EP, AL, SL
    # CY - annual cyclone number: 1 through 99
    # YYYYMMDDHH - Warning Date-Time-Group: 0000010100 through 9999123123. (note, 4 digit year)
    # TECHNUM/MIN - objective technique sorting number, minutes for best track: 00 - 99
    # TECH - acronym for each objective technique or CARQ or WRNG, BEST for best track.
    # TAU - forecast period: -24 through 240 hours, 0 for best-track, negative taus used for CARQ and WRNG records.
    # LatN/S - Latitude (tenths of degrees) for the DTG: 0 through 900, N/S is the hemispheric index.
    # LonE/W - Longitude (tenths of degrees) for the DTG: 0 through 1800, E/W is the hemispheric index.
    # VMAX - Maximum sustained wind speed in knots: 0 through 300.
    # MSLP - Minimum sea level pressure, 1 through 1100 MB.
    # TY - Level of tc development: DB, TD, TS, TY, ST, TC, HU, SD, SS, EX, IN, DS, LO, WV, ET, XX
    # RAD - Wind intensity (kts) for the radii defined in this record: 34, 50, 64.
    # WINDCODE - Radius code: AAA - full circle; OR quadrant - NNQ, NEQ, EEQ, SEQ, SSQ, SWQ, WWQ, NWQ
    # RAD1, RAD2, RAD3, RAD4
    # RADP - pressure in millibars of the last closed isobar, 900 - 1050 mb.
    # RRP - radius of the last closed isobar in nm, 0 - 9999 nm.
    # MRD - radius of max winds, 0 - 999 nm.
    # GUSTS, EYE
    # SUBREGION - subregion code: W, A, B, S, P, C, E, L, Q.
    # MAXSEAS, INITIALS, DIR, SPEED
    # STORMNAME - literal storm name, NONAME or INVEST. TCcyx used pre-1999...
    # DEPTH, SEAS, SEASCODE (AAA or NNQ, NEQ, EEQ, SEQ, SSQ, SWQ, WWQ, NWQ), SEAS1, SEAS2, SEAS3, SEAS4
    #AL, 05, 2019082506,   , BEST,   0, 110N,  510W,  40, 1007, TS,  34, NEQ,   20,    0,    0,   20, 1014,  100,  10,  45,   0,   L,   0,    ,   0,   0,     DORIAN, M, 12, NEQ,   30,    0,    0,   30, 
    # res = {'dtst':[], 'lat':[], 'lon':[], 'vmax':[], 'mslp':[], 'levl':[], 'rtyp':[], 'r34k':[], 'r34k_1':[], 'r34k_2':[], 'r34k_3':[], 'r34k_4':[], 'r50k':[], 'r50k_1':[], 'r50k_2':[], 'r50k_3':[], 'r50k_4':[], 'r64k':[], 'r64k_1':[], 'r64k_2':[], 'r64k_3':[], 'r64k_4':[], 'ocip':[], 'roci':[], 'rmw':[], 'stnm':[], 'stid':[]};
    res = {'dtst':[], 'lat':[], 'lon':[], 'vmax':[], 'mslp':[], 'levl':[], 'rtyp':[], 'r34k':[], 'r34k_1':[], 'r34k_2':[], 'r34k_3':[], 'r34k_4':[], 'r50k':[], 'r50k_1':[], 'r50k_2':[], 'r50k_3':[], 'r50k_4':[], 'r64k':[], 'r64k_1':[], 'r64k_2':[], 'r64k_3':[], 'r64k_4':[], 'ocip':[], 'roci':[], 'rmw':[], 'tdir':[], 'tspeed':[], 'stnm':[], 'stid':[]};
    for line in reader:
        fields = line[0].split(',')
        # We do not want Best Track records with non-zero MINUTES...
        if ( fields[3].strip() != '' ) :
            continue;
        # We do not want weirdo short Best Track records with no storm name either...
        if ( len(fields) < 27 ):
            #DEBUG:            print(f'SKIPPING LINE from {pth}');
            continue;
        # AL, 05, 2019082500,   , BEST,   0, 108N,  499W,  35, 1008, TS,  34, NEQ,   20,    0,    0,   20, 1014,  100,  10,  45,   0,   L,   0,    ,   0,   0,     DORIAN, M, 
        # AL, 05, 2019082506,   , BEST,   0, 110N,  510W,  40, 1007, TS,  34, NEQ,   20,    0,    0,   20, 1014,  100,  10,  45,   0,   L,   0,    ,   0,   0,     DORIAN, M, 12, NEQ,   30,    0,    0,   30, 
        rtyp = np.double(fields[11].strip());
        #DEBUG:        print('BDECK'); code.interact(local=locals());
        if ( rtyp == 50 ):
            res['rtyp'][-1] = np.double(50.0);
            res['r50k_1'][-1] = np.double(fields[13].strip());
            res['r50k_2'][-1] = np.double(fields[14].strip());
            res['r50k_3'][-1] = np.double(fields[15].strip());
            res['r50k_4'][-1] = np.double(fields[16].strip());
            res['r50k'][-1] = np.nanmax([res['r50k_1'][-1],res['r50k_2'][-1],res['r50k_3'][-1],res['r50k_4'][-1]]);
            #DEBUG:            breakpoint();
        elif ( rtyp == 64 ):
            res['rtyp'][-1] = np.double(64.0);
            res['r64k_1'][-1] = np.double(fields[13].strip());
            res['r64k_2'][-1] = np.double(fields[14].strip());
            res['r64k_3'][-1] = np.double(fields[15].strip());
            res['r64k_4'][-1] = np.double(fields[16].strip());
            res['r64k'][-1] = np.nanmax([res['r64k_1'][-1],res['r64k_2'][-1],res['r64k_3'][-1],res['r64k_4'][-1]]);
            #DEBUG:            breakpoint();
        else:
            dt = fields[2].strip();
            if ( len(res['dtst']) > 0 and res['dtst'][-1] == dt ):
                raise ValueError(f'Duplicate date {dt} with rtyp {rtyp}!');
            res['dtst'].append(fields[2].strip());
            res['lat'].append(np.double(fields[6][:-1])/10.0);
            res['lon'].append(-np.double(fields[7][:-1])/10.0);
            res['vmax'].append(np.double(fields[8].strip()));
            res['mslp'].append(np.double(fields[9].strip()));
            #res['levl'].append(np.str(fields[10].strip()));
            res['levl'].append(str(fields[10].strip()));
            res['rtyp'].append(np.double(fields[11].strip()));
            res['r34k_1'].append(np.double(fields[13].strip()));
            res['r34k_2'].append(np.double(fields[14].strip()));
            res['r34k_3'].append(np.double(fields[15].strip()));
            res['r34k_4'].append(np.double(fields[16].strip()));
            res['r34k'].append(np.nanmax([res['r34k_1'][-1],res['r34k_2'][-1],res['r34k_3'][-1],res['r34k_4'][-1]]));
            res['r50k_1'].append(np.double(0));
            res['r50k_2'].append(np.double(0));
            res['r50k_3'].append(np.double(0));
            res['r50k_4'].append(np.double(0));
            res['r50k'].append(np.double(0));
            res['r64k_1'].append(np.double(0));
            res['r64k_2'].append(np.double(0));
            res['r64k_3'].append(np.double(0));
            res['r64k_4'].append(np.double(0));
            res['r64k'].append(np.double(0));
            res['ocip'].append(np.double(fields[17].strip()));
            res['roci'].append(np.double(fields[18].strip()));
            res['rmw'].append(np.double(fields[19].strip()));
            res['tdir'].append(np.double(fields[25].strip()));
            res['tspeed'].append(np.double(fields[26].strip()));
            #res['stnm'].append(np.str(fields[27].strip()));
            res['stnm'].append(str(fields[27].strip()));
            res['stid'].append(stid);
    ifile.close()
    #DEBUG:    breakpoint();
    res['dts'] = np.array([datetime.datetime.strptime(s,'%Y%m%d%H') for s in res['dtst']]);
    res['dtst'] = np.array(res['dtst']);
    res['lat'] = np.array(res['lat']);
    res['lon'] = np.array(res['lon']);
    res['vmax'] = np.array(res['vmax']);
    res['mslp'] = np.array(res['mslp']);
    res['levl'] = np.array(res['levl']);
    res['rtyp'] = np.array(res['rtyp']);
    res['r34k'] = np.array(res['r34k']);
    res['r34k_1'] = np.array(res['r34k_1']);
    res['r34k_2'] = np.array(res['r34k_2']);
    res['r34k_3'] = np.array(res['r34k_3']);
    res['r34k_4'] = np.array(res['r34k_4']);
    res['r50k'] = np.array(res['r50k']);
    res['r50k_1'] = np.array(res['r50k_1']);
    res['r50k_2'] = np.array(res['r50k_2']);
    res['r50k_3'] = np.array(res['r50k_3']);
    res['r50k_4'] = np.array(res['r50k_4']);
    res['r64k'] = np.array(res['r64k']);
    res['r64k_1'] = np.array(res['r64k_1']);
    res['r64k_2'] = np.array(res['r64k_2']);
    res['r64k_3'] = np.array(res['r64k_3']);
    res['r64k_4'] = np.array(res['r64k_4']);
    res['ocip'] = np.array(res['ocip']);
    res['roci'] = np.array(res['roci']);
    res['rmw'] = np.array(res['rmw']);
    res['tdir'] = np.array(res['tdir']);
    res['tspeed'] = np.array(res['tspeed']);
    res['stnm'] = np.array(res['stnm']);
    res['stid'] = np.array(res['stid']);
    #DEBUG:    breakpoint();
    return res;
#def read_bdeck


def subset_bdeck(res,dtrng):
    '''Return the subset of the Best Track DICT "res" such that res["dts"] lies between dtrng[0] and dtrng[-1] inclusive.'''
    if ( dtrng is not None ):
        newres = {};
        if ( 'iloc' in dir(dtrng) ):
            bdt = dtrng.iloc[0];
            edt = dtrng.iloc[-1];
        else:
            bdt = dtrng[0];
            edt = dtrng[-1];
        ix = np.where((bdt <= res['dts']) & (res['dts'] <= edt));
        for k in res:
            newres[k] = res[k][ix];
        res = newres;
    return(res);


def read_adeck_bad_line(ln):
    if (len(ln) >= 17):
        return(ln[0:17]);
    else:
        raise ValueException(f'Unable to parse line {ln}');
        #return None;

def read_adeck(fname=f'{adeck_idir}/aal142024.dat',grep_patt=None):
    '''Read A-Deck file for a TC, all forecast cycles, all models, and return it as a pandas DataFrame. Merges all records for a given Model, IDate and FHr into a single record, adding Series "r50_rad[1234]" and "r64_rad[1234]". NOTE: Unlike READ_ATCF (below), assumes there are 18 useable columns in adeck, ignoring lines with 30 (or !=18) columns. Because A-Decks can be large, optional GREP_PATT filters only matching lines for reading, e.g., for a specific Initialization Date, YYYYMMDDHH.'''
#AL, 14, 2024100500, 03, CLP5, 120, 297N,  914W,   0,    0,   ,   0,    ,    0,    0,    0,    0, 
#AL, 14, 2024100500, 03, CTCX,   0, 209N,  951W,  24, 1009, XX,  34, NEQ,    0,    0,    0,    0,    0,    0,  86,   0,   0, 
    

    if ( grep_patt is None ):
        data = pd.read_csv(fname,header=None,engine='python',on_bad_lines=read_adeck_bad_line);
    else:
        # Run grep command and capture output
        grep_output = subprocess.check_output(['grep', grep_patt, fname], text=True)
        data = pd.read_csv(io.StringIO(grep_output),header=None,engine='python',on_bad_lines=read_adeck_bad_line);
    data = data.map(lambda x: str2latlon(x,10));
    if ( data.shape[1] == 30 ):
        data = data.iloc[:,0:18];
    if ( data.shape[1] == 18 ):
        data.columns = ['basin','stno','idtstr','mdlno','mdl','fhr','lat','lon','vmax','mslp','typ','rad','windcode','rad1','rad2','rad3','rad4','blank'];
    else:
        raise ValueError(f'ADECK NOT 18 columns! {fname}');
    data['idt'] = pd.to_datetime(data['idtstr'],format='%Y%m%d%H');
    data['vdt'] = data['idt'] + pd.to_timedelta(data['fhr'],'hours');
    data['vdtstr'] = data['vdt'].dt.strftime('%Y%m%d%H');
    # Process ugliness with wind radii...
    data64 = data.loc[data.rad == 64].copy();
    data50 = data.loc[data.rad == 50].copy();
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

def read_atcf(fname=f'{default_idir}/B20C/2018091206/florence06l.2018091206.trak.hwrf.atcfunix'):
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


def read_htcf(idir=f'{default_idir}/ensemble/IJK_o_v0_r341.00/2017090718',domn='d03',idt=None):
    #/scratch2/AOML/aoml-hafs1/Lew.Gramer/ocean/ensemble/IJK_o_v0_r341.00/2017090718/hifreq_d03.htcf
    #fname = f'{idir}/{mem}/{dtst}/hifreq_{domn}.htcf';
    fname = f'{idir}/hifreq_{domn}.htcf';
    data = pd.read_csv(fname,header=None);
    #       0.00,  913.217, 20.605N,  70.341W, 152.209, 20.368N,  70.275W, 20.816N,  70.506W
    #       3.33,  912.576, 20.588N,  70.368W, 152.209, 20.368N,  70.275W, 20.816N,  70.506W
    ##data.applymap(lambda x: np.double(x.replace('N','')) if str(x).endswith('N') else x)
    #data = data.applymap(str2latlon);
    data = data.map(str2latlon);
    data.columns = ['time','mslp','plat','plon','vmax','vlat','vlon','edgelat','edgelon'];
    if ( idt is not None ):
        data['time'] = pd.to_datetime(data['time'],unit='s',origin=idt);
    data['vdist'] = np.sqrt((data.vlon-data.plon)**2 + (data.vlat-data.plat)**2);
    return(data);


def read_mettc(year=2019,stid='AL05',mdl=default_mdl,expt=default_expt,INIT=default_INIT,LEADs=None,ddir=default_ddir):
    '''Read preprocessed MET-TC output (v. parse_mettc.py, and GPLOT README), for year YEAR, storm STID, model initialization time INIT, and optionally, for forecast lead hours LEADs.
    >>> m = read_mettc();
    >>> m.keys()
    Index(['VERSION', 'AMODEL', 'BMODEL', 'DESC', 'STORM_ID', 'BASIN', 'CYCLONE',
           'STORM_NAME', 'INIT', 'LEAD', 'VALID', 'INIT_MASK', 'VALID_MASK',
           'LINE_TYPE', 'TOTAL', 'INDEX', 'LEVEL', 'WATCH_WARN', 'INITIALS',
           'ALAT', 'ALON', 'BLAT', 'BLON', 'TK_ERR', 'X_ERR', 'Y_ERR', 'ALTK_ERR',
           'CRTK_ERR', 'ADLAND', 'BDLAND', 'AMSLP', 'BMSLP', 'AMAX_WIND',
           'BMAX_WIND', 'AAL_WIND_34', 'BAL_WIND_34', 'ANE_WIND_34', 'BNE_WIND_34',
           'ASE_WIND_34', 'BSE_WIND_34', 'ASW_WIND_34', 'BSW_WIND_34',
           'ANW_WIND_34', 'BNW_WIND_34', 'AAL_WIND_50', 'BAL_WIND_50',
           'ANE_WIND_50', 'BNE_WIND_50', 'ASE_WIND_50', 'BSE_WIND_50',
           'ASW_WIND_50', 'BSW_WIND_50', 'ANW_WIND_50', 'BNW_WIND_50',
           'AAL_WIND_64', 'BAL_WIND_64', 'ANE_WIND_64', 'BNE_WIND_64',
           'ASE_WIND_64', 'BSE_WIND_64', 'ASW_WIND_64', 'BSW_WIND_64',
           'ANW_WIND_64', 'BNW_WIND_64', 'ARADP', 'BRADP', 'ARRP', 'BRRP', 'AMRD',
           'BMRD', 'AGUSTS', 'BGUSTS', 'AEYE', 'BEYE', 'ADIR', 'BDIR', 'ASPEED',
           'BSPEED', 'ADEPTH', 'BDEPTH', 'VMAX_ERR', 'MSLP_ERR', 'R34_ERR',
           'R50_ERR', 'R64_ERR', 'stid', 'idtst', 'vdtst', 'idts', 'vdts', 'fhr',
           'GPLOT_head', 'GPLOT_tail', 'GPLOT_WIND_ERR', 'GPLOT_ABS_WIND_ERR',
           'GPLOT_TK_ERR', 'GPLOT_ALTK_ERR', 'GPLOT_CRTK_ERR'],
          dtype='object')
    '''
    try:
        tcst = pd.read_pickle(f'{ddir}/tcst.{expt}.{year}.pickle')
    #except FileNotFoundError as EX:
        #raise FileNotFoundError(f'No METTC file "{ddir}/tcst.{year}.pickle"? Try running PARSE_METTC.py') from EX;
    except Exception as EX:
        #https://stackoverflow.com/questions/9157210/how-do-i-raise-the-same-exception-with-a-custom-message-in-python
        raise Exception(f'No METTC file "{ddir}/tcst.{year}.pickle"? Try running PARSE_METTC.py').with_traceback(EX.__traceback__);
        # print(f'No METTC file "{ddir}/tcst.{year}.pickle"? Try running PARSE_METTC.py');
        # raise EX;
    
    if ( INIT is not None ):
        if ( LEADs is not None ):
            trks = tcst.loc[(tcst.AMODEL == mdl) & (tcst.STORM_ID==f'{stid}{year}') & (tcst.INIT==INIT) & (tcst.LEAD.isin(LEADs))].copy();
        else:
            trks = tcst.loc[(tcst.AMODEL == mdl) & (tcst.STORM_ID==f'{stid}{year}') & (tcst.INIT==INIT)].copy();
    else:
        trks = tcst.loc[(tcst.AMODEL == mdl) & (tcst.STORM_ID==f'{stid}{year}')].copy();
    return(trks);
#read_mettc(year=2019,stid='AL05',mdl=default_mdl,expt=default_expt,INIT=default_INIT,LEADs=None,ddir=default_ddir):


def read_ships_shear(stid='larry12l',idtst='2021090206',shipsdir=f'{default_idir}/data/ships'):
    shrd = np.loadtxt(f'{shipsdir}/{stid}.SHRD.{idtst}.ships.dat') # Deep Shear Magnitude
    shtd = np.loadtxt(f'{shipsdir}/{stid}.SHTD.{idtst}.ships.dat') # Deep Shear Direction
    ds = pd.DataFrame(shrd,columns=['Hour','SHRD']);
    ds = ds.merge(pd.DataFrame(shtd,columns=['Hour','SHTD']));
    ds.index = shrd[:,0];
    return(ds);

def read_ships_center(stid='larry12l',idtst='2021090206',shipsdir=f'{default_idir}/data/ships'):
    tccen = np.loadtxt(f'{shipsdir}/{stid}.TCCEN.{idtst}.ships.dat') # TC Center
    tchod = np.loadtxt(f'{shipsdir}/{stid}.TCHODO.{idtst}.ships.dat') # Hodograph Magnitude and Direction
    ds = pd.DataFrame(tccen,columns=['Hour','Pressure','Lat','Lon','Aligned']);
    ds = ds.merge(pd.DataFrame(tchod,columns=['Hour','Pressure','HODm','HODd']));
    #ds.index = tccen[:,0];
    return(ds);

def read_remss_sst(dtst='20190903',idir=default_idir):
    '''Read netCDF data file for REMSS mw_ir (blended microwave/infrared, MWIR) SST. See also: /lfs4/HFIP/hur-aoml/Lew.Gramer/ocean/get-historical-MISST.csh'''
    dtst = dtst[0:8] + '120000';
    # Always prefer the postprocessed file over real-time, if it is present
    fname = f"{idir}/satellite/{dtst}-REMSS-L4_GHRSST-SSTfnd-MW_IR_OI-GLOB-v02.0-fv05.0.nc";
    if ( not os.path.isfile(fname) ):
        fname = f"{idir}/satellite/{dtst}-REMSS-L4_GHRSST-SSTfnd-MW_IR_OI-GLOB-v02.0-fv05.0-rt.nc";
    if ( not os.path.isfile(fname) ):
        raise FileNotFoundError(errno.ENOENT, os.strerror(errno.ENOENT), fname);
    print(fname);
    nc = nc4.Dataset(fname);
    lat = nc['lat'][:];
    lon = nc['lon'][:];
    sst = np.squeeze(nc['analysed_sst'][:,:,:]) - 273.14;
    nc.close();
    return(lon,lat,sst);
#read_remss_sst(dtst='20190903',idir=default_idir):


def read_hycom_archv_b(basefnm):
  '''Read .b file containing text description of the format contained in the raw binary .a file from a HYCOM native output.'''
  hycom={}  #define empty dictionary
  bfilenm = basefnm+'.b'
  lines=[line.rstrip() for line in open(bfilenm)]
  idm=int([line.split() for line in lines if 'longitudinal' in line][0][0])
  jdm=int([line.split() for line in lines if 'latitudinal' in line][0][0])
  hycom['idm']=idm
  hycom['jdm']=jdm
  
  count=0
  for line in lines:
    count+=1
    if (line[0:5] == 'field'):
      break
  lines=lines[count:]
  vars=[line.split()[0] for line in lines]
  
  count=0
  for var in vars:
    count += 1
    if hycom.__contains__(var):
      hycom[var]=hycom[var] +[count] 
    else:
      hycom[var]=[count] 
  return(hycom);
#read_hycom_archv_b(bfilenm)

def read_hycom_fix_grid(idm,jdm,domnm='htrop',hafs_fix_idir=default_fix_idir,ddir=default_ddir):
  '''Read HYCOM GRID fix file for domain DOMNM with x- and y-grid templates IDM and JDM.'''
  ijdm=idm*jdm
  
  pickle_fname = f'{ddir}/{domnm}_hycom_fix_grid.pickle';
  if ( os.path.exists(pickle_fname) ):
    #DEBUG:    print(f'Loading {pickle_fname}: {datetime.datetime.now()}');
    res = pd.read_pickle(pickle_fname);
    #DEBUG:    print(f'DONE loading {pickle_fname}: {datetime.datetime.now()}');
  else:
    res = {};
    fname = f'{hafs_fix_idir}/hafs_hycom_{domnm}.basin.regional.grid.a';
    print(f'Loading {fname} at {datetime.datetime.now()}');
    fid=open(fname,'rb')
    # plon:  min,max =        24.96000      354.95999
    # plat:  min,max =       -42.97202       46.98734
    # qlon:  min,max =        24.92000      354.92001
    # qlat:  min,max =       -43.00128       46.96005
    # ulon:  min,max =        24.92000      354.92001
    # ulat:  min,max =       -42.97202       46.98734
    # vlon:  min,max =        24.96000      354.95999
    # vlat:  min,max =       -43.00128       46.96005
    for fldnm in ('plon','plat','qlon','qlat','ulon','ulat','vlon','vlat'):
      #DEBUG:
      print(f'{fldnm} at {datetime.datetime.now()}');
      res[fldnm] = ma.array([],fill_value=1e30);
      fld=fid.read(ijdm*4)
      fld=struct.unpack('>'+str(ijdm)+'f',fld)
      fld=np.array(fld)
      fld=ma.reshape(fld,(jdm,idm))
      res[fldnm]=fld.copy()
      res[fldnm]=ma.masked_greater(res[fldnm],1e10)
    fid.close()
    #DEBUG:      print(f'Saving {pickle_fname}: {datetime.datetime.now()}');
    with open(pickle_fname, 'wb') as fid:
      pickle.dump(res,fid,protocol=pickle.HIGHEST_PROTOCOL);
      print(f'Saved {pickle_fname}: {datetime.datetime.now()}');
  
  return(res)
#read_hycom_fix_grid

def read_mom6_fix_depth(domnm='nhc',hafs_fix_mom6_idir=default_fix_mom6_idir):
  #/scratch1/NCEPDEV/hwrf/noscrub/hafs-fix-files/hafs-20240703-fix/fix/fix_mom6/nhc/ocean_topog.nc
  ds = xr.open_dataset(f'{hafs_fix_mom6_idir}/{domnm}/ocean_topog.nc');
  res = ds.depth.values;
  return(res);

def read_hycom_fix_depth(domnm='htrop',idm=None,jdm=None,hafs_fix_idir=default_fix_idir,ddir=default_ddir):
  '''Read HYCOM DEPTH fix file for domain DOMNM with x- and y-grid templates IDM and JDM. If IDM or JDM not given, reads .b file for them both.'''
  
  pickle_fname = f'{ddir}/{domnm}_hycom_fix_depth.pickle';
  if ( os.path.exists(pickle_fname) ):
    #DEBUG:    print(f'Loading {pickle_fname}: {datetime.datetime.now()}');
    res = pd.read_pickle(pickle_fname);
    #DEBUG:    print(f'DONE loading {pickle_fname}: {datetime.datetime.now()}');
  else:
    res = {};
    basefname = f'{hafs_fix_idir}/hafs_hycom_{domnm}.basin.regional.depth';
    if ( idm is None or jdm is None ):
        lines=[line.rstrip() for line in open(basefname+'.b')]
        rangelines = [line.split() for line in lines if 'i/jdm = ' in line];
        idm = int(rangelines[0][2])
        jdm = int(rangelines[0][3].split(';')[0])
    fname = basefname+'.a';
    ijdm=idm*jdm
    print(f'Loading {fname} at {datetime.datetime.now()}');
    fid=open(fname,'rb')
    fldnm = 'h';
    res[fldnm] = ma.array([],fill_value=1e30);
    fld=fid.read(ijdm*4)
    fld=struct.unpack('>'+str(ijdm)+'f',fld)
    fld=np.array(fld)
    fld=ma.reshape(fld,(jdm,idm))
    res[fldnm]=fld.copy()
    res[fldnm]=ma.masked_greater(res[fldnm],1e10)
    fid.close()
    #DEBUG:      print(f'Saving {pickle_fname}: {datetime.datetime.now()}');
    with open(pickle_fname, 'wb') as fid:
      pickle.dump(res,fid,protocol=pickle.HIGHEST_PROTOCOL);
      print(f'Saved {pickle_fname}: {datetime.datetime.now()}');
  return(res)
#read_hycom_fix_depth

def read_hycom_archv(basefnm,domnm='htrop'):
  '''Parse FIX and forecast .a and .b files for HYCOM ARCHV 3D data.'''
  hycom = read_hycom_archv_b(basefnm)
  ijdm=hycom['idm']*hycom['jdm']
  npad=4096-(ijdm%4096)
  
  #res = {}
  #DEBUG:  print('Reading FIX grid file');
  res = read_hycom_fix_grid(hycom['idm'],hycom['jdm'],domnm);
  #DEBUG:  print('READ FIX grid file');
  
  #DEBUG:  print('Reading FIX depth file');
  hres = read_hycom_fix_depth(domnm,hycom['idm'],hycom['jdm']);
  #DEBUG:  print('READ FIX depth file');
  res['h'] = -hres['h'];
  del(hres);
  #DEBUG:  breakpoint();
  
  afilenm = basefnm+'.a'
  
  #DEBUG:  print(f'START loading {afilenm} at {datetime.datetime.now()}');
  fid=open(afilenm,'rb')
  
  # montg1:	Montgomery Potential (1)
  # srfhgt:	Sea Surface Height
  # surflx:	Sea Surface Heat Flux
  # salflx:	Sea Surface Salt Flux
  # bl_dpth:	Surface Boundary Layer Depth
  # mix_dpth:	Mixed Layer Depth
  # u_btrop:	Barotropic W-E Current
  # v_btrop:	Barotropic S-N Current
  # u-vel.:	Per Layer W-E Current Profile
  # v-vel.:	Per Layer S-N Current Profile
  # thknss:	Layer Thicknesses
  # temp: 	Per Layer Temperature
  # salin:	Per Layer Salinity
  for fldnm in ('montg1','srfhgt','surflx','salflx','bl_dpth','mix_dpth','u_btrop','v_btrop','u-vel.','v-vel.','thknss','temp','salin'):
    #DEBUG:
    print(f'{fldnm} at {datetime.datetime.now()}');
    res[fldnm] = ma.array([],fill_value=1e30);
    Index=hycom[fldnm]
    for lyr in range(0,len(Index),1):
      #DEBUG:
      # Are all these file seeks slowing us way down??
      fid.seek((hycom[fldnm][lyr-1]-1)*4*(npad+ijdm),0)
      fld=fid.read(ijdm*4)
      fld=struct.unpack('>'+str(ijdm)+'f',fld)
      fld=np.array(fld)
      fld=ma.reshape(fld,(hycom['jdm'],hycom['idm']))
      if res[fldnm].size == 0:
        res[fldnm]=fld.copy()
      else:
        res[fldnm]=ma.dstack((res[fldnm],fld))
    res[fldnm]=ma.masked_greater(res[fldnm],1e10)
    res[fldnm][res[fldnm]>1e10] = np.nan
  fid.close()
  #DEBUG:  print(f'DONE loading {afilenm} at {datetime.datetime.now()}');
  
  return(res);
#read_hycom_archv

def subset_hycom_archv(hycom_ds_all,minlon,maxlon,minlat,maxlat):
  '''Return hycom dataset restricted to a bounding box.'''
  hycom_ds = {}
  hycom_lon_all = hycom_ds_all['plon'][0,:];
  hycom_lat_all = hycom_ds_all['plat'][:,-1];
  hycom_lon_ix = np.argwhere((minlon<=hycom_lon_all) & (hycom_lon_all<=maxlon));
  hycom_lat_ix = np.argwhere((minlat<=hycom_lat_all) & (hycom_lat_all<=maxlat));
  for k in hycom_ds_all.keys():
    if ( len(hycom_ds_all[k].shape) > 2 ):
      #hycom_ds[k] = hycom_ds_all[k][hycom_lat_ix[np.newaxis,:],hycom_lon_ix[:,np.newaxis],:].squeeze()
      hycom_ds[k] = hycom_ds_all[k][hycom_lat_ix,hycom_lon_ix.T,:]
    else:
      #hycom_ds[k] = hycom_ds_all[k][hycom_lat_ix[np.newaxis,:],hycom_lon_ix[:,np.newaxis]].T
      hycom_ds[k] = hycom_ds_all[k][hycom_lat_ix,hycom_lon_ix.T]
  #breakpoint();
  return(hycom_ds);
#subset_hycom_archv

def retrieve_hycom_archv(expt,IDTST,vdt=None,fhr=None,domnm='htrop',idir=default_idir,ddir=default_ddir):
  '''Call read_hycom_archv and pickle result. If pickle already exists, load it instead.'''
  res = None;
  
  idt = datetime.datetime.strptime(IDTST,'%Y%m%d%H');
  if ( fhr is not None ):
    if ( vdt is not None ):
      raise ValueError('Specified both "vdt" and "fhr"');
    vdt = idt + datetime.timedelta(hours=fhr);
  hycom_vdtst = vdt.strftime('%Y_%j_%H');
  hycom_fname = f'{idir}/{expt}/{IDTST}/archv.{hycom_vdtst}';
  pickle_fname = f'{ddir}/{expt}_{IDTST}.archv.{hycom_vdtst}';
  # The following two lines will create pickles on Hera DISTINCT from those on Jet
  if ( is_hera() ):
    pickle_fname = pickle_fname + '.HERA';
  pickle_fname = pickle_fname + '.pickle';
  
  if ( os.path.exists(pickle_fname) ):
    #DEBUG:
    print(f'Loading {pickle_fname}: {datetime.datetime.now()}');
    res = pd.read_pickle(pickle_fname);
    if ( 'file' not in res ):
      res['file'] = hycom_fname;
    #DEBUG:
    print(f'DONE loading {pickle_fname}: {datetime.datetime.now()}');
    # HACK HACK HACK
    if ( 'h' not in res.keys() ):
        #DEBUG:        print('Reading FIX depth file');
        hres = read_hycom_fix_depth(domnm);
        #DEBUG:        print('READ FIX depth file');
        res['h'] = -hres['h'];
        del(hres);
        # If we just added depth to this pickle file - RESAVE
        with open(pickle_fname, 'wb') as fid:
            pickle.dump(res,fid,protocol=pickle.HIGHEST_PROTOCOL);
            print(f'(RE-)Saved {pickle_fname}: {datetime.datetime.now()}');
        #DEBUG:        breakpoint();
        
  else:
    #DEBUG:    print(f'Parsing for {pickle_fname}: {datetime.datetime.now()}');
    print(hycom_fname);
    if ( not os.path.exists(hycom_fname+'.a') or not os.path.exists(hycom_fname+'.b') ):
      print(f'MISSING {hycom_fname}.[ab]');
    else:
      res = read_hycom_archv(hycom_fname);
      res['file'] = hycom_fname;
      res['time'] = vdt;
      #DEBUG:      print(f'Saving {pickle_fname}: {datetime.datetime.now()}');
      with open(pickle_fname, 'wb') as fid:
        pickle.dump(res,fid,protocol=pickle.HIGHEST_PROTOCOL);
        print(f'Saved {pickle_fname}: {datetime.datetime.now()}');
  return(res);
#retrieve_hycom_archv

def convert_hycom_dict_to_xarray(res):
  dsres = {}
  # for fldnm in ('plon','plat','qlon','qlat','ulon','ulat','vlon','vlat'):
  # for fldnm in ('montg1','srfhgt','surflx','salflx','bl_dpth','mix_dpth','u_btrop','v_btrop','u-vel.','v-vel.','thknss','temp','salin'):
  # dsres = {
  #     "t": {"dims": ("t"), "data": t},
  #     "a": {"dims": ("t"), "data": x},
  #     "b": {"dims": ("t"), "data": y},
  # }
  #   #"attrs": {"file": res['file']},
  #dsres = { "plon": {"dims": ("plon"), "data": res['plon'][0,:]}, "plat": {"dims": ("plat"), "data": res['plat'][:,-1]}, "temp": {"dims": ("plat","plon","vert"), "data": res['temp']} };
  #        "time": {"dims": ("time"), "data": res['time'], "attrs": {"units": "datetime64"}},
  #        "time": {"dims": ("time"), "data": 0, "attrs": {"units": "datetime64"}},
  if ( 'z' not in res ):
      #DEBUG:      print(f'CALC Z {datetime.datetime.now()}');
      # https://github.com/abozec/BB86_PACKAGE/blob/master/MATLAB/plot_res_bb86.m
      rho = 1000.0;	# reference density
      g = 9.806;	# gravity
      res['bldm'] = res['bl_dpth'] / (rho*g);  # *1./(rho*g) to get in m
      res['mldm'] = res['mix_dpth'] / (rho*g);  # *1./(rho*g) to get in m
      res['thkm'] = res['thknss'] / (rho*g);  # *1./(rho*g) to get in m
      res['z'] = res['thkm'].cumsum(axis=2);
      #DEBUG:      print(f'CALCed Z {datetime.datetime.now()}');
  dt64 = [np.datetime64(res['time'])];
  dsres = {
      "coords": {
          "time": {"dims": ("time"), "data": dt64, "attrs": {"units": "datetime64"}},
          "plon": {"dims": ("plon"), "data": res['plon'][0,:], "attrs": {"units": "deg"}},
          "plat": {"dims": ("plat"), "data": res['plat'][:,-1], "attrs": {"units": "deg"}},
          "vert": {"dims": ("vert"), "data": range(0,res['temp'].shape[2]), "attrs": {"units": "level"}},
      },
      "attrs": {"file": res['file']},
      "dims": ("time","plat","plon","vert"),
      "data_vars": {
          "h": {"dims": ("plat","plon"), "data": res['h']},
          "elb": {"dims": ("plat","plon"), "data": res['srfhgt']},
          "flx": {"dims": ("plat","plon"), "data": res['surflx']},
          "bld_p": {"dims": ("plat","plon"), "data": res['bl_dpth']},
          "mld_p": {"dims": ("plat","plon"), "data": res['mix_dpth']},
          "thk_p": {"dims": ("plat","plon","vert"), "data": res['thknss']},
          "bld": {"dims": ("plat","plon"), "data": res['bldm']},
          "mld": {"dims": ("plat","plon"), "data": res['mldm']},
          "thk": {"dims": ("plat","plon","vert"), "data": res['thkm']},
          "z": {"dims": ("plat","plon","vert"), "data": res['z']},
          "u": {"dims": ("plat","plon","vert"), "data": res['u-vel.']},
          "v": {"dims": ("plat","plon","vert"), "data": res['v-vel.']},
          "t": {"dims": ("plat","plon","vert"), "data": res['temp']},
          "s": {"dims": ("plat","plon","vert"), "data": res['salin']},
      },
  };
  ds = xr.Dataset.from_dict(dsres);
  return(ds);

def retrieve_hycom_archv_as_xarray(expt,IDTST,vdt=None,fhr=None,domnm='htrop',idir=default_idir,ddir=default_ddir):
  res = retrieve_hycom_archv(expt,IDTST,vdt,fhr,domnm,idir,ddir);
  if ( res is None ):
    return(None);
  else:
    return(convert_hycom_dict_to_xarray(res));


def fastload_hafs_grb2(gbpat,sd,var='sst',preproc=(lambda x: x-273.14)):
  '''Reading GRB2 with xarray(v.) takes ~ 50 s per file, or > 30 min per forecast!'''
  if ( sd is None ):
    sd = {};
  gb = pygrib.open(gbpat);
  gb.seek(0);
  # sst_var = gb.select(name='Sea surface temperature')[0];
  # res['lat'],res['lon'] = sst_var.latlons();
  # sst_mno = sst_var.messagenumber;
  # res['sst'] = np.empty((len(res['hrs']),n_lat,n_lon));
  #var = gb.select(shortname='sst')[0];
  try:
    v = gb.select(shortName=var)[0];
  except:
    breakpoint()
  dt = np.datetime64(datetime.datetime.strptime(str(v['dataDate']),'%Y%m%d') + datetime.timedelta(hours=v['hour']+v['forecastTime']));
  if ( 'lat' not in sd ):
    (sd['lat'],sd['lon']) = v.latlons();
    sd['vdt'] = dt;
  if ( var not in sd ):
    sd[var] = pd.Series();
  sd[var][dt] = preproc(v.values);
  return(sd);


def drop_duplicates(ds,dim,keep="first"):
    indexes = {dim: ~ds.get_index(dim).duplicated(keep=keep)}
    return ds.isel(indexes)

def drop_duplicates_all_dims(ds, keep="first"):
    deduplicated = ds
    for dim in ds.dims:
        indexes = {dim: ~deduplicated.get_index(dim).duplicated(keep=keep)}
        deduplicated = deduplicated.isel(indexes)
    return deduplicated


def cacheload_hafs_grb2(gbpat,ds,ncpat=None,types=['surface.instant','surface.avg','surface.accum','surface.max','10m','10m.max','pressure','2m','whole'],wraplons=False,debugoutput=False):
  '''
    Load a HAFS_(A/B/D/*) GRB2 file and cache it as one or more netCDF files for future access. For example,
      ds = cacheload_hafs_grb2( \
        gbpat = f'{oceandir}/hafsv0p2a_2021rt/{cpl}/grb2/{idtst}/natl00l.{idtst}.hafsprs.synoptic.0p03.f126.grb2', \
        ncpat = f'{oceandir}/data/hafsv0p2a_2021rt_{cpl}_natl00l.{idtst}.hafsprs.synoptic.0p03.f126', \
        types=['surface.avg',10m']);
    would load all 3-hourly average, level='surface' variables and cache in f'{ncpat}.surface.instant.nc',
    and all heightAboveGround, level='10' variables and cache them in f'{ncpat}.10m.nc'. A DICTIONARY is
    returned with a key for each type of data requested in TYPES. Available TYPES (currently) are:
      surface.instant	= sea/land-surface values, stepType instant[eous]
      surface.avg	= sea/land-surface values, stepType (three-hourly) avg
      surface.accum	= sea/land-surface values, stepType (three-hourly) accumulated
      surface.max	= sea/land-surface values, stepType (three?-hourly) maximum
      10m		= heightAboveGround, level=10 (stepType instant - good for most fields)
      10m.max		= heightAboveGround, level=10 (stepType max - just one value)
      2m		= heightAboveGround, level=2
      pressure		= isobaricInhPa
      whole		= (Whole-air column instanteous: e.g., reflectivity, or total precipitable water. These USED to show as typeOfLevel='unknown'.)
    If a dictionary DS is passed in from a previous call, new TYPEs may be added to it before it is returned.
  '''
  #DEBUG:  print('Started',gbpat,datetime.datetime.now());
  if ( ncpat is None ):
    ncpat = gbpat.replace('.grb2','');
  if ( ncpat == gbpat ):
    raise ValueError(f'GRB2 and NC file patterns match! (Cannot guess NCPAT: {gbpat})');
  for nctyp in types:
    if ( hasattr(ds,'keys') and nctyp in ds.keys() ):
      if ( debugoutput ):
        print(f'Already extracted ds["{nctyp}"]');
      pass;
    elif ( os.path.exists(ncpat+f'.{nctyp}.nc') ):
      # # ds[nctyp] = xr.open_mfdataset(ncpat+f'.{nctyp}.nc',preprocess=drop_duplicates_all_dims);
      # ds[nctyp] = xr.open_mfdataset(ncpat+f'.{nctyp}.nc',concat_dim='valid_time',combine='nested');
      #DEBUG:      breakpoint();
      ds[nctyp] = xr.open_mfdataset(ncpat+f'.{nctyp}.nc',combine='by_coords',preprocess=(lambda y: y.squeeze('time')))
      if ( wraplons ):
        lon = ds[nctyp].longitude.values;
        lon[lon>180] = lon[lon>180] - 360;
        ds[nctyp] = ds[nctyp].assign_coords({'longitude':lon});
      if ( debugoutput ):
        print(ncpat+f'.{nctyp}.nc exists');
    else:
      if ( nctyp == 'surface.instant' ):
        ds[nctyp] = xr.open_mfdataset(gbpat,engine='cfgrib',concat_dim='time',combine='nested',\
                                      backend_kwargs={'filter_by_keys': {'typeOfLevel': 'surface', 'stepType':'instant'},})
      if ( nctyp == 'surface.avg' ):
        ds[nctyp] = xr.open_mfdataset(gbpat,engine='cfgrib',concat_dim='time',combine='nested',\
                                      backend_kwargs={'filter_by_keys': {'typeOfLevel': 'surface', 'stepType':'avg'},})
      if ( nctyp == 'surface.accum' ):
        ds[nctyp] = xr.open_mfdataset(gbpat,engine='cfgrib',concat_dim='time',combine='nested',\
                                      backend_kwargs={'filter_by_keys': {'typeOfLevel': 'surface', 'stepType':'accum'},})
      if ( nctyp == 'surface.max' ):
        ds[nctyp] = xr.open_mfdataset(gbpat,engine='cfgrib',concat_dim='time',combine='nested',\
                                      backend_kwargs={'filter_by_keys': {'typeOfLevel': 'surface', 'stepType':'max'},})
      if ( nctyp == '10m' ):
        ds[nctyp] = xr.open_mfdataset(gbpat,engine='cfgrib',concat_dim='time',combine='nested',\
                                      backend_kwargs={'filter_by_keys': {'typeOfLevel': 'heightAboveGround', 'level': 10, 'stepType':'instant'},})
      if ( nctyp == '10m.max' ):
        ds[nctyp] = xr.open_mfdataset(gbpat,engine='cfgrib',concat_dim='time',combine='nested',\
                                      backend_kwargs={'filter_by_keys': {'typeOfLevel': 'heightAboveGround', 'level': 10, 'stepType':'max'},})
      if ( nctyp == 'pressure' ):
        ds[nctyp] = xr.open_mfdataset(gbpat,engine='cfgrib',concat_dim='time',combine='nested',\
                                      backend_kwargs={'filter_by_keys': {'typeOfLevel': 'isobaricInhPa', 'stepType':'instant'},})
      if ( nctyp == '2m' ):
        ds[nctyp] = xr.open_mfdataset(gbpat,engine='cfgrib',concat_dim='time',combine='nested',\
                                      backend_kwargs={'filter_by_keys': {'typeOfLevel': 'heightAboveGround', 'level': 2, 'stepType':'instant'},});
      if ( nctyp == 'whole' ):
        #DEBUG:        breakpoint();
        # ds[nctyp] = xr.open_mfdataset(gbpat,engine='cfgrib',concat_dim='time',combine='nested',\
        #                               backend_kwargs={'filter_by_keys': {'typeOfLevel': 'unknown', 'stepType':'instant'},});
        ds[nctyp] = xr.open_mfdataset(gbpat,engine='cfgrib',concat_dim='time',combine='nested',\
                                      backend_kwargs={'filter_by_keys': {'typeOfLevel': 'atmosphereSingleLayer', 'stepType':'instant'},});
      if ( wraplons ):
        lon = ds[nctyp].longitude.values;
        lon[lon>180] = lon[lon>180] - 360;
        ds[nctyp] = ds[nctyp].assign_coords({'longitude':lon});
      #DEBUG:      print('EXPANDING DIMS to include "valid_time"');
      # Allows multiple netCDF files for a forecast to be opened using xarray.open_mfdataset: added 2023-05-03
      ds[nctyp] = ds[nctyp].expand_dims('valid_time');
      ds[nctyp].to_netcdf(ncpat+f'.{nctyp}.nc');
      if ( debugoutput ):
        print(ncpat+f'.{nctyp}.nc',datetime.datetime.now());
  #DEBUG:  print('Finished',gbpat,datetime.datetime.now());
  return(ds);

#https://stackoverflow.com/questions/27320549/disable-displaying-coordinate-in-matplotlib-plot
def disable_data_cursor(ax):
    '''ax.format_coord = lambda x, y: '';'''
    ax.format_coord = lambda x, y: '';

def resize_figure(f,w,h):
    '''Resize figure f to width w, height h'''
    mgr = plt.get_fig_manager(f).resize(w,h);

# https://stackoverflow.com/questions/7449585/how-do-you-set-the-absolute-position-of-figure-windows-with-matplotlib
def reposition_figure(f, x, y, w=None, h=None):
    '''Move figure's upper left corner to pixel (x,y). If w,h are not None, also calls resize_figure(v.)'''
    backend = plt.get_backend();
    #https://stackoverflow.com/questions/3580027/how-do-you-determine-which-backend-is-being-used-by-matplotlib
    #matplotlib.use('agg')
    if backend == 'TkAgg':
        f.canvas.manager.window.wm_geometry("+%d+%d" % (x, y));
    elif backend == 'WXAgg':
        f.canvas.manager.window.SetPosition((x, y));
    else:
        # This works for QT and GTK: You can also use window.setGeometry
        f.canvas.manager.window.move(x, y);
    if ( w is not None and h is not None ):
        resize_figure(f,w,h);

def lat_daspect(ax=None,lat=25):
    '''Set aspect ratio of X-Y axes AX, based on the figure center latitude LAT. If LAT is non-scalar, use np.median(LAT).'''
    if ( ax is None ):
        ax = plt.gca();
    if ( not np.isscalar(lat) ):
        lat = np.median(lat);
    ar = np.cos(np.deg2rad(lat));
    ax.set_aspect(ar);

#https://stackoverflow.com/questions/44726675/custom-markers-using-python-matplotlib
def make_hurricane_marker(ratio=3):
    '''Make a "circle-S" shaped hurricane Marker for plot() calls.'''
    u = np.array([  [2.444,7.553],
                    [0.513,7.046],
                    [-1.243,5.433],
                    [-2.353,2.975],
                    [-2.578,0.092],
                    [-2.075,-1.795],
                    [-0.336,-2.870],
                    [2.609,-2.016]  ])
    u[:,0] -= 0.098
    codes = [1] + [2]*(len(u)-2) + [2] 
    u = np.append(u, -u[::-1], axis=0)
    codes += codes
    return mpath.Path(ratio*u,codes,closed=False)


def plot_wind_contours(lon,lat,spd,minspd=34):
    '''Plot contour lines of wind field in colors appropriate for each Saffir-Simpson range.'''
    rngs = NHC_vmax_ranges();
    clrs = NHC_vmax_colors();
    plt.contour(lon,lat,spd,levels=rngs,colors=clrs);

def plot_bdeck_track(res,year=2019,stid='AL05',fh=None,ax=None,curdate=None,windradius=None,windrng='NHC',colorbar='default',dtrng=None):
    '''Plot Best Track, track-wide intensity (in color, scaled by WINDRNG), and if optional CURDATE is specified, current storm location and wind radius.'''
    #windrng=range(0,170)
    if ( res is None ):
        res = read_bdeck(year,stid);
    res = subset_bdeck(res,dtrng);
    #fig, axs = plt.subplots(2, 1, sharex=True, sharey=True)
    if ( fh is None ):
        print(f'NEW FIGURE in plot_bdeck_track!');
        fh = plt.figure(figsize=(11,8));
    if ( ax is None ):
        ax = plt.gca();
    # https://matplotlib.org/3.1.1/gallery/lines_bars_and_markers/multicolored_line.html
    cmap = 'jet';
    if ( windrng is None ):
        norm = plt.Normalize(res['vmax'].min(), res['vmax'].max());
    elif ( windrng == 'NHC' ):
        bounds = NHC_vmax_ranges();
        cmap = NHC_vmax_cmap();
        norm = BoundaryNorm(boundaries=bounds, ncolors=len(bounds)-1);
    elif ( hasattr(windrng,'min') ):
        norm = plt.Normalize(windrng.min(), windrng.max());
    elif ( hasattr(windrng,'start') ):
        norm = plt.Normalize(windrng.start, windrng.stop);
    else:
        raise ValueError(f'Do not understand WINDRNG kwarg');

    # https://stackoverflow.com/questions/12729529/can-i-give-a-border-outline-to-a-line-in-matplotlib-plot-function/35762000
    points = np.array([res['lon'], res['lat']]).T.reshape(-1, 1, 2)
    segments = np.concatenate([points[:-1], points[1:]], axis=1)
    lc = LineCollection(segments, cmap=cmap, norm=norm)
    # Set the values used for colormapping
    lc.set_array(res['vmax'])
    lc.set_linewidth(3)
    #DO MARKERS INSTEAD:
    lc.set_path_effects([path_effects.Stroke(linewidth=5,foreground='black'),path_effects.Normal()]);
    line = ax.add_collection(lc)
    #DEBUG:    print('BDECK'); breakpoint();
    #https://stackoverflow.com/questions/54199914/access-attribute-of-elements-within-numpy-array
    ix = np.vectorize(lambda x: x.hour)(res['dts']) == 0;
    plt.plot(res['lon'][ix],res['lat'][ix],'x',color='black',
             markersize=6,markerfacecolor='None',markeredgecolor='black', markeredgewidth=1.5);
    #https://matplotlib.org/3.1.1/tutorials/advanced/patheffects_guide.html

    if ( colorbar is not None ):
        if ( str(type(colorbar)).lower().find('colorbar') < 0 ):
            colorbar = fh.colorbar(line, ax=ax);
        colorbar.ax.set_ylabel('Vmax [kt.]');
    if ( curdate is not None ):
        dtix,curdt = min( enumerate(res['dts']), key=(lambda x: abs(x[1] - curdate)) );
        plt.plot(res['lon'][dtix],res['lat'][dtix],'kx');
        if ( windradius is not None ):
            try:
                #DEBUG:                print('BEST trying to plot windradius'); code.interact(local=locals());
                switcher = {
                    'roci': 	res['roci'][dtix],
                    'rmw':	res['rmw'][dtix],
                    'r34k':	res['r34k'][dtix],
                    'r50k':	res['r50k'][dtix],
                    'r64k':	res['r64k'][dtix],
                };
                radkm = switcher.get(windradius,windradius);
                raddeg = radkm / 111;
                #https://stackoverflow.com/questions/9215658/plot-a-circle-with-pyplot
                cir = plt.Circle((res['lon'][dtix],res['lat'][dtix]),radius=raddeg,color='black',linewidth=3,fill=False);
                ax.add_artist(cir);
                #Fancier method: https://matplotlib.org/3.1.1/gallery/shapes_and_collections/patch_collection.html#sphx-glr-gallery-shapes-and-collections-patch-collection-py
            except Exception as EX:
                print(f'Unable to plot BEST storm radius {windradius} for {curdt}');
                raise EX;
    
    return(fh,res,line);
#plot_bdeck_track


def plot_bdeck_intensity(res,year=2019,stid='AL05',fh=None,colorbar='default'):
    if ( res is None ):
        res = read_bdeck(year,stid);
    # https://matplotlib.org/3.1.1/gallery/text_labels_and_annotations/date.html
    dts = pltdts.date2num(res['dts']);
    
    if ( fh is None ):
        #fig, axs = plt.subplots(2, 1, sharex=True, sharey=True)
        fh = plt.figure(figsize=(11,8));
    ax = plt.gca();
    norm = plt.Normalize(res['vmax'].min(), res['vmax'].max())
    # Draw thicker black line to form outline of intensity-colored plot below
    plt.plot(res.ALON,res.ALAT,'k-',linewidth=3);
    points = np.array([dts, res['vmax']]).T.reshape(-1, 1, 2)
    segments = np.concatenate([points[:-1], points[1:]], axis=1)
    #lc = LineCollection(segments, cmap='viridis', norm=norm)
    lc = LineCollection(segments, cmap='jet', norm=norm)
    # Set the values used for colormapping
    lc.set_array(res['vmax'])
    lc.set_linewidth(2)
    line = ax.add_collection(lc)
    
    # https://matplotlib.org/3.1.1/gallery/text_labels_and_annotations/date.html
    # Oh how I miss my old DATETICK3 M-function... :(
    ax.xaxis.set_major_locator(pltdts.DayLocator())                     # every day
    ax.xaxis.set_minor_locator(pltdts.HourLocator(range(0,24,6)))       # every 6 h
    ax.xaxis.set_major_formatter(pltdts.DateFormatter('%m%d'))          # tick format
    ax.format_xdata = pltdts.DateFormatter('%m%d%H')			# coords message box format
    
    plt.axis('tight');
    if ( colorbar is not None ):
        if ( str(type(colorbar)).lower().find('colorbar') < 0 ):
            colorbar = fh.colorbar(line, ax=ax);
        colorbar.ax.set_ylabel('Vmax [kt.]');
    return(fh,res,line);
#plot_bdeck_intensity(res,year=2019,stid='AL05'):


#### FUNCTION plot_atcf_track
def plot_atcf_track(res,fh=None,ax=None,curdate=None,windradius=None,windrng='NHC',colorbar='default'):
    '''Plot a map of model track from "ATCF" output, color-coded for intensity. If curdate is a datetime, highlight that point on the track, including optional windradius circle. If curdate is iterable, highlight all those dates. If WINDRADIUS is not None, plot a circle showing radius of winds at that velocity ("r34","r50","r64","rmw").'''
    #windrng=range(0,170)
    #fig, axs = plt.subplots(2, 1, sharex=True, sharey=True)
    if ( fh is None ):
        print(f'NEW FIGURE in plot_atcf_track!');
        fh = plt.figure(figsize=(11,8));
    if ( ax is None ):
        ax = plt.gca();
    cmap = 'jet';
    if ( windrng is None ):
        norm = plt.Normalize(res.vmax.min(), res.vmax.max());
    elif ( windrng == 'NHC' ):
        bounds = NHC_vmax_ranges();
        cmap = NHC_vmax_cmap();
        norm = BoundaryNorm(boundaries=bounds, ncolors=len(bounds)-1);
    elif ( hasattr(windrng,'min') ):
        norm = plt.Normalize(windrng.min(), windrng.max());
    elif ( hasattr(windrng,'start') ):
        norm = plt.Normalize(windrng.start, windrng.stop);
    else:
        raise ValueError(f'Do not understand WINDRNG kwarg');
    
    # https://stackoverflow.com/questions/12729529/can-i-give-a-border-outline-to-a-line-in-matplotlib-plot-function/35762000
    #DEBUG:    plt.plot(res.lon,res.lat,':',linewidth=5,color='red');
    points = np.array([res.lon, res.lat]).T.reshape(-1, 1, 2)
    segments = np.concatenate([points[:-1], points[1:]], axis=1)
    lc = LineCollection(segments,cmap=cmap,norm=norm)
    # Set the values used for colormapping
    lc.set_array(res.vmax)
    lc.set_linewidth(3)
    ##lc.set_path_effects([path_effects.Stroke(linewidth=5,foreground='red'),path_effects.PathPatchEffect(linestyle=':'),path_effects.Normal()]);
    #DO MARKERS INSTEAD:
    #lc.set_path_effects([path_effects.PathPatchEffect(linewidth=5,color='red',linestyle='-'),path_effects.Normal()]);
    #lc.set_path_effects([path_effects.PathPatchEffect(linewidth=5,color='darkgray',linestyle=':'),path_effects.Normal()]);
    lc.set_path_effects([path_effects.PathPatchEffect(linewidth=5,color='k',linestyle=':'),path_effects.Normal()]);
    line = ax.add_collection(lc)
    #DEBUG:    print('ATCF'); breakpoint();
    # Find the index of every valid date whose HOURS is 0 (i.e., every 24 h of forecast)
    ix = res.vdt.dt.hour == 0;
    # plt.plot(res.lon[ix],res.lat[ix],'s',color='red',
    #          markersize=6,markerfacecolor='None',markeredgecolor='red', markeredgewidth=1.5);
    # plt.plot(res.lon[ix],res.lat[ix],'s',color='darkgray',
    #          markersize=6,markerfacecolor='None',markeredgecolor='darkgray', markeredgewidth=1.5);
    plt.plot(res.lon[ix],res.lat[ix],'s',color='k',
             markersize=8,markerfacecolor='None',markeredgecolor='black', markeredgewidth=1.5);
    #https://matplotlib.org/3.1.1/tutorials/advanced/patheffects_guide.html
    
    if ( colorbar is not None ):
        if ( str(type(colorbar)).lower().find('colorbar') < 0 ):
            colorbar = fh.colorbar(line, ax=ax);
        colorbar.ax.set_ylabel('Vmax [kt.]');
    if ( curdate is not None ):
        # If not iterable, make iterable
        if ( not hasattr(curdate, "__iter__") ):
            curdate = list([curdate]);
        for thiscurdate in curdate:
            dtix,curdt = min(enumerate(res.vdt), key=lambda x: abs(x[1] - thiscurdate));
            #plt.plot(res.lon.iloc[dtix],res.lat.iloc[dtix],'+',color='red');
            #plt.plot(res.lon.iloc[dtix],res.lat.iloc[dtix],'+',color='darkgray');
            plt.plot(res.lon.iloc[dtix],res.lat.iloc[dtix],'+',color='k');
            if ( windradius is not None ):
                try:
                    #DEBUG:                print('ATCF trying to plot windradius'); code.interact(local=locals());
                    switcher = {
                        'roci': 	res.roci.iloc[dtix],
                        'rmw':  	res.rmw.iloc[dtix],
                        'r34':  	res.r34.iloc[dtix],
                        'r50':  	res.r50.iloc[dtix],
                        'r64':  	res.r64.iloc[dtix],
                    };
                    radkm = switcher.get(windradius,windradius);
                    if ( isinstance(radkm,str) or (radkm <= 0) ):
                        print(f'No ATCF storm radius {windradius} for {curdt}');
                        continue;
                    raddeg = radkm / 111;
                    # #cir = plt.Circle((res.lon.iloc[dtix],res.lat.iloc[dtix]),radius=raddeg,color='red',linestyle='--',linewidth=3,fill=False);
                    # #cir = plt.Circle((res.lon.iloc[dtix],res.lat.iloc[dtix]),radius=raddeg,color='darkgray',linestyle=':',linewidth=3,fill=False);
                    # #https://stackoverflow.com/questions/9215658/plot-a-circle-with-pyplot
                    # cir = plt.Circle((res.lon.iloc[dtix],res.lat.iloc[dtix]),radius=raddeg,color='k',linestyle='--',linewidth=3,fill=False);
                    cir = plt.Circle((res.lon.iloc[dtix],res.lat.iloc[dtix]),radius=raddeg,color='dodgerblue',linestyle='--',linewidth=3,fill=False);
                    ax.add_artist(cir);
                    #Fancier method: https://matplotlib.org/3.1.1/gallery/shapes_and_collections/patch_collection.html#sphx-glr-gallery-shapes-and-collections-patch-collection-py
                except Exception as EX:
                    print(f'Unable to plot ATCF storm radius {windradius} for {curdt}');
                    raise EX;
    return(fh,res,line);
#plot_atcf_track

#### FUNCTION plot_htcf_track
def plot_htcf_track(res,fh=None,ax=None,curdate=None,windradius=None,windrng='NHC',colorbar='default'):
    '''Plot a map of model track from high-frequency "HTCF" output, color-coded for intensity. If WINDRADIUS is not None, plot a circle showing radius of maximum wind.'''
    #fig, axs = plt.subplots(2, 1, sharex=True, sharey=True)
    #windrng=range(0,170)
    if ( fh is None ):
        print(f'NEW FIGURE in plot_htcf_track!');
        fh = plt.figure(figsize=(11,8));
    if ( ax is None ):
        ax = plt.gca();
    # https://matplotlib.org/3.1.1/gallery/lines_bars_and_markers/multicolored_line.html
    cmap = 'jet';
    if ( windrng is None ):
        norm = plt.Normalize(res.vmax.min(), res.vmax.max())
    elif ( windrng == 'NHC' ):
        bounds = NHC_vmax_ranges();
        cmap = NHC_vmax_cmap();
        norm = BoundaryNorm(boundaries=bounds, ncolors=len(bounds)-1);
    elif ( hasattr(windrng,'min') ):
        norm = plt.Normalize(windrng.min(), windrng.max());
    elif ( hasattr(windrng,'start') ):
        norm = plt.Normalize(windrng.start, windrng.stop);
    else:
        raise ValueError(f'Do not understand WINDRNG kwarg');
    
    # Draw thicker white line to form outline of intensity-colored plot below
    #plt.plot(res.ALON,res.ALAT,'w-',linewidth=3);
    
    # https://stackoverflow.com/questions/12729529/can-i-give-a-border-outline-to-a-line-in-matplotlib-plot-function/35762000
    ## create line plot including an outline (stroke) using path_effects
    #plt.plot(x, y, color='k', lw=2, path_effects=[pe.Stroke(linewidth=5, foreground='g'), pe.Normal()])
    ## create line plot including an simple line shadow using path_effects
    #plt.plot(x, y, color='k', lw=2, path_effects=[pe.SimpleLineShadow(shadow_color='g'), pe.Normal()])
    
    points = np.array([res.plon, res.plat]).T.reshape(-1, 1, 2)
    segments = np.concatenate([points[:-1], points[1:]], axis=1)
    lc = LineCollection(segments, cmap=cmap, norm=norm)
    # Set the values used for colormapping
    lc.set_array(res.vmax)
    lc.set_linewidth(3)
    #DO MARKERS INSTEAD: lc.set_path_effects([path_effects.Stroke(linewidth=5,foreground='blue'),path_effects.Normal()]);
    line = ax.add_collection(lc)
    ix = np.vectorize(lambda x: x.hour)(res.time) == 0;
    plt.plot(res.plon[ix],res.plat[ix],'s',color='blue',
             markersize=6,markerfacecolor='None',markeredgecolor='blue', markeredgewidth=1.5);
    #https://matplotlib.org/3.1.1/tutorials/advanced/patheffects_guide.html
    
    ## Alternative with scatter plot:
    #https://matplotlib.org/tutorials/introductory/pyplot.html
    #data = {'a': np.arange(50),
    #        'c': np.random.randint(0, 50, 50),
    #        'd': np.random.randn(50)}
    #data['b'] = data['a'] + 10 * np.random.randn(50)
    #data['d'] = np.abs(data['d']) * 100
    #plt.scatter('a', 'b', c='c', s='d', data
    #plt.axis('tight');
    # HACK: Should use projection. Or at least median of latitudes...
    #plt.gca().set_aspect(np.cos(np.deg2rad(25)));
    # #lat_daspect(lat=25);
    # lat_daspect(lat=res.ALAT);
    # #https://stackoverflow.com/questions/3373256/set-colorbar-range-in-matplotlib
    if ( colorbar is not None ):
        if ( str(type(colorbar)).lower().find('colorbar') < 0 ):
            colorbar = fh.colorbar(line, ax=ax);
        colorbar.ax.set_ylabel('Vmax [kt.]');
    if ( curdate is not None ):
        dtix,curdt = min(enumerate(res.time), key=lambda x: abs(x[1] - curdate));
        plt.plot(res.plon.iloc[dtix],res.plat.iloc[dtix],'bx');
        #DEBUG:        print(f'METTC Loc plotted'); code.interact(local=locals());
        if ( windradius is not None ):
            cir = plt.Circle((res.plon.iloc[dtix],res.plat.iloc[dtix]),radius=res.vdist,color='blue',linewidth=3,fill=False);
            ax.add_artist(cir);
    return(fh,res,line);


def plot_mettc_track(res,expt=default_expt,year=2019,stid='AL05',INIT=default_INIT,LEADs=None,fh=None,ax=None,curdate=None,windradius=None,windrng='NHC',colorbar='default'):
    '''Plot a map of model track from TCSTS (MET-TC) output, color-coded for intensity. Calls read_mettc(v.) if res is None. If WINDRADIUS is not None, plot a circle showing wind radii of that type ("a_r34k", etc.). Special WINDRADIUS "poly_b" draws an annular Wedge(v.) showing the 34 and 64 kt. radii, as well as an inner circle showing the Radius of Maximum Winds.'''
    #windrng=range(0,170)
    if ( res is None ):
        res = read_mettc(year,stid,INIT,LEADs,expt=expt);
    #fig, axs = plt.subplots(2, 1, sharex=True, sharey=True)
    if ( fh is None ):
        print(f'NEW FIGURE in plot_mettc_track!');
        fh = plt.figure(figsize=(11,8));
    if ( ax is None ):
        ax = plt.gca();
    # NOTE: In TCSTS format, "A" refers to the model data, "B" to Best Track data...
    # https://matplotlib.org/3.1.1/gallery/lines_bars_and_markers/multicolored_line.html
    cmap = 'jet';
    if ( windrng is None ):
        norm = plt.Normalize(res.AMAX_WIND.min(), res.AMAX_WIND.max())
    elif ( windrng == 'NHC' ):
        bounds = NHC_vmax_ranges();
        cmap = NHC_vmax_cmap();
        norm = BoundaryNorm(boundaries=bounds, ncolors=len(bounds)-1);
    elif ( hasattr(windrng,'min') ):
        norm = plt.Normalize(windrng.min(), windrng.max());
    elif ( hasattr(windrng,'start') ):
        norm = plt.Normalize(windrng.start, windrng.stop);
    else:
        raise ValueError(f'Do not understand WINDRNG kwarg');
    
    # Draw thicker white line to form outline of intensity-colored plot below
    #plt.plot(res.ALON,res.ALAT,'w-',linewidth=3);
    
    # https://stackoverflow.com/questions/12729529/can-i-give-a-border-outline-to-a-line-in-matplotlib-plot-function/35762000
    ## create line plot including an outline (stroke) using path_effects
    #plt.plot(x, y, color='k', lw=2, path_effects=[pe.Stroke(linewidth=5, foreground='g'), pe.Normal()])
    ## create line plot including an simple line shadow using path_effects
    #plt.plot(x, y, color='k', lw=2, path_effects=[pe.SimpleLineShadow(shadow_color='g'), pe.Normal()])
    
    points = np.array([res.ALON, res.ALAT]).T.reshape(-1, 1, 2)
    segments = np.concatenate([points[:-1], points[1:]], axis=1)
    lc = LineCollection(segments, cmap=cmap, norm=norm)
    # Set the values used for colormapping
    lc.set_array(res.AMAX_WIND)
    lc.set_linewidth(3)
    #DO MARKERS INSTEAD: lc.set_path_effects([path_effects.Stroke(linewidth=5,foreground='cyan'),path_effects.Normal()]);
    line = ax.add_collection(lc)
    ix = np.vectorize(lambda x: x.hour)(res.vdts) == 0;
    plt.plot(res.ALON[ix],res.ALAT[ix],'^',color='cyan',
             markersize=6,markerfacecolor='None',markeredgecolor='cyan', markeredgewidth=1.5);
    #https://matplotlib.org/3.1.1/tutorials/advanced/patheffects_guide.html
    
    ## Alternative with scatter plot:
    #https://matplotlib.org/tutorials/introductory/pyplot.html
    #data = {'a': np.arange(50),
    #        'c': np.random.randint(0, 50, 50),
    #        'd': np.random.randn(50)}
    #data['b'] = data['a'] + 10 * np.random.randn(50)
    #data['d'] = np.abs(data['d']) * 100
    #plt.scatter('a', 'b', c='c', s='d', data
    plt.axis('tight');
    # HACK: Should use projection. Or at least median of latitudes...
    #plt.gca().set_aspect(np.cos(np.deg2rad(25)));
    #lat_daspect(lat=25);
    lat_daspect(lat=res.ALAT);
    #https://stackoverflow.com/questions/3373256/set-colorbar-range-in-matplotlib
    if ( colorbar is not None ):
        if ( str(type(colorbar)).lower().find('colorbar') < 0 ):
            colorbar = fh.colorbar(line, ax=ax);
        colorbar.ax.set_ylabel('Vmax [kt.]');
    if ( curdate is not None ):
        dtix,curdt = min(enumerate(res.vdts), key=lambda x: abs(x[1] - curdate));
        plt.plot(res.ALON.iloc[dtix],res.ALAT.iloc[dtix],'cx');
        #DEBUG:        print(f'METTC Loc plotted'); code.interact(local=locals());
        if ( windradius is not None ):
            try:
                if ( 'poly_b' in windradius ):
                    ne_34_rad = res[['BAL_WIND_34','BNE_WIND_34']].iloc[dtix].max() / 111.0;
                    se_34_rad = res[['BAL_WIND_34','BSE_WIND_34']].iloc[dtix].max() / 111.0;
                    sw_34_rad = res[['BAL_WIND_34','BSW_WIND_34']].iloc[dtix].max() / 111.0;
                    nw_34_rad = res[['BAL_WIND_34','BNW_WIND_34']].iloc[dtix].max() / 111.0;
                    ne_64_rad = res[['BAL_WIND_64','BNE_WIND_64','BAL_WIND_50','BNE_WIND_50']].iloc[dtix].min() / 111.0;
                    se_64_rad = res[['BAL_WIND_64','BSE_WIND_64','BAL_WIND_50','BSE_WIND_50']].iloc[dtix].min() / 111.0;
                    sw_64_rad = res[['BAL_WIND_64','BSW_WIND_64','BAL_WIND_50','BSW_WIND_50']].iloc[dtix].min() / 111.0;
                    nw_64_rad = res[['BAL_WIND_64','BNW_WIND_64','BAL_WIND_50','BNW_WIND_50']].iloc[dtix].min() / 111.0;
                    patches = [];
                    patches += [
                        Wedge((res.ALON.iloc[dtix],res.ALAT.iloc[dtix]), ne_34_rad,  0, 90,width=ne_64_rad,fill=False),	# NE sector
                        Wedge((res.ALON.iloc[dtix],res.ALAT.iloc[dtix]), se_34_rad, 90,180,width=se_64_rad,fill=False),	# SE sector
                        Wedge((res.ALON.iloc[dtix],res.ALAT.iloc[dtix]), sw_34_rad,180,270,width=sw_64_rad,fill=False),	# SW sector
                        Wedge((res.ALON.iloc[dtix],res.ALAT.iloc[dtix]), nw_34_rad,270,  0,width=nw_64_rad,fill=False),	# NW sector
                    ];
                    p = PatchCollection(patches, color='cyan',alpha=1.00);
                    ax.add_collection(p);
                    
                    # As a bonus, also plot Radius of Maximum Wind as a circle
                    cir = plt.Circle((res.ALON.iloc[dtix],res.ALAT.iloc[dtix]),radius=res.BMRD.iloc[dtix]/111.0,color='cyan',linewidth=3,fill=False);
                    ax.add_artist(cir);

                else:
                    #DEBUG:                print('TCSTS trying to plot windradius'); code.interact(local=locals());
                    a_r34k = res[['AAL_WIND_34','ANW_WIND_34','ANE_WIND_34','ASE_WIND_34','ASW_WIND_34']].iloc[dtix].max();
                    a_r50k = res[['AAL_WIND_50','ANW_WIND_50','ANE_WIND_50','ASE_WIND_50','ASW_WIND_50']].iloc[dtix].max();
                    a_r64k = res[['AAL_WIND_64','ANW_WIND_64','ANE_WIND_64','ASE_WIND_64','ASW_WIND_64']].iloc[dtix].max();
                    b_r34k = res[['BAL_WIND_34','BNW_WIND_34','BNE_WIND_34','BSE_WIND_34','BSW_WIND_34']].iloc[dtix].max();
                    b_r50k = res[['BAL_WIND_50','BNW_WIND_50','BNE_WIND_50','BSE_WIND_50','BSW_WIND_50']].iloc[dtix].max();
                    b_r64k = res[['BAL_WIND_64','BNW_WIND_64','BNE_WIND_64','BSE_WIND_64','BSW_WIND_64']].iloc[dtix].max();
                    
                    switcher = {
                        'roci': 	res.ARRP.iloc[dtix] if res.ARRP.iloc[dtix] > 0 else res.BRRP.iloc[dtix],
                        'rmw':		res.AMRD.iloc[dtix] if res.AMRD.iloc[dtix] > 0 else res.BMRD.iloc[dtix],
                        'r34k':		a_r34k if a_r34k > 0 else b_r34k,
                        'r50k':		a_r50k if a_r50k > 0 else b_r50k,
                        'r64k':		a_r64k if a_r64k > 0 else b_r64k,
                        'a_roci': 	res.ARRP.iloc[dtix],
                        'a_rmw':	res.AMRD.iloc[dtix],
                        'a_r34k':	a_r34k,
                        'a_r50k':	a_r50k,
                        'a_r64k':	a_r64k,
                        'b_roci': 	res.BRRP.iloc[dtix],
                        'b_rmw':	res.BMRD.iloc[dtix],
                        'b_r34k':	b_r34k,
                        'b_r50k':	b_r50k,
                        'b_r64k':	b_r64k,
                    };
                    radkm = switcher.get(windradius,windradius);
                    raddeg = radkm / 111;
                    cir = plt.Circle((res.ALON.iloc[dtix],res.ALAT.iloc[dtix]),radius=raddeg,color='cyan',linewidth=3,fill=False);
                    ax.add_artist(cir);
            except Exception as EX:
                print(f'Unable to plot METTC storm radius {windradius} for {curdt}');
                raise EX;
    return(fh,res,line);
#plot_mettc_track



def plot_mettc_intensity(res,expt=default_expt,year=2019,stid='AL05',INIT=default_INIT,LEADs=None,fh=None,colorbar=None):
    if ( res is None ):
        res = read_mettc(year,stid,INIT,LEADs,expt=expt);
    # https://matplotlib.org/3.1.1/gallery/text_labels_and_annotations/date.html
    dts = pltdts.date2num(res['dts']);
    
    if ( fh is None ):
        #fig, axs = plt.subplots(2, 1, sharex=True, sharey=True)
        fh = plt.figure(figsize=(11,8));
    ax = plt.gca();
    norm = plt.Normalize(res['vmax'].min(), res['vmax'].max())
    points = np.array([dts, res['vmax']]).T.reshape(-1, 1, 2)
    segments = np.concatenate([points[:-1], points[1:]], axis=1)
    #lc = LineCollection(segments, cmap='viridis', norm=norm)
    lc = LineCollection(segments, cmap='jet', norm=norm)
    # Set the values used for colormapping
    lc.set_array(res['vmax'])
    lc.set_linewidth(2)
    line = ax.add_collection(lc)

    # https://matplotlib.org/3.1.1/gallery/text_labels_and_annotations/date.html
    # Oh how I miss DATETICK3... :(
    ax.xaxis.set_major_locator(pltdts.DayLocator())                     # every day
    ax.xaxis.set_minor_locator(pltdts.HourLocator(range(0,24,6)))       # every 6 h
    ax.xaxis.set_major_formatter(pltdts.DateFormatter('%m%d'))          # tick format
    ax.format_xdata = pltdts.DateFormatter('%m%d%H')			# coords message box format
    
    plt.axis('tight');
    if ( colorbar is not None ):
        if ( str(type(colorbar)).lower().find('colorbar') < 0 ):
            colorbar = fh.colorbar(line, ax=ax);
        colorbar.ax.set_ylabel('Vmax [kt.]');
    return(fh,res,line);
#plot_mettc_intensity(res,year=2019,stid='AL05'):

def goofball_workaround():
    try:
        fh = plt.figure();
        plt.close()
    except:
        pass;
    time.sleep(0.1);

def plot_forecast_summary(desc,bt,ct,ut=None):
    '''Make "standard" 7-panel summary figure vs. Best Track: track, intensity, storm speed, RMW, R34, R50, R64. DESC is a descriptive title string, BT is Best Track, CT is coupled track (if available), UT is uncoupled track (if available).'''
    
    if ( bt is not None ):
      if ( ct is not None ):
        btfhr = [((pd.Timestamp(vdt)-ct.vdt.iloc[0]).total_seconds()/3600) for vdt in bt['dts']]
      elif ( ut is not None ):
        btfhr = [((pd.Timestamp(vdt)-ut.vdt.iloc[0]).total_seconds()/3600) for vdt in bt['dts']]
    
    if ( bt is not None ):
      bdist = np.sqrt((np.diff(bt['lat'])**2) + ((np.diff(bt['lon'])*np.cos(np.deg2rad(bt['lat'].iloc[0:-1])))**2))*111e3/(6*3600)
    else:
      bdist = None;
    if ( ct is not None ):
      cdist = np.sqrt((np.diff(ct.lat)**2) + ((np.diff(ct.lon.values)*np.cos(np.deg2rad(ct.lat[0:-1])))**2))*111e3/(3*3600);
    else:
      cdist = None;
    if ( ut is not None ):
      udist = np.sqrt((np.diff(ut.lat)**2) + ((np.diff(ut.lon.values)*np.cos(np.deg2rad(ut.lat[0:-1])))**2))*111e3/(3*3600)
    else:
      udist = None;
    
    # Track and intensity
    goofball_workaround()
    fh = plt.figure(figsize=(13,9));
    gs = fh.add_gridspec(12, 6)
    
    ax11 = fh.add_subplot(gs[0:8, 0:3], projection=ccrs.PlateCarree())
    #ax11.set_extent([-180, 180, -90, 90], crs=ccrs.PlateCarree())
    if ( bt is not None ):
      ax11.plot(bt['lon'],bt['lat'],'k+-');
      for dtix,dt in enumerate(bt['dts']):
        #LJG 2023-12-07: make figure panels more "repurposeable"
        # if ( dt.hour == 0 ):
        #   ax11.text(bt['lon'].iloc[dtix],bt['lat'].iloc[dtix],int(btfhr[dtix]),color='k',backgroundcolor='w',alpha=0.9);
        # LJG 2024-06-26: Temporarily turn off hour labels!
        # if ( dt.hour == bt['dts'][0].hour ):
        #   ax11.text(bt['lon'].iloc[dtix],bt['lat'].iloc[dtix],int(btfhr[dtix]),color='k',backgroundcolor='w',alpha=0.5);
        pass;
    if ( ct is not None ):
      ax11.plot(ct.lon,ct.lat,'bs:');
      for dtix,dt in enumerate(ct.vdt):
        #LJG 2023-12-07: make figure panels more "repurposeable"
        # if ( dt.hour == 0 ):
        #   ax11.text(ct.lon.iloc[dtix],ct.lat.iloc[dtix],int(ct.fhr.iloc[dtix]),color='b',backgroundcolor='w',alpha=0.9);
        # LJG 2024-06-26: Temporarily turn off hour labels!
        # if ( dt.hour == bt['dts'][0].hour ):
        #   ax11.text(ct.lon.iloc[dtix],ct.lat.iloc[dtix],int(ct.fhr.iloc[dtix]),color='b',backgroundcolor='w',alpha=0.5);
        pass;
    if ( ut is not None ):
      ax11.plot(ut.lon,ut.lat,'rd--');
      for dtix,dt in enumerate(ut.vdt):
        #LJG 2023-12-07: make figure panels more "repurposeable"
        # if ( dt.hour == 0 ):
        #   ax11.text(ut.lon.iloc[dtix],ut.lat.iloc[dtix],int(ut.fhr.iloc[dtix]),color='r',backgroundcolor='w',alpha=0.9);
        # LJG 2024-06-26: Temporarily turn off hour labels!
        # if ( dt.hour == bt['dts'][0].hour ):
        #   ax11.text(ut.lon.iloc[dtix],ut.lat.iloc[dtix],int(ut.fhr.iloc[dtix]),color='r',backgroundcolor='w',alpha=0.5);
        pass;


    #ax11.set_xlim(ax11.get_xlim()); ax11.set_ylim(ax11.get_ylim());
    ax11.set_xlim(np.array(ax11.get_xlim()) + [-5,+5]); ax11.set_ylim(np.array(ax11.get_ylim()) + [-5,+5]);
    plot_coastline(ax11);
    latlon_gridlines(ax11);
    
    ax12 = fh.add_subplot(gs[8:11, 0:3])
    if ( bt is not None ):
      ax12.plot(btfhr,bt['vmax'],'k+-');
    if ( ct is not None ):
      ax12.plot(ct.fhr,ct.vmax,'bs:');
    if ( ut is not None ):
      ax12.plot(ut.fhr,ut.vmax,'rd--');
    ax12.grid(True);
    ax12.set_ylabel('Intensity kt.');
    #LJG 2023-12-07: make figure panels more "repurposeable"
    #plt.setp(ax12.get_xticklabels(),visible=False);
    
    ax13 = fh.add_subplot(gs[11, 0:3], sharex=ax12)
    if ( bt is not None ):
      ax13.plot(btfhr[0:-1],bdist,'k+-');
    if ( ct is not None ):
      ax13.plot(ct.fhr[0:-1],cdist,'bs:');
    if ( ut is not None ):
      ax13.plot(ut.fhr[0:-1],udist,'rd--');
    ax13.grid(True);
    ax13.set_ylabel('Sp.');
    
    plt.xticks(np.arange(0, 126+1, 24))
    
    #plt.suptitle(f'{desc}');
    #figfname = f'figs/hafs_case_{desc}.png'; print(figfname);
    #DEBUG:        plt.savefig(figfname);
    #DEBUG:        plt.show(block=False); breakpoint();
    
    # Wind radii
    #fh = plt.figure(figsize=(9,9));
    #gs = fh.add_gridspec(2, 2)
    #gs = fh.add_gridspec(4, 1)
    # RMW
    ax21 = fh.add_subplot(gs[0:3,3:6])
    # ax21.plot(btfhr,bt['rmw'],'k+-');
    # ax21.plot(ct.fhr,ct.rmw,'bs:');
    # ax21.plot(ut.fhr,ut.rmw,'rd--');
    if ( bt is not None ):
      ax21.plot(btfhr,bt['rmw'],'k+-');
    if ( ct is not None ):
      ax21.plot(ct.fhr,ct.rmw,'bs-');
    if ( ut is not None ):
      ax21.plot(ut.fhr,ut.rmw,'rd-');
    ax21.grid(True);
    ax21.set_title('RMW');
    #LJG 2023-12-07: make figure panels more "repurposeable"
    #plt.setp(ax21.get_xticklabels(),visible=False);
    # R34
    ax22 = fh.add_subplot(gs[3:6,3:6],sharex=ax21)
    if ( bt is not None ):
      ax22.plot(btfhr,bt['r34k'],'k+-');
    if ( ct is not None ):
      ax22.plot(ct.fhr,ct.r34,'bs-');
    if ( ut is not None ):
      ax22.plot(ut.fhr,ut.r34,'rd-');
    if ( bt is not None ):
      b_minr34 = np.nanmin([bt['r34k_1'],bt['r34k_2'],bt['r34k_3'],bt['r34k_4']],axis=0);
      ax22.plot(btfhr,b_minr34,'+:',color='grey');
    if ( ct is not None ):
      warnings.filterwarnings('ignore');
      c_minr34 = np.nanmin(ct[['rad1','rad2','rad3','rad4']],axis=1);
      ax22.plot(ct.fhr,c_minr34,'s:',color='lightblue');
      warnings.resetwarnings();
    if ( ut is not None ):
      warnings.filterwarnings('ignore');
      u_minr34 = np.nanmin(ut[['rad1','rad2','rad3','rad4']],axis=1);
      ax22.plot(ut.fhr,u_minr34,'d:',color='pink');
      warnings.resetwarnings();
    ax22.grid(True);
    ax22.set_title('R34');
    #LJG 2023-12-07: make figure panels more "repurposeable"
    #plt.setp(ax22.get_xticklabels(),visible=False);
    # R50
    ax23 = fh.add_subplot(gs[6:9,3:6],sharex=ax21)
    if ( bt is not None ):
      ax23.plot(btfhr,bt['r50k'],'k+-');
    if ( ct is not None ):
      ax23.plot(ct.fhr,ct.r50,'bs-');
    if ( ut is not None ):
      ax23.plot(ut.fhr,ut.r50,'rd-');
    if ( bt is not None ):
      b_minr50 = np.nanmin([bt['r50k_1'],bt['r50k_2'],bt['r50k_3'],bt['r50k_4']],axis=0);
      ax23.plot(btfhr,b_minr50,'+:',color='grey');
    if ( ct is not None ):
      warnings.filterwarnings('ignore');
      c_minr50 = np.nanmin(ct[['r50_rad1','r50_rad2','r50_rad3','r50_rad4']],axis=1);
      ax23.plot(ct.fhr,c_minr50,'s:',color='lightblue');
      warnings.resetwarnings();
    if ( ut is not None ):
      warnings.filterwarnings('ignore');
      u_minr50 = np.nanmin(ut[['r50_rad1','r50_rad2','r50_rad3','r50_rad4']],axis=1);
      ax23.plot(ut.fhr,u_minr50,'d:',color='pink');
      warnings.resetwarnings();
    ax23.grid(True);
    ax23.set_title('R50');
    #LJG 2023-12-07: make figure panels more "repurposeable"
    #plt.setp(ax23.get_xticklabels(),visible=False);
    # R64
    ax24 = fh.add_subplot(gs[9:12,3:6],sharex=ax21)
    if ( bt is not None ):
      ax24.plot(btfhr,bt['r64k'],'k+-');
    if ( ct is not None ):
      ax24.plot(ct.fhr,ct.r64,'bs-');
    if ( ut is not None ):
      ax24.plot(ut.fhr,ut.r64,'rd-');
    if ( bt is not None ):
      b_minr64 = np.nanmin([bt['r64k_1'],bt['r64k_2'],bt['r64k_3'],bt['r64k_4']],axis=0);
      ax24.plot(btfhr,b_minr64,'+:',color='grey');
    if ( ct is not None ):
      warnings.filterwarnings('ignore');
      c_minr64 = np.nanmin(ct[['r64_rad1','r64_rad2','r64_rad3','r64_rad4']],axis=1);
      ax24.plot(ct.fhr,c_minr64,'s:',color='lightblue');
      warnings.resetwarnings();
    if ( ut is not None ):
      warnings.filterwarnings('ignore');
      u_minr64 = np.nanmin(ut[['r64_rad1','r64_rad2','r64_rad3','r64_rad4']],axis=1);
      ax24.plot(ut.fhr,u_minr64,'d:',color='pink');
      warnings.resetwarnings();
    ax24.grid(True);
    ax24.set_title('R64');
    #plt.setp(ax24.get_xticklabels(),visible=False);
    
    plt.xticks(np.arange(0, 126+1, 24))

    plt.suptitle(f'{desc}');
    #figfname = f'figs/hafs_case_{desc}_radii.png'; print(figfname);
    #DEBUG:        plt.savefig(figfname);
    fh.tight_layout();
    
    return(fh,(ax11,ax12,ax13,ax21,ax22,ax23,ax24));

#def plot_forecast_summary(desc,bt,ct,ut):
