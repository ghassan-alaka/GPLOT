#! /bin/env python3

'''
Lew.Gramer@noaa.gov: produce graphics differencing key fields for HAFS (HERC) ensemble outlier members
'''

import cartopy.crs as ccrs;
import cartopy.feature as cfeature;
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
import datetime
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import os
import pandas as pd
import sys
import time
import xarray as xr

import gp_tools as gpt



idir = gpt.default_idir;
#odir = gpt.default_odir;
odir = gpt.default_idir+'/ensemble';
ddir = gpt.default_ddir;
adir = gpt.adeck_idir;

#cmapdir = '/lfs5/HFIP/hur-aoml/Lew.Gramer/src/GPLOT/sorc/GPLOT/python/colormaps';
#cmapdir = '/scratch2/AOML/aoml-hafs1/Lew.Gramer/src/hrd_gplot/sorc/GPLOT/python/colormaps';
cmapdir = f'{idir}/../src/GPLOT/sorc/GPLOT/python/colormaps';

# First hour to plot
fig0hr = 0;
#fig0hr = 24;  ######## HACK HACK HACK
#fig0hr = 48;  ######## HACK HACK HACK
#fig0hr = 96;  ######## HACK HACK HACK

dhr = 24;
#dhr = 48;  ######## HACK HACK HACK
#dhr = checkhr;  ######## HACK HACK HACK

#cvars = ['vmaxrms','vmaxerr','trkrms','trkerr','r34rms','r34err','r50rms','r50err','r64rms','r64err','angerr','lon','lat',];
#cvars = ['trkrms',];
#cvars = ['trkerr','vmaxerr','angerr','lon','lat',];
#cvars = ['trkerr','vmaxerr',];  ######## HACK HACK HACK
#cvars = ['vmaxerr',];  ######## HACK HACK HACK
#cvars = ['r34rms','vmaxrms','trkrms',];
cvars = ['r34err','r34rms',];

prs = [700,500,300];
#prs = [500];   ######## HACK HACK HACK


idtst = '2024080400'; highlight_stid='AL04';
beghr = 0; endhr = 126; checkhr = 96;
benchstr = None; #"ofcl" in real time, "best" after the fact - unless specified below

if ( len(sys.argv)>1 and sys.argv[1].find("-h")>=0 ):
  print(f'USAGE: {sys.argv[0]} [YYYYMMDDHH] [<basin><storm-no>] [<forecast-hour>] [benchmark: "best", "ofcl", etc.]');
  print(f'    Any positional arg may be "-" to use its default: {idtst} {highlight_stid} {checkhr}');
  sys.exit(0)

if ( len(sys.argv)>1 and sys.argv[1] != "-" ):	idtst = sys.argv[1];
if ( len(sys.argv)>2 and sys.argv[2] != "-" ):	highlight_stid = sys.argv[2];
if ( len(sys.argv)>3 and sys.argv[3] != "-" ):	benchstr = sys.argv[3].lower();
if ( len(sys.argv)>4 and sys.argv[4] != "-" ):	checkhr = int(sys.argv[4]);
if ( len(sys.argv)>5 and sys.argv[5] != "-" ):	dhr = int(sys.argv[5]);

highlight_stid3 = gpt.id_from_bstn(highlight_stid);

idt = datetime.datetime.strptime(idtst,'%Y%m%d%H');
bdt = idt+datetime.timedelta(hours=beghr)
edt = idt+datetime.timedelta(hours=endhr)


def calc_mom6_OHC(ds):
 # cp = 4178;                  # Specific heat capacity of seawater [J kg^-1 K^-1]
 # rho = 1026;                 # Mean water density [kg m^-3]
 # Modified to match sorc/hafs_hycom_utils.fd/post/hafs_ab2data/archv2data3z.f
 cp = 3990;                  # Specific heat capacity of seawater [J kg^-1 K^-1]
 rho = 1025;                 # Mean water density [kg m^-3]
 kJcm2_per_Jm2 = 1e-7;       # Unit conversion kJ/cm^2 == 10^7 J/m^2
 delT = ds.temp.squeeze() - 26;
 delT = xr.where(delT > 0, delT, 0);
 #dZ = ds.z_l.broadcast_like(delT).diff(0)
 dz = ds.z_l; dz.values[1:] = ds.z_l[1:] - ds.z_l[0:-1].values
 dZ = dz.broadcast_like(delT)
 delTdz = delT * dZ;
 OHC = cp*rho*delTdz.sum(axis=0) * kJcm2_per_Jm2;
 return(OHC.where(OHC>0))

def plot_trk(emem1,emem2):
 for emem in range(0,31):
   querystrm = querystr+f' and emem=={emem} and fhr<={checkhr}'
   plt.plot(atcfs.query(querystrm).lon,atcfs.query(querystrm).lat,':',color='darkgrey',alpha=0.90,linewidth=0.75);
 #DEBUG: plt.show(block=False); breakpoint();
 querystr1 = querystr+f' and emem=={emem1} and fhr<={checkhr}'
 plt.plot(atcfs.query(querystr1).lon,atcfs.query(querystr1).lat,'b-',linewidth=1.0);
 plt.plot(atcfs.query(querystr1).lon.iloc[[0,-1]],atcfs.query(querystr1).lat.iloc[[0,-1]],'bo');
 querystr2 = querystr+f' and emem=={emem2} and fhr<={checkhr}'
 plt.plot(atcfs.query(querystr2).lon,atcfs.query(querystr2).lat,'r-',linewidth=1.0);
 plt.plot(atcfs.query(querystr2).lon.iloc[[0,-1]],atcfs.query(querystr2).lat.iloc[[0,-1]],'ro');
 plt.plot(best.lon,best.lat,'k-',linewidth=1.0);
 plt.plot(best.lon.iloc[[0,-1]],best.lat.iloc[[0,-1]],'ko');

def plot_tses(ax,cvar):
 if ( cvar in ['r34err','r34rms'] ):
   tsvar = 'r34';
 elif ( cvar in ['r50err','r50rms'] ):
   tsvar = 'r50';
 elif ( cvar in ['r64err','r64rms'] ):
   tsvar = 'r64';
 else:
   tsvar = 'vmax';
 for emem in range(0,31):
  querystrm = querystr+f' and emem=={emem} and fhr<={checkhr}'
  #atcfs.query(querystrm)[tsvar].plot(ax=ax,linestyle=':',color='darkgrey',alpha=0.90,linewidth=0.75);
  ax.plot(atcfs.query(querystrm).vdt,atcfs.query(querystrm)[tsvar],':',color='darkgrey',alpha=0.95,linewidth=0.75);
 querystr1 = querystr+f' and emem=={min_emem} and fhr<={checkhr}'
 ax.plot(atcfs.query(querystr1).vdt,atcfs.query(querystr1)[tsvar],'b-',linewidth=1.0);
 querystr2 = querystr+f' and emem=={max_emem} and fhr<={checkhr}'
 ax.plot(atcfs.query(querystr2).vdt,atcfs.query(querystr2)[tsvar],'r-',linewidth=1.0);
 querystrm = querystr+f' and fhr<={checkhr}'
 tsvars = atcfs.query(querystrm)[tsvar]; nfhrs = len(tsvars) / 31;
 if ( nfhrs != int(nfhrs) ):
   raise ValueException(f'Misshapen {tsvar}?? nmems=31, nfhrs={nfhrs}');
 tsar = np.reshape(tsvars,[31,int(nfhrs)])
 tsmn = tsar.mean(axis=0);
 tssd = tsar.std(axis=0);
 ax.plot(atcfs.query(querystr2).vdt,tsmn-tssd,':',color='black',linewidth=1.0);
 ax.plot(atcfs.query(querystr2).vdt,tsmn,'--',color='black',linewidth=1.0);
 ax.plot(atcfs.query(querystr2).vdt,tsmn+tssd,':',color='black',linewidth=1.0);
 ax.plot(best.vdt,best[tsvar],'k-',linewidth=1.5);
 ax.set_ylabel(tsvar.upper());
 ax.set_xlim([np.min([best.vdt.min(),atcfs.query(querystr2).vdt.min()]),
              np.max([best.vdt.max(),atcfs.query(querystr2).vdt.max()])]);
 ax.grid(True);


#color_data_rh = np.genfromtxt(f'/scratch2/AOML/aoml-hafs1/Lew.Gramer/src/hrd_gplot/sorc/GPLOT/python/colormaps/colormap_brown_to_green.txt')
color_data_rh = np.genfromtxt(f'{cmapdir}/colormap_brown_to_green.txt')
colormap_rh = matplotlib.colors.ListedColormap(color_data_rh)
# levs_rh = np.linspace(0,100,41,endpoint=True)
# norm_rh = colors.BoundaryNorm(levs_rh,256)

def plot_fld(mnds,mxds,var,vrng,drng,lv,panels=False):
 '''Plot "var" fields & field differences between ensemble members,
    from XArray datasets mnds (e.g., for member with lowest error) and
    mxds (e.g., member with greatest error). Filled contours are done
    with levels "vrng" for the fields, "drng" for differences. If
    "panels", then put all three in ONE figure. Use "lv" in titles.
    Also plots respective tracks and intensities for all members,
    highlighting "mnds" in blue, "mxds" in red, "best" in black.'''
 #breakpoint();
 
 figbasenm = f'{highlight_stid3.lower()}.ensemble_{idtst}_f{checkhr:03}_{cvar.lower()}'
 figendnm = f'{var.lower()}_{lv.lower()}.{BENCHSTR}.f{fhr:03}'
 
 if ( var == 'r' ):
   cmap = colormap_rh;
   extend = 'neither';
 else:
   cmap = 'jet';
   extend = 'both';
 if ( panels ):
  fh,axen = plt.subplots(1,3,sharex=True,sharey=True,figsize=(15,5),subplot_kw={'projection':ccrs.PlateCarree()});
 
 # DIFFERENCE FIELD
 if ( panels ):	ax = axen[2]
 else:		fh,ax = plt.subplots(figsize=(9,9),subplot_kw={'projection':ccrs.PlateCarree()});
 fh,ax = plt.subplots(figsize=(9,9),subplot_kw={'projection':ccrs.PlateCarree()});
 (mxds[var].squeeze()-mnds[var].values.squeeze()).squeeze().plot(ax=ax,levels=drng);
 plot_trk(min_emem,max_emem);
 gpt.plot_coastline(ax,facecolor='None');
 gpt.latlon_gridlines(ax,dlon=5.0,dlat=5.0);
 if ( cvar in ['vmaxerr','vmaxrms','r34err','r34rms','r50err','r50rms','r64err','r64rms',] ):
  if ( nearminlon is not None ):
   ax.set_xlim([nearminlon,nearmaxlon]);
   ax.set_ylim([nearminlat,nearmaxlat]);
 else:
  if ( minlon is not None ):
   ax.set_xlim([minlon,maxlon]);
   ax.set_ylim([minlat,maxlat]);
 
 # INTENSITY (or other time series) INSET
 if ( not panels ):
  #ax2 = fig.add_axes([0.25, 0.6, 0.2, 0.2]);
  #ax2 = fh.add_axes([0.15, 0.03, 0.6, 0.1]);
  #ax2 = fh.add_axes([0.15, 0.10, 0.60, 0.10]);
  ax2 = fh.add_axes([0.10, 0.10, 0.66, 0.10]);
  plot_tses(ax2,cvar);
 
 if ( not panels ):
  ax.set_title(f'{idtst} F{fhr:03} {highlight_stid} {cvar.upper()} {var.upper()} {lv.upper()} diff {max_emem}-{min_emem}');
  figfname = f'{odir}/{figbasenm}_diff_{figendnm}.png';
  #DEBUG:  plt.show(block=False); breakpoint();
  print(figfname); plt.savefig(figfname);
 #DEBUG: plt.show(block=False); breakpoint();
 # if ( var == 'r' and lv == '400-700' ):
 #  plt.show(block=False); breakpoint();
 
 # MAX FIELD
 if ( panels ):	ax = axen[0]
 else:		fh,ax = plt.subplots(figsize=(9,9),subplot_kw={'projection':ccrs.PlateCarree()});
 mxds[var].squeeze().plot(ax=ax,levels=vrng,cmap=cmap,extend=extend);
 plot_trk(min_emem,max_emem);
 gpt.plot_coastline(ax,facecolor='None');
 gpt.latlon_gridlines(ax,dlon=5.0,dlat=5.0);
 if ( minlon is not None ):
  ax.set_xlim([minlon,maxlon]);
  ax.set_ylim([minlat,maxlat]);
 if ( not panels ):
  ax.set_title(f'{idtst} F{fhr:03} {highlight_stid} {cvar.upper()} {var.upper()} {lv.upper()} max MEM#{max_emem}');
  figfname = f'{odir}/{figbasenm}_max_mem{max_emem:02}_{figendnm}.png';
  print(figfname); plt.savefig(figfname);
 
 # MIN FIELD
 if ( panels ):	ax = axen[1]
 else:		fh,ax = plt.subplots(figsize=(9,9),subplot_kw={'projection':ccrs.PlateCarree()});
 mnds[var].squeeze().plot(ax=ax,levels=vrng,cmap=cmap,extend=extend);
 plot_trk(min_emem,max_emem);
 gpt.plot_coastline(ax,facecolor='None');
 gpt.latlon_gridlines(ax,dlon=5.0,dlat=5.0);
 if ( minlon is not None ):
  ax.set_xlim([minlon,maxlon]);
  ax.set_ylim([minlat,maxlat]);
 if ( not panels ):
  ax.set_title(f'{idtst} F{fhr:03} {highlight_stid} {cvar.upper()} {var.upper()} {lv.upper()} min MEM#{min_emem}');
  figfname = f'{odir}/{figbasenm}_min_mem{min_emem:02}_{figendnm}.png';
  print(figfname); plt.savefig(figfname);
 
 if ( panels ):
  plt.suptitle(f'{idtst} F{fhr:03} {highlight_stid} {cvar.upper()} {var.upper()} {lv.upper()} {max_emem}-{min_emem}');
  figfname = f'{odir}/{figbasenm}_3panel_{figendnm}.png';
  print(figfname); plt.savefig(figfname);
 
 #DEBUG: plt.show(block=False); breakpoint();
 # if ( var == 'r' and lv == '400-700' ):
 #  plt.show(block=False); breakpoint();
 plt.close('all');


gpt.goofball_workaround()
time.sleep(1);
# breakpoint();
# try:
#  fh,ax = plt.subplots(figsize=(9,9),subplot_kw={'projection':ccrs.PlateCarree()});
# except:
#  print('Ugh!');
#  breakpoint();

cycpth = f'{ddir}/ensemble/{idtst}';
if ( not os.path.exists(cycpth) ):
  raise ValueError(f'No ensemble data path {cycpth}');

#best_edt = idt+datetime.timedelta(hours=endhr+9)
best_edt = idt+datetime.timedelta(hours=checkhr+1)


# Best Track
bestdict = gpt.read_bdeck(year=idt.year,stid=highlight_stid);
bestdict = gpt.subset_bdeck(bestdict,[bdt,best_edt]);
bfhr = [((pd.Timestamp(vdt)-idt).total_seconds()/3600) for vdt in bestdict['dts']]
best = pd.DataFrame(bestdict);
best.index = bfhr;
best['r34'] = best.r34k; best['r50'] = best.r50k; best['r64'] = best.r64k;
best['vdt'] = [((pd.Timestamp(vdt))) for vdt in bestdict['dts']]
lon0 = np.tile(best.lon[0],best.lon.shape);
lat0 = np.tile(best.lat[0],best.lat.shape);
az,dst = gpt.azimuth_distance_wgs84(lon0,lat0,best.lon,best.lat);
best['endang'] = az;
best['enddst'] = dst;

# NHC Official (OFCL) forecast, consensus guidance (RVCN, TVCN)
doguidance = True
ofcl = None
if ( doguidance ):
 adeck = gpt.read_adeck(f'{adir}/a{highlight_stid.lower()}{idt.year}.dat',idtst);
 #DEBUG: breakpoint();
 ofcl = adeck[(adeck.idtstr==int(idtst)) & (adeck.mdl=='OFCL')];
 gfhr = [((vdt-idt).total_seconds()/3600) for vdt in ofcl['vdt']]
 ofcl.index = gfhr
 lon0 = np.tile(ofcl.lon[0],ofcl.lon.shape);
 lat0 = np.tile(ofcl.lat[0],ofcl.lat.shape);
 az,dst = gpt.azimuth_distance_wgs84(lon0,lat0,ofcl.lon,ofcl.lat);
 ofcl.loc[:,'endang'] = az;
 ofcl.loc[:,'enddst'] = dst;
 
 rvcn = adeck[(adeck.idtstr==int(idtst)) & (adeck.mdl=='RVCN')];
 gfhr = [((vdt-idt).total_seconds()/3600) for vdt in rvcn['vdt']]
 rvcn.index = gfhr
 tvcn = adeck[(adeck.idtstr==int(idtst)) & (adeck.mdl=='TVCN')];
 gfhr = [((vdt-idt).total_seconds()/3600) for vdt in tvcn['vdt']]
 tvcn.index = gfhr

if ( benchstr == 'best' ):
  bench = best;
elif  ( benchstr == 'ofcl' ):
  bench = ofcl;
else:
  if ( (ofcl is None) | (best.index[-1] >= ofcl.index[-1]) ):
    bench = best; benchstr='best';
  else:
    bench = ofcl; benchstr='ofcl';

BENCHSTR = benchstr.upper();
print(f'Benchmark = {BENCHSTR}');

# # minlon = None; maxlon = None;
# # minlat = None; maxlat = None;
# # TO DO: Set automatically for each storm, below...
# minlon = -110; maxlon =  -30;
# minlat =  -10; maxlat =   50;
# TO DO: Set automatically for each storm, below...
# HERC domain: -115 to -5
minlat =  -10; maxlat =   50;
if ( best.lon[0] <= -70 ):
  minlon = -115;
  maxlon =  -30;
elif ( -70 < best.lon[0] and best.lon[0] <= -30 ):
  minlon = -90;
  maxlon =  -5;
nearminlon = best.lon[0]-20;
nearmaxlon = best.lon[0]+20;
nearminlat = best.lat[0]-20;
nearmaxlat = best.lat[0]+20;

atcfs = None

#DEBUG:
print('Processing ensemble ATCFs...');
for emem in range(0,31):
  #/scratch2/AOML/aoml-hafs1/Lew.Gramer/ocean/data/ensemble/2024080400/01/00l.2024080400.hfsa.parent.trak.atcfunix.all
  #/scratch2/AOML/aoml-hafs1/Lew.Gramer/ocean/data/ensemble/2024080400/01/00l.2024080400.hfsa.trak.atcfunix.all
  atcfall = gpt.read_atcf(f'{cycpth}/{emem:02}/00l.{idtst}.hfsa.trak.atcfunix.all');
  atcfall = atcfall[(beghr <= atcfall.fhr) & (atcfall.fhr <= endhr)];
  bsts = atcfall.drop_duplicates(subset=['basin','stno'])[['basin','stno']]
  #bsts = [f'{a[1].basin}{a[1].stno:02}' for a in atcfall.iterrows()]
  for bst in bsts.iterrows():
    atcf = atcfall.where( (atcfall.basin==bst[1].basin) & (atcfall.stno==bst[1].stno) ).dropna(axis=0,how='all')
    atcf['emem'] = emem;
    atcf['stid'] = f'{atcf.basin.values[0]}{int(atcf.stno.values[0]):02}';
    # Calculate track, intensity, wind field errors
    atcf.set_index('fhr',inplace=True);
    lonstretch = np.cos(np.deg2rad(atcf.lat.mean()));
    atcf['trkerr'] = (np.sqrt( (((atcf.lon-bench.lon)**2)*lonstretch) + ((atcf.lat-bench.lat)**2)) * 111e3);
    atcf['vmaxerr'] = np.abs(atcf.vmax-bench.vmax);
    atcf['r34err'] = np.abs(atcf.r34-bench.r34);
    atcf['r50err'] = np.abs(atcf.r50-bench.r50);
    atcf['r64err'] = np.abs(atcf.r64-bench.r64);
    try:
      atcf['rmwerr'] = np.abs(atcf.rmw-bench.rmw);
    except:
      atcf['rmwerr'] = atcf['r64err']
      atcf.loc[:,'rmwerr'] = np.nan
    
    #atcf['vmaxrms'] = np.sqrt( np.sum( (atcf.loc[fhr].vmax-bench.loc[fhr].vmax)**2 ) / len(atcf.vmax) );
    atcf['trkrms'] = atcf['trkerr'];
    atcf['vmaxrms'] = atcf['vmaxerr'];
    atcf['r34rms'] = atcf['r34err'];
    atcf['r50rms'] = atcf['r50err'];
    atcf['r64rms'] = atcf['r64err'];
    for fhrix,fhr in enumerate(atcf.index):
      atcf.loc[fhr,'trkrms'] = np.sqrt( np.sum( ((((atcf.lon.where(atcf.index<=fhr)-bench.lon.where(bench.index<=fhr))**2)*lonstretch) + ((atcf.lat.where(atcf.index<=fhr)-bench.lat.where(bench.index<=fhr))**2)) * 111e3) / (fhrix+1) );
      atcf.loc[fhr,'vmaxrms'] = np.sqrt( np.sum( (atcf.vmax.where(atcf.index<=fhr) - bench.vmax.where(bench.index<=fhr))**2 ) / (fhrix+1) );
      atcf.loc[fhr,'r34rms'] = np.sqrt( np.sum( (atcf.r34.where(atcf.index<=fhr) - bench.r34.where(bench.index<=fhr))**2 ) / (fhrix+1) );
      atcf.loc[fhr,'r50rms'] = np.sqrt( np.sum( (atcf.r50.where(atcf.index<=fhr) - bench.r50.where(bench.index<=fhr))**2 ) / (fhrix+1) );
      atcf.loc[fhr,'r64rms'] = np.sqrt( np.sum( (atcf.r64.where(atcf.index<=fhr) - bench.r64.where(bench.index<=fhr))**2 ) / (fhrix+1) );
    
    lon0 = np.tile(atcf.lon[0],atcf.lon.shape);
    lat0 = np.tile(atcf.lat[0],atcf.lat.shape);
    az,dst = gpt.azimuth_distance_wgs84(lon0,lat0,atcf.lon,atcf.lat);
    atcf['endang'] = az;
    atcf['enddst'] = dst;
    atcf['angerr'] = np.sin(np.deg2rad(atcf.endang-bench.endang));
    #DEBUG:    breakpoint();
    atcf.reset_index(inplace=True);
    #atcf.set_index(['basin','stno','emem','idtstr','fhr'],drop=False,inplace=True);
    atcf.set_index(['basin','stno','emem','idtstr','fhr'],inplace=True);
    if ( atcfs is None ):
      atcfs = atcf;
    else:
      #atcfs = atcfs.merge(atcf,how='outer');
      #atcfs = atcfs.merge(atcf,on=['basin','stno','emem','idtstr','fhr']);
      #DEBUG:      breakpoint()
      atcfs = pd.concat([atcfs,atcf]);
    # if ( highlight_stid == f'{atcf.basin.values[0]}{int(atcf.stno.values[0]):02}' ):
    #   atcfs.append(atcf);


trng = np.arange(10,30+1e-6,0.5)+273.14;	dtrng = np.arange(-6,+6+1e-6,0.2);
rrng = np.arange(50,100+1e-6,1.0);		drrng = np.arange(-20,20+1e-6,1);
sprng = np.arange(1000,1028+1e-6,0.5);		dsprng = np.arange(-5,+5+1e-6,0.2);
ghrng = np.arange(5000,6000+1e-6,0.20);		dghrng = np.arange(-30,30+1e-6,1); #ghrng for 500 mb
absvrng = np.arange(-6,+6+1e-6,0.2)-4;		dabsvrng = np.arange(-2,2+1e-6,0.05);
sstrng = np.arange(25,31+1e-6,0.2);		dsstrng = np.arange(-2,+2+1e-6,0.1);
ohcrng = np.arange(40,140+1e-6,5);		dohcrng = np.arange(-50,+50+1e-6,5);


print(f'Processing hours [{fig0hr}:{dhr}:{checkhr}]...');

# atcfs.query('basin=="AL" and stno==4 and fhr == 126')['trkerr'].max()
# atcfs.query('fhr == 48').index[0][4] == 48
querystr = f'basin=="{highlight_stid[0:2]}" and stno=={int(highlight_stid[2:])}';

if ( len(atcfs.query(querystr+f' and fhr == {checkhr}')) < 31 ):
 raise ValueError(f"Missing ATCF data for CHECKHR {checkhr}: {len(atcfs.query(querystr+f' and fhr == {checkhr}'))} members found")

print(f'Goofball workaround');
gpt.goofball_workaround()

for fhr in np.arange(fig0hr,checkhr+1,dhr):
 for cvar in cvars:
  #DEBUG:
  print(f'F{fhr:03} {cvar}');
  min_emem = atcfs.query(querystr+f' and fhr == {checkhr}')[cvar].argmin()
  max_emem = atcfs.query(querystr+f' and fhr == {checkhr}')[cvar].argmax()
  
  min_fname = f'{cycpth}/{min_emem:02}/00l.{idtst}.hfsa.parent.atm.f{fhr:03}.grb2';
  min_dses = gpt.cacheload_hafs_grb2(min_fname,{},ncpat=None,
                                          types=['surface.instant','surface.avg','10m','pressure','2m','whole'],wraplons=True);
  max_fname = f'{cycpth}/{max_emem:02}/00l.{idtst}.hfsa.parent.atm.f{fhr:03}.grb2';
  max_dses = gpt.cacheload_hafs_grb2(max_fname,{},ncpat=None,
                                          types=['surface.instant','surface.avg','10m','pressure','2m','whole'],wraplons=True);
  
  # Some ensemble members may couple to HYCOM, some to MOM6...
  # MIN member ocean data
  min_ofname = f'{cycpth}/{min_emem:02}/00l.{idtst}.hfsa.hycom.3z.f{fhr:03}.nc';
  try:
   min_ods = xr.open_dataset(min_ofname);
   min_ods.ohc = min_ods.ocean_heat_content
   min_ods.ohc.values = min_ods.ohc.where(min_ods.ohc>=0)
   min_ods = min_ods.squeeze()
  except:
   min_ofname = f'{cycpth}/{min_emem:02}/00l.{idtst}.hfsa.mom6.f{fhr:03}.nc';
   try:
    min_ods = xr.open_dataset(min_ofname);
    # NEED TO CALCULATE OHC, i26, drho/dz from ensemble outputs
    #min_ods['ocean_heat_content'] = calc_mom6_OHC(min_ods);
    min_ods['ohc'] = calc_mom6_OHC(min_ods);
   except:
    min_ods = None;
  # MAX member ocean data
  max_ofname = f'{cycpth}/{max_emem:02}/00l.{idtst}.hfsa.hycom.3z.f{fhr:03}.nc';
  try:
   max_ods = xr.open_dataset(max_ofname);
   max_ods.ohc = max_ods.ocean_heat_content
   max_ods.ohc.values = max_ods.ohc.where(max_ods.ohc>=0)
   max_ods = max_ods.squeeze()
  except:
   max_ofname = f'{cycpth}/{max_emem:02}/00l.{idtst}.hfsa.mom6.f{fhr:03}.nc';
   try:
    max_ods = xr.open_dataset(max_ofname);
    # NEED TO CALCULATE OHC, i26, drho/dz from ensemble outputs
    #max_ods['ocean_heat_content'] = calc_mom6_OHC(max_ods);
    max_ods['ohc'] = calc_mom6_OHC(max_ods);
   except:
    max_ods = None;
  
  mnds = min_dses['pressure'].where((400<=min_dses['pressure'].isobaricInhPa) &
                                    (min_dses['pressure'].isobaricInhPa<=700)).mean(dim='isobaricInhPa').squeeze();
  mxds = max_dses['pressure'].where((400<=min_dses['pressure'].isobaricInhPa) &
                                    (min_dses['pressure'].isobaricInhPa<=700)).mean(dim='isobaricInhPa').squeeze();
  plot_fld(mnds,mxds,'r',rrng,drrng,'400-700'); #RH
  plot_fld(mnds,mxds,'t',trng,dtrng,'400-700');
  #plot_fld(mnds,mxds,'gh',ghrng,dghrng,'400-700');
  
  mnds = min_dses['pressure'].where((850<=min_dses['pressure'].isobaricInhPa) &
                                    (min_dses['pressure'].isobaricInhPa<=1050)).mean(dim='isobaricInhPa').squeeze();
  mxds = max_dses['pressure'].where((850<=min_dses['pressure'].isobaricInhPa) &
                                    (min_dses['pressure'].isobaricInhPa<=1050)).mean(dim='isobaricInhPa').squeeze();
  plot_fld(mnds,mxds,'r',rrng,drrng,'850-sfc'); # RH
  plot_fld(mnds,mxds,'t',trng,dtrng,'850-sfc');
  #plot_fld(mnds,mxds,'gh',ghrng,dghrng,'850-sfc');
  
  for pr in prs:
   mnds = min_dses['pressure'].sel({'isobaricInhPa':pr},method='nearest'); mnds.absv.values = mnds.absv.values*1e4;
   mxds = max_dses['pressure'].sel({'isobaricInhPa':pr},method='nearest'); mxds.absv.values = mxds.absv.values*1e4;
   ######## HACK HACK HACK
   #plot_fld(mnds,mxds,'absv',absvrng,dabsvrng,f'{pr}');
   plot_fld(mnds,mxds,'gh',ghrng,dghrng,f'{pr}');
  
  mnds = min_dses['surface.instant']; mnds.sp.values = mnds.sp.values/1e2; mnds.sst.values = mnds.sst.values - 273.14;
  mxds = max_dses['surface.instant']; mxds.sp.values = mxds.sp.values/1e2; mxds.sst.values = mxds.sst.values - 273.14;
  plot_fld(mnds,mxds,'sp',sprng,dsprng,'sfc');
  plot_fld(mnds,mxds,'sst',sstrng,dsstrng,'sfc');
  if ( min_ods is not None and max_ods is not None ):
   plot_fld(min_ods,max_ods,'ohc',ohcrng,dohcrng,'ocn');
  else:
   print(f'MISSING OHC: {min_ofname} or {max_ofname} (or both)');
  #DEBUG:  plt.show(block=False); breakpoint();
  
  #DEBUG:  plt.show(block=False); breakpoint();
  plt.close('all')
  del(min_dses); del(max_dses);
  if ( min_ods is not None ): del(min_ods);
  if ( max_ods is not None ): del(max_ods);

#DEBUG:plt.show(block=False); breakpoint();
