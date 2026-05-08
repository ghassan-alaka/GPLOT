#!/usr/bin/env python
"""GPLOT ocean maps plotter — HYCOM and MOM6 support."""

import os
import sys
import re
import glob
import math
import argparse
from datetime import datetime

import numpy as np
import numpy.ma as ma
import xarray as xr
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.colors as colors
from matplotlib.axes import Axes

from gplot_utils import namelist as nml_utils
from gplot_utils import atcf as atcf_utils
from gplot_utils import plot_utils
from gplot_utils import colormaps as cmap_utils
from gplot_utils import ocean_reader
from gplot_utils.domains import is_storm_named_filename

GPLOT_DIR = os.environ['GPLOT_DIR']
print('MSG: Found this GPLOT location --> ' + GPLOT_DIR)
print('MSG: Importing Everything Needed')


def debug_dump_range(FHR, varnm, var):
    pass


def add_center_label(ax1, centerlon, centerlat, minpressure):
    ax1.text(centerlon, centerlat, f'{minpressure}\n  L', color='black', fontsize=28, fontweight='extra bold')
    ax1.text(centerlon, centerlat, f'{minpressure}\n  L', color='red', fontsize=28)


def load_axbt_data(BASEFDT, ndays=(-3, -2, -1, 0, 1, 2, 3, 4),
                   ddir='/scratch2/AOML/aoml-hafs1/Lew.Gramer/ocean/data'):
    """Create XArray Dataset of AXBT float profiles for days in BASEFDT +/- NDAYS."""
    import pandas as pd
    from datetime import timedelta
    basefdt = datetime.strptime(BASEFDT, '%Y%m%d')
    for nday in ndays:
        fdt = basefdt + timedelta(days=nday)
        FDT = fdt.strftime('%Y%m%d')
        fdfpatt = f'{ddir}/axbts/{FDT}*/{FDT}*.dat'
        for fdfname in sorted(glob.glob(fdfpatt)):
            try:
                fse = pd.read_csv(fdfname, r'\s+', low_memory=False, header=2)
            except Exception:
                print(f'Failed read_csv: {fdfname}')
                continue
            flight = os.path.basename(fdfname).split('_')[0]
            t = datetime.strptime(fse.columns[0] + fse.columns[1], '%Y%m%d%H%M%S')
            try:
                lat = np.double(fse.columns[2])
                lon = np.double(fse.columns[3])
            except Exception:
                lat = np.nan
                lon = np.nan
            platform = fse.columns[4]
            storm = fse.columns[5]
            fse.dropna(axis=1, inplace=True)
            fse.columns = ['depth', 'temperature', 'ignore']
            T = fse.temperature.values.reshape((1, len(fse.temperature)))
            T[T < 0] = np.nan
            z = fse.depth
            fds = xr.Dataset(
                {'T': (('t', 'z'), T), 'lon': ('t', [lon]), 'lat': ('t', [lat]),
                 'flight': ('t', [flight]), 'storm': ('t', [storm]),
                 'platform': ('t', [platform])},
                coords={'t': [t], 'z': (('z',), z)},
            )
            if 'fdses' in locals():
                fdses = xr.merge([fdses, fds])
            else:
                fdses = fds
    return fdses


def _parse_args():
    parser = argparse.ArgumentParser(description='GPLOT Ocean Maps plotter')
    parser.add_argument('--idate', required=True, help='Forecast init date YYYYMMDDHH')
    parser.add_argument('--sid', required=True, help='Storm ID (e.g. 13L)')
    parser.add_argument('--ocean-domain', required=True, dest='ocean_domain')
    parser.add_argument('--tier', required=True)
    parser.add_argument('--ensid', default='')
    parser.add_argument('--force', default='')
    parser.add_argument('--resolution', type=float, required=True)
    parser.add_argument('--rmax', type=float, required=True)
    parser.add_argument('--levs', type=int, required=True)
    parser.add_argument('--master-nml', required=True, dest='master_nml')
    parser.add_argument('--ocean-source', default='HYCOM', dest='ocean_source',
                        choices=['HYCOM', 'MOM6'])
    parser.add_argument('--ocean-cfg', default='NHC', dest='ocean_cfg')
    parser.add_argument('--fix-dir', default='', dest='fix_dir')
    parser.add_argument('--wrap-lon', action='store_true', default=False, dest='wrap_lon')
    return parser.parse_args()


##############################
def main():
    args = _parse_args()

    IDATE = args.idate
    SID = args.sid
    OCEAN_DOMAIN = args.ocean_domain
    TIER = args.tier
    OCEAN_SOURCE = args.ocean_source or 'HYCOM'
    OCEAN_CFG = args.ocean_cfg or 'NHC'
    OCEAN_WRAP_LON = args.wrap_lon
    resolution = args.resolution
    rmax = args.rmax
    zsize_pressure = args.levs

    # Locate master namelist
    NMLIST = args.master_nml
    if os.path.exists(NMLIST):
        MASTER_NML_IN = NMLIST
    elif os.path.exists(os.path.join(GPLOT_DIR, 'parm', NMLIST)):
        MASTER_NML_IN = os.path.join(GPLOT_DIR, 'parm', NMLIST)
    else:
        print("ERROR: I couldn't find the Master Namelist.")
        sys.exit(1)

    # Read master namelist
    nml = nml_utils.read_master_namelist(MASTER_NML_IN)
    DSOURCE = nml.get('DSOURCE', 'HAFS')
    OCEAN_DSOURCE = (nml.get('OCEAN_DSOURCE') or DSOURCE).strip()
    EXPT = nml.get('EXPT', '').strip()
    ODIR_base = nml.get('ODIR', '').strip()
    ODIR_TYPE = int(nml.get('ODIR_TYPE', 0) or 0)
    DO_CONVERTGIF = bool(nml.get('DO_CONVERTGIF', False))

    NMLDIR = os.path.join(GPLOT_DIR, 'parm')
    FIX_DIR = args.fix_dir.strip() if args.fix_dir else os.path.join(GPLOT_DIR, 'fix')

    if ODIR_TYPE == 1:
        ODIR = ODIR_base + '/ocean_' + OCEAN_DOMAIN + '/'
    else:
        ODIR = ODIR_base + '/' + EXPT + '/' + IDATE.strip() + '/ocean_' + OCEAN_DOMAIN + '/'

    print(f'DEBUG: OCEAN_SOURCE {OCEAN_SOURCE}')
    print(f'DEBUG: OCEAN_DSOURCE {OCEAN_DSOURCE}')

    # Get ocean depths
    if OCEAN_SOURCE == 'HYCOM':
        depths = ocean_reader.read_hycom_depth(FIX_DIR, OCEAN_DSOURCE.lower(), OCEAN_CFG.lower())
        DEPTH_FILE = os.path.join(
            FIX_DIR,
            f'{OCEAN_DSOURCE.lower()}_hycom_{OCEAN_CFG.lower()}.basin.regional.depth')
    else:
        DEPTH_FILE = os.path.join(FIX_DIR, OCEAN_CFG.lower(), 'ocean_topog.nc')
        depths_ds = xr.open_dataset(DEPTH_FILE)
        depths = depths_ds.depth.values
        depths_ds.close()
    print(f'DEBUG:: DEPTH_FILE={DEPTH_FILE}, shape={depths.shape}')

    # EXPT title lookup
    TBLDIR = os.path.join(GPLOT_DIR, 'tbl')
    EXPT_TITLE = EXPT
    tbl_path = os.path.join(TBLDIR, 'ExptInfo.dat')
    if os.path.isfile(tbl_path):
        pat = re.compile(r'^\s+' + re.escape(EXPT) + r'\s*,')
        with open(tbl_path) as f:
            for line in f:
                if pat.match(line):
                    parts = line.split(',')
                    if len(parts) > 1:
                        EXPT_TITLE = parts[1].strip()
                    break
    print(f'EXPT --> {EXPT}')
    print(f'EXPT_TITLE --> {EXPT_TITLE}')

    # File name components
    if OCEAN_DOMAIN in ['hwrf', 'd03', 'd02', 'tkfull', 'alld03', 'storm', 'core', 'tcparent']:
        STORMTAG = '.' + SID.strip()
    else:
        STORMTAG = ''
    UNPLOTTED_FILE = ODIR.strip() + 'UnplottedOceanFiles.' + OCEAN_DOMAIN.strip() + '.' + TIER.strip() + STORMTAG + '.log'
    PLOTTED_FILE   = ODIR.strip() + 'PlottedOceanFiles.'   + OCEAN_DOMAIN.strip() + '.' + TIER.strip() + STORMTAG + '.log'
    ALLFHR_FILE    = ODIR.strip() + 'AllForecastHours.'    + OCEAN_DOMAIN.strip() + '.' + TIER.strip() + STORMTAG + '.log'
    STATUS_FILE    = ODIR.strip() + 'status.'              + OCEAN_DOMAIN.strip() + '.' + TIER.strip() + STORMTAG + '.log'
    ST_LOCK_FILE   = STATUS_FILE + '.lock'

    # Find ATCF file
    atcf_list_path = ODIR + 'ATCF_FILES.dat'
    if os.path.isfile(atcf_list_path):
        ATCF_LIST = np.atleast_1d(np.genfromtxt(atcf_list_path, dtype='str'))
    else:
        print(f'WARNING: Missing ATCF file list: {atcf_list_path}')
        ATCF_LIST = np.array(['NONE'])
    valid_atcfs = [str(s) for s in ATCF_LIST if str(s) and str(s) != 'NONE']
    if len(valid_atcfs) > 1:
        print('Found multiple ATCFs')
        print(f'DEBUG:: {valid_atcfs}: {SID.lower()}')
        sid_key = f'{SID.lower()}.'
        sid_matches = [s for s in valid_atcfs if sid_key in s.lower()]
        # Fallback to first valid entry when no direct SID match is found.
        ATCF = sid_matches[0] if sid_matches else valid_atcfs[0]
    elif len(valid_atcfs) == 1:
        ATCF = valid_atcfs[0]
    else:
        ATCF = 'NONE'
    print('MSG: Found this ATCF --> ' + str(ATCF))

    if str(ATCF) == 'NONE':
        LONGSID = '00L'
    else:
        LONGSID = str(ATCF).split('/')[-1].split('.')[0]
    TCNAME  = LONGSID[::-1][3:][::-1]
    SNUM    = LONGSID[::-1][1:3][::-1]
    BASINID = LONGSID[::-1][0]

    # Load ATCF DataFrame for d03
    atcf_df = None
    if OCEAN_DOMAIN == 'd03' and str(ATCF) != 'NONE':
        atcf_df = atcf_utils.read_atcf(str(ATCF))

    # Forecast hour / file lists
    UNPLOTTED_LIST = np.array(np.genfromtxt(UNPLOTTED_FILE, dtype='str'))
    FHR_LIST = np.array(np.genfromtxt(ALLFHR_FILE, dtype='int'))
    if FHR_LIST.size == 1:
        FHR_LIST = np.append(FHR_LIST, '999')
        UNPLOTTED_LIST = np.append(UNPLOTTED_LIST, 'MISSING')

    SST0 = None
    dSST = None
    OHC0 = None
    dOHC = None
    SSH0 = None
    dSSH = None
    DFHR = int(FHR_LIST[-1]) - int(FHR_LIST[-2])

    forecastinit = IDATE.strip()
    figext2 = '.gif' if DO_CONVERTGIF else '.png'

    for (FILE, fff) in zip(UNPLOTTED_LIST, np.array(range(UNPLOTTED_LIST.size))):

        if FILE == 'MISSING':
            continue

        print('MSG: Working on this file --> ' + str(FILE) + '  ' + str(fff))

        os.system('lockfile -r-1 -l 180 ' + ST_LOCK_FILE)
        os.system('echo "working" > ' + STATUS_FILE)
        os.system('rm -f ' + ST_LOCK_FILE)

        FILE_BASE = os.path.basename(FILE)
        FILE_DIR  = os.path.dirname(FILE)
        FHR = int(FHR_LIST[fff])

        if OCEAN_DOMAIN == 'd03':
            row = atcf_df[atcf_df['fhr'] == FHR]
            if row.empty:
                plot_utils.update_plotted_file(PLOTTED_FILE, FILE)
                continue
            row = row.iloc[0]
            centerlon = float(row['lon'])
            if centerlon < 0:
                centerlon += 360
            centerlat   = float(row['lat'])
            forecastinit = str(row['cycle'])
            maxwind     = str(int(row['vmax']))
            minpressure = str(int(row['mslp']))
            rmwnmi      = row['rmw']
            neq34 = float(row['rad_ne'])
            seq34 = float(row['rad_se'])
            swq34 = float(row['rad_sw'])
            nwq34 = float(row['rad_nw'])

            if centerlat > 50.0:
                print('WARNING: The latitude is poleward of +/- 50. Skipping.')
                plot_utils.update_plotted_file(PLOTTED_FILE, FILE)
                continue

        print(f'MSG: Searching for graphics products that match --> {ODIR}/*{LONGSID.lower()}*f{FHR:03}{figext2}')
        figuretest = np.shape([g for g in glob.glob(f"{ODIR}/*{LONGSID.lower()}*f{format(FHR,'03d')}{figext2}")])[0]
        if figuretest > 0:
            print(f'MSG: Found {figuretest} matching graphical products for this lead time.')
            print(f'MSG: Please delete all {figext2} files for this lead time to reproduce graphics. Skipping.')
            plot_utils.update_plotted_file(PLOTTED_FILE, FILE)
            continue

        print(f"MSG: I can't find the graphical products for this lead time (figuretest={figuretest}). Proceeding.")

        gribfiletest = os.system('ls ' + FILE)
        if gribfiletest > 0:
            print(f'MSG: The input file does not exist. Nothing to do. Skipping.')
            continue

        if OCEAN_DOMAIN == 'd03':
            yoffset = 6
            xoffset = None
            NL = yoffset - 1
            while not xoffset:
                if NL > 25:
                    print(f'ERROR: YOU NEED A BIGGER BOX THAN {NL} DEGREES. rmax={rmax}, centerlat={centerlat}')
                    sys.exit(1)
                NL = NL + 1
                test = np.cos((abs(centerlat) + yoffset) * 3.14159 / 180) * 111.1 * NL
                if test > rmax:
                    xoffset, yoffset = NL, NL
            print(f'MSG: Will use a box with side of {NL} degrees.')
            lonmax = centerlon + xoffset
            lonmin = centerlon - xoffset
            latmax = centerlat + yoffset
            latmin = centerlat - yoffset

        if OCEAN_DOMAIN == 'd03':
            print('MSG: Getting Data Now. Using an xoffset of ' + str(xoffset) + ' degrees')
        else:
            print('MSG: Getting Data Now.')

        all_ds = xr.open_dataset(FILE)

        ########################################################################
        # HYCOM POST OUTPUT
        if OCEAN_SOURCE == 'HYCOM':
            if OCEAN_WRAP_LON:
                print('MSG: Wrapping 3D Longitudes')
                all_ds['Longitude'] = all_ds.Longitude + 360
            ds = all_ds.where(~depths.mask)
            if SST0 is None:
                SST0 = ds.temperature[0, ...].squeeze()
                if len(SST0.dims) > 2:
                    SST0 = SST0[0, ...].squeeze()
            if OHC0 is None:
                OHC0 = ds.ocean_heat_content.squeeze()
            if OCEAN_DOMAIN == 'd03':
                ds = ds.where(
                    (lonmin <= all_ds.Longitude) & (all_ds.Longitude <= lonmax) &
                    (latmin <= all_ds.Latitude)  & (all_ds.Latitude  <= latmax))
            lon  = ds.Longitude.squeeze()
            lat  = ds.Latitude.squeeze()
            modelt = ds.MT.values[0]
            ucurr = ds.u_velocity.squeeze()
            vcurr = ds.v_velocity.squeeze()
            wcurr = ds.w_velocity.squeeze()
            print('MSG: Done With u,v,w')
            MLD = ds.mixed_layer_thickness.squeeze()
            T   = ds.temperature.squeeze()
            S   = ds.salinity.squeeze()
            SST = T[0, ...].squeeze()
            SSS = S[0, ...].squeeze()
            OHC = ds.ocean_heat_content.squeeze()
            i26 = ds['depth of 26C isotherm'].squeeze()
            i20 = ds['depth of 20C isotherm'].squeeze()
            # Save Z coordinate and dZ before closing
            Z_coord = ds.Z
            dZ = Z_coord.diff(dim='Z')
            MLu = ucurr.where(Z_coord < MLD).mean(axis=0)
            MLv = vcurr.where(Z_coord < MLD).mean(axis=0)
            MLT = T.where(Z_coord < MLD).mean(axis=0)
            MLS = S.where(Z_coord < MLD).mean(axis=0)
            ds.close()
            print('MSG: Done with T, S, MLD, OHC, Iso')

            dSST = (SST - SST0) / (np.double(FHR) / 24)
            dOHC = (OHC - OHC0) / (np.double(FHR) / 24)
            print('MSG: Done with MLu, MLv, MLT, MLS')

            # 2D surface data
            oFILE = FILE.replace('3z', '2d')
            ofiletest = os.system('ls ' + oFILE)
            SHF = None
            SSH = None
            if ofiletest < 1:
                all_ods = xr.open_dataset(oFILE)
                if OCEAN_WRAP_LON:
                    print('MSG: Wrapping 2D Longitudes')
                    all_ods['Longitude'] = all_ods.Longitude + 360
                ods = all_ods.where(~depths.mask)
                if SSH0 is None:
                    SSH0 = ods.sea_surface_height.squeeze()
                if OCEAN_DOMAIN == 'd03':
                    ods = ods.where(
                        (lonmin <= all_ods.Longitude) & (all_ods.Longitude <= lonmax) &
                        (latmin <= all_ods.Latitude)  & (all_ods.Latitude  <= latmax))
                SHF  = ods.surface_heat_flux.squeeze()
                SSH  = ods.sea_surface_height.squeeze()
                dSSH = (SSH - SSH0) / (np.double(FHR) / 24)
                Mon  = ods.montgomery_potential_surf.squeeze()
                ods.barotropic_u_velocity.squeeze()
                ods.barotropic_v_velocity.squeeze()
                ods.close()
                print(f'MSG: Done with surface vars (e.g., redo of MLu,MLv) {datetime.now()}')

        ########################################################################
        # MOM6 POST OUTPUT
        elif OCEAN_SOURCE == 'MOM6':
            all_ds = all_ds.interp({'xq': all_ds.xh, 'yq': all_ds.yh})
            if OCEAN_WRAP_LON:
                print('MSG: Wrapping 3D Longitudes')
                all_ds['xh']     = all_ds.xh + 360
                all_ds['geolon'] = all_ds.geolon + 360
            ds = all_ds
            if SST0 is None:
                SST0 = ds.squeeze().temp[0, ...].squeeze()
            if OHC0 is None:
                print(f'WARNING:: Initial full-domain OHC - and dOHC - is not available (yet) for MOM6!')
            if SSH0 is None:
                SSH0 = ds.SSH.squeeze() * 1e2
            if OCEAN_DOMAIN == 'd03':
                ds = ds.where(
                    (lonmin <= all_ds.xh) & (all_ds.xh <= lonmax) &
                    (latmin <= all_ds.yh) & (all_ds.yh <= latmax))
            lon  = ds.xh.squeeze()
            lat  = ds.yh.squeeze()
            ucurr = ds.uo.squeeze()
            vcurr = ds.vo.squeeze()
            print('MSG: Done With u,v,w')
            MLD = ds.MLD_0125.squeeze()
            T   = ds.temp.squeeze()
            S   = ds.so.squeeze()
            SST = T[0, ...].squeeze()
            SSS = S[0, ...].squeeze()
            i26 = ds.z_l.where(T >= 26).max(axis=0)
            i20 = ds.z_l.where(T >= 20).max(axis=0)
            cp          = ocean_reader.CP_SW_MOM6
            rho         = ocean_reader.RHO_SW
            kJcm2_per_Jm2 = ocean_reader.KJ_CM2_PER_J_M2
            delT = xr.where(T - 26 > 0, T - 26, 0)
            dz = ds.z_l.copy()
            dz.values[1:] = ds.z_l[1:] - ds.z_l[0:-1].values
            dZ = dz.broadcast_like(delT)
            OHC = cp * rho * (delT * dZ).sum(axis=0) * kJcm2_per_Jm2
            print('MSG: Done with T, S, MLD, Iso, OHC')

            MLu = ucurr.where(ds.z_l < MLD).mean(axis=0)
            MLv = vcurr.where(ds.z_l < MLD).mean(axis=0)
            MLT = T.where(ds.z_l < MLD).mean(axis=0)
            MLS = S.where(ds.z_l < MLD).mean(axis=0)
            print('MSG: Done with MLu, MLv, MLT, MLS')

            SHF  = -(ds.LwLatSens.squeeze() + ds.SW.squeeze())
            dSST = (SST - SST0) / (np.double(FHR) / 24)
            SSH  = ds.SSH.squeeze() * 1e2
            dSSH = (SSH - SSH0) / (np.double(FHR) / 24)
            ds.close()
            print(f'MSG: Done with surface vars (e.g., redo of MLu,MLv) {datetime.now()}')

        else:
            print(f'ERROR: OCEAN_SOURCE {OCEAN_SOURCE} not yet handled by PLOT_OCEAN_MAPS.py!')
            sys.exit(1)

        lonstretch = np.cos(np.deg2rad(lon.mean()))
        secPerDay  = 24 * 3600

        print(f'MSG: Doing Plots Now {datetime.now()}')

        # Read module namelist flags
        nml_flags = None
        for candidate in [
            os.path.join(NMLDIR, f'namelist.ocean_maps.{EXPT}'),
            os.path.join(NMLDIR, 'namelist.ocean_maps'),
        ]:
            if os.path.isfile(candidate):
                nml_flags = nml_utils.read_ocean_maps_namelist(candidate)
                break
        if nml_flags is None:
            nml_flags = nml_utils.read_ocean_maps_namelist.__doc__ and {}
            nml_flags = {
                'do_iso_26': True,  'do_iso_20': True,  'do_ohc': True,
                'do_sfc_conv': True, 'do_sfc_vort': True, 'do_ml_conv': True,
                'do_ml_vort': True, 'do_ssh': True,     'do_shf': True,
                'do_dpi': False,    'do_mld': True,      'do_sss': True,
                'do_mlt': True,     'do_mls': True,      'do_ssh_tendency': True,
                'do_ships_output': True, 'do_sst_tendency': True,
                'do_ohc_tendency': True, 'do_obs_profiles': False,
            }

        do_iso_26       = nml_flags['do_iso_26']
        do_iso_20       = nml_flags['do_iso_20']
        do_ohc          = nml_flags['do_ohc']
        do_sfc_conv     = nml_flags['do_sfc_conv']
        do_sfc_vort     = nml_flags['do_sfc_vort']
        do_ml_conv      = nml_flags['do_ml_conv']
        do_ml_vort      = nml_flags['do_ml_vort']
        do_ssh          = nml_flags['do_ssh']
        do_shf          = nml_flags['do_shf']
        do_dpi          = nml_flags['do_dpi']
        do_mld          = nml_flags['do_mld']
        do_sss          = nml_flags['do_sss']
        do_mlt          = nml_flags['do_mlt']
        do_mls          = nml_flags['do_mls']
        do_ssh_tendency = nml_flags['do_ssh_tendency']
        do_ships_output = nml_flags['do_ships_output']
        do_sst_tendency = nml_flags['do_sst_tendency']
        do_ohc_tendency = nml_flags['do_ohc_tendency']
        do_obs_profiles = nml_flags['do_obs_profiles']

        # Wind colormap (used by some legacy callers)
        color_data_vt = np.genfromtxt(GPLOT_DIR + '/sorc/GPLOT/python/colormaps/colormap_wind.txt')
        colormap_vt   = matplotlib.colors.ListedColormap(color_data_vt)
        levs_vt  = np.linspace(0, 80, 41, endpoint=True)
        norm_vt  = colors.BoundaryNorm(levs_vt, 256)

        # Convergence / vorticity per source
        if OCEAN_SOURCE == 'HYCOM':
            ml_u_x  = MLu.differentiate('Longitude') / 1e2 / (lonstretch * 111e3)
            ml_u_y  = MLu.differentiate('Latitude')  / 1e2 / 111e3
            ml_v_x  = MLv.differentiate('Longitude') / 1e2 / (lonstretch * 111e3)
            ml_v_y  = MLv.differentiate('Latitude')  / 1e2 / 111e3
            sfc_u_x = ucurr[1, :].differentiate('Longitude') / 1e2 / (lonstretch * 111e3)
            sfc_u_y = ucurr[1, :].differentiate('Latitude')  / 1e2 / 111e3
            sfc_v_x = vcurr[1, :].differentiate('Longitude') / 1e2 / (lonstretch * 111e3)
            sfc_v_y = vcurr[1, :].differentiate('Latitude')  / 1e2 / 111e3
        else:
            ml_u_x  = MLu.differentiate('xh') / 1e2 / (lonstretch * 111e3)
            ml_u_y  = MLu.differentiate('yh') / 1e2 / 111e3
            ml_v_x  = MLv.differentiate('xh') / 1e2 / (lonstretch * 111e3)
            ml_v_y  = MLv.differentiate('yh') / 1e2 / 111e3
            sfc_u_x = ucurr[1, :].differentiate('xh') / 1e2 / (lonstretch * 111e3)
            sfc_u_y = ucurr[1, :].differentiate('yh') / 1e2 / 111e3
            sfc_v_x = vcurr[1, :].differentiate('xh') / 1e2 / (lonstretch * 111e3)
            sfc_v_y = vcurr[1, :].differentiate('yh') / 1e2 / 111e3

        ml_conv  = -(ml_u_x  + ml_v_y)  * secPerDay
        ml_vort  =  (ml_v_x  - ml_u_y)  * secPerDay
        sfc_conv = -(sfc_u_x + sfc_v_y) * secPerDay
        sfc_vort =  (sfc_v_x - sfc_u_y) * secPerDay

        # Colormap levels (source-independent)
        iso_26_levs = cmap_utils.get_contour_levels('ISO26')
        iso_26_ticks = iso_26_levs[::4]
        iso_20_levs  = cmap_utils.get_contour_levels('ISO20')
        iso_20_ticks = iso_20_levs[::2]
        dOHC_levs  = cmap_utils.get_contour_levels('DOHC')
        dOHC_ticks = np.arange(-40, 42, 5)
        dSST_levs  = cmap_utils.get_contour_levels('DSST')
        dSST_ticks = np.arange(-3, 3.2, 0.5)
        SSH_levs  = cmap_utils.get_contour_levels('SSH')
        SSH_ticks = SSH_levs[::4]
        DPI_levs  = cmap_utils.get_contour_levels('DPI')
        DPI_ticks = DPI_levs[::2]
        MLD_levs  = cmap_utils.get_contour_levels('MLD')
        MLD_ticks = MLD_levs[::2]
        SSS_levs  = cmap_utils.get_contour_levels('SSS')
        SSS_ticks = np.arange(32, 38.2, 0.5)
        MLT_levs  = cmap_utils.get_contour_levels('MLT')
        MLT_ticks = np.arange(26, 30.2, 0.5)
        MLS_levs  = cmap_utils.get_contour_levels('MLS')
        MLS_ticks = np.arange(32, 38.2, 0.5)

        # Source-dependent levels
        OHC_src   = 'HYCOM' if OCEAN_SOURCE == 'HYCOM' else ''
        OHC_levs  = cmap_utils.get_contour_levels('OHC', OHC_src)
        OHC_ticks = OHC_levs[::4]
        SHF_src   = 'HYCOM' if OCEAN_SOURCE == 'HYCOM' else ''
        SHF_levs  = cmap_utils.get_contour_levels('SHF', SHF_src)
        SHF_ticks = SHF_levs[::4]

        if OCEAN_SOURCE == 'HYCOM':
            sfc_conv_levs  = cmap_utils.get_contour_levels('CONV')
            sfc_conv_ticks = np.arange(-20, 22, 2)
            sfc_vort_levs  = cmap_utils.get_contour_levels('VORT_OCN')
            sfc_vort_ticks = np.arange(-20, 22, 2)
            ml_conv_levs   = cmap_utils.get_contour_levels('CONV')
            ml_conv_ticks  = np.arange(-20, 22, 2)
            ml_vort_levs   = cmap_utils.get_contour_levels('VORT_OCN')
            ml_vort_ticks  = np.arange(-20, 22, 2)
            dSSH_levs  = cmap_utils.get_contour_levels('DSSH')
            dSSH_ticks = np.arange(-20, 22, 5)
        else:  # MOM6
            sfc_conv_levs  = np.arange(-2e-2, 2e-2 + 1e-8, 5e-4)
            sfc_conv_ticks = np.arange(-2e-2, 2e-2 + 1e-8, 2e-3)
            sfc_vort_levs  = np.arange(-4e-2, 4e-2 + 1e-8, 2e-3)
            sfc_vort_ticks = np.arange(-4e-2, 4e-2 + 1e-8, 5e-3)
            ml_conv_levs   = np.arange(-2e-2, 2e-2 + 1e-8, 1e-3)
            ml_conv_ticks  = np.arange(-2e-2, 2e-2 + 1e-8, 5e-3)
            ml_vort_levs   = np.arange(-4e-2, 4e-2 + 1e-8, 2e-3)
            ml_vort_ticks  = np.arange(-4e-2, 4e-2 + 1e-8, 5e-3)
            dSSH_levs  = SSH_levs
            dSSH_ticks = SSH_ticks

        # Dynamic potential intensity (Balaguru et al. 2015)
        rho0  = 1025.0
        ustar = 0.20
        U     = 10
        R     = 50e3
        tmix  = R / U
        kappa = 0.40
        g     = 9.806
        T0    = (-75 + 273.14)  # outflow temperature
        alpha = 0.03
        oL = MLD + (((2 * rho0 * (ustar ** 3) * tmix) / (kappa * g * alpha)) ** (1 / 3))
        if OCEAN_SOURCE == 'HYCOM':
            Tdy = (1 / oL) * (dZ * (T + 273.14)).where(Z_coord <= oL).sum(dim='Z')
        else:
            Tdy = (1 / oL) * (dZ * (T + 273.14)).where(dZ.z_l <= oL).sum(dim='z_l')
        Ck_Cd = 0.9
        cpa   = 1.006
        hg    = 2549
        aT    = 26 + 273.14
        aQ    = 0.01
        k     = (cpa * aT)  + hg * aQ
        kdy   = (cpa * Tdy) + hg * 1
        DPI   = np.sqrt(((Tdy - T0) / T0) * Ck_Cd * (kdy - k))
        print('DPI', DPI.max().values)

        # Streamplot grid (requires equally-spaced coordinates)
        print(float(lon.min()), float(lon.max()), lon.shape[0])
        xi = np.linspace(float(lon.min()), float(lon.max()), lon.shape[0])
        yi = np.linspace(float(lat.min()), float(lat.max()), lat.shape[0])

        figsize       = (24, 24)
        fontsize      = 24
        small_fontsize = 24
        plt.rcParams.update({'font.size': 20})

        # SHIPS output
        if do_ships_output and OCEAN_DOMAIN == 'd03':
            T0 = T[0, :, :].squeeze()
            dlats = (lat * (math.pi) / 180.) - (centerlat * (math.pi) / 180.)
            dlons = (lon * (math.pi) / 180.) - (centerlon * (math.pi) / 180.)
            aa  = ((np.sin(dlats / 2)) ** 2
                   + np.cos((centerlat * (math.pi) / 180))
                   * np.cos((lat * (math.pi) / 180))
                   * (np.sin(dlons / 2)) ** 2)
            cc  = 2 * np.arctan2(np.sqrt(aa), np.sqrt(1 - aa))
            rad_distances = cc * 6371.
            bearings1 = ((np.arctan2(
                (np.sin(dlons)) * (np.cos(lat * (math.pi) / 180.)),
                ((np.cos(centerlat * (math.pi) / 180.)) * (np.sin(lat * (math.pi) / 180.)))
                - (((np.sin(centerlat * (math.pi) / 180.)) * (np.cos(lat * (math.pi) / 180.)))
                   * (np.cos(dlons))))) * (180. / (math.pi))) % 360
            bearings1 = np.array(bearings1)
            bearings  = np.transpose(bearings1)
            where500  = np.where(rad_distances <= 500.)
            where200  = np.where(rad_distances <= 200.)

            wherecov = np.where(T0 > 0.)
            try:
                if (np.size(wherecov) / np.size(where500)) < .5:
                    T500 = np.nan; OHC500 = np.nan
                else:
                    T500 = np.nanmean(T0[where500]); OHC500 = np.nanmean(OHC[where500])
            except Exception:
                T500 = np.nan; OHC500 = np.nan

            try:
                if (np.size(wherecov) / np.size(where200)) < .5:
                    T200 = np.nan; OHC200 = np.nan
                else:
                    T200 = np.nanmean(T0[where200]); OHC200 = np.nanmean(OHC[where200])
            except Exception:
                T200 = np.nan; OHC200 = np.nan

            where34 = np.where(
                ((bearings < 90.)  & (rad_distances < neq34)) |
                ((bearings < 180.) & (bearings >= 90.)  & (rad_distances < seq34)) |
                ((bearings < 270.) & (bearings >= 180.) & (rad_distances < swq34)) |
                ((bearings < 360.) & (bearings >= 270.) & (rad_distances < nwq34)))
            print(np.size(where34)); print(neq34)
            T34   = np.nanmean(T0[where34])
            OHC34 = np.nanmean(OHC[where34])

            sstfname = ODIR + '/' + LONGSID.lower() + '.ships.sst.' + forecastinit + '.ocean_' + OCEAN_DOMAIN + '.dat'
            ohcfname = ODIR + '/' + LONGSID.lower() + '.ships.ohc.' + forecastinit + '.ocean_' + OCEAN_DOMAIN + '.dat'
            with open(sstfname, 'a+') as f:
                print(f'{FHR}, {T200}, {T500}, {T34} ', file=f)
            with open(ohcfname, 'a+') as f:
                print(f'{FHR}, {OHC200}, {OHC500}, {OHC34}', file=f)

        # ------------------------------------------------------------------ #
        # Figure helpers
        # Storm-named domains (d03, hwrf) embed LONGSID in the
        # filename + show storm-specific titles. Large-scale domains
        # (atl, basin, global, ...) are storm-agnostic.
        _storm_named = is_storm_named_filename(OCEAN_DOMAIN)

        def _figfname(tag):
            prefix = (LONGSID.lower() + '.') if _storm_named else ''
            return (ODIR + '/' + prefix + tag + '.'
                    + forecastinit + '.ocean_' + OCEAN_DOMAIN
                    + '.f' + format(FHR, '03d'))

        def _set_titles_storm(ax, left_title):
            ax.set_title(left_title, fontsize=small_fontsize, weight='bold', loc='left')
            ax.set_title('VMAX= ' + maxwind + ' kt\nPMIN= ' + minpressure + ' hPa\n' + LONGSID.upper(),
                         fontsize=fontsize, color='brown', loc='right')
            ax.set_xlim([lonmin, lonmax]); ax.set_ylim([latmin, latmax])

        def _apply_domain(ax, left_title_d03, left_title_global, stream=True):
            ax.contour(lon, lat, depths, levels=[150],
                       colors='lightblue', linestyles='--', linewidths=3)
            if _storm_named:
                add_center_label(ax, centerlon, centerlat, minpressure)
                if stream:
                    Axes.streamplot(ax, xi, yi, MLu, MLv, color='gray', density=0.5)
                _set_titles_storm(ax, left_title_d03)
            else:
                ax.set_title(left_title_global, fontsize=small_fontsize, weight='bold', loc='left')

        def _save(fig, tag):
            plot_utils.save_figure(fig, _figfname(tag), do_trim=False,
                                   do_gif=DO_CONVERTGIF, dpi='figure')

        # ------------------------------------------------------------------ #
        # FIGURE: Depth of the 26 oC isotherm
        if do_iso_26:
            fig1 = plt.figure(figsize=figsize)
            ax1  = fig1.add_subplot(1, 1, 1)
            co1  = ax1.contourf(lon, lat, i26, levels=iso_26_levs, extend='both')
            debug_dump_range(FHR, 'i26', i26)
            cbar1 = plt.colorbar(co1, ticks=iso_26_ticks)
            cbar1.ax.tick_params(labelsize=fontsize)
            _apply_domain(ax1,
                r"Depth of 26 $^oC$ Isotherm (m, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR),
                r"Depth of 26 $^oC$ Isotherm (m, Shading)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR))
            _save(fig1, 'iso_26')

        # FIGURE: Depth of the 20 oC isotherm
        if do_iso_20:
            fig1 = plt.figure(figsize=figsize)
            ax1  = fig1.add_subplot(1, 1, 1)
            co1  = ax1.contourf(lon, lat, i20, levels=iso_20_levs, extend='both')
            debug_dump_range(FHR, 'i20', i20)
            cbar1 = plt.colorbar(co1, ticks=iso_20_ticks)
            cbar1.ax.tick_params(labelsize=fontsize)
            _apply_domain(ax1,
                r"Depth of 20 $^oC$ Isotherm (m, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR),
                r"Depth of 20 $^oC$ Isotherm (m, Shading)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR))
            _save(fig1, 'iso_20')

        # FIGURE: Ocean heat content
        if do_ohc:
            fig1 = plt.figure(figsize=figsize)
            ax1  = fig1.add_subplot(1, 1, 1)
            co1  = ax1.contourf(lon, lat, OHC, levels=OHC_levs, cmap='Reds', extend='both')
            debug_dump_range(FHR, 'OHC', OHC)
            cbar1 = plt.colorbar(co1, ticks=OHC_ticks)
            cbar1.ax.tick_params(labelsize=fontsize)
            _apply_domain(ax1,
                r"Ocean Heat Content ($kJ\ cm^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR),
                r"Ocean Heat Content ($kJ\ cm^{-2}$, Shading)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR))
            _save(fig1, 'ohc')

        # FIGURE: Ocean heat content TENDENCY
        if do_ohc_tendency and dOHC is not None:
            fig1 = plt.figure(figsize=figsize)
            ax1  = fig1.add_subplot(1, 1, 1)
            co1  = ax1.contourf(lon, lat, dOHC, levels=dOHC_levs, cmap='Reds', extend='both')
            debug_dump_range(FHR, 'dOHC', dOHC)
            cbar1 = plt.colorbar(co1, ticks=dOHC_ticks)
            cbar1.ax.tick_params(labelsize=fontsize)
            _apply_domain(ax1,
                r"Oc. Ht. Content Tendency ($kJ\ cm^{-2} d^{-1}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR),
                r"Oc. Ht. Content Tendency ($kJ\ cm^{-2} d^{-1}$, Shading)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR))
            _save(fig1, 'dohc')

        # FIGURE: Convergence in surface currents
        if do_sfc_conv:
            fig1 = plt.figure(figsize=figsize)
            ax1  = fig1.add_subplot(1, 1, 1)
            co1  = ax1.contourf(lon, lat, sfc_conv, levels=sfc_conv_levs, cmap='seismic', extend='both')
            debug_dump_range(FHR, 'sfc_conv', sfc_conv)
            cbar1 = plt.colorbar(co1, ticks=sfc_conv_ticks)
            cbar1.ax.tick_params(labelsize=fontsize)
            _apply_domain(ax1,
                r"Surface Convergence ($d^{-1}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR),
                r"Surface Convergence ($d^{-1}$, Shading)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR))
            _save(fig1, 'sfc_conv')

        # FIGURE: Curl in surface currents
        if do_sfc_vort:
            fig1 = plt.figure(figsize=figsize)
            ax1  = fig1.add_subplot(1, 1, 1)
            co1  = ax1.contourf(lon, lat, sfc_vort, levels=sfc_vort_levs, cmap='seismic', extend='both')
            debug_dump_range(FHR, 'sfc_vort', sfc_vort)
            cbar1 = plt.colorbar(co1, ticks=sfc_vort_ticks)
            cbar1.ax.tick_params(labelsize=fontsize)
            _apply_domain(ax1,
                r"Surface Vorticity ($d^{-1}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR),
                r"Surface Vorticity ($d^{-1}$, Shading)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR))
            _save(fig1, 'sfc_vort')

        # FIGURE: Convergence in mixed-layer currents
        if do_ml_conv:
            fig1 = plt.figure(figsize=figsize)
            ax1  = fig1.add_subplot(1, 1, 1)
            co1  = ax1.contourf(lon, lat, ml_conv, levels=ml_conv_levs, cmap='seismic', extend='both')
            debug_dump_range(FHR, 'ml_conv', ml_conv)
            cbar1 = plt.colorbar(co1, ticks=ml_conv_ticks)
            cbar1.ax.tick_params(labelsize=fontsize)
            _apply_domain(ax1,
                r"Mixed-layer Convergence ($d^{-1}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR),
                r"Mixed-layer Convergence ($d^{-1}$, Shading)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR))
            _save(fig1, 'ml_conv')

        # FIGURE: Curl in mixed-layer currents
        if do_ml_vort:
            fig1 = plt.figure(figsize=figsize)
            ax1  = fig1.add_subplot(1, 1, 1)
            co1  = ax1.contourf(lon, lat, ml_vort, levels=ml_vort_levs, cmap='seismic', extend='both')
            debug_dump_range(FHR, 'ml_vort', ml_vort)
            cbar1 = plt.colorbar(co1, ticks=ml_vort_ticks)
            cbar1.ax.tick_params(labelsize=fontsize)
            _apply_domain(ax1,
                r"Mixed-layer Vorticity ($d^{-1}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR),
                r"Mixed-layer Vorticity ($d^{-1}$, Shading)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR))
            _save(fig1, 'ml_vort')

        # FIGURE: SST tendency
        if do_sst_tendency and dSST is not None:
            fig1 = plt.figure(figsize=figsize)
            ax1  = fig1.add_subplot(1, 1, 1)
            co1  = ax1.contourf(lon, lat, dSST, levels=dSST_levs, cmap='seismic', extend='both')
            debug_dump_range(FHR, 'dSST', dSST)
            cbar1 = plt.colorbar(co1, ticks=dSST_ticks)
            cbar1.ax.tick_params(labelsize=fontsize)
            _apply_domain(ax1,
                r"Sea-Sfc. Temp. Tendency ($K/d$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR),
                r"Sea-Sfc. Temp. Tendency ($K/d$, Shading)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR))
            _save(fig1, 'dsst')

        # FIGURE: Sea-surface height
        if do_ssh and SSH is not None:
            fig1 = plt.figure(figsize=figsize)
            ax1  = fig1.add_subplot(1, 1, 1)
            co1  = ax1.contourf(lon, lat, SSH, levels=SSH_levs, cmap='seismic', extend='both')
            debug_dump_range(FHR, 'SSH', SSH)
            cbar1 = plt.colorbar(co1, ticks=SSH_ticks)
            cbar1.ax.tick_params(labelsize=fontsize)
            _apply_domain(ax1,
                r"Sea-Surface Height ($cm$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR),
                r"Sea-Surface Height ($cm$, Shading)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR))
            _save(fig1, 'ssh')

        # FIGURE: SSH tendency
        if do_ssh_tendency and dSSH is not None:
            fig1 = plt.figure(figsize=figsize)
            ax1  = fig1.add_subplot(1, 1, 1)
            co1  = ax1.contourf(lon, lat, dSSH, levels=dSSH_levs, cmap='seismic', extend='both')
            debug_dump_range(FHR, 'dSSH', dSSH)
            cbar1 = plt.colorbar(co1, ticks=dSSH_ticks)
            cbar1.ax.tick_params(labelsize=fontsize)
            _apply_domain(ax1,
                r"Sea-Sfc. Ht. Tendency ($cm/d$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR),
                r"Sea-Sfc. Ht. Tendency ($cm/d$, Shading)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR))
            _save(fig1, 'dssh')

        # FIGURE: Surface heat flux
        if do_shf and SHF is not None:
            fig1 = plt.figure(figsize=figsize)
            ax1  = fig1.add_subplot(1, 1, 1)
            co1  = ax1.contourf(lon, lat, SHF, levels=SHF_levs, cmap='seismic', extend='both')
            debug_dump_range(FHR, 'SHF', SHF)
            cbar1 = plt.colorbar(co1, ticks=SHF_ticks)
            cbar1.ax.tick_params(labelsize=fontsize)
            _apply_domain(ax1,
                r"Surface Heat Flux ($W m^{-2}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR),
                r"Surface Heat Flux ($W m^{-2}$, Shading)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR))
            _save(fig1, 'shf')

        # FIGURE: Dynamic Potential Intensity
        if do_dpi:
            fig1 = plt.figure(figsize=figsize)
            ax1  = fig1.add_subplot(1, 1, 1)
            co1  = ax1.contourf(lon, lat, DPI, levels=DPI_levs, extend='both')
            debug_dump_range(FHR, 'DPI', DPI)
            cbar1 = plt.colorbar(co1, ticks=DPI_ticks)
            cbar1.ax.tick_params(labelsize=fontsize)
            _apply_domain(ax1,
                r"Dynamic Potential Intensity ($m\ s^{-1}$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR),
                r"Dynamic Potential Intensity ($m\ s^{-1}$, Shading)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR))
            _save(fig1, 'dpi')

        # FIGURE: Mixed-layer depth
        if do_mld and MLD is not None:
            fig1 = plt.figure(figsize=figsize)
            ax1  = fig1.add_subplot(1, 1, 1)
            co1  = ax1.contourf(lon, lat, MLD, levels=MLD_levs, extend='both')
            debug_dump_range(FHR, 'MLD', MLD)
            cbar1 = plt.colorbar(co1, ticks=MLD_ticks)
            cbar1.ax.tick_params(labelsize=fontsize)
            _apply_domain(ax1,
                r"Mixed-Layer Depth ($m$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR),
                r"Mixed-Layer Depth ($m$, Shading)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR))
            _save(fig1, 'mld')

        # FIGURE: Sea-surface salinity
        if do_sss and SSS is not None:
            fig1 = plt.figure(figsize=figsize)
            ax1  = fig1.add_subplot(1, 1, 1)
            co1  = ax1.contourf(lon, lat, SSS, levels=SSS_levs, cmap='Blues', extend='both')
            debug_dump_range(FHR, 'SSS', SSS)
            cbar1 = plt.colorbar(co1, ticks=SSS_ticks)
            cbar1.ax.tick_params(labelsize=fontsize)
            _apply_domain(ax1,
                r"Sea-Surface Salinity ($psu$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR),
                r"Sea-Surface Salinity ($psu$, Shading)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR))
            _save(fig1, 'sss')

        # FIGURE: Mixed-layer temperature
        if do_mlt and MLT is not None:
            fig1 = plt.figure(figsize=figsize)
            ax1  = fig1.add_subplot(1, 1, 1)
            co1  = ax1.contourf(lon, lat, MLT, levels=MLT_levs, cmap='Reds', extend='both')
            debug_dump_range(FHR, 'MLT', MLT)
            cbar1 = plt.colorbar(co1, ticks=MLT_ticks)
            cbar1.ax.tick_params(labelsize=fontsize)
            _apply_domain(ax1,
                r"Mixed-Layer Temperature ($^oC$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR),
                r"Mixed-Layer Temperature ($^oC$, Shading)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR))
            _save(fig1, 'mlt')

        # FIGURE: Mixed-layer salinity
        if do_mls and MLS is not None:
            fig1 = plt.figure(figsize=figsize)
            ax1  = fig1.add_subplot(1, 1, 1)
            co1  = ax1.contourf(lon, lat, MLS, levels=MLS_levs, cmap='Blues', extend='both')
            debug_dump_range(FHR, 'MLS', MLS)
            cbar1 = plt.colorbar(co1, ticks=MLS_ticks)
            cbar1.ax.tick_params(labelsize=fontsize)
            _apply_domain(ax1,
                r"Mixed-Layer Salinity ($psu$, Shading), U$_{MLD}$ ($cm\ s^{-1}$, Strmlns.)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR),
                r"Mixed-Layer Salinity ($psu$, Shading)" + '\nInit: ' + forecastinit + ' Forecast Hour:[{:03d}]'.format(FHR))
            _save(fig1, 'mls')

        if do_obs_profiles and OCEAN_DOMAIN == 'd03':
            if 'AXBTs' not in locals():
                try:
                    AXBTs = load_axbt_data(IDATE[0:8])
                except Exception:
                    print(f'WARNING: Unable to load AXBTs for {IDATE[0:8]}. Skipping do_obs_profiles.')
            if 'AXBTs' in locals():
                if FHR == 0:
                    mint = modelt - np.timedelta64(2, 'D')
                    maxt = modelt + np.timedelta64(DFHR, 'h')
                else:
                    mint = modelt - np.timedelta64(DFHR, 'h')
                    maxt = modelt + np.timedelta64(DFHR, 'h')
                samples = AXBTs.where(
                    (lonmin <= AXBTs.lon) & (AXBTs.lon <= lonmax) &
                    (latmin <= AXBTs.lat) & (AXBTs.lat <= latmax) &
                    (mint <= AXBTs.t)     & (AXBTs.t   <= maxt))
                if np.any(~np.isnan(samples.lon.values)):
                    pass

        plot_utils.update_plotted_file(PLOTTED_FILE, FILE)
        print(f'MSG: Done with Plots {datetime.now()}')

    print('MSG: COMPLETING')
    os.system('lockfile -r-1 -l 180 ' + ST_LOCK_FILE)
    os.system('echo "complete" > ' + STATUS_FILE)
    os.system('rm -f ' + ST_LOCK_FILE)


##############################
if __name__ == '__main__':
    main()
