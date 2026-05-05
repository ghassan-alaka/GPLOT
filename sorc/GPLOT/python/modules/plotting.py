import itertools
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.colors as colors
from matplotlib.ticker import ScalarFormatter
from matplotlib.patches import Rectangle, Wedge
from matplotlib.collections import PatchCollection
from metpy.plots import add_metpy_logo, SkewT
import numpy as np
import os

# gplot_utils.colormaps gives us the same maps reflectivity palette
# (REFD.rgb on 5-75 dBZ, white set_under) used by GPLOT_maps.py, so
# the inset reflectivity context on Skew-T figures matches the
# operational d01/d03 maps look without re-defining the levels here.
try:
    from gplot_utils import colormaps as _gplot_cmaps
except ImportError:
    # Fallback for unusual import paths (e.g. running plotting.py
    # standalone). The inset will still render, just with a default
    # matplotlib palette.
    _gplot_cmaps = None

##############################
def axes_wavenumber(ax, xmax, xmin, nx=7):
	"""Set up common axes attributes for wavenumber graphics.
	@param ax:   the axes object
	@param xmax: max value of both x/y axes
	@param xmin: min value of both x/y axes
	@kwarg nx:   number of ticks along each axis (default 7)
	"""
	ticks = np.linspace(xmin,xmax,nx)

	ax.set_xlim(xmin,xmax)
	ax.set_xticks(ticks)
	ax.set_xticklabels([str(int(x)) for x in ticks], fontsize=18)
	ax.set_xlabel('X (km)', fontsize=20)

	ax.set_ylim(xmin,xmax)
	ax.set_yticks(ticks)
	ax.set_yticklabels([str(int(x)) for x in ticks], fontsize=18)
	ax.set_ylabel('Y (km)', fontsize=20)

	ax.set_aspect('equal', adjustable='box')
	ax.grid()

	return ax


##############################
def axes_radpres(ax, xmax, xmin, ymax=1000, ymin=100):
	"""Set up common axes attributes for radius-pressure graphics.
	@param ax:   the axes object
	@param xmax: max value of both x/y axes
	@param xmin: min value of both x/y axes
	@kwarg ymax: max value of y-axis
	@kwarg ymin: min value of y-axis
	"""
	xticks = np.linspace(xmin,xmax,11)
	yticks = np.linspace(ymax,ymin,10)

	ax.set_xlim(xmin, xmax)
	ax.set_xticks(xticks)
	ax.set_xticklabels([str(int(x)) for x in xticks], fontsize=24)
	ax.set_xlabel('Radius (km)', fontsize=24)

	ax.set_yscale('log')
	ax.set_ylim(ymin,ymax)
	ax.invert_yaxis()
	ax.set_yticks(yticks)
	ax.set_yticklabels([str(int(x)) for x in yticks], fontsize=24)
	ax.set_ylabel('Pressure Level (hPa)', fontsize=24)

	ax.grid()

	return ax


##############################
def axes_radhgt(ax, xmax=200, xmin=0, nx=11, xunit='km', ymax=18, ymin=0, ny=10, yunit='km', formatters=False):
	"""Set up common axes attributes for wavenumber graphics.
	@param ax:   the axes object
	@param xmax: max value of x-axis
	@param xmin: min value of x-axis
	@kwarg ymax: max value of y-axis
	@kwarg ymin: min value of y-axis
	"""
	xticks = np.linspace(xmin,xmax,nx)
	yticks = np.linspace(ymin,ymax,ny)

	ax.set_xlim(xmin, xmax)
	ax.set_xticks(xticks)
	ax.set_xticklabels([str(int(x)) for x in xticks], fontsize=24)
	ax.set_xlabel(f'Radius ({xunit})', fontsize=24)

	ax.set_ylim(ymin,ymax)
	ax.set_yticks(yticks)
	ax.set_yticklabels([str(int(x)) for x in yticks], fontsize=24)
	ax.set_ylabel(f'Height ({yunit})', fontsize=24)

	ax.grid()

	if formatters:
		ax.yaxis.set_major_formatter(ScalarFormatter())
		ax.yaxis.set_minor_formatter(plt.NullFormatter())

	return ax


##############################
#Define a plotting function first
def colored_bar(left, height, z=None, width=0.8, bottom=0.2, ax=None, **kwargs):
	"""Create a colorbar
	"""
	if ax is None:  ax = plt.gca()
	width = itertools.cycle(np.atleast_1d(width))
	bottom = itertools.cycle(np.atleast_1d(bottom))
	rects = []
	for x, y, h, w in zip(left, bottom, height, width):
		rects.append(Rectangle((x,y), w, h))
	coll = PatchCollection(rects, array=z, **kwargs)
	#ax.set_clim(coll, vmin=30, vmax=90)
	ax.add_collection(coll)
	ax.autoscale()
	return coll


##############################
# Inset reflectivity-context map for the Skew-T figures.
##############################

# Quadrant -> (theta1, theta2) in matplotlib's CCW-from-east
# convention (matches index_theta_E/N/W/S = 0/90/180/270 deg used by
# skewTmodelTCpolar). Used for Wedge highlights on the inset.
_QUAD_DEG = {
    'NE': (0,    90),
    'NW': (90,  180),
    'SW': (180, 270),
    'SE': (270, 360),
}


def _draw_reflectivity_inset(fig, skew_ax, GPLOT_DIR, inset, region):
	"""Render a small reflectivity inset INSIDE the SkewT axes' lower-left.

	The inset is created via ``skew_ax.inset_axes`` so the bounds are
	expressed as axes-fractions of the SkewT plot box. That keeps the
	inset visually inside the Skew-T no matter how the surrounding
	figure margins / colorbar position lay out.

	Parameters
	----------
	fig : matplotlib.figure.Figure
	    The Skew-T figure (kept for API symmetry; not used directly
	    now that the inset rides on ``skew_ax``).
	skew_ax : matplotlib.axes.Axes
	    The SkewT plot axes (i.e. ``skew.ax``) — the inset is anchored
	    to its bounding box.
	GPLOT_DIR : str
	    GPLOT root, used to resolve the maps reflectivity palette.
	inset : dict or None
	    Payload built by ``skewTmodelTCpolar._build_inset_payload``.
	    No-op when ``inset`` is None.
	region : dict or None
	    Highlighted source region for the sounding.
	"""
	if inset is None:
		return

	dbz_polar  = inset['dbz_polar']
	r_km       = np.asarray(inset['r_km'])
	theta_rad  = np.asarray(inset['theta_rad'])
	rmax_km    = float(inset['rmax_km'])
	# Inset is labeled in nautical miles to match the sounding band
	# names ("50-100 nmi" etc.). Convert km -> nmi (1 nmi = 1.852 km)
	# for the data, axes range, ring radii, and wedge geometry.
	KM_PER_NMI = 1.852
	r_nmi      = r_km / KM_PER_NMI
	rmax_nmi   = rmax_km / KM_PER_NMI

	# Polar -> Cartesian (nmi from storm center).
	R, TH = np.meshgrid(r_nmi, theta_rad)
	X = R * np.cos(TH)
	Y = R * np.sin(TH)

	# Reflectivity palette: prefer the maps registry (5-75 dBZ on
	# REFD.rgb with white set_under). Fall back to viridis on any
	# import / loader hiccup so the rest of the figure still renders.
	if _gplot_cmaps is not None:
		try:
			cmap = _gplot_cmaps.get_colormap('REFL', '', GPLOT_DIR)
			levels = _gplot_cmaps.get_contour_levels('REFL', '')
			dcmap = _gplot_cmaps.build_discrete_cmap(
				cmap, len(levels) - 1, extend='both')
			norm = _gplot_cmaps.get_norm(levels)
		except Exception:
			dcmap, norm = plt.cm.viridis, None
	else:
		dcmap, norm = plt.cm.viridis, None

	# Anchor the inset to the SkewT axes -- bounds are axes-fraction
	# (0..1) of the SkewT plot box. ~30% wide, ~25% tall, lower-left.
	inset_ax = skew_ax.inset_axes([0.02, 0.02, 0.30, 0.25])
	inset_ax.pcolormesh(X, Y, dbz_polar, cmap=dcmap, norm=norm,
	                    shading='auto')
	inset_ax.set_xlim(-rmax_nmi, rmax_nmi)
	inset_ax.set_ylim(-rmax_nmi, rmax_nmi)
	inset_ax.set_aspect('equal')
	inset_ax.tick_params(labelsize=7)
	inset_ax.set_xticks([-200, -100, 0, 100, 200])
	inset_ax.set_yticks([-200, -100, 0, 100, 200])
	inset_ax.grid(alpha=0.3, linestyle=':', linewidth=0.5)

	# Subtle range rings at the sounding-band boundaries.
	for nmi in (50, 100, 150, 200):
		ring = plt.Circle((0, 0), nmi, fill=False,
		                   edgecolor='gray', linewidth=0.4,
		                   linestyle=':', zorder=2)
		inset_ax.add_patch(ring)

	# Storm-center crosshair.
	inset_ax.plot(0, 0, '+', color='black', markersize=7,
	              markeredgewidth=1.2, zorder=4)

	inset_ax.set_title('2-km Reflectivity (nmi)', fontsize=9,
	                    weight='bold')

	# Region highlight.
	if region is None:
		return

	kind = region.get('kind')
	if kind == 'center':
		inset_ax.plot(0, 0, marker='*', color='red', markersize=12,
		              markeredgecolor='black', markeredgewidth=0.9,
		              linestyle='', zorder=5)
		return

	if kind not in ('rmw_quadrant', 'annulus_quadrant'):
		return  # silently skip unknown region kinds

	theta1, theta2 = _QUAD_DEG[region['quadrant']]
	# Region geometry comes from the caller in km; convert to nmi
	# to match the inset's axis units.
	if kind == 'rmw_quadrant':
		half_km = float(region.get('swath_km', 2.0)) / 2.0
		r_outer = (float(region['rmw_km']) + half_km) / KM_PER_NMI
		width   = (2.0 * half_km) / KM_PER_NMI
	else:
		r_outer_km = float(region['r_outer_km'])
		r_inner_km = float(region['r_inner_km'])
		r_outer = r_outer_km / KM_PER_NMI
		width   = (r_outer_km - r_inner_km) / KM_PER_NMI

	wedge = Wedge((0, 0), r_outer, theta1, theta2, width=width,
	              facecolor='red', alpha=0.35,
	              edgecolor='red', linewidth=1.5, zorder=6)
	inset_ax.add_patch(wedge)


def _add_metpy_logo_inside_skewt(fig, skew_ax):
	"""Place the MetPy logo at the top-left INSIDE the SkewT axes.

	Earlier versions used ``add_metpy_logo`` with figure-pixel coords
	derived from ``skew_ax.get_position()``; that placement still
	leaks outside the SkewT bbox because ``bbox_inches='tight'`` at
	save time crops to encompass the logo + title block as one unit.
	Anchoring the logo on a dedicated inset_axes inside the SkewT --
	same trick as the reflectivity map -- guarantees the logo lives
	inside the plot box regardless of save-time crop / colorbar
	layout.
	"""
	import os as _os
	import matplotlib.image as _mpimg
	import metpy.plots as _mpp

	logo_path = _os.path.join(_mpp.__path__[0], '_static',
	                           'metpy_75x75.png')
	if not _os.path.isfile(logo_path):
		return  # silently skip if the bundled logo isn't where we expect

	logo_img = _mpimg.imread(logo_path)

	# 6% × 6% of the SkewT box, anchored top-left with a small inset
	# margin. The 75x75 source image has a 1:1 aspect ratio, so a
	# square inset preserves it.
	logo_ax = skew_ax.inset_axes([0.015, 0.92, 0.07, 0.07])
	logo_ax.imshow(logo_img, interpolation='bilinear')
	logo_ax.set_axis_off()


##############################
def skewplot(sounding_p, sounding_t, sounding_td, sounding_u, sounding_v, location, GPLOT_DIR, \
             EXPT, FHR, maxwind, minpressure, LONGSID, ODIR, forecastinit, DO_CONVERTGIF, \
             *, inset=None, region=None):
	""" Skew-T plotting function
	"""

	color_data_wind = np.genfromtxt(GPLOT_DIR+'/sorc/GPLOT/python/colormaps/colormap_wind.txt')
	colormap_wind = matplotlib.colors.ListedColormap(color_data_wind)
	levs_wind = np.linspace(0,160,41,endpoint=True)
	norm_wind = colors.BoundaryNorm(levs_wind,256)

	figext = '.png'
	fig = plt.figure(figsize=(16, 12))
	skew = SkewT(fig, rotation=45, aspect=80.5)
	skew.plot(sounding_p, sounding_t, 'r', linewidth=3)
	skew.plot(sounding_p, sounding_td, 'g', linewidth=3)
	img = skew.plot_barbs(sounding_p, sounding_u, sounding_v, c=np.hypot(sounding_u,sounding_v),cmap=colormap_wind,norm=norm_wind)
	cbar = plt.colorbar(img, cmap=colormap_wind, norm=norm_wind, boundaries=levs_wind,ticks=np.linspace(0,160,9),pad = 0.1)
	cbar.ax.tick_params(labelsize=24)
	skew.ax.set_ylim(1000, 100)
	skew.ax.set_xlim(-40, 50)
	skew.ax.axvline(0, color='c', linestyle='--', linewidth=2)
	skew.plot_dry_adiabats()
	skew.plot_moist_adiabats()
	skew.plot_mixing_lines()
	skew.ax.set_xlabel('degC',weight='bold',fontsize=24)
	skew.ax.set_ylabel('Pressure (hPa)',weight='bold',fontsize=24)
	skew.ax.set_title(f'{EXPT.strip()}\nSounding {location}\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
		     fontsize=24, weight='bold', loc='left')
	skew.ax.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', \
		     fontsize=24, color='brown', loc='right')

	# MetPy logo + reflectivity inset are anchored to the SkewT axes
	# bbox so they always sit inside the plot box regardless of the
	# surrounding figure margins / colorbar position.
	_add_metpy_logo_inside_skewt(fig, skew.ax)
	_draw_reflectivity_inset(fig, skew.ax, GPLOT_DIR, inset, region)

	figfname = f'{ODIR}/{LONGSID.lower()}.sounding_{location.replace(" ","_")}.{forecastinit}.polar.f{FHR:03}'
	fig.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
	plt.close(fig)
	if DO_CONVERTGIF:
		os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')


##############################
def skewplot_blank(location, GPLOT_DIR, EXPT, FHR, maxwind, minpressure, LONGSID, ODIR, forecastinit, DO_CONVERTGIF, \
                   *, inset=None, region=None):
	""" Blank skew-T plotting function
	"""
	figext = '.png'
	fig = plt.figure(figsize=(16, 12))
	skew = SkewT(fig, rotation=45, aspect=80.5)
	skew.ax.set_ylim(1000, 100)
	skew.ax.set_xlim(-40, 50)
	skew.ax.axvline(0, color='c', linestyle='--', linewidth=2)
	skew.plot_dry_adiabats()
	skew.plot_moist_adiabats()
	skew.plot_mixing_lines()
	skew.ax.set_xlabel('degC',weight='bold',fontsize=24)
	skew.ax.set_ylabel('Pressure (hPa)',weight='bold',fontsize=24)
	# Axes-coord placement so the message stays inside the box even
	# under the skew transform (legacy data-coords (-30, 200) ended up
	# outside the visible plot area on some axis ranges).
	skew.ax.text(0.5, 0.55, 'No Data\nRMW > 50 nmi',
	             transform=skew.ax.transAxes,
	             ha='center', va='center',
	             fontsize=28, weight='bold')
	skew.ax.set_title(f'{EXPT.strip()}\nSounding {location}\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
			  fontsize=24, weight='bold', loc='left')
	skew.ax.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', \
			  fontsize=24, color='brown', loc='right')

	_add_metpy_logo_inside_skewt(fig, skew.ax)
	_draw_reflectivity_inset(fig, skew.ax, GPLOT_DIR, inset, region)

	figfname = f'{ODIR}/{LONGSID.lower()}.sounding_{location.replace(" ","_")}.{forecastinit}.polar.f{FHR:03}'
	fig.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
	plt.close(fig)
	if DO_CONVERTGIF:
	        os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')

