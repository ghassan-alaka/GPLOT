import itertools
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.colors as colors
from matplotlib.ticker import ScalarFormatter
from matplotlib.patches import Rectangle
from matplotlib.collections import PatchCollection
from metpy.plots import add_metpy_logo, SkewT
import numpy as np
import os

##############################
def axes_wavenumber(ax, xmax, xmin):
	"""Set up common axes attributes for wavenumber graphics.
	@param ax:   the axes object
	@param xmax: max value of both x/y axes
	@param xmin: min value of both x/y axes
	"""
	ticks = np.linspace(xmin,xmax,7)

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
def skewplot(sounding_p, sounding_t, sounding_td, sounding_u, sounding_v, location, GPLOT_DIR, \
             EXPT, FHR, maxwind, minpressure, LONGSID, ODIR, forecastinit, DO_CONVERTGIF):
	""" Skew-T plotting function
	"""

	color_data_wind = np.genfromtxt(GPLOT_DIR+'/sorc/GPLOT/python/colormaps/colormap_wind.txt')
	colormap_wind = matplotlib.colors.ListedColormap(color_data_wind)
	levs_wind = np.linspace(0,160,41,endpoint=True)
	norm_wind = colors.BoundaryNorm(levs_wind,256)

	figext = '.png'
	fig = plt.figure(figsize=(16, 12))
	#ax = fig.add_subplot(1, 1, 1)
	add_metpy_logo(fig, 150, 100)
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
	figfname = f'{ODIR}/{LONGSID.lower()}.sounding_{location.replace(" ","_")}.{forecastinit}.polar.f{FHR:03}'
	fig.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
	plt.close(fig)
	if DO_CONVERTGIF:
		os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')


##############################
def skewplot_blank(location, GPLOT_DIR, EXPT, FHR, maxwind, minpressure, LONGSID, ODIR, forecastinit, DO_CONVERTGIF):
	""" Blank skew-T plotting function
	"""
	figext = '.png'
	fig = plt.figure(figsize=(16, 12))
	ax = fig.add_subplot(1, 1, 1)
	add_metpy_logo(fig, 150, 100)
	skew = SkewT(ax, rotation=45, aspect=80.5)
	skew.ax.set_ylim(1000, 100)
	skew.ax.set_xlim(-40, 50)
	skew.ax.axvline(0, color='c', linestyle='--', linewidth=2)
	skew.plot_dry_adiabats()
	skew.plot_moist_adiabats()
	skew.plot_mixing_lines()
	ax.set_xlabel('degC',weight='bold',fontsize=24)
	ax.set_ylabel('Pressure (hPa)',weight='bold',fontsize=24)
	ax.text(-30,200,'No Data' + '\n' + 'RMW > 50 nmi',fontsize=28,weight='bold')
	ax.set_title(f'{EXPT.strip()}\nSounding {location}\nInit: {forecastinit} Forecast Hour:[{FHR:03}]', \
		     fontsize=24, weight='bold', loc='left')
	ax.set_title(f'VMAX= {maxwind} kt\nPMIN= {minpressure} hPa\n{LONGSID.upper()}', \
		     fontsize=24, color='brown', loc='right')
	figfname = f'{ODIR}/{LONGSID.lower()}.sounding_{location.replace(" ","_")}.{forecastinit}.polar.f{FHR:03}'
	fig.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
	plt.close(fig)
	if DO_CONVERTGIF:
	        os.system(f'convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}')

