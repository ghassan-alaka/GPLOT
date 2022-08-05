#!/work/noaa/aoml-hafs1/galaka/anaconda3/envs/GPLOT/bin/python3
#This script reads in storm-centered model data on a polar grid and calculates area averaged shear and RH for plotting
#
# Needed input arguments:
# Xpolar: Polar coordinates interpolated to X (km, 2-D)
# Ypolar: Polar coordinates interpolated to Y (km, 2-D)
# theta: Azimuth (radians, 1-D)
# r: Radius (km, 1-D)
#
#
#
#
#------------------------------------------------------------------------------------------------------------------------

#print('Importing Everything Needed')
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import itertools
from matplotlib.patches import Rectangle
from matplotlib.collections import PatchCollection
import os


##############################
##############################
def shearandrhplot(Xpolar, Ypolar, theta, r, ushear_polar, vshear_polar, rh_mean_polar, rh_level_lower, rh_level_upper, \
		   rmw_lower, rmw_upper, GPLOT_DIR, EXPT, FHR, maxwind, minpressure, LONGSID, ODIR, forecastinit, DO_CONVERTGIF):
	"""
	"""
	
	ushear_domain_mean = np.nanmean(ushear_polar,1)
	vshear_domain_mean = np.nanmean(vshear_polar,1)
	rh_mean_domain_mean = np.nanmean(rh_mean_polar,1)

	#Make Plot
	figext = '.png' 
	xmin = np.nanmin(Xpolar)
	xmax = np.nanmax(Xpolar)
	ymin = np.nanmin(Ypolar)
	ymax = np.nanmax(Ypolar)
	skiptheta = np.int(15/np.mean(np.gradient(theta*180/np.pi)))
	skipr = np.int(50/np.mean(np.gradient(r)))

	x = theta
	y = np.hypot(ushear_domain_mean,vshear_domain_mean)*1.94
	z = rh_mean_domain_mean

	color_data_rh_2 = np.genfromtxt(GPLOT_DIR+'/sorc/GPLOT/python/colormaps/colormap_brown_to_green_2.txt')
	colormap_rh_2 = matplotlib.colors.ListedColormap(color_data_rh_2)

	fig = plt.figure(figsize=(16,8),constrained_layout=True)
	fig.set_constrained_layout_pads(wspace=0.2)
	ax = fig.add_subplot(121, projection='polar')
	cmap = colormap_rh_2
	coll = colored_bar(x, y, z, ax=ax, width=np.gradient(theta)[0], cmap=cmap)
	coll.set_clim(vmin=30, vmax=90)
	cbar = fig.colorbar(coll,ticks=[30,40,50,60,70,80,90],location='bottom')
	cbar.set_ticklabels([30,40,50,60,70,80,90])
	cbar.ax.tick_params(labelsize=20)
	ax.plot(theta,np.ones(np.shape(theta))*10,color='xkcd:green',linewidth=3,linestyle='--')
	ax.plot(theta,np.ones(np.shape(theta))*20,color='xkcd:yellow',linewidth=3,linestyle='--')
	ax.plot(theta,np.ones(np.shape(theta))*30,color='xkcd:red',linewidth=3,linestyle='--')
	ax.plot(theta,np.ones(np.shape(theta))*40,color='xkcd:crimson',linewidth=3,linestyle='--')
	ax.set_yticks([10., 20., 30., 40.])
	ax.set_ylim([0,40])
	ax.set_yticklabels([10,20,30,40],fontsize=20)
	ax.set_thetamin(0)
	ax.set_thetamax(360)
	ax.tick_params(axis="x",direction="in", pad=-15)
	ax.set_xticklabels(['E', 'NE', 'N', 'NW', 'W', 'SW', 'S', 'SE'],fontsize=20)
	plt.title(EXPT.strip()+'\n'+'850-200 hPa Shear, '+rh_level_lower+'-'+rh_level_upper+' RH'+'\n'+'Init: '+forecastinit+'\n'+'Forecast Hour:['+format(FHR,'03d')+']',fontsize=20, weight = 'bold',loc='left')
	ax.text(.5,.9,'Domain-Mean',horizontalalignment='center',transform=ax.transAxes,fontsize=20, weight = 'bold')

	ax2 = fig.add_subplot(122)
	skipr = 16
	skiptheta = 3
	Q = ax2.quiver(Xpolar[::skiptheta,::skipr],Ypolar[::skiptheta,::skipr],1.94*ushear_polar[::skiptheta,::skipr],1.94*vshear_polar[::skiptheta,::skipr],rh_mean_polar[::skiptheta,::skipr],cmap=colormap_rh_2,width=0.025,headwidth=2,units='inches',scale=50)
	Q.set_clim(vmin=30, vmax=90)
	qk = plt.quiverkey(Q, 0.9, -0.1, 20, '20 kt', labelpos='W')
	cbar = fig.colorbar(Q,ticks=[30,40,50,60,70,80,90],location='bottom')
	cbar.set_ticklabels([30,40,50,60,70,80,90])
	cbar.ax.tick_params(labelsize=20)
	c1=plt.Circle((0,0),rmw_lower,color='xkcd:black',linewidth=3,linestyle='--',fill=False)
	c2=plt.Circle((0,0),rmw_upper,color='xkcd:gray',linewidth=3,linestyle='--',fill=False)
	ax2.add_patch(c1)
	ax2.add_patch(c2)
	ax2.set_xlim([xmin,xmax])
	ax2.set_ylim([ymin,ymax])
	ax2.set_xticks(np.arange(xmin,xmax+1,200))
	ax2.set_xticklabels(np.arange(xmin,xmax+1,200),fontsize=20)
	ax2.set_yticks(np.arange(ymin,ymax+1,200))
	ax2.set_yticklabels(np.arange(ymin,ymax+1,200),fontsize=20)
	ax2.set_xlabel('X (km)',fontsize=20)
	ax2.set_ylabel('Y (km)',fontsize=20)
	ax2.set_aspect('equal', adjustable='box')
	ax2.grid()
	plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
	ax2.text(.5,.9,'Full-Domain',horizontalalignment='center',transform=ax2.transAxes,fontsize=20, weight = 'bold')

	figfname = ODIR+'/'+LONGSID.lower()+'.shear_and_rh_combo_plot.'+forecastinit+'.polar.f'+format(FHR,'03d')
	print(figfname+figext)
	plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
	plt.close()                
	if ( DO_CONVERTGIF ):
		os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}")
##############################


##############################
##############################
#Define a plotting function first
def colored_bar(left, height, z=None, width=0.8, bottom=0.2, ax=None, **kwargs):
	"""Create a colorbar
	"""
	if ax is None:
		ax = plt.gca()
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
