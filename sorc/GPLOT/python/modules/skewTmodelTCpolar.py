#!/work/noaa/aoml-hafs1/galaka/anaconda3/envs/GPLOT/bin/python3

#------------------------------------------------------------------------------------------------------------------------
# This script reads in storm-centered model data and Calculates Soundings at the Following Radii and Azimuths
# Center
# RMW (NE, NW, SW, SE - averaged over each quadrant)
# RMW-050 nmi (NE, NW, SW, SE - averaged over each quadrant and the radial band)
# 050-100 nmi (NE, NW, SW, SE - averaged over each quadrant and the radial band)
# 100-150 nmi (NE, NW, SW, SE - averaged over each quadrant and the radial band)
# 150-200 nmi (NE, NW, SW, SE - averaged over each quadrant and the radial band)
#
# Needed input arguments:
# r_grid: Radius in km
# theta_grid: Azimuth in radians
# p_sounding_polar: Pressure (Pa) in storm-centered polar cylindrical coordinates
# u_sounding_polar, v_sounding_polar: U/V winds (m/s) of the same size as p_sounding_polar
# temp_sounding_polar: Temperature (K) of the same size as p_sounding_polar
# rh_sounding_polar: Relative Humidity (%) of the same size as p_sounding_polar
# rmw: Radius of Maximum Winds in nmi
#------------------------------------------------------------------------------------------------------------------------

print('Importing Everything Needed')
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import metpy
import metpy.calc as mpcalc
from metpy.cbook import get_test_data
from metpy.plots import add_metpy_logo, SkewT
from metpy.units import units
import os

def skewTmodelTCpolar(r_grid,theta_grid,p_sounding_polar,u_sounding_polar,v_sounding_polar,temp_sounding_polar,rh_sounding_polar,rmw,GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF):

       #Let's Define Our Plotting Function First
	def skewplot(sounding_p,sounding_t,sounding_td,sounding_u,sounding_v,location,GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF):
		color_data_wind = np.genfromtxt(GPLOT_DIR+'/python/colormaps/colormap_wind.txt')
		colormap_wind = matplotlib.colors.ListedColormap(color_data_wind)
		levs_wind = np.linspace(0,160,41,endpoint=True)
		norm_wind = colors.BoundaryNorm(levs_wind,256)

		figext = '.png'
		fig = plt.figure(figsize=(16, 12))
		add_metpy_logo(fig, 150, 100)
		skew = SkewT(fig, rotation=45, aspect=80.5)
		skew.plot(sounding_p, sounding_t, 'r', linewidth=3)
		skew.plot(sounding_p, sounding_td, 'g', linewidth=3)
		img=skew.plot_barbs(sounding_p, sounding_u, sounding_v, c=np.hypot(sounding_u,sounding_v),cmap=colormap_wind,norm=norm_wind)
		cbar=plt.colorbar(img, cmap=colormap_wind, norm=norm_wind, boundaries=levs_wind,ticks=np.linspace(0,160,9),pad = 0.1)
		cbar.ax.tick_params(labelsize=24)
		skew.ax.set_ylim(1000, 100)
		skew.ax.set_xlim(-40, 50)
		skew.ax.axvline(0, color='c', linestyle='--', linewidth=2)
		skew.plot_dry_adiabats()
		skew.plot_moist_adiabats()
		skew.plot_mixing_lines()
		plt.xlabel('degC',weight='bold',fontsize=24)
		plt.ylabel('Pressure (hPa)',weight='bold',fontsize=24)
		plt.title(EXPT.strip()+'\n'+'Sounding '+location+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
		plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
		figfname = ODIR+'/'+LONGSID.lower()+'.sounding_'+location.replace(' ','_')+'.'+forecastinit+'.polar.f'+format(FHR,'03d')
		plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
		plt.close()
		if ( DO_CONVERTGIF ):
			os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");


		#plt.title(title,weight='bold',fontsize=28)
		#plt.gcf().savefig('sounding_test_'+title.replace(' ','_')+figext, bbox_inches='tight', dpi='figure')
		#figfname = ODIR+'/'+LONGSID.lower()+'.'+varstring+'_time_series.'+forecastinit+'.polar'
		#plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
		#plt.close()

	def skewplot_blank(location,GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF):
		figext = '.png'
		fig = plt.figure(figsize=(16, 12))
		add_metpy_logo(fig, 150, 100)
		skew = SkewT(fig, rotation=45, aspect=80.5)
		skew.ax.set_ylim(1000, 100)
		skew.ax.set_xlim(-40, 50)
		skew.ax.axvline(0, color='c', linestyle='--', linewidth=2)
		skew.plot_dry_adiabats()
		skew.plot_moist_adiabats()
		skew.plot_mixing_lines()
		plt.xlabel('degC',weight='bold',fontsize=24)
		plt.ylabel('Pressure (hPa)',weight='bold',fontsize=24)		
		plt.text(-30,200,'No Data' + '\n' + 'RMW > 50 nmi',fontsize=28,weight='bold')
		plt.title(EXPT.strip()+'\n'+'Sounding '+location+'\n'+'Init: '+forecastinit+' Forecast Hour:['+format(FHR,'03d')+']',fontsize=24, weight = 'bold',loc='left')
		plt.title('VMAX= '+maxwind+' kt'+'\n'+'PMIN= '+minpressure+' hPa'+'\n'+LONGSID.upper(),fontsize=24,color='brown',loc='right')
		figfname = ODIR+'/'+LONGSID.lower()+'.sounding_'+location.replace(' ','_')+'.'+forecastinit+'.polar.f'+format(FHR,'03d')
		plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
		plt.close()
		if ( DO_CONVERTGIF ):
		        os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");

	p_sounding_polar = p_sounding_polar/100
	u_sounding_polar = u_sounding_polar*1.94
	v_sounding_polar = v_sounding_polar*1.94
	temp_sounding_polar = temp_sounding_polar-273.15

	td_sounding_polar = metpy.calc.dewpoint_rh(temp_sounding_polar * units.celsius,rh_sounding_polar * units.percent)

	index_r_rmw = np.argmin(np.abs(r_grid-1.852*rmw))
	index_r_050 = np.argmin(np.abs(r_grid-1.852*50))
	index_r_100 = np.argmin(np.abs(r_grid-1.852*100))
	index_r_150 = np.argmin(np.abs(r_grid-1.852*150))
	index_r_200 = np.argmin(np.abs(r_grid-1.852*200))

	index_theta_E = np.argmin(np.abs(theta_grid*180/np.pi-0))
	index_theta_N = np.argmin(np.abs(theta_grid*180/np.pi-90))
	index_theta_W = np.argmin(np.abs(theta_grid*180/np.pi-180))
	index_theta_S = np.argmin(np.abs(theta_grid*180/np.pi-270))

	#Do Center Sounding
	p_sounding_center = p_sounding_polar[0,0,:]
	u_sounding_center = u_sounding_polar[0,0,:]
	v_sounding_center = v_sounding_polar[0,0,:]
	temp_sounding_center = temp_sounding_polar[0,0,:]
	td_sounding_center = td_sounding_polar[0,0,:]
	skewplot(p_sounding_center,temp_sounding_center,td_sounding_center,u_sounding_center,v_sounding_center,'Center',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)

	###Do Soundings at the RMW Averaged Along Each Quadrant###
	if ( rmw < 50):
		p_sounding_NE_rmw = np.nanmean(p_sounding_polar[index_theta_E:index_theta_N+1,index_r_rmw,:],0)
		u_sounding_NE_rmw = np.nanmean(u_sounding_polar[index_theta_E:index_theta_N+1,index_r_rmw,:],0)
		v_sounding_NE_rmw = np.nanmean(v_sounding_polar[index_theta_E:index_theta_N+1,index_r_rmw,:],0)
		temp_sounding_NE_rmw = np.nanmean(temp_sounding_polar[index_theta_E:index_theta_N+1,index_r_rmw,:],0)
		td_sounding_NE_rmw = np.nanmean(td_sounding_polar[index_theta_E:index_theta_N+1,index_r_rmw,:],0)
		skewplot(p_sounding_NE_rmw,temp_sounding_NE_rmw,td_sounding_NE_rmw,u_sounding_NE_rmw,v_sounding_NE_rmw,'NE RMW',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)

		p_sounding_NW_rmw = np.nanmean(p_sounding_polar[index_theta_N:index_theta_W+1,index_r_rmw,:],0)
		u_sounding_NW_rmw = np.nanmean(u_sounding_polar[index_theta_N:index_theta_W+1,index_r_rmw,:],0)
		v_sounding_NW_rmw = np.nanmean(v_sounding_polar[index_theta_N:index_theta_W+1,index_r_rmw,:],0)
		temp_sounding_NW_rmw = np.nanmean(temp_sounding_polar[index_theta_N:index_theta_W+1,index_r_rmw,:],0)
		td_sounding_NW_rmw = np.nanmean(td_sounding_polar[index_theta_N:index_theta_W+1,index_r_rmw,:],0)
		skewplot(p_sounding_NW_rmw,temp_sounding_NW_rmw,td_sounding_NW_rmw,u_sounding_NW_rmw,v_sounding_NW_rmw,'NW RMW',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)

		p_sounding_SW_rmw = np.nanmean(p_sounding_polar[index_theta_W:index_theta_S+1,index_r_rmw,:],0)
		u_sounding_SW_rmw = np.nanmean(u_sounding_polar[index_theta_W:index_theta_S+1,index_r_rmw,:],0)
		v_sounding_SW_rmw = np.nanmean(v_sounding_polar[index_theta_W:index_theta_S+1,index_r_rmw,:],0)
		temp_sounding_SW_rmw = np.nanmean(temp_sounding_polar[index_theta_W:index_theta_S+1,index_r_rmw,:],0)
		td_sounding_SW_rmw = np.nanmean(td_sounding_polar[index_theta_W:index_theta_S+1,index_r_rmw,:],0)
		skewplot(p_sounding_SW_rmw,temp_sounding_SW_rmw,td_sounding_SW_rmw,u_sounding_SW_rmw,v_sounding_SW_rmw,'SW RMW',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)

		p_sounding_SE_rmw = np.nanmean(p_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_rmw,:],0)
		u_sounding_SE_rmw = np.nanmean(u_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_rmw,:],0)
		v_sounding_SE_rmw = np.nanmean(v_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_rmw,:],0)
		temp_sounding_SE_rmw = np.nanmean(temp_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_rmw,:],0)
		td_sounding_SE_rmw = np.nanmean(td_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_rmw,:],0)
		skewplot(p_sounding_SE_rmw,temp_sounding_SE_rmw,td_sounding_SE_rmw,u_sounding_SE_rmw,v_sounding_SE_rmw,'SE RMW',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)
		###

		###Do Soundings from the RMW to 50 nmi Averaged Along Each Quadrant###
		p_sounding_NE_rmw_050 = np.nanmean(np.nanmean(p_sounding_polar[index_theta_E:index_theta_N+1,index_r_rmw:index_r_050+1,:],0),0)
		u_sounding_NE_rmw_050 = np.nanmean(np.nanmean(u_sounding_polar[index_theta_E:index_theta_N+1,index_r_rmw:index_r_050+1,:],0),0)
		v_sounding_NE_rmw_050 = np.nanmean(np.nanmean(v_sounding_polar[index_theta_E:index_theta_N+1,index_r_rmw:index_r_050+1,:],0),0)
		temp_sounding_NE_rmw_050 = np.nanmean(np.nanmean(temp_sounding_polar[index_theta_E:index_theta_N+1,index_r_rmw:index_r_050+1,:],0),0)
		td_sounding_NE_rmw_050 = np.nanmean(np.nanmean(td_sounding_polar[index_theta_E:index_theta_N+1,index_r_rmw:index_r_050+1,:],0),0)
		skewplot(p_sounding_NE_rmw_050,temp_sounding_NE_rmw_050,td_sounding_NE_rmw_050,u_sounding_NE_rmw_050,v_sounding_NE_rmw_050,'NE RMW-50nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)

		p_sounding_NW_rmw_050 = np.nanmean(np.nanmean(p_sounding_polar[index_theta_N:index_theta_W+1,index_r_rmw:index_r_050+1,:],0),0)
		u_sounding_NW_rmw_050 = np.nanmean(np.nanmean(u_sounding_polar[index_theta_N:index_theta_W+1,index_r_rmw:index_r_050+1,:],0),0)
		v_sounding_NW_rmw_050 = np.nanmean(np.nanmean(v_sounding_polar[index_theta_N:index_theta_W+1,index_r_rmw:index_r_050+1,:],0),0)
		temp_sounding_NW_rmw_050 = np.nanmean(np.nanmean(temp_sounding_polar[index_theta_N:index_theta_W+1,index_r_rmw:index_r_050+1,:],0),0)
		td_sounding_NW_rmw_050 = np.nanmean(np.nanmean(td_sounding_polar[index_theta_N:index_theta_W+1,index_r_rmw:index_r_050+1,:],0),0)
		skewplot(p_sounding_NW_rmw_050,temp_sounding_NW_rmw_050,td_sounding_NW_rmw_050,u_sounding_NW_rmw_050,v_sounding_NW_rmw_050,'NW RMW-50nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)

		p_sounding_SW_rmw_050 = np.nanmean(np.nanmean(p_sounding_polar[index_theta_W:index_theta_S+1,index_r_rmw:index_r_050+1,:],0),0)
		u_sounding_SW_rmw_050 = np.nanmean(np.nanmean(u_sounding_polar[index_theta_W:index_theta_S+1,index_r_rmw:index_r_050+1,:],0),0)
		v_sounding_SW_rmw_050 = np.nanmean(np.nanmean(v_sounding_polar[index_theta_W:index_theta_S+1,index_r_rmw:index_r_050+1,:],0),0)
		temp_sounding_SW_rmw_050 = np.nanmean(np.nanmean(temp_sounding_polar[index_theta_W:index_theta_S+1,index_r_rmw:index_r_050+1,:],0),0)
		td_sounding_SW_rmw_050 = np.nanmean(np.nanmean(td_sounding_polar[index_theta_W:index_theta_S+1,index_r_rmw:index_r_050+1,:],0),0)
		skewplot(p_sounding_SW_rmw_050,temp_sounding_SW_rmw_050,td_sounding_SW_rmw_050,u_sounding_SW_rmw_050,v_sounding_SW_rmw_050,'SW RMW-50nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)

		p_sounding_SE_rmw_050 = np.nanmean(np.nanmean(p_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_rmw:index_r_050+1,:],0),0)
		u_sounding_SE_rmw_050 = np.nanmean(np.nanmean(u_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_rmw:index_r_050+1,:],0),0)
		v_sounding_SE_rmw_050 = np.nanmean(np.nanmean(v_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_rmw:index_r_050+1,:],0),0)
		temp_sounding_SE_rmw_050 = np.nanmean(np.nanmean(temp_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_rmw:index_r_050+1,:],0),0)
		td_sounding_SE_rmw_050 = np.nanmean(np.nanmean(td_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_rmw:index_r_050+1,:],0),0)
		skewplot(p_sounding_SE_rmw_050,temp_sounding_SE_rmw_050,td_sounding_SE_rmw_050,u_sounding_SE_rmw_050,v_sounding_SE_rmw_050,'SE RMW-50nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)
	else:
		skewplot_blank('NE RMW',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)
		skewplot_blank('NW RMW',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)
		skewplot_blank('SW RMW',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)	
		skewplot_blank('SE RMW',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)	
		skewplot_blank('NE RMW-50nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)
		skewplot_blank('NW RMW-50nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)
		skewplot_blank('SW RMW-50nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)
		skewplot_blank('SE RMW-50nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF) 	
	###	

	###Do Soundings from 50 nmi to 100 nmi Averaged Along Each Quadrant###
	p_sounding_NE_050_100 = np.nanmean(np.nanmean(p_sounding_polar[index_theta_E:index_theta_N+1,index_r_050:index_r_100+1,:],0),0)
	u_sounding_NE_050_100 = np.nanmean(np.nanmean(u_sounding_polar[index_theta_E:index_theta_N+1,index_r_050:index_r_100+1,:],0),0)
	v_sounding_NE_050_100 = np.nanmean(np.nanmean(v_sounding_polar[index_theta_E:index_theta_N+1,index_r_050:index_r_100+1,:],0),0)
	temp_sounding_NE_050_100 = np.nanmean(np.nanmean(temp_sounding_polar[index_theta_E:index_theta_N+1,index_r_050:index_r_100+1,:],0),0)
	td_sounding_NE_050_100 = np.nanmean(np.nanmean(td_sounding_polar[index_theta_E:index_theta_N+1,index_r_050:index_r_100+1,:],0),0)
	skewplot(p_sounding_NE_050_100,temp_sounding_NE_050_100,td_sounding_NE_050_100,u_sounding_NE_050_100,v_sounding_NE_050_100,'NE 50-100nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)

	p_sounding_NW_050_100 = np.nanmean(np.nanmean(p_sounding_polar[index_theta_N:index_theta_W+1,index_r_050:index_r_100+1,:],0),0)
	u_sounding_NW_050_100 = np.nanmean(np.nanmean(u_sounding_polar[index_theta_N:index_theta_W+1,index_r_050:index_r_100+1,:],0),0)
	v_sounding_NW_050_100 = np.nanmean(np.nanmean(v_sounding_polar[index_theta_N:index_theta_W+1,index_r_050:index_r_100+1,:],0),0)
	temp_sounding_NW_050_100 = np.nanmean(np.nanmean(temp_sounding_polar[index_theta_N:index_theta_W+1,index_r_050:index_r_100+1,:],0),0)
	td_sounding_NW_050_100 = np.nanmean(np.nanmean(td_sounding_polar[index_theta_N:index_theta_W+1,index_r_050:index_r_100+1,:],0),0)
	skewplot(p_sounding_NW_050_100,temp_sounding_NW_050_100,td_sounding_NW_050_100,u_sounding_NW_050_100,v_sounding_NW_050_100,'NW 50-100nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)

	p_sounding_SW_050_100 = np.nanmean(np.nanmean(p_sounding_polar[index_theta_W:index_theta_S+1,index_r_050:index_r_100+1,:],0),0)
	u_sounding_SW_050_100 = np.nanmean(np.nanmean(u_sounding_polar[index_theta_W:index_theta_S+1,index_r_050:index_r_100+1,:],0),0)
	v_sounding_SW_050_100 = np.nanmean(np.nanmean(v_sounding_polar[index_theta_W:index_theta_S+1,index_r_050:index_r_100+1,:],0),0)
	temp_sounding_SW_050_100 = np.nanmean(np.nanmean(temp_sounding_polar[index_theta_W:index_theta_S+1,index_r_050:index_r_100+1,:],0),0)
	td_sounding_SW_050_100 = np.nanmean(np.nanmean(td_sounding_polar[index_theta_W:index_theta_S+1,index_r_050:index_r_100+1,:],0),0)
	skewplot(p_sounding_SW_050_100,temp_sounding_SW_050_100,td_sounding_SW_050_100,u_sounding_SW_050_100,v_sounding_SW_050_100,'SW 50-100nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)

	p_sounding_SE_050_100 = np.nanmean(np.nanmean(p_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_050:index_r_100+1,:],0),0)
	u_sounding_SE_050_100 = np.nanmean(np.nanmean(u_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_050:index_r_100+1,:],0),0)
	v_sounding_SE_050_100 = np.nanmean(np.nanmean(v_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_050:index_r_100+1,:],0),0)
	temp_sounding_SE_050_100 = np.nanmean(np.nanmean(temp_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_050:index_r_100+1,:],0),0)
	td_sounding_SE_050_100 = np.nanmean(np.nanmean(td_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_050:index_r_100+1,:],0),0)
	skewplot(p_sounding_SE_050_100,temp_sounding_SE_050_100,td_sounding_SE_050_100,u_sounding_SE_050_100,v_sounding_SE_050_100,'SE 50-100nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)
	###

	###Do Soundings From 100 nmi to 150 nmi Averaged Along Each Quadrant###
	p_sounding_NE_100_150 = np.nanmean(np.nanmean(p_sounding_polar[index_theta_E:index_theta_N+1,index_r_100:index_r_150+1,:],0),0)
	u_sounding_NE_100_150 = np.nanmean(np.nanmean(u_sounding_polar[index_theta_E:index_theta_N+1,index_r_100:index_r_150+1,:],0),0)
	v_sounding_NE_100_150 = np.nanmean(np.nanmean(v_sounding_polar[index_theta_E:index_theta_N+1,index_r_100:index_r_150+1,:],0),0)
	temp_sounding_NE_100_150 = np.nanmean(np.nanmean(temp_sounding_polar[index_theta_E:index_theta_N+1,index_r_100:index_r_150+1,:],0),0)
	td_sounding_NE_100_150 = np.nanmean(np.nanmean(td_sounding_polar[index_theta_E:index_theta_N+1,index_r_100:index_r_150+1,:],0),0)
	skewplot(p_sounding_NE_100_150,temp_sounding_NE_100_150,td_sounding_NE_100_150,u_sounding_NE_100_150,v_sounding_NE_100_150,'NE 100-150nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)

	p_sounding_NW_100_150 = np.nanmean(np.nanmean(p_sounding_polar[index_theta_N:index_theta_W+1,index_r_100:index_r_150+1,:],0),0)
	u_sounding_NW_100_150 = np.nanmean(np.nanmean(u_sounding_polar[index_theta_N:index_theta_W+1,index_r_100:index_r_150+1,:],0),0)
	v_sounding_NW_100_150 = np.nanmean(np.nanmean(v_sounding_polar[index_theta_N:index_theta_W+1,index_r_100:index_r_150+1,:],0),0)
	temp_sounding_NW_100_150 = np.nanmean(np.nanmean(temp_sounding_polar[index_theta_N:index_theta_W+1,index_r_100:index_r_150+1,:],0),0)
	td_sounding_NW_100_150 = np.nanmean(np.nanmean(td_sounding_polar[index_theta_N:index_theta_W+1,index_r_100:index_r_150+1,:],0),0)
	skewplot(p_sounding_NW_100_150,temp_sounding_NW_100_150,td_sounding_NW_100_150,u_sounding_NW_100_150,v_sounding_NW_100_150,'NW 100-150nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)

	p_sounding_SW_100_150 = np.nanmean(np.nanmean(p_sounding_polar[index_theta_W:index_theta_S+1,index_r_100:index_r_150+1,:],0),0)
	u_sounding_SW_100_150 = np.nanmean(np.nanmean(u_sounding_polar[index_theta_W:index_theta_S+1,index_r_100:index_r_150+1,:],0),0)
	v_sounding_SW_100_150 = np.nanmean(np.nanmean(v_sounding_polar[index_theta_W:index_theta_S+1,index_r_100:index_r_150+1,:],0),0)
	temp_sounding_SW_100_150 = np.nanmean(np.nanmean(temp_sounding_polar[index_theta_W:index_theta_S+1,index_r_100:index_r_150+1,:],0),0)
	td_sounding_SW_100_150 = np.nanmean(np.nanmean(td_sounding_polar[index_theta_W:index_theta_S+1,index_r_100:index_r_150+1,:],0),0)
	skewplot(p_sounding_SW_100_150,temp_sounding_SW_100_150,td_sounding_SW_100_150,u_sounding_SW_100_150,v_sounding_SW_100_150,'SW 100-150nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)

	p_sounding_SE_100_150 = np.nanmean(np.nanmean(p_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_100:index_r_150+1,:],0),0)
	u_sounding_SE_100_150 = np.nanmean(np.nanmean(u_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_100:index_r_150+1,:],0),0)
	v_sounding_SE_100_150 = np.nanmean(np.nanmean(v_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_100:index_r_150+1,:],0),0)
	temp_sounding_SE_100_150 = np.nanmean(np.nanmean(temp_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_100:index_r_150+1,:],0),0)
	td_sounding_SE_100_150 = np.nanmean(np.nanmean(td_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_100:index_r_150+1,:],0),0)
	skewplot(p_sounding_SE_100_150,temp_sounding_SE_100_150,td_sounding_SE_100_150,u_sounding_SE_100_150,v_sounding_SE_100_150,'SE 100-150nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)
	###

	###Do Soundings From 150 nmi to 200 nmi Averaged Along Each Quadrant###
	p_sounding_NE_150_200 = np.nanmean(np.nanmean(p_sounding_polar[index_theta_E:index_theta_N+1,index_r_150:index_r_200+1,:],0),0)
	u_sounding_NE_150_200 = np.nanmean(np.nanmean(u_sounding_polar[index_theta_E:index_theta_N+1,index_r_150:index_r_200+1,:],0),0)
	v_sounding_NE_150_200 = np.nanmean(np.nanmean(v_sounding_polar[index_theta_E:index_theta_N+1,index_r_150:index_r_200+1,:],0),0)
	temp_sounding_NE_150_200 = np.nanmean(np.nanmean(temp_sounding_polar[index_theta_E:index_theta_N+1,index_r_150:index_r_200+1,:],0),0)
	td_sounding_NE_150_200 = np.nanmean(np.nanmean(td_sounding_polar[index_theta_E:index_theta_N+1,index_r_150:index_r_200+1,:],0),0)
	skewplot(p_sounding_NE_150_200,temp_sounding_NE_150_200,td_sounding_NE_150_200,u_sounding_NE_150_200,v_sounding_NE_150_200,'NE 150-200nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)

	p_sounding_NW_150_200 = np.nanmean(np.nanmean(p_sounding_polar[index_theta_N:index_theta_W+1,index_r_150:index_r_200+1,:],0),0)
	u_sounding_NW_150_200 = np.nanmean(np.nanmean(u_sounding_polar[index_theta_N:index_theta_W+1,index_r_150:index_r_200+1,:],0),0)
	v_sounding_NW_150_200 = np.nanmean(np.nanmean(v_sounding_polar[index_theta_N:index_theta_W+1,index_r_150:index_r_200+1,:],0),0)
	temp_sounding_NW_150_200 = np.nanmean(np.nanmean(temp_sounding_polar[index_theta_N:index_theta_W+1,index_r_150:index_r_200+1,:],0),0)
	td_sounding_NW_150_200 = np.nanmean(np.nanmean(td_sounding_polar[index_theta_N:index_theta_W+1,index_r_150:index_r_200+1,:],0),0)
	skewplot(p_sounding_NW_150_200,temp_sounding_NW_150_200,td_sounding_NW_150_200,u_sounding_NW_150_200,v_sounding_NW_150_200,'NW 150-200nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)

	p_sounding_SW_150_200 = np.nanmean(np.nanmean(p_sounding_polar[index_theta_W:index_theta_S+1,index_r_150:index_r_200+1,:],0),0)
	u_sounding_SW_150_200 = np.nanmean(np.nanmean(u_sounding_polar[index_theta_W:index_theta_S+1,index_r_150:index_r_200+1,:],0),0)
	v_sounding_SW_150_200 = np.nanmean(np.nanmean(v_sounding_polar[index_theta_W:index_theta_S+1,index_r_150:index_r_200+1,:],0),0)
	temp_sounding_SW_150_200 = np.nanmean(np.nanmean(temp_sounding_polar[index_theta_W:index_theta_S+1,index_r_150:index_r_200+1,:],0),0)
	td_sounding_SW_150_200 = np.nanmean(np.nanmean(td_sounding_polar[index_theta_W:index_theta_S+1,index_r_150:index_r_200+1,:],0),0)
	skewplot(p_sounding_SW_150_200,temp_sounding_SW_150_200,td_sounding_SW_150_200,u_sounding_SW_150_200,v_sounding_SW_150_200,'SW 150-200nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)

	p_sounding_SE_150_200 = np.nanmean(np.nanmean(p_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_150:index_r_200+1,:],0),0)
	u_sounding_SE_150_200 = np.nanmean(np.nanmean(u_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_150:index_r_200+1,:],0),0)
	v_sounding_SE_150_200 = np.nanmean(np.nanmean(v_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_150:index_r_200+1,:],0),0)
	temp_sounding_SE_150_200 = np.nanmean(np.nanmean(temp_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_150:index_r_200+1,:],0),0)
	td_sounding_SE_150_200 = np.nanmean(np.nanmean(td_sounding_polar[index_theta_S:np.shape(theta_grid)[0],index_r_150:index_r_200+1,:],0),0)
	skewplot(p_sounding_SE_150_200,temp_sounding_SE_150_200,td_sounding_SE_150_200,u_sounding_SE_150_200,v_sounding_SE_150_200,'SE 150-200nmi',GPLOT_DIR,EXPT,FHR,maxwind,minpressure,LONGSID,ODIR,forecastinit,DO_CONVERTGIF)
	###
