#!/lfs3/projects/hur-aoml/Andrew.Hazelton/anaconda3/bin/python
#Import necessary modules

print('Importing Everything Needed')
import numpy as np
import os
import glob
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import sys

datafile = sys.argv[1]
EXPT = sys.argv[2]
ODIR = sys.argv[3]
forecastinit = sys.argv[4]
LONGSID = sys.argv[5]
figext = '.png'

data = np.genfromtxt(datafile,delimiter=',',dtype='str')

header = data[0,:]

data = data[1::,:]

FCHR = data[:,0]
VMAX = data[:,1].astype('float')
RMW2 = data[:,2].astype('float')
SLO1 = data[:,3].astype('float')
SLO2 = data[:,4].astype('float')
ALPH = data[:,5].astype('float')
VODV = data[:,6].astype('float')
TMPM = data[:,7].astype('float')
TDPM = data[:,8].astype('float')
TMVM = data[:,9].astype('float')
TDVM = data[:,10].astype('float')
TMPD = data[:,11].astype('float')
TDPD = data[:,12].astype('float')
TMVD = data[:,13].astype('float')
TDVD = data[:,14].astype('float')
PWEI = data[:,15].astype('float')
PSTI = data[:,16].astype('float')
PSHI = data[:,17].astype('float')
PMOI = data[:,18].astype('float')
PDEI = data[:,19].astype('float')
PWEO = data[:,20].astype('float')
PSTO = data[:,21].astype('float')
PSHO = data[:,22].astype('float')
PMOO = data[:,23].astype('float')
PDEO = data[:,24].astype('float')
CLST = data[:,25].astype('float')
CLSH = data[:,26].astype('float')
CLMO = data[:,27].astype('float')
CLDE = data[:,28].astype('float')
SRW1 = data[:,29].astype('float')
SRWA = data[:,30].astype('float')
SWW1 = data[:,31].astype('float')
SWWA = data[:,32].astype('float')
SLMS = data[:,33].astype('float')
SLDS = data[:,34].astype('float')
SLMM = data[:,35].astype('float')
SLDM = data[:,36].astype('float')
SLMD = data[:,37].astype('float')
SLDD = data[:,38].astype('float')

print('Defining Figure Functions')

def lineplot1(x,y,ycolor,xmin,xmax,ymin,ymax,xticks,yticks,xaxislabel,yaxislabel,ylegendlabel,varstring_long,varstring,ysecondary,ysecondarycolor,ysecondarymin,ysecondarymax,ysecondaryticks,ysecondaryaxislabel,ysecondarylegendlabel):
	fig = plt.figure()
	fig.set_size_inches(20.5,10.5)
	plt.plot(x,y,color=ycolor,linewidth=4,linestyle='-',label=ylegendlabel)
	plt.xlim(xmin,xmax)
	plt.ylim(ymin,ymax)
	plt.grid()
	plt.xticks(xticks,fontsize=24,fontweight='bold')
	plt.yticks(yticks,fontsize=24,fontweight='bold')
	plt.xlabel(xaxislabel,fontsize=24,fontweight='bold')
	plt.ylabel(yaxislabel,fontsize=24,fontweight='bold')
	plt.legend(loc='upper left',fontsize=20)
	plt.title(EXPT.strip()+'\n'+'Time Series of '+varstring_long+'\n'+'Init: '+forecastinit,fontsize=24, weight = 'bold',loc='left')
	plt.title(LONGSID.upper(),fontsize=24,color='brown',loc='right')
	ax1 = plt.gca()
	ax2 = ax1.twinx()
	ax2.plot(x,ysecondary,color=ysecondarycolor,linewidth=4,linestyle='-',label=ysecondarylegendlabel)
	ax2.set_ybound([ysecondarymin,ysecondarymax])
	ax2.set_yticklabels(ysecondaryticks,fontsize=24,fontweight='bold',color=ysecondarycolor)
	ax2.set_ylabel(ysecondaryaxislabel,fontsize=24,fontweight='bold',color=ysecondarycolor)
	ax2.legend(loc='upper right',fontsize=20)
	figfname = ODIR+'/'+LONGSID.lower()+'.'+varstring+'_time_series.'+forecastinit+'.polar'
	fig.savefig(figfname+figext, bbox_inches='tight', dpi='figure')
	plt.close()
	os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");

def lineplot2(x,y1,ycolor1,y2,ycolor2,xmin,xmax,ymin,ymax,xticks,yticks,xaxislabel,yaxislabel,ylegendlabel1,ylegendlabel2,varstring_long,varstring,ysecondary,ysecondarycolor,ysecondarymin,ysecondarymax,ysecondaryticks,ysecondaryaxislabel,ysecondarylegendlabel):
	plt.figure()
	plt.gcf().set_size_inches(20.5, 10.5)
	plt.plot(x,y1,color=ycolor1,linewidth=4,linestyle='-',label=ylegendlabel1)
	plt.plot(x,y2,color=ycolor2,linewidth=4,linestyle='-',label=ylegendlabel2)
	plt.xlim(xmin,xmax)
	plt.ylim(ymin,ymax)
	plt.grid()
	plt.xticks(xticks,fontsize=24,fontweight='bold')
	plt.yticks(yticks,fontsize=24,fontweight='bold')
	plt.xlabel(xaxislabel,fontsize=24,fontweight='bold')
	plt.ylabel(yaxislabel,fontsize=24,fontweight='bold')
	plt.legend(loc='upper left',fontsize=20)
	plt.title(EXPT.strip()+'\n'+'Time Series of '+varstring_long+'\n'+'Init: '+forecastinit,fontsize=24, weight = 'bold',loc='left')
	plt.title(LONGSID.upper(),fontsize=24,color='brown',loc='right')
	ax1 = plt.gca()
	ax2 = ax1.twinx()
	ax2.plot(x,ysecondary,color=ysecondarycolor,linewidth=4,linestyle='-',label=ysecondarylegendlabel)
	ax2.set_ybound([ysecondarymin,ysecondarymax])
	ax2.set_yticklabels(ysecondaryticks,fontsize=24,fontweight='bold',color=ysecondarycolor)
	ax2.set_ylabel(ysecondaryaxislabel,fontsize=24,fontweight='bold',color=ysecondarycolor)
	ax2.legend(loc='upper right',fontsize=20)
	figfname = ODIR+'/'+LONGSID.lower()+'.'+varstring+'_time_series.'+forecastinit+'.polar'
	plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
	plt.close()
	os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");

def lineplot3(x,y1,ycolor1,y2,ycolor2,y3,ycolor3,xmin,xmax,ymin,ymax,xticks,yticks,xaxislabel,yaxislabel,ylegendlabel1,ylegendlabel2,ylegendlabel3,varstring_long,varstring,ysecondary,ysecondarycolor,ysecondarymin,ysecondarymax,ysecondaryticks,ysecondaryaxislabel,ysecondarylegendlabel):
	plt.figure()
	plt.gcf().set_size_inches(20.5, 10.5)
	plt.plot(x,y1,color=ycolor1,linewidth=4,linestyle='-',label=ylegendlabel1)
	plt.plot(x,y2,color=ycolor2,linewidth=4,linestyle='-',label=ylegendlabel2)
	plt.plot(x,y3,color=ycolor3,linewidth=4,linestyle='-',label=ylegendlabel3)
	plt.xlim(xmin,xmax)
	plt.ylim(ymin,ymax)
	plt.grid()
	plt.xticks(xticks,fontsize=24,fontweight='bold')
	plt.yticks(yticks,fontsize=24,fontweight='bold')
	plt.xlabel(xaxislabel,fontsize=24,fontweight='bold')
	plt.ylabel(yaxislabel,fontsize=24,fontweight='bold')
	plt.legend(loc='upper left',fontsize=20)
	plt.title(EXPT.strip()+'\n'+'Time Series of '+varstring_long+'\n'+'Init: '+forecastinit,fontsize=24, weight = 'bold',loc='left')
	plt.title(LONGSID.upper(),fontsize=24,color='brown',loc='right')
	ax1 = plt.gca()
	ax2 = ax1.twinx()
	ax2.plot(x,ysecondary,color=ysecondarycolor,linewidth=4,linestyle='-',label=ysecondarylegendlabel)
	ax2.set_ybound([ysecondarymin,ysecondarymax])
	ax2.set_yticklabels(ysecondaryticks,fontsize=24,fontweight='bold',color=ysecondarycolor)
	ax2.set_ylabel(ysecondaryaxislabel,fontsize=24,fontweight='bold',color=ysecondarycolor)
	ax2.legend(loc='upper right',fontsize=20)
	figfname = ODIR+'/'+LONGSID.lower()+'.'+varstring+'_time_series.'+forecastinit+'.polar'
	plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
	plt.close()
	os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");

def lineplot4(x,y1,ycolor1,y2,ycolor2,y3,ycolor3,y4,ycolor4,xmin,xmax,ymin,ymax,xticks,yticks,xaxislabel,yaxislabel,ylegendlabel1,ylegendlabel2,ylegendlabel3,ylegendlabel4,varstring_long,varstring,ysecondary,ysecondarycolor,ysecondarymin,ysecondarymax,ysecondaryticks,ysecondaryaxislabel,ysecondarylegendlabel):
	plt.figure()
	plt.gcf().set_size_inches(20.5, 10.5)
	plt.plot(x,y1,color=ycolor1,linewidth=4,linestyle='-',label=ylegendlabel1)
	plt.plot(x,y2,color=ycolor2,linewidth=4,linestyle='-',label=ylegendlabel2)
	plt.plot(x,y3,color=ycolor3,linewidth=4,linestyle='-',label=ylegendlabel3)
	plt.plot(x,y4,color=ycolor4,linewidth=4,linestyle='-',label=ylegendlabel4)
	plt.xlim(xmin,xmax)
	plt.ylim(ymin,ymax)
	plt.grid()
	plt.xticks(xticks,fontsize=24,fontweight='bold')
	plt.yticks(yticks,fontsize=24,fontweight='bold')
	plt.xlabel(xaxislabel,fontsize=24,fontweight='bold')
	plt.ylabel(yaxislabel,fontsize=24,fontweight='bold')
	plt.legend(loc='upper left',fontsize=20)
	ax1 = plt.gca()
	ax2 = ax1.twinx()
	ax2.plot(x,ysecondary,color=ysecondarycolor,linewidth=4,linestyle='-',label=ysecondarylegendlabel)
	ax2.set_ybound([ysecondarymin,ysecondarymax])
	ax2.set_yticklabels(ysecondaryticks,fontsize=24,fontweight='bold',color=ysecondarycolor)
	ax2.set_ylabel(ysecondaryaxislabel,fontsize=24,fontweight='bold',color=ysecondarycolor)
	ax2.legend(loc='upper right',fontsize=20)
	plt.title(EXPT.strip()+'\n'+'Time Series of '+varstring_long+'\n'+'Init: '+forecastinit,fontsize=24, weight = 'bold',loc='left')
	plt.title(LONGSID.upper(),fontsize=24,color='brown',loc='right')
	figfname = ODIR+'/'+LONGSID.lower()+'.'+varstring+'_time_series.'+forecastinit+'.polar'
	plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
	plt.close()
	os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");

def lineplot5(x,y1,ycolor1,y2,ycolor2,y3,ycolor3,y4,ycolor4,y5,ycolor5,xmin,xmax,ymin,ymax,xticks,yticks,xaxislabel,yaxislabel,ylegendlabel1,ylegendlabel2,ylegendlabel3,ylegendlabel4,ylegendlabel5,varstring_long,varstring,ysecondary,ysecondarycolor,ysecondarymin,ysecondarymax,ysecondaryticks,ysecondaryaxislabel,ysecondarylegendlabel):
	plt.figure()
	plt.gcf().set_size_inches(20.5, 10.5)
	plt.plot(x,y1,color=ycolor1,linewidth=4,linestyle='-',label=ylegendlabel1)
	plt.plot(x,y2,color=ycolor2,linewidth=4,linestyle='-',label=ylegendlabel2)
	plt.plot(x,y3,color=ycolor3,linewidth=4,linestyle='-',label=ylegendlabel3)
	plt.plot(x,y4,color=ycolor4,linewidth=4,linestyle='-',label=ylegendlabel4)
	plt.plot(x,y5,color=ycolor5,linewidth=4,linestyle='-',label=ylegendlabel5)
	plt.xlim(xmin,xmax)
	plt.ylim(ymin,ymax)
	plt.grid()
	plt.xticks(xticks,fontsize=24,fontweight='bold')
	plt.yticks(yticks,fontsize=24,fontweight='bold')
	plt.xlabel(xaxislabel,fontsize=24,fontweight='bold')
	plt.ylabel(yaxislabel,fontsize=24,fontweight='bold')
	plt.legend(loc='upper left',fontsize=20)
	plt.title(EXPT.strip()+'\n'+'Time Series of '+varstring_long+'\n'+'Init: '+forecastinit,fontsize=24, weight = 'bold',loc='left')
	plt.title(LONGSID.upper(),fontsize=24,color='brown',loc='right')
	ax1 = plt.gca()
	ax2 = ax1.twinx()
	ax2.plot(x,ysecondary,color=ysecondarycolor,linewidth=4,linestyle='-',label=ysecondarylegendlabel)
	ax2.set_ybound([ysecondarymin,ysecondarymax])
	ax2.set_yticklabels(ysecondaryticks,fontsize=24,fontweight='bold',color=ysecondarycolor)
	ax2.set_ylabel(ysecondaryaxislabel,fontsize=24,fontweight='bold',color=ysecondarycolor)
	ax2.legend(loc='upper right',fontsize=20)
	figfname = ODIR+'/'+LONGSID.lower()+'.'+varstring+'_time_series.'+forecastinit+'.polar'
	plt.gcf().savefig(figfname+figext, bbox_inches='tight', dpi='figure')
	plt.close()
	os.system(f"convert {figfname}{figext} +repage gif:{figfname}.gif && /bin/rm {figfname}{figext}");

print('Making Plots')
#Define Options for All Plots
xline = FCHR.astype('int')
xmin = 0
xmax = 168
xticks = np.linspace(0,168,15).astype('int')
xaxislabel = 'Forecast Hour'

ysecondary = VMAX.astype('float')
ysecondarycolor = 'xkcd:purple'
ysecondarymin = 0
ysecondarymax = 160
ysecondaryticks = np.linspace(0,160,9).astype('int')
ysecondaryaxislabel = 'Maximum Wind (kt)'
ysecondarylegendlabel = 'Vmax'

#Make plot of RMW2
lineplot1(xline,RMW2,'xkcd:red',xmin,xmax,0,200,xticks,np.linspace(0,200,9),xaxislabel,'RMW (km)',r'$RMW_{2km}$','Radius of Maximum Wind','RMW2',ysecondary,ysecondarycolor,ysecondarymin,ysecondarymax,ysecondaryticks,ysecondaryaxislabel,ysecondarylegendlabel)

#Make plot of slope
lineplot2(xline,SLO1,'xkcd:red',SLO2,'blue',xmin,xmax,0,20,xticks,np.linspace(0,20,11),xaxislabel,'RMW Slope',r'RMW Slope (Regression)',r'RMW Slope $(\frac{\delta R}{\delta Z})$','RMW Slope','RMWslope',ysecondary,ysecondarycolor,ysecondarymin,ysecondarymax,ysecondaryticks,ysecondaryaxislabel,ysecondarylegendlabel)

#Make plot of alpha
lineplot1(xline,ALPH,'xkcd:orange',xmin,xmax,0,1,xticks,np.linspace(0,1,11),xaxislabel,r'$\alpha$',r'$\alpha_{2km}$','Horizontal Wind Decay Rate','alpha',ysecondary,ysecondarycolor,ysecondarymin,ysecondarymax,ysecondaryticks,ysecondaryaxislabel,ysecondarylegendlabel)

#Make plot of vortex depth
lineplot1(xline,VODV,'xkcd:wheat',xmin,xmax,0,18,xticks,np.linspace(0,18,10),xaxislabel,'Vortex Depth (km)',r'Vortex Depth Based on Vorticity','Vortex Depth','vortexdepth',ysecondary,ysecondarycolor,ysecondarymin,ysecondarymax,ysecondaryticks,ysecondaryaxislabel,ysecondarylegendlabel)

#Make plot of tilt magnitude (2-5 km and 2-10 km)
lineplot4(xline,TMPM,'xkcd:tomato',TMVM,'xkcd:lightblue',TMPD,'xkcd:crimson',TMVD,'xkcd:darkblue',xmin,xmax,0,100,xticks,np.linspace(0,100,11),xaxislabel,'Vortex Tilt (km)','2-5km Pressure Tilt','2-5km Vorticity Tilt','2-10km Pressure Tilt','2-10km Vorticity Tilt','Mid-Level and Upper-Level Vortex Tilt Magnitude','vortextiltmag',ysecondary,ysecondarycolor,ysecondarymin,ysecondarymax,ysecondaryticks,ysecondaryaxislabel,ysecondarylegendlabel)

#Make plot of tilt direction (2-5 km and 2-10 km)
lineplot4(xline,TDPM,'xkcd:tomato',TDVM,'xkcd:lightblue',TDPD,'xkcd:crimson',TDVD,'xkcd:darkblue',xmin,xmax,0,360,xticks,np.linspace(0,360,9),xaxislabel,'Vortex Tilt Direction (degrees)','2-5km Pressure Tilt','2-5km Vorticity Tilt','2-10km Pressure Tilt','2-10km Vorticity Tilt','Mid-Level and Upper-Level Vortex Tilt Direction','vortextiltdir',ysecondary,ysecondarycolor,ysecondarymin,ysecondarymax,ysecondaryticks,ysecondaryaxislabel,ysecondarylegendlabel)

#Make plot of precipitation type percentages inside the eyewall
lineplot5(xline,PWEI*100,'xkcd:grey',PSTI*100,'xkcd:green',PSHI*100,'xkcd:yellow',PMOI*100,'xkcd:orange',PDEI*100,'xkcd:red',xmin,xmax,0,100,xticks,np.linspace(0,100,11),xaxislabel,'Percentage of Eyewall Area','Weak','Stratiform','Shallow Convection','Moderate Convection','Deep Convection','Precipitation Type Percentages in the Inner Core','ptypepercentinner',ysecondary,ysecondarycolor,ysecondarymin,ysecondarymax,ysecondaryticks,ysecondaryaxislabel,ysecondarylegendlabel)

#Make plot of precipitation type percentages outside the eyewall
lineplot5(xline,PWEO*100,'xkcd:grey',PSTO*100,'xkcd:green',PSHO*100,'xkcd:yellow',PMOO*100,'xkcd:orange',PDEO*100,'xkcd:red',xmin,xmax,0,100,xticks,np.linspace(0,100,11),xaxislabel,'Percentage of Outer Core Area','Weak','Stratiform','Shallow Convection','Moderate Convection','Deep Convection','Precipitation Type Percentages in the Outer Core','ptypepercentouter',ysecondary,ysecondarycolor,ysecondarymin,ysecondarymax,ysecondaryticks,ysecondaryaxislabel,ysecondarylegendlabel)

#Make plot of closure
lineplot4(xline,CLST,'xkcd:green',CLSH,'xkcd:yellow',CLMO,'xkcd:orange',CLDE,'xkcd:red',xmin,xmax,0,1,xticks,np.linspace(0,1,11),xaxislabel,'Eyewall Closure','Stratiform','Shallow Convection','Moderate Convection','Deep Convection','Eyewall Closure by Each Precipitation Type','closure',ysecondary,ysecondarycolor,ysecondarymin,ysecondarymax,ysecondaryticks,ysecondaryaxislabel,ysecondarylegendlabel)

#Make plot of rainfall symmetry
lineplot2(xline,SRW1,'xkcd:lightgreen',SRWA,'xkcd:darkgreen',xmin,xmax,0,1,xticks,np.linspace(0,1,11),xaxislabel,'5-km Reflectivity Symmetry','Wavenumber 0 vs. Wavenumber 1','Wavenumber 0 vs. All Wavenumbers','Eyewall Precipitation Symmetry','symmetryrain',ysecondary,ysecondarycolor,ysecondarymin,ysecondarymax,ysecondaryticks,ysecondaryaxislabel,ysecondarylegendlabel)

#Make plot of wind symmetry
lineplot2(xline,SWW1,'xkcd:pink',SWWA,'xkcd:magenta',xmin,xmax,0,1,xticks,np.linspace(0,1,11),xaxislabel,'10-m Wind Symmetry','Wavenumber 0 vs. Wavenumber 1','Wavenumber 0 vs. All Wavenumbers','Eyewall Wind Symmetry','symmetrywind',ysecondary,ysecondarycolor,ysecondarymin,ysecondarymax,ysecondaryticks,ysecondaryaxislabel,ysecondarylegendlabel)

#Make Plot of Local Shear Magnitude
lineplot3(xline,SLMS*1.94,'xkcd:yellow',SLMM*1.94,'xkcd:orange',SLMD*1.94,'xkcd:red',xmin,xmax,0,40,xticks,np.linspace(0,40,9),xaxislabel,'Shear Magnitude (kt)','2-5 km','2-8 km','2-10 km','Local Shear Magnitude','shearlocalmag',ysecondary,ysecondarycolor,ysecondarymin,ysecondarymax,ysecondaryticks,ysecondaryaxislabel,ysecondarylegendlabel)

#Make Plot of Local Shear Direction
lineplot3(xline,SLDS,'xkcd:yellow',SLDM,'xkcd:orange',SLDD,'xkcd:red',xmin,xmax,0,360,xticks,np.linspace(0,360,9),xaxislabel,'Shear Direction (Degrees)','2-5 km','2-8 km','2-10 km','Local Shear Direction','shearlocaldir',ysecondary,ysecondarycolor,ysecondarymin,ysecondarymax,ysecondaryticks,ysecondaryaxislabel,ysecondarylegendlabel)
