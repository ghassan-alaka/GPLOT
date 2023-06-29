import sys

import warnings

import pickle

from math import pi, sin, cos, sqrt

import numpy as np

import matplotlib
matplotlib.use('Agg')

import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from matplotlib.colors import Normalize
from matplotlib.colors import LinearSegmentedColormap

from mpl_toolkits.axes_grid1 import make_axes_locatable
from pylab import *

def tilt_plot(kmin,kmax,x,y,z,u,v,w,vort,ic,jc,flg,olon_mean,olat_mean,fieldname,SHIPS_ShearMagNum,SHIPS_ShearDirMetNum,titlestring1,titlestring2,figfile):
    """
    Produce filled contour figure with tilt profile overlaid. Write figure to png file
    with flightid stamp.
     
    Global variables required:
    ms2kt
    tdr_dir
    stmname, fltname, timedisplaystart, timedisplayend
    SHIPS_ShearMagNum, SHIPS_ShearDirMetNum, X_SHIPS_Shr, Y_SHIPS_Shr

    Keyword arguments: (nz* = kmax-kmin+1)
    kmin/kmax      - vertical index of minimum/maximum vertical levels of tilt arrays (int)
    x(nx)          - zonal coordinate in km of Cartesian grid (float array)
    y(ny)          - meridional coordinate in km of Cartesian grid (float array)
    z(nz)          - vertical coordinate in km of Cartesian grid (float array)    
    u(nx,ny,nz)    - storm-relative zonal velocity in m/s (float array)
    v(nx,ny,nz)    - storm-relative meridional velocity in m/s (float array)
    w(nx,ny,nz)    - maximum vertical velocity in m/s (float array)
    vort(nx,ny,nz) - relative vorticity in 1/s (float array)
    ic/jc(nz*)     - zonal/meridional index of vortex center (int array)
    flg(nz*)       - flag [0 = ok, 1 = not ok] based on coverage and avg. wind thresholds (float)
    olon/olat_mean - mean longitude/latitude of merged analysis (float)
    fieldname      - contoured field over which to plot tilt (str)
    SHIPS_ShearMagNum -
    SHIPS_ShearDirMetNum -  
    titlestring1 -
    titlestring2 -
    figfile -    
    """

    ms2kt = 1.94384
    deg2rad = pi/180.

    SHIPS_ShearDirNum = 90 - SHIPS_ShearDirMetNum
    if (SHIPS_ShearDirNum < 0):
       SHIPS_ShearDirNum = SHIPS_ShearDirNum + 360

    X_SHIPS_Shr = 7.0*SHIPS_ShearMagNum*np.cos(SHIPS_ShearDirNum*deg2rad) # Scaled for plotting
    Y_SHIPS_Shr = 7.0*SHIPS_ShearMagNum*np.sin(SHIPS_ShearDirNum*deg2rad)

    # Grid parameters
    nx = u.shape[0]; ny = u.shape[1]; nz = u.shape[2]
    Y,X = np.meshgrid(y,x)

    ws = np.sqrt(u**2 + v**2)

    convec = np.zeros((nx,ny))
    # Pick out locations with w>6.0 m/s between 4 and 16 km
    # Thin out so that Xs aren't plotted too densely
    for j in range(1,ny,int(ny/96)):
        for i in range(1,nx,int(nx/96)):
            k=8
            while (k < 33):
                if (w[i,j,k] > 6.0):
                    convec[i,j] = 1
                    break
                k+=1

    # Color table arrays
    ct1 = [0 for i in range(3)]
    ct2 = [0 for i in range(3)]

    NWSRef_data = {
        'blue': [
            (0.0, 0.92549019607843142, 0.92549019607843142),(0.07142857, 0.96470588235294119, \
            0.96470588235294119),(0.14285714, 0.96470588235294119, 0.96470588235294119), \
            (0.21428571, 0.0, 0.0),(0.28571429, 0.0, 0.0),(0.35714286, 0.0, 0.0),(0.42857143, \
            0.0, 0.0),(0.50000000, 0.0, 0.0),(0.57142857, 0.0, 0.0),(0.64285714, 0.0, 0.0), \
            (0.71428571, 0.0, 0.0),(0.78571429, 0.0, 0.0),(0.85714286, 1.0, 1.0),(0.92857143, \
            0.78823529411764703, 0.78823529411764703),(1.0, 0.0, 0.0)], \
        'green': [
            (0.0, 0.92549019607843142, 0.92549019607843142),(0.07142857, 0.62745098039215685, \
            0.62745098039215685),(0.14285714, 0.0, 0.0),(0.21428571, 1.0, 1.0),(0.28571429, \
            0.78431372549019607, 0.78431372549019607),(0.35714286, 0.56470588235294117, \
            0.56470588235294117),(0.42857143, 1.0, 1.0),(0.50000000, 0.75294117647058822, \
            0.75294117647058822),(0.57142857, 0.56470588235294117, 0.56470588235294117), \
            (0.64285714, 0.0, 0.0),(0.71428571, 0.0, 0.0),(0.78571429, 0.0, 0.0),(0.85714286, \
            0.0, 0.0),(0.92857143, 0.33333333333333331, 0.33333333333333331),(1.0, 0.0, 0.0)], \
        'red': [
            (0.0, 0.0, 0.0),(0.07142857, 0.0039215686274509803, 0.0039215686274509803), \
            (0.14285714, 0.0, 0.0),(0.21428571, 0.0, 0.0),(0.28571429, 0.0, 0.0),(0.35714286, \
            0.0, 0.0),(0.42857143, 1.0, 1.0),(0.50000000, 0.90588235294117647, 0.90588235294117647), \
            (0.57142857, 1.0, 1.0),(0.64285714, 1.0, 1.0),(0.71428571, 0.83921568627450982, \
            0.83921568627450982),(0.78571429, 0.75294117647058822, 0.75294117647058822), \
            (0.85714286, 1.0, 1.0),(0.92857143, 0.59999999999999998, 0.59999999999999998), \
            (1.0, 0.0, 0.0)]
    }

    Tidbits_colors = [(241,241,241),(111,237,241),(80,205,213),(43,147,158),(16,139,159), \
        (0,180,50),(102,210,81),(204,240,111),(255,221,82),(255,109,41), \
        (255,0,0),(176,0,0),(95,0,0),(184,34,255),(212,106,255), \
        (241,177,255),(255,197,234),(255,166,193),(255,135,152), \
        (255,105,110),(230,73,83),(178,41,71),(153,25,66)]

    normTidbits_colors = []
    for ct1 in Tidbits_colors:
        ct2[0]=ct1[0]/255.
        ct2[1]=ct1[1]/255.
        ct2[2]=ct1[2]/255.
        normTidbits_colors.append((ct2[0],ct2[1],ct2[2]))

    ####### Set Up Figure

    figa = plt.figure(figsize=(19.5,12))

    ####### Left Panel

    if (fieldname == "ws"):
        titlea = ' Storm-relative WS (kt) at '
    elif (fieldname == "vort"):
        titlea = ' Storm-rel. Flow; Vorticity ($\mathregular{10^{-4}}$ $\mathregular{s^{-1}}$) at '

    level = np.argmin(abs(z-2.0))
    mulevel = np.transpose(u[:,:,level])
    mvlevel = np.transpose(v[:,:,level])

    if (fieldname == "ws"):
        mdatalevel = ws[:,:,level]*ms2kt
    elif (fieldname == "vort"):
        mdatalevel = vort[:,:,level]*1E4

    titleb = str(z[level])

    #plottitle=fltname+" ("+stmname.strip()+")\n"+timedisplaystart+" to "+timedisplayend+ \
    #    " UTC\n"+titlea+titleb+" km"
    plottitle=titlestring1

    fig = figa.add_subplot(121)

    if (fieldname == "ws"):
        n_bin = 256
        cmap_name = 'Tidbits'
        cmap_l = LinearSegmentedColormap.from_list(cmap_name, normTidbits_colors, N=n_bin)
        levs = [0,7,10,13,16,19,22,25,28,31,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64, \
            69.333,74.666,80,85.333,90.666,96,100.666,105.333,110,115,120,125,130,135,140, \
            145,150,155,160] # kt
        ticks = [7,16,25,34,40,46,52,58,64,80,96,110,125,140,155] # kt
    elif (fieldname == "vort"):
        color_data = np.genfromtxt('/home/Andrew.Hazelton/python/colormaps/bluewhitered_vorticity.txt')
        cmap_l = mcolors.ListedColormap(color_data)
        levs = [-500,-100,-50,-30,-10,-2,2,10,30,50,100,500]
        ticks = [-500,-100,-50,-30,-10,-2,2,10,30,50,100,500]

    norm = mcolors.BoundaryNorm(levs,256)

    print(np.shape(X))
    print(np.shape(Y))
    print(np.shape(mdatalevel))
    fig2 = fig.contourf(X,Y,mdatalevel,levels=levs,norm=norm,cmap=cmap_l) # kt
    fig.set_title(plottitle,fontsize=18)

    fig.grid()
    fig.set_xlabel("East-West distance (km)",fontsize=18)
    fig.set_ylabel("North-South distance (km)",fontsize=18)
    fig.tick_params(axis='both',which='major',labelsize=14)

    domsz = 80
    plt.xlim(-domsz, domsz)
    plt.ylim(-domsz, domsz)

    arrscl=0.3; arrwdt=2; arrheadwdt=6; arrheadlgt=6

    if (SHIPS_ShearMagNum > 0):
        fig.arrow(0,0,X_SHIPS_Shr*arrscl,Y_SHIPS_Shr*arrscl,width=arrwdt, \
            head_width=arrheadwdt,head_length=arrheadlgt,fc='k',ec='k')
        fig.text(0.63, 0.97, "SHIPS Shear (SHDC):\n"+'{0:.1f}'.format(SHIPS_ShearMagNum)+" kt"+ \
            " @ "+'{0:.0f}'.format(SHIPS_ShearDirMetNum)+" deg",verticalalignment='top', \
            horizontalalignment='left',transform=fig.transAxes,color='k', fontsize=14,fontweight='bold')

    fig.text(0.42, 0.92,"X: w>6 m/s",verticalalignment='top',horizontalalignment='left', \
        transform=fig.transAxes,color='k',fontsize=14,fontweight='bold')

    fig.text(0.05,0.02,"Mean Center Fix:\n"+'{0:.2f}'.format(olat_mean)+ \
        ', {0:.2f}'.format(olon_mean),verticalalignment='bottom',horizontalalignment='left', \
        transform=fig.transAxes,color='k',fontsize=14,fontweight='bold')

    densval = 4
    fig3=fig.streamplot(Y,X,mulevel,mvlevel,density=densval,color='k',linewidth=2, \
        arrowstyle='->',arrowsize=2)

    fig3=fig.imshow(X,extent=(-domsz,domsz,-domsz,domsz),alpha=0)
    divider=make_axes_locatable(fig)
    cax=divider.append_axes("right",size="5%",pad=0.05)
    cbar_l=plt.colorbar(fig2,cax=cax,ticks=ticks,norm=norm,drawedges=False)
    cbar_l.ax.tick_params(labelsize=14)
    #cbar_l.outline.set_color('black')
    #cbar_l.outline.set_linewidth(1)
    #cbar_l.dividers.set_color('black')
    #cbar_l.dividers.set_linewidth(1)

    w_thresh_loc = (convec == 1)
    if(np.any(w_thresh_loc == True)):
        fig4=fig.scatter(X[w_thresh_loc],Y[w_thresh_loc],s=25,c='k',marker='x')

    lvl_mark = int(level-kmin)
    fig5=fig.plot(x[ic[flg==0]],y[jc[flg==0]],marker='o',ms=8,mec='0.3',mew=1,mfc='g',linewidth=2,color='0.3')
    if (flg[lvl_mark] == 0):
        fig5=fig.plot(x[ic[lvl_mark]],y[jc[lvl_mark]],marker='o',ms=8,mew=3,mec='k',mfc='g',color='0.3')

    # Include label on uppermost good center
    kk_up = np.nan; k_up = np.nan
    for kk in range(len(flg)-1,-1,-1):
        if (flg[kk] == 0):
            kk_up = kk
            k_up = kmin+kk
            break    
    if np.isfinite(kk_up):
        fig.annotate(str(int(z[k_up]))+" km",xy=(x[ic[kk_up]]+1,y[jc[kk_up]]-4), \
            xytext=(x[ic[kk_up]]+1,y[jc[kk_up]]-4),color='k',fontsize=14,fontweight='bold')
 
    ####### Right Panel
 
    if (fieldname == "ws"):
        titlea = ' Storm-relative WS (kt) at '
    elif (fieldname == "vort"):
        titlea = ' Storm-rel. Flow; Vorticity ($\mathregular{10^{-4}}$ $\mathregular{s^{-1}}$) at '

    level = np.argmin(abs(z-5.0))
    mulevel = np.transpose(u[:,:,level])
    mvlevel = np.transpose(v[:,:,level])

    if (fieldname == "ws"):
        mdatalevel = ws[:,:,level]*ms2kt
    elif (fieldname =="vort"):
        mdatalevel = vort[:,:,level]*1E4
    
    titleb = str(z[level])

    #plottitle=fltname+" ("+stmname.strip()+")\n"+timedisplaystart+" to "+timedisplayend+ \
    #   " UTC\n"+titlea+titleb+" km"
    plottitle=titlestring2

    fig = figa.add_subplot(122)

    if (fieldname == "ws"):
        n_bin = 256
        cmap_name = 'Tidbits'
        cmap_r = LinearSegmentedColormap.from_list(cmap_name, normTidbits_colors, N=n_bin)
        levs = [0,7,10,13,16,19,22,25,28,31,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64, \
            69.333,74.666,80,85.333,90.666,96,100.666,105.333,110,115,120,125,130,135,140, \
            145,150,155,160] # kt
        ticks = [7,16,25,34,40,46,52,58,64,80,96,110,125,140,155] # kt
    elif (fieldname == "vort"):
        color_data = np.genfromtxt('/home/Andrew.Hazelton/python/colormaps/bluewhitered_vorticity.txt')
        cmap_r = mcolors.ListedColormap(color_data)
        levs = [-500,-100,-50,-30,-10,-2,2,10,30,50,100,500]
        ticks = [-500,-100,-50,-30,-10,-2,2,10,30,50,100,500]

    norm = mcolors.BoundaryNorm(levs,256)

    fig2 = fig.contourf(X,Y,mdatalevel,levels=levs,norm=norm,cmap=cmap_r) # kt
    fig.set_title(plottitle,fontsize=18)

    fig.grid()
    fig.set_xlabel("East-West distance (km)",fontsize=18)
    fig.set_ylabel("North-South distance (km)",fontsize=18)
    fig.tick_params(axis='both',which='major',labelsize=14)

    domsz = 80
    plt.xlim(-domsz, domsz)
    plt.ylim(-domsz, domsz)

    arrscl=0.3; arrwdt=2; arrheadwdt=6; arrheadlgt=6

    if (SHIPS_ShearMagNum > 0):
        fig.arrow(0,0,X_SHIPS_Shr*arrscl,Y_SHIPS_Shr*arrscl,width=arrwdt, \
            head_width=arrheadwdt,head_length=arrheadlgt,fc='k',ec='k')
        fig.text(0.63, 0.97, "SHIPS Shear (SHDC):\n"+'{0:.1f}'.format(SHIPS_ShearMagNum)+" kt"+ \
            " @ "+'{0:.0f}'.format(SHIPS_ShearDirMetNum)+" deg",verticalalignment='top', \
            horizontalalignment='left',transform=fig.transAxes,color='k', fontsize=14,fontweight='bold')

    fig.text(0.42, 0.92,"X: w>6 m/s",verticalalignment='top',horizontalalignment='left', \
        transform=fig.transAxes,color='k',fontsize=14,fontweight='bold')

    fig.text(0.05,0.02,"Mean Center Fix:\n"+'{0:.2f}'.format(olat_mean)+ \
        ', {0:.2f}'.format(olon_mean),verticalalignment='bottom',horizontalalignment='left', \
        transform=fig.transAxes,color='k',fontsize=14,fontweight='bold')

    densval = 4
    fig3=fig.streamplot(Y,X,mulevel,mvlevel,density=densval,color='k',linewidth=2, \
        arrowstyle='->',arrowsize=2)

    fig3=fig.imshow(X,extent=(-domsz,domsz,-domsz,domsz),alpha=0)
    divider=make_axes_locatable(fig)
    cax=divider.append_axes("right",size="5%",pad=0.05)
    cbar_r=plt.colorbar(fig2,cax=cax,ticks=ticks,norm=norm,drawedges=False)
    cbar_r.ax.tick_params(labelsize=14)
    #cbar_r.outline.set_color('black')
    #cbar_r.outline.set_linewidth(1)
    #cbar_r.dividers.set_color('black')
    #cbar_r.dividers.set_linewidth(1)

    w_thresh_loc = (convec == 1)
    if(np.any(w_thresh_loc == True)):
        fig4=fig.scatter(X[w_thresh_loc],Y[w_thresh_loc],s=25,c='k',marker='x')

    lvl_mark = int(level-kmin)
    fig5=fig.plot(x[ic[flg==0]],y[jc[flg==0]],marker='o',ms=8,mec='0.3',mew=1,mfc='g',linewidth=2,color='0.3')
    if (flg[lvl_mark] == 0):
        fig5=fig.plot(x[ic[lvl_mark]],y[jc[lvl_mark]],marker='o',ms=8,mew=3,mec='k',mfc='g',color='0.3')

    # Include label on uppermost good center
    kk_up = np.nan; k_up = np.nan
    for kk in range(len(flg)-1,-1,-1):
        if (flg[kk] == 0):
            kk_up = kk
            k_up = kmin+kk
            break    
    if np.isfinite(kk_up):
        fig.annotate(str(int(z[k_up]))+" km",xy=(x[ic[kk_up]]+1,y[jc[kk_up]]-4), \
            xytext=(x[ic[kk_up]]+1,y[jc[kk_up]]-4),color='k',fontsize=14,fontweight='bold')

    ####### Final Adjustments

    figa.subplots_adjust(wspace=.25)

    ####### Save Figure to File
    #figfile = fltname+"_"+stmname+"_"+timedisplaystart[0:4]+ \
    #    "_"+timedisplayend[0:4]+"_"+fieldname+"_tilt.png"
    plt.savefig(figfile)

    plt.clf()



