
import numpy as np
import matplotlib.pyplot as plt


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
def axes_radhgt(ax, xmax, xmin, ymax=18, ymin=0):
        """Set up common axes attributes for wavenumber graphics.
        @param ax:   the axes object
        @param xmax: max value of x-axis
        @param xmin: min value of x-axis
        @kwarg ymax: max value of y-axis
        @kwarg ymin: min value of y-axis
        """
        xticks = np.linspace(xmin,xmax,11)
        yticks = np.linspace(ymin,ymax,10)

        ax.set_xlim(xmin, xmax)
        ax.set_xticks(xticks)
        ax.set_xticklabels([str(int(x)) for x in xticks], fontsize=24)
        ax.set_xlabel('Radius (km)', fontsize=24)

        ax.set_ylim(ymin,ymax)
        ax.set_yticks(yticks)
        ax.set_yticklabels([str(int(x)) for x in yticks], fontsize=24)
        ax.set_ylabel('Height (km)', fontsize=24)

        ax.grid()

        return ax
