
import metpy
from scipy import interpolate


##############################
def interp_to_isosurface(hgt, varPrs, lev, idx, ivar, verbose=False):
        """Interpolate input data (varPrs) from pressure levels to a height
        level (i.e., isosurface). Return 2D data (lat, lon) along with
        the height level and the height level index.
        @param hgt:     3D height data on pressure levels (lev, lat, lon)
        @param varPrs:  3D input data on pressure levels (lev, lat, lon)
        @param lev:     Height level to interpolate to
        @param idx:     Level index
        @param ivar:    Variable index
        @kwarg verbose: Logical to determine level of verbosity
        """
        if verbose:  print(f'MSG: Interpolating to the {int(lev)}-m isosurface for var{ivar} - {datetime.datetime.now()}')
        return metpy.interpolate.interpolate_to_isosurface(hgt, varPrs, lev), lev, idx


##############################
def interp_to_polarcylindrical(varIn, lev, x, y, xi, yi, idx, ivar, verbose=False):
        """Interpolate 2D input data (varIn) from cartesian coordinates to polar
	cylindrical coordinates for a given height level.
	@param varIn:   2D input data on a standard lat/lon or x/y grid
	@param lev:     Vertical level
	@param x:	Actual distance in the x-direction
	@param y:       Actual distance in the y-direction
	@param xi:      X locations that correspond to polar coordinates
	@param yi:      Y locations that correspond to polar coordinates
	@param idx:     Index of the current level
	@param ivar:    Index of the current input variable
	@kwarg verbose: Logical to control verbosity
        """
        if verbose:  print(f'MSG: Interpolating to polar cylindrical coordinates for level {int(lev)} for var{ivar} - {datetime.datetime.now()}')
        varTmp = interpolate.RegularGridInterpolator((y, x), varIn)
        varPolar = varTmp((yi, xi), method='linear')
        return varPolar, lev, idx


