"""
Physical and numerical constants for GPLOT.

Replaces load_constants() from GPLOT_util.ncl.
"""

import math

# Gravitational acceleration [m s-2]
g = 9.80665

# Universal gravitational constant [N m-2 kg-2]
G_UNIV = 6.67e-11

# Pi
pi = math.pi

# Speed of light [m s-1]
c_light = 2.998e8

# Specific gas constant for dry air [J K-1 kg-1]
R_d = 287.058

# Specific heat at constant pressure [J K-1 kg-1]
c_p = 1004.0

# Specific heat at constant volume [J K-1 kg-1]
c_v = 717.0

# Dry adiabatic lapse rate [K m-1]
dalr = g / c_p

# Degrees-to-radians conversion [rad deg-1]
d2r = pi / 180.0

# Radians-to-degrees conversion [deg rad-1]
r2d = 180.0 / pi

# Earth radius [m]
r_earth = 6.371e6

# Earth angular rotation rate [rad s-1]
omega = 7.27e-5

# Poisson constant (R/c_p)
kappa = R_d / c_p

# Stefan-Boltzmann constant [J s-1 m-2 K-4]
sigma = 5.67e-8

# Air density at sea level [kg m-3]
rho_0 = 1.25

# Meters-per-second to knots conversion
ms2kts = 1.94384449

# Knots to meters-per-second conversion
kts2ms = 1.0 / ms2kts

# Nautical miles to km
nm2km = 1.852

# Km to nautical miles
km2nm = 1.0 / nm2km

# Degrees latitude to km (approximate)
deg2km = 111.2

# Fill/missing values (matching NCL conventions)
FILL_FLOAT = 9.96921e36
FILL_INT = -2147483647
FILL_FLOAT2 = 1.0e20
FILL_STR = "missing"

# Baseline date string for time conversions
TIME_UNITS = "hours since 1970-01-01 00:00:00"

# ---------------------------------------------------------------------------
# Seawater / ocean physical constants
# ---------------------------------------------------------------------------

# Specific heat capacity of seawater [J kg-1 K-1]
# Note: HYCOM and MOM6 use 3990 J kg-1 K-1 internally; observational
# (OHC) code historically uses 4178 J kg-1 K-1 (pure water ~25°C).
cp_sw = 4178.0           # observational OHC default
cp_sw_hycom = 3990.0     # HYCOM/MOM6 integration
cp_sw_mom6 = 3990.0

# Reference seawater density [kg m-3]
rho_sw = 1026.0          # observational OHC default
rho_sw_hycom = 1025.0    # HYCOM/MOM6 integration

# Unit conversion: J/m² -> kJ/cm² (1e-7)
kJcm2_per_Jm2 = 1e-7

# OHC integration temperature threshold [°C]
OHC_T_THRESHOLD = 26.0
