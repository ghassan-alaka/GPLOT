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
def update_plottedfile(OFILE, IFILE):
        """Update the GPLOT PlottedFiles file to mark a file as processed.
        @param OFILE: the plotted file path as a string
        @param IFILE: the model output file that was processed
        """
        os.system(f'sed -i \'/{os.path.basename(IFILE)}/d\' {OFILE}')
        os.system(f'echo "{IFILE} 1" >> {OFILE}')
        os.system(f'sort -u {OFILE} > {OFILE}.TMP')
        os.system(f'mv {OFILE}.TMP {OFILE}')

