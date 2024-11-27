import concurrent.futures
import numpy as np
import modules.interp as interp
import modules.io_extra as io

##############################
def multiprocess_polar_vars(x, y, xi, yi, varList=None, levels=None):
	"""
	"""
	if varList is None:
		print('ERROR: List of variables to be processed must be provided.')
		sys.exit(1)
	elif levels is None:
		print('ERROR: List of height levels (m) must be provided.')
		sys.exit(1)

	# Initiate thread pool to perform tasks for all levels in parallel.
	allstacks, indices = [], []
	with concurrent.futures.ThreadPoolExecutor(max_workers=4) as executor:
		results = [executor.submit(multiprocess_polar_interp, var, x, y, xi, yi, levels, idx) for idx,var in enumerate(varList)]
		for job in concurrent.futures.as_completed(results):
			(data,ix) = job.result()
			#print(ix, data.shape, type(data))
			allstacks.append(data), indices.append(ix)
	executor.shutdown()

	indices_sorted = np.argsort(np.array(indices))
	varPolarList = np.stack(allstacks)[indices_sorted,:,:,:]

	return varPolarList



##############################
def multiprocess_polar_interp(varIn, x, y, xi, yi, levels, idx):
	"""
	"""
	# Initiate thread pool to perform tasks for all levels in parallel.
	allstacks, indices = [], []
	with concurrent.futures.ThreadPoolExecutor(max_workers=8) as executor:
		results = [executor.submit(interp.interp_to_polarcylindrical, var, lev, x, y, xi, yi, iii, idx) for (iii,(var,lev)) in enumerate(zip(varIn,levels))]
		for job in concurrent.futures.as_completed(results):
			(data,lll,ix) = job.result()
			#print(f'MSG: Finished interpolating to polar cylindrical coordinates for level {int(lll)} for var{idx} -  {datetime.datetime.now()}')
			allstacks.append(data), indices.append(ix)
	executor.shutdown()

	indices_sorted = np.argsort(np.array(indices))
	varPolar = np.transpose(np.stack(allstacks)[indices_sorted,:,:], (1,2,0))

	return varPolar, idx


##############################
def multiprocess_height_interp(hgt=None, varPrs=None, levels=None, idx=0):
	"""Multiprocessing function to parallelize interpolation from
	pressure surfaces to height surfaces.
	@kwarg hgt:    3D height data on pressure levels (lev, lat, lon)
	@kwarg varPrs: 3D input data on pressure levels (lev, lat, lon)
	@kwarg levels: 1D array/list of height levels in meters
	@kwarg idx:    Variable index
	"""

	# Check that required variables exist
	if hgt is None:
		print(f'ERROR: Height data must be defined')
		sys.exit(1)
	elif varPrs is None:
		print('ERROR: Data on pressure levels must be provided.')
		sys.exit(1)
	elif levels is None:
		print('ERROR: List of height levels (m) must be provided.')
		sys.exit(1)

	# Initiate thread pool to perform tasks for all levels in parallel.
	allstacks, indices = [], []
	with concurrent.futures.ThreadPoolExecutor(max_workers=8) as executor:
		results = [executor.submit(interp.interp_to_isosurface, hgt, varPrs, lev, iii, idx) for (iii,lev) in enumerate(levels)]
		for job in concurrent.futures.as_completed(results):
			(data,lll,ix) = job.result()
			#print(f'MSG: Finished interpolating to the {int(lll)}-m isosurface for var{idx} -  {datetime.datetime.now()}')
			allstacks.append(data), indices.append(ix)
	executor.shutdown()

	indices_sorted = np.argsort(np.array(indices))
	varHgt = np.transpose(np.stack(allstacks)[indices_sorted,:,:], (1,2,0))

	return varHgt, idx


##############################
def multiprocess_height_vars(hgt=None, varList=None, varNames=None, levels=None):
	"""Multiprocessing function to parallelize similar processing for
	multiple variables.
	@kwarg hgt:      3D height data on pressure levels (lev, lat, lon)
	@kwarg varList:  List of 3D data on pressure levels for each variable (lev, lat, lon)
	@kwarg varNames: List of variable names (not required)
	@kwarg levels:   1D array/list of height levels in meters
	"""

	# Check that required variables exist
	if hgt is None:
		print(f'ERROR: Height data must be defined')
		sys.exit(1)
	elif varList is None:
		print('ERROR: List of variables to be processed must be provided.')
		sys.exit(1)
	elif levels is None:
		print('ERROR: List of height levels (m) must be provided.')
		sys.exit(1)

	# Initiate thread pool to perform tasks for all levels in parallel.
	allstacks, indices = [], []
	with concurrent.futures.ThreadPoolExecutor(max_workers=4) as executor:
		results = [executor.submit(multiprocess_height_interp, hgt, var, levels, idx) for idx,var in enumerate(varList)]
		for job in concurrent.futures.as_completed(results):
			(data,ix) = job.result()
			#print(ix, data.shape, type(data))
			allstacks.append(data), indices.append(ix)
	executor.shutdown()

	indices_sorted = np.argsort(np.array(indices))
	varHgtList = np.stack(allstacks)[indices_sorted,:,:,:]

	return varHgtList


##############################
def multiprocess_prs_vars(ga=None, names=None):

	# Initiate thread pool to perform tasks for all levels in parallel.
	allstacks, indices = [], []
	with concurrent.futures.ThreadPoolExecutor(max_workers=1) as executor:
		results = [executor.submit(io.read_grads, ga, name, idx) for (idx,name) in enumerate(names)]
		for job in concurrent.futures.as_completed(results):
			(data,N,ix) = job.result()
			print(f'MSG: Finished reading {N}')
			allstacks.append(data), indices.append(ix)
	executor.shutdown()

	indices_sorted = np.argsort(np.array(indices))
	varPrsList = np.stack(allstacks)[indices_sorted,:,:,:]

	return varPrsList

