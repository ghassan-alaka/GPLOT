import os
from py3grads import Grads

##############################
def read_grads(ga, name=None, idx=None):
	"""Read data given a GrADs object
	@param ga:   GrADs object
	@kwarg name: variable name string
	@kwarg idx:  variable index
	"""
	print(f'MSG: Using GrADs to read {name}')
	data = ga.exp(name)

	return data, name, idx


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

