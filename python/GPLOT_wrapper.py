#!python

# Import necessary modules
import os
import sys
import subprocess
import f90nml
import shutil

"""
GPLOT Wrapper: Main submission script for all GPLOT child processes for a
               set of experiments.

GPLOT is the Graphical Post-processed Locus for Output of Tropical cyclones.
It consists of independent modules used to create graphics targeted toward
presentations, posters, publications, and basic research related to tropical
cyclones. To learn more, please download the code from GitHub:
  https://github.com/ghassan-alaka/GPLOT

To learn more about the AOML/Hurricane Research Division, please visit:
  https://www.aoml.noaa.gov/our-research/hurricane-research-division/

Created By:   Ghassan Alaka Jr.
Assisted By:
Date Created: Febrary 6, 2020

Example call: python ./GPLOT_wrapper.py

Modification Log:


"""

__version__ = '0.1.0';


# Move to namelist.py
def nml_read_f90(nml_file):
    """Read a fortran-style namelist using the f90nml module.
    @param nml_file: the namelist full path
    """
    nml = f90nml.read(nml_file)
    return(nml);


# Move to namelist.py
def nml_get_keys(NML):
    """Return the namelist keys as a list.
    @param NML: the namelist dictionary
    @return KEYS: the list of namelist keys
    """
    KEYS = list(NML.keys())
    return(KEYS);


# Move to namelist.py
def nml_get_value(NML,KEY,VAL,DEF="MISSING"):
    """Return the specific value of a namelist entry.
    @params NML: the namelist dictionary
    @param KEY: the specific namelist key
    @param VAL: the specific value within KEY
    @param DEF: the default value if KEY.VAL does not exist
    @return VALUE: the specific value of a namelist entry
    """
    VALUE = NML[KEY].get(VAL,DEF)
    return(VALUE);


# Move to namelist.py
def nml_get_all_values_for_key(NML,KEY):
    """Return a list of values for a specific namelist key.
    @param NML: the namelist dictionary
    @param KEY: the specific namelist key
    @return VALUES: the list of values for a specific namelist key
    """
    VALUES = list(NML[KEY])
    return(VALUES);


def gplot_check(gvar='GPLOT_DIR'):
    """Check if the GPLOT_DIR variable is defined in the user environment.
    @param gvar: the environmental variable, default is 'GPLOT_DIR'
    """
    GPLOT_DIR = os.environ[gvar]
    if not GPLOT_DIR:
        GPLOT_DIR = "/home/"+os.environ['USER']+"/GPLOT"
        print("WARNING: Variable GPLOT_DIR not found in environment.")
        print("MSG: Setting GPLOT_DIR --> "+GPLOT_DIR)
    else:
        print("MSG: GPLOT_DIR found in environment --> "+GPLOT_DIR)

    if not os.path.exists(GPLOT_DIR):
        print("ERROR: GPLOT_DIR not found. Can't continue.")
        sys.exit(2)

    return(GPLOT_DIR);


def spawn_prep(MOD,EXPT,SFILE,LOGDIR,CPU_ACCT,PARTITION,QOS):
    """Prepare the slurm header for a given spawn file
    @param MOD:       the GPLOT module ID
    @param EXPT:      the experiment name, typically from the master namelist file name
    @param SFILE:     the spawn file full path specific to the current experiment
    @param LOGDIR:    the GPLOT log directory
    @param CPU_ACCT:  the batch cpu account for the runs
    @param PARTITION: the batch partition(s) for the runs
    @param QOS:       the quality of service for the runs
    """
    subprocess.call(["sed -i 's/^#SBATCH --job-name=.*/#SBATCH --job-name=\"GPLOT.spawn_"+MOD+"."+EXPT+"\"/g' "+SFILE], shell=True)
    subprocess.call(["sed -i 's%^#SBATCH --output=.*%#SBATCH --output=\""+LOGDIR+"spawn_"+MOD+"."+EXPT+".out\"%g' "+SFILE], shell=True)
    subprocess.call(["sed -i 's%^#SBATCH --error=.*%#SBATCH --error=\""+LOGDIR+"spawn_"+MOD+"."+EXPT+".err\"%g' "+SFILE], shell=True)
    subprocess.call(["sed -i 's/^#SBATCH --account=.*/#SBATCH --account="+CPU_ACCT+"/g' "+SFILE], shell=True)
    subprocess.call(["sed -i 's/^#SBATCH --partition=.*/#SBATCH --partition="+PARTITION+"/g' "+SFILE], shell=True)
    subprocess.call(["sed -i 's/^#SBATCH --qos=.*/#SBATCH --qos="+QOS+"/g' "+SFILE], shell=True)


# Move to namelist.py
def nml_get_opt(NML,OPT):
    """Read the namelist and search for a specific option.
    @param NML:  the namelist
    @param OPT:  the namelist option, must begin the line
    @return VAR: the value of the namelist option, could be MISSING
    """
    VAR = "MISSING"
    with open(NML,"r") as f:
        for line in f.readlines():
            if line.strip().startswith(OPT):
                VAR = line.split("=")[1].strip()
                break
    if VAR == "MISSING":
        print("WARNING: Could not find "+OPT+" in "+NML)
    return(VAR);


def exec_subprocess(shchk=True,*args):
    """Call a shell subprocess.
    @param shchk: check if the subprocess.call shell option should be True/False
    @param args:  at least 1 string containing information on what to run
    """
    # Check that at least 1 argument has been supplied.
    if len(args[:]) < 1:
        print("ERROR: subprocess needs at least 1 argument")
        sys.exit(2)

    # Run the subprocess according to the shchk option.
    if shchk:
        subprocess.call([args[0]], shell=True)
    else:
        subprocess.call([','.join(args[:])], shell=False)



def main():

    # GPLOT_DIR must be set in the environment
    GPLOT_DIR = gplot_check()
    
    
    # Create variables for GPLOT subdirectories.
    NMLDIR=GPLOT_DIR+"/nmlist/"
    BATCHDIR=GPLOT_DIR+"/batch/"
    LOGDIR=GPLOT_DIR+"/log/"
    

    # Get the input arguments as independent experiments.    
    if len(sys.argv) <= 1:
    	print("ERROR: Need at least 1 argument.")
    	sys.exit(2)
    EXPTS = sys.argv[1:]
    print("MSG: Found these experiments --> "+repr(EXPTS))


    # Looping over all experiments. Could be one.
    for EXPT in EXPTS:
    
        # Construct the namelist file name
        #NML_FILE = '/lfs1/projects/hur-aoml/Ghassan.Alaka/pyGPLOT/nmlist/NEW.namelist.master'
        NML_FILE = 'NEW.namelist.master.'+EXPT
        
        
        # Check if the namelist actually exists
        if os.path.exists(NMLDIR+NML_FILE):
            print("MSG: Found this namelist --> "+NMLDIR+NML_FILE)
        else:
            print("ERROR: Namelist doesn't exist --> "+NMLDIR+NML_FILE)
            continue

        # Read the namelist into a dictionary
        NML = nml_read_f90(NMLDIR+NML_FILE)
        #print(list(nml.keys()))
        #for key in nml.keys():
        #    print(key)
        #    print(nml[key])
        #sys.exit()
        
        
        # Determine what modules to run
        DO_MAPS = nml_get_value(NML,'modules','do_maps',False)
        DO_STATS = nml_get_value(NML,'modules','do_stats',False)
        DO_SHIPS = nml_get_value(NML,'modules','do_ships',False)
        DO_POLAR = nml_get_value(NML,'modules','do_polar',False)
        
        
        # Get the submission mode from the namelist
        BATCH_MODE = nml_get_value(NML,'system','batch_mode','background')
        print("MSG: Batch submission mode --> "+BATCH_MODE)
        
        
        # Get the system environment and project account for batch submissions ($BATCH_MODE="SBATCH")
        SYS_ENV = nml_get_value(NML,'system','sys_env','jet')
        BATCH_DFLTS = NMLDIR+"batch.defaults."+SYS_ENV.lower()
        if BATCH_MODE.lower() == 'sbatch':
            CPU_ACCT = nml_get_value(NML,'system','cpu_acct','MISSING')
            if CPU_ACCT == "MISSING":
                CPU_ACCT = nml_get_opt(BATCH_DFLTS,'CPU_ACCT')
            PARTITION = ','.join(nml_get_value(NML,'system','partition','MISSING'))
            if PARTITION == "MISSING":
                PARTITION = nml_get_opt(BATCH_DFLTS,'PARTITION')
            QOS = nml_get_value(NML,'system','qos','batch')
            if QOS == "MISSING":
                QOS = nml_get_opt(BATCH_DFLTS,'QOS')
        else:
            CPU_ACCT, PARTITION, QOS = (), (), ()
       
 
        # GPLOT MODULE SUBMISSION CHECK
        MOD_ID = []
        if DO_MAPS:
            print("MSG: Submission for GPLOT Module MAPS turned on.")
            MOD_ID.append("maps");
        if DO_STATS:
            print("MSG: Submission for GPLOT Module STATS turned on.")
            MOD_ID.append("stats");
        if DO_SHIPS:
            print("MSG: Submission for GPLOT Module SHIPS turned on.")
            MOD_ID.append("ships");
        if DO_POLAR:
            print("MSG: Submission for GPLOT Module POLAR turned on.")
            MOD_ID.append("polar");


        # Loop over all MOD_IDs and submit spawn jobs.
        for MOD in MOD_ID:
            print("MSG: Working on submission for GPLOT Module "+MOD.upper())
            SPAWNFILE1 = BATCHDIR+"spawn_"+MOD+".generic.sh"
            SPAWNFILE2 = BATCHDIR+"spawn_"+MOD+"."+EXPT+".sh"
            SPAWNLOG = LOGDIR+"spawn_"+MOD+"."+EXPT+".log"
            
            shutil.copy2(SPAWNFILE1,SPAWNFILE2)
            if BATCH_MODE.lower() == 'sbatch':
                spawn_prep(MOD,EXPT,SPAWNFILE2,LOGDIR,CPU_ACCT,PARTITION,QOS)
                print("MSG: Submitting "+SPAWNFILE2+" to the slurm batch scheduler.")
                print("sbatch "+SPAWNFILE2+" "+NML_FILE+" > "+SPAWNLOG)
                exec_subprocess(True,"sbatch "+SPAWNFILE2+" "+NML_FILE+" > "+SPAWNLOG)
            elif BATCH_MODE.lower() == 'foreground':
                print("MSG: Submitting "+SPAWNFILE2+" to the foreground.")
                exec_subprocess(True,SPAWNFILE2+" "+NML_FILE+" > "+SPAWNLOG)
            else:
                print("MSG: Submitting "+SPAWNFILE2+" to the background.")
                exec_subprocess(True,SPAWNFILE2+" "+NML_FILE+" > "+SPAWNLOG+" &")
        
        



if __name__ == '__main__':
    main()
