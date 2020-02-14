#!python


# Import necessary modules
import os
import sys
import f90nml
#import shutil


"""
package namelist: Python functionary for namelists.

GPLOT is the Graphical Post-processed Locus for Output of Tropical cyclones.
It consists of independent modules used to create graphics targeted toward
presentations, posters, publications, and basic research related to tropical
cyclones. To learn more, please download the code from GitHub:
  https://github.com/ghassan-alaka/GPLOT

To learn more about the AOML/Hurricane Research Division, please visit:
  https://www.aoml.noaa.gov/our-research/hurricane-research-division/

Created By:   Ghassan Alaka Jr.
Date Created: Febrary 12, 2020

Example call: python ./namelist.py

Modification Log:


"""

__version__ = '0.1.0';


class Namelist_Basic:
    """This class provides basic information about a namelist

    """

    def __init__(self, NML_DIR, EXPT, NO_FAIL=True, **kwargs):

        # Get information about the namelist file & its location
        self.EXPT = EXPT
        #self.FILE = 'namelist.master.'+EXPT
        self.FILE = kwargs.get('NML_FILE','NEW.namelist.master.'+EXPT)
        self.DIR = NML_DIR
        self.PATH = self.DIR+self.FILE

        # Check if the namelist actually exists
        if os.path.exists(self.PATH):
            print("MSG: Found this namelist --> "+self.PATH)
        else:
            print("ERROR: Namelist doesn't exist --> "+self.PATH)
            if NO_FAIL:
                sys.exit(2)

        # Read the namelist into a dictionary
        self.DICT = self.nml_read_f90()
        self.KEYS = self.nml_get_keys(self.DICT)
        self.SUBKEYS = []
        for KEY in self.DICT.keys():
            SUBDICT = dict(self.DICT[KEY])
            VALUE = self.nml_get_keys(SUBDICT)
            self.SUBKEYS.append(VALUE)

        # Determine what modules to run
        KEY1 = kwargs.get('MODULE_KEY',"modules")
        self.DO_MAPS = self.nml_get_value(KEY1,'do_maps',False)
        self.DO_STATS = self.nml_get_value(KEY1,'do_stats',False)
        self.DO_SHIPS = self.nml_get_value(KEY1,'do_ships',False)
        self.DO_POLAR = self.nml_get_value(KEY1,'do_polar',False)

        # GPLOT Module submisiong check
        self.MOD_ID = []
        if self.DO_MAPS:
            self.MOD_ID.append("maps");
        if self.DO_STATS:
            self.MOD_ID.append("stats");
        if self.DO_SHIPS:
            self.MOD_ID.append("ships");
        if self.DO_POLAR:
            self.MOD_ID.append("polar");

        # Get system and project information for batch submission
        KEY2 = kwargs.get('BATCH_KEY',"system")
        self.BATCH_MODE = self.nml_get_value(KEY2,'batch_mode','background')
        print("MSG: Batch submission mode --> "+self.BATCH_MODE)
        self.SYS_ENV = self.nml_get_value(KEY2,'sys_env','jet')
        self.BATCH_DFLTS = NML_DIR+"batch.defaults."+self.SYS_ENV.lower()
        if self.BATCH_MODE.lower() == 'sbatch':
            self.CPU_ACCT = self.nml_get_value(KEY2,'cpu_acct','MISSING')
            if self.CPU_ACCT == "MISSING":
                self.CPU_ACCT = self.nml_get_opt(self.BATCH_DFLTS,'CPU_ACCT')
            self.PARTITION = ','.join(self.nml_get_value(KEY2,'partition','MISSING'))
            if self.PARTITION == "MISSING":
                self.PARTITION = self.nml_get_opt(self.BATCH_DFLTS,'PARTITION')
            self.QOS = self.nml_get_value(KEY2,'qos','batch')
            if self.QOS == "MISSING":
                self.QOS = self.nml_get_opt(self.BATCH_DFLTS,'QOS')
        else:
            self.CPU_ACCT, self.PARTITION, self.QOS = (), (), ()


    def nml_read_f90(self):
        """Read a fortran-style namelist as a dictionary using the
           f90nml module.
        @return nml: the namelist dictionary
        """
        nml = f90nml.read(self.PATH)
        return(nml);
    
    
    def nml_get_keys(self,DICT):
        """Return the namelist keys as a list.
        @return KEYS: the list of namelist keys
        """
        KEYS = list(DICT.keys())
        return(KEYS);
    
    
    def nml_get_value(self,KEY,VAL,DEF="MISSING"):
        """Return the specific value of a namelist entry.
        @param KEY: the specific namelist key as a string
        @param VAL: the specific value within KEY as a string
        @param DEF: the default value if KEY.VAL does not exist
        @return VALUE: the specific value of a namelist entry
        """

        if KEY in self.KEYS:
            VALUE = self.DICT[KEY].get(VAL,DEF)
        else:
            VALUE = DEF
        return(VALUE);
    
    
    def nml_get_all_values_for_key(self,KEY):
        """Return a list of values for a specific namelist key.
        @param KEY: the specific namelist key
        @return VALUES: the list of values for a specific namelist key
        """
        VALUES = list(self.DICT[KEY])
        return(VALUES);
    
    
    def nml_get_opt(self,NML,OPT):
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


