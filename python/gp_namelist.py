#!/usr/bin/env python


# Import necessary modules
import os, sys
import pandas as pd
from pandas import DataFrame as df
import f90nml
#import shutil

# Import GPLOT modules
import gp_database as db
import gp_util as gpu


"""
GPLOT Package namelist
Python functionality for namelists.

GPLOT is the Graphical Post-processed Locus for Output of Tropical cyclones.
It consists of independent modules used to create graphics targeted toward
presentations, posters, publications, and basic research related to tropical
cyclones. To learn more, please download the code from GitHub:
  https://github.com/ghassan-alaka/GPLOT

To learn more about the AOML/Hurricane Research Division, please visit:
  https://www.aoml.noaa.gov/our-research/hurricane-research-division/

Created By:    Ghassan Alaka Jr.
Modified By:
Date Created:  February 12, 2020
Last Modified: February 27, 2020

Example call: For Internal Calls Only

Modification Log:
2020-Feb-27: GJA changed the file name from namelist.py to gp_namelist.py

"""

__version__ = '0.2.0';



###########################################################
###########################################################
class Namelist_Launch:
    """This class provides namelist information retrieved from
       the fortran-style namelist.

    """

    ###########################################################
    def __init__(self, DIR, EXPT, NO_FAIL=True, **kwargs):
        """This runs everytime Namelist_Launch is assigned.
        @param self:    the instance of the class (no need to pass this)
        @param DIR:     the directories object
        @param EXPT:    the experiment name
        @kwarg NO_FAIL: logical to not allow continuation if the namelist is not found.
        @kwargs:        other keyword arguments may be passed
        """

        # Define basic information like paths, the namelist, and the experiment.
        self.EXPT = EXPT
        self.TBL = "namelist_master"
        #self.NMLFILE = 'namelist.master.'+EXPT
        self.NMLFILE = kwargs.get('NML_FILE','NEW.namelist.master.'+EXPT)
        self.NMLDIR = DIR.NMLDIR
        self.NMLPATH = self.NMLDIR+self.NMLFILE
        self.GPLOT_DIR = DIR.GPLOT_DIR
        self.BATCHDIR = DIR.BATCHDIR
        self.LOGDIR = DIR.LOGDIR

        # Check if the namelist actually exists
        if os.path.exists(self.NMLPATH):
            print("MSG: Found this namelist --> "+self.NMLPATH)
        else:
            print("ERROR: Namelist doesn't exist --> "+self.NMLPATH)
            if NO_FAIL:
                sys.exit(2)

        # Read the namelist into a dictionary
        self.DICT = self.nml_read_f90()
        self.KEYS = self.nml_get_keys(self.DICT)
        self.SUBKEYS = []
        for K in self.DICT.keys():
            SUBDICT = dict(self.DICT[K])
            VALUE = self.nml_get_keys(SUBDICT)
            self.SUBKEYS.append(VALUE)

        # Determine what modules to run
        KEY = kwargs.get('MODULE_KEY',"modules")
        self.DO_MAPS = self.nml_get_value(KEY,'do_maps',False)
        self.DO_STATS = self.nml_get_value(KEY,'do_stats',False)
        self.DO_SHIPS = self.nml_get_value(KEY,'do_ships',False)
        self.DO_POLAR = self.nml_get_value(KEY,'do_polar',False)

        # GPLOT Module submission check
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
        KEY = kwargs.get('BATCH_KEY',"system")
        self.BATCH_MODE = self.nml_get_value(KEY,'batch_mode','background')
        print("MSG: Batch submission mode --> "+self.BATCH_MODE)
        self.SYS_ENV = self.nml_get_value(KEY,'sys_env','jet')
        self.BATCH_DFLTS = self.NMLDIR+"batch.defaults."+self.SYS_ENV.lower()
        if self.BATCH_MODE.lower() == 'sbatch':
            self.CPU_ACCT = self.nml_get_value(KEY,'cpu_acct','MISSING')
            if self.CPU_ACCT == "MISSING":
                self.CPU_ACCT = nml_get_opt(self.BATCH_DFLTS,'CPU_ACCT')
            self.PARTITION = ','.join(self.nml_get_value(KEY,'partition','MISSING'))
            if self.PARTITION == "MISSING":
                self.PARTITION = nml_get_opt(self.BATCH_DFLTS,'PARTITION')
            self.QOS = self.nml_get_value(KEY,'qos','batch')
            if self.QOS == "MISSING":
                self.QOS = nml_get_opt(self.BATCH_DFLTS,'QOS')
        else:
            self.CPU_ACCT, self.PARTITION, self.QOS = (), (), ()

        # Get the GPLOT output directory
        self.GPOUT = self.nml_get_value('config','odir')



    ###########################################################
    def nml_read_f90(self):
        """Read a fortran-style namelist as a dictionary using the
           f90nml module.
        @return nml: the namelist dictionary
        """
        nml = f90nml.read(self.NMLPATH)
        return(nml);
    
    
    ###########################################################
    def nml_get_keys(self,DICT):
        """Return the namelist keys as a list.
        @return KEYS: the list of namelist keys
        """
        KEYS = list(DICT.keys())
        return(KEYS);
    

    ###########################################################
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
    

    ###########################################################
    def nml_get_all_values_for_key(self,KEY):
        """Return a list of values for a specific namelist key.
        @param KEY: the specific namelist key
        @return VALUES: the list of values for a specific namelist key
        """
        VALUES = list(self.DICT[KEY])
        return(VALUES);
    

    ###########################################################
    #def nml_get_opt(self,NML,OPT):
    #    """Read the namelist and search for a specific option.
    #    @param NML:  the namelist
    #    @param OPT:  the namelist option, must begin the line
    #    @return VAR: the value of the namelist option, could be MISSING
    #    """
    #    VAR = "MISSING"
    #    with open(NML,"r") as f:
    #        for line in f.readlines():
    #            if line.strip().startswith(OPT):
    #                VAR = line.split("=")[1].strip()
    #                break
    #    if VAR == "MISSING":
    #        print("WARNING: Could not find "+OPT+" in "+NML)
    #    return(VAR);


    ###########################################################
    def nml_db_initialize(self,destroy=False):
        """Initialize the namelist table in the database
        @kwarg destroy: logical argument to delete table if it exists
        """
        # Define variables
        self.DB_FILE = db.db_name(self.GPOUT,self.EXPT)
        CNAME = nml_db_columns()[0]
        CTYPE = nml_db_columns()[1]

        # Establish connection to database
        self.CONN = db.create_connection(self.DB_FILE)

        # Delete the table if it already exists
        if destroy:
            db.delete_table(self.TBL,CONN=self.CONN,DFILE=self.DB_FILE,close=False)
            print("MSG: Deleting database table --> "+self.TBL)

        # Create the table. Duplicate entries are automatically ignored
        db.create_table(self.TBL,CNAME,CTYPE,CONN=self.CONN,DFILE=self.DB_FILE,close=False)

        # Add namelist table entries for each key seperately.
        # Also, add some manual entries
        N = 1
        for K in self.KEYS:
            # Check if this key has entries
            if len(self.DICT[K].keys()) == 0:
                continue
            print("MSG: Adding key '"+K+"' to the database.")

            # Add each row of data for this key to DATA
            DATA = [];
            for (E,V) in zip(self.DICT[K].keys(),self.DICT[K].values()):
                DATA.append((N,E,K,str(V)))
                N+=1

            # Add custom namelist table entries
            if K == 'config':
                MOREKEYS = ['gplot_dir','nml_dir','expt','master_nml','master_path']
                MOREVALS = [self.GPLOT_DIR,self.NMLDIR,self.EXPT,self.NMLFILE,self.NMLPATH]
                for (E,V) in zip(MOREKEYS,MOREVALS):
                    DATA.append((N,E,K,V))
                    N+=1

            # Actually add the data 'DATA' to the database table 'self.TBL' for this key.
            db.add_table_row(self.TBL,DATA,CONN=self.CONN,DFILE=self.DB_FILE,close=False)

        # Close the database connection so it is available for other requests
        db.close_connection(self.CONN)




###########################################################
###########################################################
class Namelist_Retrieve:
    """This class provides namelist information retrieved from a database

    """

    ###########################################################
    def __init__(self,DIR,EXPT,DFILE,MOD=None,**kwargs):
        """This runs everytime Namelist_Launch is assigned.
        @param self:  the instance of the class (no need to pass this)
        @param DIR:   the Directories class object
        @param EXPT:  the experiment name
        @param DFILE: the database file (full path)
        @kwarg MOD:   the module nickname
        @kwargs:      other keyword arguments may be passed
        """
        print("MSG: Construction zone for Class Namelist_Retrieve.")

        # Define basic information like paths, the namelist, and the experiment.
        self.EXPT = EXPT
        #self.NMLFILE = 'namelist.master.'+EXPT
        self.NMLFILE = kwargs.get('NML_FILE','NEW.namelist.master.'+EXPT)
        self.NMLDIR = DIR.NMLDIR
        self.NMLPATH = self.NMLDIR+self.NMLFILE
        self.GPLOT_DIR = DIR.GPLOT_DIR
        self.BATCHDIR = DIR.BATCHDIR
        self.LOGDIR = DIR.LOGDIR
        self.TBLDIR = DIR.TBLDIR

        # Define some information about the database table
        self.DB_FILE = DFILE
        self.CNAME = nml_db_columns()[0]
        self.CTYPE = nml_db_columns()[1]
        self.TBL = "namelist_master"

        # Retrieve the database as a DataFrame
        self.nml_db_retrieve()

        #print(self.D['section'])
        #print(self.D.loc[self.D.entry=='dsource'].value)

        # Define modules variables
        SEC = 'modules'
        self.DO_MAPS = self.nml_pd_extract_value(E='do_maps',S=SEC,DEF=False)
        self.DO_STATS = self.nml_pd_extract_value(E='do_stats',S=SEC,DEF=False)
        self.DO_SHIPS = self.nml_pd_extract_value(E='do_ships',S=SEC,DEF=False)
        self.DO_POLAR = self.nml_pd_extract_value(E='do_polar',S=SEC,DEF=False)

        # GPLOT Module submission check
        self.MOD_ID = []
        if self.DO_MAPS:
            self.MOD_ID.append("maps");
        if self.DO_STATS:
            self.MOD_ID.append("stats");
        if self.DO_SHIPS:
            self.MOD_ID.append("ships");
        if self.DO_POLAR:
            self.MOD_ID.append("polar");

        # Define config variables
        SEC = 'config'
        self.BDECK_DIR = self.nml_pd_extract_value(E='adeck_dir',S=SEC)
        self.ADECK_DIR = self.nml_pd_extract_value(E='bdeck_dir',S=SEC)
        self.GPOUT = self.nml_pd_extract_value(E='odir',S=SEC)
        self.HR_INIT = int(self.nml_pd_extract_value(E='init_hr',S=SEC,DEF=0))
        self.HR_FNL = int(self.nml_pd_extract_value(E='fnl_hr',S=SEC,DEF=126))
        self.HR_FMT = self.nml_pd_extract_value(E='fmt_hr',S=SEC,DEF=3)
        self.DT = int(self.nml_pd_extract_value(E='dt',S=SEC,DEF=3))
        self.DO_RMWHITE = self.nml_pd_extract_value(E='do_rmwhite',S=SEC,DEF=False)
        self.DO_CONVERTGIF = self.nml_pd_extract_value(E='do_convertgif',S=SEC,DEF=False)
        self.DO_SRCLBL = self.nml_pd_extract_value(E='do_srclbl',S=SEC,DEF=False)
        self.FORCE = self.nml_pd_extract_value(E='force',S=SEC,DEF=False)

        # Define case_main variables
        SEC = 'case_main'
        self.DSOURCE = self.nml_pd_extract_value(E='dsource',S=SEC,DEF='HWRF')
        self.IS_MSTORM = self.nml_pd_extract_value(E='is_mstorm',S=SEC,DEF=False)
        self.ENSID = self.nml_pd_extract_value(E='ensmem',S=SEC)
        if not isinstance(self.ENSID,list):
            self.ENSID = [self.ENSID]
        self.DATADIR = self.nml_pd_extract_value(E='idir',S=SEC)
        self.DTAG = self.nml_pd_extract_value(E='itag',S=SEC)
        self.DEXT = self.nml_pd_extract_value(E='ext',S=SEC)
        self.IDATE = self.nml_pd_extract_value(E='idate',S=SEC)
        self.SID = self.nml_pd_extract_value(E='sid',S=SEC)
        self.ATCF_INFO = []
        for x in range(1,9):
            if self.nml_pd_extract_value(E='atcf'+str(x)+'_dir',S=SEC) is not None:
                if self.nml_pd_extract_value(E='atcf1'+str(x)+'_tag',S=SEC) is None:
                    self.ATCF_INFO.append([self.nml_pd_extract_value(E='atcf'+str(x)+'_dir',S=SEC),'atcfunix'])
                else:
                    self.ATCF_INFO.append([self.nml_pd_extract_value(E='atcf'+str(x)+'_dir',S=SEC),self.nml_pd_extract_value(E='atcf'+str(x)+'_tag',S=SEC)])


        # Define case_diff variables (if necessary)
        SEC = 'case_compare'
        self.DO_COMPARE = self.nml_pd_extract_value(E='do_compare',S=SEC,DEF=False)
        if self.DO_COMPARE:
            self.EXPT2 = self.nml_pd_extract_value(E='expt',S=SEC)
            self.DSOURCE2 = self.nml_pd_extract_value(E='dsource',S=SEC,DEF='HWRF')
            self.IS_MS2 = self.nml_pd_extract_value(E='is_mstorm',S=SEC,DEF=False)
            self.ENSID2 = self.nml_pd_extract_value(E='ensmem',S=SEC)
            self.DATADIR2 = self.nml_pd_extract_value(E='idir',S=SEC)
            self.DTAG2 = self.nml_pd_extract_value(E='itag',S=SEC)
            self.DEXT2 = self.nml_pd_extract_value(E='ext',S=SEC)
            self.IDATE2 = self.nml_pd_extract_value(E='idate',S=SEC)
            self.SID2 = self.nml_pd_extract_value(E='sid',S=SEC)
            for x in range(1,9):
                if self.nml_pd_extract_value(E='atcf'+str(x)+'_dir',S=SEC) is not None:
                    if self.nml_pd_extract_value(E='atcf'+str(x)+'_tag',S=SEC) is None:
                        self.ATCF_INFO2.append([self.nml_pd_extract_value(E='atcf'+str(x)+'_dir',S=SEC),'atcfunix'])
                    else:
                        self.ATCF_INFO2.append([self.nml_pd_extract_value(E='atcf'+str(x)+'_dir',S=SEC),self.nml_pd_extract_value(E='atcf'+str(x)+'_tag',S=SEC)])

        # Define system variables
        SEC = 'system'
        self.SYS_ENV = self.nml_pd_extract_value(E='sys_env',S=SEC,DEF='jet')
        self.BATCH_MODE = self.nml_pd_extract_value(E='batch_mode',S=SEC,DEF='foreground')
        self.CPU_ACCT = self.nml_pd_extract_value(E='cpu_acct',S=SEC,DEF='hur-aoml')
        self.QOS = self.nml_pd_extract_value(E='qos',S=SEC,DEF='batch')
        self.PARTITION = self.nml_pd_extract_value(E='partition',S=SEC,DEF='xjet')
        self.AUTO_BATCH = self.nml_pd_extract_value(E='auto_batch',S=SEC,DEF=False)

        # Add specific options for the specific module
        self.nml_module_input(MOD)

        


    ###########################################################
    def nml_db_value_split(self,IN):
        """Take a database value and split it into an array
        @param self: the instance of the class (no need to pass this)
        @param IN:   the input database value
        @return OUT: the output array
        """
        OUT = IN.replace('[','').replace(']','').replace('\'','').replace(' ','').split(',')
        return(OUT);


    ###########################################################
    def nml_db_retrieve(self):
        """ Retrieve the namelist from the database
        @param self: the instance of the class (no need to pass this)
        """
        self.CONN = db.create_connection(self.DB_FILE)
        self.D = db.select_rows([self.TBL],"*",CONN=self.CONN,DFILE=self.DB_FILE)
        self.CONN.close()
        return;


    ###########################################################
    def nml_enesmble_check(self,EN=0):
        """Check if the namelist specifies an ensemble
        @param self: the instance of the class (no need to pass this)
        @kwarg EN:   the ensemble ID number. Set to 99 if no ensemble
        """
        try:
            if int(EN) == 99:
                self.IS_ENSEMBLE = False
            else:
                self.IS_ENSEMBLE = True
                #self.DATADIR = self.DATADIR+"/%02d/"%EN
                #if not os.exists.isdir(self.DATADIR):
                #    print("ERROR: Ensemble directory does not exist --> "+self.DATADIR)
                #    sys.exit(2)
                print("MSG: Working on ensemble member %d02"%EN)
        except TypeError as e:
            print(e)
        finally:
            return;


    ###########################################################
    def nml_pd_extract_value(self,E=None,S=None,DEF=None):
        """Extract the first matching value from a pandas DateFrame
        @param self: the instance of the class (no need to pass this)
        @kwarg E:    the input entry
        @kwarg S:    the input section
        @kwarg DEF:  the default value if not entry is found
        @return V:   the return value
        """
        if E is not None and S is not None:
            V =  self.D.loc[self.D.section==S].loc[self.D.entry==E]#.value.iloc[0]
        elif E is not None and S is None:
            V =  self.D.loc[self.D.entry==E]
        elif E is None and S is not None:
            V =  self.D.loc[self.D.section==S]
        else:
            print("ERROR: Can't search for namelist value without section & entry.")
            sys.exit(2)

        # Check if the database returned nothing
        EMPTY = V.empty

        # Extract the actual value. If V is empty, assign default value
        if not EMPTY:
            V = V.value.iloc[0]
        else:
            V = DEF

        # Correct 'None' strings to None
        if V == 'None':
            V = None
     
        # Convert if boolean 
        V = gpu.convert_boolean(V)

        return(V);



    ###########################################################
    def nml_module_input(self,M):
        """Extract the first matching balue from a pandas DateFrame
        @param self: the instance of the class (no need to pass this)
        @kwarg M:    the module nickname
        """
        if M is not None:
            if M == 'maps':
                self.DOMAIN = self.nml_db_value_split(self.nml_pd_extract_value(E='domain',S='maps'))
                self.TIER = self.nml_db_value_split(self.nml_pd_extract_value(E='tier',S='maps'))
                if not isinstance(self.TIER,list):
                    self.TIER = [self.TIER]
                self.ATCF_REQD = self.nml_pd_extract_value(E='atcf_reqd',S='maps')
    
            elif M == 'stats':
                self.MORIG = self.nml_pd_extract_value(E='morig',S='stats')
                self.MCODE = self.nml_pd_extract_value(E='mcode',S='stats')
                self.MOCODEI = self.nml_pd_extract_value(E='mcodei',S='stats')
                self.MCODE12 = self.nml_pd_extract_value(E='mcode12',S='stats')
                self.TRKM00 = self.nml_db_value_split(self.nml_pd_extract_value(E='trkm00',S='stats'))
                self.TRKM06 = self.nml_db_value_split(self.nml_pd_extract_value(E='trkm06',S='stats'))
                self.INTM = self.nml_db_value_split(self.nml_pd_extract_value(E='intm',S='stats'))
                self.PRSM = self.nml_db_value_split(self.nml_pd_extract_value(E='prsm',S='stats'))
                self.TRKMI00 = self.nml_db_value_split(self.nml_pd_extract_value(E='trkmi00',S='stats'))
                self.TRKMI06 = self.nml_db_value_split(self.nml_pd_extract_value(E='trkmi06',S='stats'))
                self.INTMI = self.nml_db_value_split(self.nml_pd_extract_value(E='intmi',S='stats'))
                self.TRKMT00 = self.nml_db_value_split(self.nml_pd_extract_value(E='trkmt00',S='stats'))
                self.TRKMT06 = self.nml_db_value_split(self.nml_pd_extract_value(E='trkmt06',S='stats'))
                self.INTMT = self.nml_db_value_split(self.nml_pd_extract_value(E='intmt',S='stats'))
                self.ETM = self.nml_db_value_split(self.nml_pd_extract_value(E='etm',S='stats'))
                self.EIM = self.nml_db_value_split(self.nml_pd_extract_value(E='eim',S='stats'))
                self.LTM = self.nml_db_value_split(self.nml_pd_extract_value(E='ltm',S='stats'))
                self.LIM = self.nml_db_value_split(self.nml_pd_extract_value(E='lim',S='stats'))
                self.LEAD_T = self.nml_db_value_split(self.nml_pd_extract_value(E='lead_times',S='stats'),DEF=[0,12,24,36,48,60,72,84,96,108,120])
                self.NTREND = self.nml_pd_extract_value(E='ntrend',S='stats',DEF=6)
                self.DO_INTERP = self.nml_pd_extract_value(E='do_interp',S='stats',DEF=False)
    
            elif M == 'ships':
                print("WARNING: No namelist options for GPLOT module 'ships'")
    
            elif M == 'polar':
                self.MORIG = self.nml_pd_extract_value(E='resolution',S='polar')
                self.MORIG = self.nml_pd_extract_value(E='rmax',S='polar',DEF=600)
                self.MORIG = self.nml_pd_extract_value(E='levs',S='polar')

        return;



###########################################################
def nml_db_columns():
    """Centralize the column names in the namelist table of the database
    @return: an array of the column names
    """
    return([["id","entry","section","value"],["integer","text","text","text"]]);



###########################################################
def nml_master_default(EXPT):
    """Default name for the master namelist
    @param EXPT: the GPLOT experiment name
    @return:     the master namelist name
    """
    return('NEW.namelist.master.'+EXPT)



###########################################################
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
    if type(VAR) == str:
        VAR = VAR.strip('"')
    if VAR == "MISSING":
        print("WARNING: Could not find "+OPT+" in "+NML)
    return(VAR);
