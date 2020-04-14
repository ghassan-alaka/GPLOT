#!/usr/bin/env python


# Import necessary modules
import os, sys
import configparser as cparse
import pandas as pd
from pandas import DataFrame as df
#import f90nml
#import shutil

# Import GPLOT modules
import gp_database as db
import gp_execute as xc
import gp_log as log
import gp_util as gpu
from gp_log import Main_Logger


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

Created By:    Ghassan Alaka, Jr.
Modified By:   Ghassan Alaka, Jr.
Date Created:  February 12, 2020
Last Modified: March 12, 2020

Example call: For Internal Calls Only

Modification Log:
2020-Feb-27 -- GJA changed the file name from namelist.py to gp_namelist.py
2020-Mar-12 -- GJA updated nml_pd_extract_value() to assign blank strings as None
2020-Mar-19:   GJA added logging capabilities.
"""

__version__ = '0.2.2';



###########################################################
###########################################################
class Namelist_Launch:
    """This class provides namelist information retrieved from
       the fortran-style namelist.

    """

    ###########################################################
    def __init__(self, DIR, EXPT, CONFS, NO_FAIL=True, **kwargs):
        """This runs everytime Namelist_Launch is assigned.
        @param self:    the instance of the class (no need to pass this)
        @param DIR:     the directories object
        @param EXPT:    the experiment name
        @param CONFS:   the list of namelist/configuration files
        @kwarg NO_FAIL: logical to not allow continuation if the namelist is not found.
        @kwargs:        other keyword arguments may be passed
        """

        # Check for the logging object
        L = kwargs.get('logger',Main_Logger('Namelist'))

        # Define basic information like paths, the namelist, and the experiment.
        self.EXPT = EXPT
        self.TBL = "namelist_master"
        self.NMLDIR = DIR.NMLDIR
        self.GPLOT_DIR = DIR.GPLOT_DIR
        self.BATCHDIR = DIR.BATCHDIR
        self.LOGDIR = DIR.LOGDIR
        self.TBLDIR = DIR.TBLDIR


        # Define the namelist by parsing configuration files listed in CONFS
        self.nml_parse_config(CONFS)

        # Determine what modules to run
        KEY = kwargs.get('MODULE_KEY',"modules")
        self.DO_MAPS = self.nml_get_value(KEY,'do_maps',False)
        self.DO_STATS = self.nml_get_value(KEY,'do_stats',False)
        self.DO_SHIPS = self.nml_get_value(KEY,'do_ships',False)
        self.DO_POLAR = self.nml_get_value(KEY,'do_polar',False)

        # GPLOT Module submission check
        self.MOD_ID = []
        if self.DO_MAPS:   self.MOD_ID.append("maps");
        if self.DO_STATS:  self.MOD_ID.append("stats");
        if self.DO_SHIPS:  self.MOD_ID.append("ships");
        if self.DO_POLAR:  self.MOD_ID.append("polar");

        # Get system and project information for batch submission
        KEY = kwargs.get('BATCH_KEY',"system")
        self.BATCH_MODE = self.nml_get_value(KEY,'batch_mode','background')
        L.logger.info("Batch submission mode --> "+self.BATCH_MODE)
        self.SYS_ENV = self.nml_get_value(KEY,'sys_env','jet')
        if self.BATCH_MODE.lower() == 'sbatch':
            self.CPU_ACCT = self.nml_get_value(KEY,'cpu_acct','MISSING')
            self.PARTITION = self.nml_get_value(KEY,'partition','MISSING')
            self.QOS = self.nml_get_value(KEY,'qos','batch')
        else:
            self.CPU_ACCT, self.PARTITION, self.QOS = (), (), ()

        # Get the GPLOT output directory
        self.GPOUT = self.nml_get_value('config','odir')



    ###########################################################
    #def nml_read_f90(self):
    #    """Read a fortran-style namelist as a dictionary using the
    #       f90nml module.
    #    @return nml: the namelist dictionary
    #    """
    #    nml = f90nml.read(self.NMLPATH)
    #    return(nml);
    
    
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

        if KEY in self.CONF_KEYS:
            VALUE = self.CONFIG[KEY].get(VAL,DEF)
        else:
            VALUE = DEF
        VALUE = gpu.convert_boolean(VALUE)
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
    def nml_parse_config(self,CONFS,NO_FAIL=True,**kwargs):
        """Return a list of values for a specific namelist key.
        @param CONFS: the list of configuration files
        @kwargs NO_FAIL: logical to not allow continuation if the namelist is not found.
        @kwargs:        extra keywords are allowed
        """

        # Check for the logging object
        L = kwargs.get('logger',Main_Logger('Namelist'))
    
        self.CONFIG = cparse.ConfigParser()
        FOUND_CONF = False
        for C in CONFS:
            if os.path.exists(self.NMLDIR+C):
                L.logger.info("Found this namelist --> "+self.NMLDIR+C)
                self.CONFIG.read(self.NMLDIR+C)
                FOUND_CONF = True
            elif os.path.exists(C):
                L.logger.info("Found this namelist --> "+C)
                self.CONFIG.read(C)
                FOUND_CONF = True

        if not FOUND_CONF and NO_FAIL:
            L.logger.error("No namelists found --> "+CONFS)
            sys.exit(2)

        self.CONF_KEYS = self.nml_get_keys(self.CONFIG)

        return;


    ###########################################################
    def nml_db_initialize(self,destroy=False,**kwargs):
        """Initialize the namelist table in the database
        @param self:    the instance of the class (no need to pass this)
        @kwarg destroy: logical argument to delete table if it exists
        @kwargs:        extra keywords are allowed
        """

        # Check for the logging object
        L = kwargs.get('logger',Main_Logger('Namelist'))

        # Define variables
        self.DB_FILE = db.db_name(self.GPOUT,self.EXPT)
        CNAME = nml_db_columns()[0]
        CTYPE = nml_db_columns()[1]

        # Establish connection to database
        self.CONN = db.create_connection(self.DB_FILE)

        # Delete the table if it already exists
        if destroy:
            db.delete_table(self.TBL,CONN=self.CONN,DFILE=self.DB_FILE,close=False)
            L.logger.info("Deleting database table --> "+self.TBL)

        # Create the table. Duplicate entries are automatically ignored
        db.create_table(self.TBL,CNAME,CTYPE,CONN=self.CONN,DFILE=self.DB_FILE,close=False)

        # Add namelist table entries for each key seperately.
        # Also, add some manual entries
        N = 1
        for K in self.CONF_KEYS:
            # Check if this key has entries
            if len(self.CONFIG[K].keys()) == 0:  continue
            L.logger.info("Adding key '"+K+"' to the database.")

            # Add each row of data for this key to DATA
            DATA = [];
            for (E,V) in zip(self.CONFIG[K].keys(),self.CONFIG[K].values()):
                DATA.append((N,E,K,str(V)))
                N+=1

            # Add custom namelist table entries
            if K == 'config':
                #MOREKEYS = ['gplot_dir','nml_dir','expt','master_nml','master_path']
                #MOREVALS = [self.GPLOT_DIR,self.NMLDIR,self.EXPT,self.NMLFILE,self.NMLPATH]
                MOREKEYS = ['gplot_dir','nml_dir','expt']
                MOREVALS = [self.GPLOT_DIR,self.NMLDIR,self.EXPT]
                for (E,V) in zip(MOREKEYS,MOREVALS):
                    DATA.append((N,E,K,V))
                    N+=1

            # Actually add the data 'DATA' to the database table 'self.TBL' for this key.
            db.add_table_row(self.TBL,DATA,CONN=self.CONN,DFILE=self.DB_FILE,close=False)

        # Close the database connection so it is available for other requests
        db.close_connection(self.CONN)

        # Write the database file and experiment to a GPLOT table file
        DB_TBL = self.TBLDIR+'DBInfo.dat'
        if os.path.exists(DB_TBL):
            print(self.EXPT)
            print(DB_TBL)
            if destroy:  xc.exec_subprocess(f"sed -i '/{self.EXPT},/d' {DB_TBL}")
            with open(DB_TBL, "r") as f:
                lines = f.readlines()
            with open(DB_TBL, "w") as f:
                for line in lines:
                    if self.EXPT not in line.strip("\n"):
                        f.write(line)
                f.write(self.EXPT+',     '+self.DB_FILE)
                f.close()
        else:
            with open(DB_TBL, "w") as f:
                f.write(self.EXPT+',     '+self.DB_FILE)
                f.close()




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
        @kwarg L:     the logger object
        @kwarg MOD:   the module nickname
        @kwargs:      other keyword arguments may be passed
        """

        # Try to log using the logging object
        L = kwargs.get('logger',Main_Logger('Namelist'))
        L.logger.info("Construction zone for Class Namelist_Retrieve.")

        # Define basic information like paths, the namelist, and the experiment.
        self.EXPT = EXPT
        self.MOD = MOD
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

        # Define modules variables
        SEC = 'modules'
        self.DO_MAPS = self.nml_pd_extract_value(E='do_maps',S=SEC,DEF=False)
        self.DO_STATS = self.nml_pd_extract_value(E='do_stats',S=SEC,DEF=False)
        self.DO_SHIPS = self.nml_pd_extract_value(E='do_ships',S=SEC,DEF=False)
        self.DO_POLAR = self.nml_pd_extract_value(E='do_polar',S=SEC,DEF=False)

        # GPLOT Module submission check
        self.MOD_ID = []
        if self.DO_MAPS:   self.MOD_ID.append("maps");
        if self.DO_STATS:  self.MOD_ID.append("stats");
        if self.DO_SHIPS:  self.MOD_ID.append("ships");
        if self.DO_POLAR:  self.MOD_ID.append("polar");

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
        self.MAX_BUNDLE = int(self.nml_pd_extract_value(E='max_bundle',S=SEC,DEF=5))

        # Define dataset1 variables. This namelist block must be available.
        SEC = 'dataset1'
        self.DSOURCE = self.nml_pd_extract_value(E='dsource',S=SEC,DEF='HWRF')
        self.IS_MSTORM = self.nml_pd_extract_value(E='is_mstorm',S=SEC,DEF=False)
        self.ENSID = gpu.list_convert(self.nml_db_value_split(self.nml_pd_extract_value(E='ensmem',S=SEC)),int)
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


        # Define dataset2 variables (if necessary)
        SEC = 'dataset2'
        self.DO_COMPARE = self.nml_pd_extract_value(E='do_compare',S=SEC,DEF=False)
        if self.DO_COMPARE:
            self.EXPT2 = self.nml_pd_extract_value(E='expt',S=SEC)
            self.DSOURCE2 = self.nml_pd_extract_value(E='dsource',S=SEC,DEF='HWRF')
            self.IS_MS2 = self.nml_pd_extract_value(E='is_mstorm',S=SEC,DEF=False)
            self.ENSID2 = gpu.list_convert(self.nml_db_value_split(self.nml_pd_extract_value(E='ensmem',S=SEC)),int)
            if not isinstance(self.ENSID2,list):
                self.ENSID2 = [self.ENSID2]
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
        #self.BATCH_MODE = 'foreground'
        self.CPU_ACCT = self.nml_pd_extract_value(E='cpu_acct',S=SEC,DEF='hur-aoml')
        self.QOS = self.nml_pd_extract_value(E='qos',S=SEC,DEF='batch')
        self.PARTITION = self.nml_pd_extract_value(E='partition',S=SEC,DEF='xjet')
        self.AUTO_BATCH = self.nml_pd_extract_value(E='auto_batch',S=SEC,DEF=False)
        self.MAX_JOBS = int(self.nml_pd_extract_value(E='auto_batch',S=SEC,DEF=25))

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
        CONN = db.create_connection(self.DB_FILE)
        self.D = db.select_rows([self.TBL],"*",CONN=CONN,DFILE=self.DB_FILE)
        CONN.close()
        return;


    ###########################################################
    def nml_enesmble_check(self,EN=0,**kwargs):
        """Check if the namelist specifies an ensemble
        @param self: the instance of the class (no need to pass this)
        @kwarg EN:   the ensemble ID number. Set to 99 if no ensemble
        @kwargs:     extra keywords are allowed
        """

        # Check for the logging object
        L = kwargs.get('logger',Main_Logger('Namelist'))

        try:
            if int(EN) == 99:  self.IS_ENSEMBLE = False
            else:
                self.IS_ENSEMBLE = True
                L.logger.info("Working on ensemble member {:02d}".format(EN))
        except TypeError as e:
            L.logger.error(e)
        finally:
            return;


    ###########################################################
    def nml_pd_extract_value(self,E=None,S=None,DEF=None,**kwargs):
        """Extract the first matching value from a pandas DateFrame
        @param self: the instance of the class (no need to pass this)
        @kwarg E:    the input entry
        @kwarg S:    the input section
        @kwarg DEF:  the default value if not entry is found
        @kwargs:     extra keywords are allowed
        @return V:   the return value
        """

        # Check for the logging object
        L = kwargs.get('logger',Main_Logger('Namelist'))

        # Decide how to find the value
        if E is not None and S is not None:
            V =  self.D.loc[self.D.section==S].loc[self.D.entry==E]#.value.iloc[0]
        elif E is not None and S is None:
            V =  self.D.loc[self.D.entry==E]
        elif E is None and S is not None:
            V =  self.D.loc[self.D.section==S]
        else:
            L.logger.error("Can't search for namelist value without section & entry.")
            sys.exit(2)

        # Check if the database returned nothing
        EMPTY = V.empty

        # Extract the actual value. If V is empty, assign default value
        if not EMPTY:
            V = V.value.iloc[0]
            if not V:
                V = DEF
        else:
            V = DEF

        # Correct 'None' strings to None
        if V == 'None':
            V = None
     
        # Convert if boolean 
        V = gpu.convert_boolean(V)

        return(V);



    ###########################################################
    def nml_module_input(self,M,**kwargs):
        """Extract the first matching balue from a pandas DateFrame
        @param self: the instance of the class (no need to pass this)
        @kwarg M:    the module nickname
        @kwargs:     extra keywords are allowed
        """

        # Check for the logging object
        L = kwargs.get('logger',Main_Logger('Namelist'))

        if M is not None:
            if M == 'maps':
                self.DOMAIN = self.nml_db_value_split(self.nml_pd_extract_value(E='domain',S=M))
                #self.TIER = self.nml_db_value_split(self.nml_pd_extract_value(E='tier',S='maps'))
                #if not isinstance(self.TIER,list):
                #    self.TIER = [self.TIER]
                self.ATCF_REQD = self.nml_pd_extract_value(E='atcf_reqd',S=M)
                self.PRIO_CUTOFF = int(self.nml_pd_extract_value(E='prio_cutoff',S=M,DEF=15))
    
            elif M == 'stats':
                self.MORIG = self.nml_pd_extract_value(E='morig',S=M)
                self.MCODE = self.nml_pd_extract_value(E='mcode',S=M)
                self.MOCODEI = self.nml_pd_extract_value(E='mcodei',S=M)
                self.MCODE12 = self.nml_pd_extract_value(E='mcode12',S=M)
                self.TRKM00 = self.nml_db_value_split(self.nml_pd_extract_value(E='trkm00',S=M))
                self.TRKM06 = self.nml_db_value_split(self.nml_pd_extract_value(E='trkm06',S=M))
                self.INTM = self.nml_db_value_split(self.nml_pd_extract_value(E='intm',S=M))
                self.PRSM = self.nml_db_value_split(self.nml_pd_extract_value(E='prsm',S=M))
                self.TRKMI00 = self.nml_db_value_split(self.nml_pd_extract_value(E='trkmi00',S=M))
                self.TRKMI06 = self.nml_db_value_split(self.nml_pd_extract_value(E='trkmi06',S=M))
                self.INTMI = self.nml_db_value_split(self.nml_pd_extract_value(E='intmi',S=M))
                self.TRKMT00 = self.nml_db_value_split(self.nml_pd_extract_value(E='trkmt00',S=M))
                self.TRKMT06 = self.nml_db_value_split(self.nml_pd_extract_value(E='trkmt06',S=M))
                self.INTMT = self.nml_db_value_split(self.nml_pd_extract_value(E='intmt',S=M))
                self.ETM = self.nml_db_value_split(self.nml_pd_extract_value(E='etm',S=M))
                self.EIM = self.nml_db_value_split(self.nml_pd_extract_value(E='eim',S=M))
                self.LTM = self.nml_db_value_split(self.nml_pd_extract_value(E='ltm',S=M))
                self.LIM = self.nml_db_value_split(self.nml_pd_extract_value(E='lim',S=M))
                self.LEAD_T = self.nml_db_value_split(self.nml_pd_extract_value(E='lead_times',S=M),DEF=[0,12,24,36,48,60,72,84,96,108,120])
                self.NTREND = self.nml_pd_extract_value(E='ntrend',S=M,DEF=6)
                self.DO_INTERP = self.nml_pd_extract_value(E='do_interp',S=M,DEF=False)
                self.PRIO_CUTOFF = int(self.nml_pd_extract_value(E='prio_cutoff',S=M,DEF=15))
    
            elif M == 'ships':
                L.logger.warning("No namelist options for GPLOT module '"+M+"'")
                self.PRIO_CUTOFF = int(self.nml_pd_extract_value(E='prio_cutoff',S=M,DEF=15))
    
            elif M == 'polar':
                self.RESOLUTION = self.nml_pd_extract_value(E='resolution',S=M)
                self.RMAX = int(self.nml_pd_extract_value(E='rmax',S=M,DEF=600))
                self.LEVS = self.nml_pd_extract_value(E='levs',S=M)
                self.PRIO_CUTOFF = int(self.nml_pd_extract_value(E='prio_cutoff',S=M,DEF=15))

        return;


    ###########################################################
    def nml_setup_filename(self):
        """Define information about the data file name
        @param self: the instance of the class (no need to pass this)
        """
        # Get file prefix information from table or namelist
        if self.DTAG is None:
            self.FILE_PRE = gpu.data_table_read(self.TBLDIR+"/FilePrefix.dat",C='D0'+str(self.NEST),TEST=f'DSOURCE == "{self.DSOURCE}"',delim_whitespace=True)[0]
        else:
            self.FILE_PRE = self.DTAG

        # Get file hour string, format information from table or namelist
        self.FILE_FHR = gpu.data_table_read(self.TBLDIR+"/FileTimeFormat.dat",C='HRSTR',TEST=f'DSOURCE == "{self.DSOURCE}"',delim_whitespace=True)[0]
        if self.HR_FMT is None:
            self.FILE_HFMT = gpu.data_table_read(self.TBLDIR+"/FileTimeFormat.dat",C='HRFMT',TEST=f'DSOURCE == "{self.DSOURCE}"',delim_whitespace=True)[0]
        else:
            self.FILE_HFMT = self.HR_FMT

        # Get file suffix information from table or namelist
        if self.DEXT is None:
            self.FILE_SUF = gpu.data_table_read(self.TBLDIR+"/FileSuffix.dat",C='SUFFIX',TEST=f'DSOURCE == "{self.DSOURCE}"',delim_whitespace=True)[0]
        else:
            self.FILE_SUF = self.DEXT

        # Now, do the same for the comparison dataset, if applicable
        if self.DO_COMPARE:

            # Get file prefix information from table or namelist
            if self.DTAG2 is None:
                self.FILE_PRE2 = gpu.data_table_read(self.TBLDIR+"/FilePrefix.dat",C='D0'+str(self.NEST),TEST=f'DSOURCE == "{self.DSOURCE2}"',delim_whitespace=True)[0]
            else:
                self.FILE_PRE2 = self.DTAG2

            # Get file hour string, format information from table or namelist
            self.FILE_FHR2 = gpu.data_table_read(self.TBLDIR+"/FileTimeFormat.dat",C='HRSTR',TEST=f'DSOURCE == "{self.DSOURCE2}"',delim_whitespace=True)[0]
            if self.HR_FMT2 is None:
                self.FILE_HFMT2 = gpu.data_table_read(self.TBLDIR+"/FileTimeFormat.dat",C='HRFMT',TEST=f'DSOURCE == "{self.DSOURCE2}"',delim_whitespace=True)[0]
            else:
                self.FILE_HFMT2 = self.HR_FMT2
    
            # Get file suffix information from table or namelist
            if self.DEXT2 is None:
                self.FILE_SUF2 = gpu.data_table_read(self.TBLDIR+"/FileSuffix.dat",C='SUFFIX',TEST=f'DSOURCE == "{self.DSOURCE2}"',delim_whitespace=True)[0]
            else:
                self.FILE_SUF2 = self.DEXT2


    ###########################################################
    def nml_setup_nest(self,DM,**kwargs):
        """Define the model nest for this particular domain
        @param self: the instance of the class (no need to pass this)
        @param DM:   the GPLOT domain
        @kwargs:     extra keywords are allowed
        """
        L = kwargs.get('logger',Main_Logger('Namelist'))

        # Get nest information from data table
        self.NEST = gpu.data_table_read(self.TBLDIR+"/DomainInfo.dat",C='NEST',TEST=f'DOMAIN == "{DM}"',delim_whitespace=True)[0]
        if self.NEST is None:
            L.logger.warning("Domain "+DM+" not found in "+self.TBLDIR+"/DomainInfo.dat. Assuming NEST=1.")
            self.NEST=1


    ###########################################################
    def find_module_nml(self,E,M,DM,SORT_BY_PRIO=True,**kwargs):
        """Find the module namelist by searching through preset namelist naming structure
        @param self: the instance of the class (no need to pass this)
        @param E:            the GPLOT experiment
        @param M:            the GPLOT module
        @param DM:           the GPLOT domain
        @kwarg SORT_BY_PRIO: logical for sorting by prioity
        @kwargs:             extra keywords are allowed
        """
        L = kwargs.get('logger',Main_Logger('Namelist'))

        # The list of possible module namelist names, in order of priority
        #self.NMODLIST = ['namelist.'+M+'.'+E+'.'+DM,'namelist.'+M+'.'+DM,'namelist.'+M+'.default']
        self.NMODLIST = ['TEST.namelist.'+M+'.'+DM,'namelist.'+M+'.'+E+'.'+DM,'namelist.'+M+'.'+DM,'namelist.'+M+'.default']

        # Loop over the possible namelists until one is found.
        for N in self.NMODLIST:
            if os.path.exists(self.NMLDIR+N):
                self.NMLMOD_DF = gpu.data_table_read(self.NMLDIR+N,KEEP_DF=True,delim_whitespace=True)
                print(self.NMLMOD_DF)
                self.NMLMOD = gpu.df_create([self.NMLMOD_DF['FILE_NAME'].tolist(),gpu.convert_boolean(self.NMLMOD_DF['PLOT_ON'].tolist()),self.NMLMOD_DF['PRIO'].tolist()],['FILE_NAME','PLOT_ON','PRIO'])
                L.logger.info("Found this module namelist --> "+self.NMLDIR+N)
                break

        # Perform checks on NMLMOD
        try:
            if not self.NMLMOD:               EMPTY = True
        except ValueError as e:
            if hasattr(self.NMLMOD,'empty'):  EMPTY = self.NMLMOD.empty
            else:                             EMPTY = False

        if EMPTY:
            L.logger.error("Module namelist not found. Can't continue.")
            sys.exit(2)

        # Sort by PLOT_ON=True, then by priority, if requested
        self.NMLMOD = self.NMLMOD[self.NMLMOD['PLOT_ON']]
        if SORT_BY_PRIO:
            self.NMLMOD = self.NMLMOD.sort_values(by=['PRIO']).sort_index()
            self.NMLMOD = self.NMLMOD[self.NMLMOD['PRIO']<=self.PRIO_CUTOFF]



    ###########################################################
#    def find_full_input_dir(self,CY=None,TC=None,EN=None):
#        """
#        @param self: the instance of the class (no need to pass this)
#        @kwarg CY:   the GPLOT cycle
#        @kwarg TC:   the GPLOT tropical cyclone
#        @kwarg EN:   the GPLOT ensemble member
#        """
#        if EN == 99:  EN = None
#
#        if CY is None and TC is None and EN is None:
#
#        elif CY is not None:
#
#        elif TC is not None:
#
#        elif EN is not None:
#
#        elif CY is not None and TC is not None:
#
#        elif CY is not None and TC is not None and EN is not None:
#
#        elif CY is not None and EN not in [None,99]:
#
#        elif TC is not None and EN not in [None,99]:




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
def nml_get_opt(NML,OPT,**kwargs):
    """Read the namelist and search for a specific option.
    @param NML:  the namelist
    @param OPT:  the namelist option, must begin the line
    @kwargs:     extra keywords are allowed
    @return VAR: the value of the namelist option, could be MISSING
    """

    # Check for the logging object
    L = kwargs.get('logger',Main_Logger('Namelist'))

    VAR = "MISSING"
    with open(NML,"r") as f:
        for line in f.readlines():
            if line.strip().startswith(OPT):
                VAR = line.split("=")[1].strip()
                break
    if type(VAR) == str:
        VAR = VAR.strip('"')
    if VAR == "MISSING":
        L.logger.warning("Could not find "+OPT+" in "+NML)
    return(VAR);
