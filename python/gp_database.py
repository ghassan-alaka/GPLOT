#!/usr/bin/env python

# Import necessary modules
import datetime, os, sys
import numpy as np
import itertools as it
import pandas as pd
import sqlite3 as sq3
from sqlite3 import Error
sq3.register_adapter(np.int64, lambda val: int(val))
sq3.register_adapter(np.int32, lambda val: int(val))

import gp_log as log
import gp_util as gpu
from gp_log import Main_Logger

"""
GPLOT Package database
Python functionality for databases. This script is not
  meant to be called directly. Rather, it should be loaded
  in appropriate python code.

GPLOT is the Graphical Post-processed Locus for Output of Tropical cyclones.
It consists of independent modules used to create graphics targeted toward
presentations, posters, publications, and basic research related to tropical
cyclones. To learn more, please download the code from GitHub:
  https://github.com/ghassan-alaka/GPLOT

To learn more about the AOML/Hurricane Research Division, please visit:
  https://www.aoml.noaa.gov/our-research/hurricane-research-division/

Created By:    Ghassan Alaka Jr.
Modified By:   
Date Created:  February 6, 2020
Last Modified: March 17, 2020

Example call: For Internal Calls Only

Modification Log:
2020-Feb-14 -- Added several functions to improve table creation in a database.
                 (v0.1.0)
2020-Feb-27 -- GJA changed the file name from database.py to gp_database.py.
                 (v0.2.0)
2020-Mar-17 -- GJA developed the Database_Status class to assist with status
                 storage in and retrievals from the database. (v0.3.0)
2020-Mar-19 -- GJA added logging capabilities. (v0.3.1)
2020-Mar-24 -- GJA developed functionality for the graphic status database
                 table. (v0.4.0)
"""

__version__ = '0.4.0';




###########################################################
class Database_Status:
    """This class provides database information to query GPLOT statuses
    """

    ###########################################################
    def __init__(self,D,EXPT,**kwargs):
        """This runs everytime Namelist_Launch is assigned.
        @param self:  the instance of the class (no need to pass this)
        @param D:     the Directories class object
        @param EXPT:  the experiment name
        @param DFILE: the database file (full path)
        @kwarg MOD:   the module nickname
        @kwargs:      other keyword arguments may be passed
        """

        # Check for the logging object
        L = kwargs.get('logger',Main_Logger('Database'))

        self.GP_TBL = D.TBLDIR+'/DBInfo.dat'
        self.DB_FILE = db_retrieve(self.GP_TBL,EXPT)
        if self.DB_FILE is None:
            L.logger.error("Could not find the database file here --> "+self.GP_TBL)
            sys.exit(2)

        # Generic variables that apply to all tables
        self.COL_STATUS = 'status'
        self.ACTIVE_STATUS = ['started','incomplete','failed']

        # Variables for the main_status table
        self.M_TBL = "main_status"
        self.M_COL = ['id','module','cycle','domain','sid','status','date_added']
        self.M_TYP = ['integer','text','text','text','text','text','timestamp']
        self.M_COL_ADD = ['module','cycle','domain','sid','status','date_added']
        self.M_COL_S1 = ['module','cycle','domain','sid']

        # Variables for the atcf_status table
        self.A_TBL = "atcf_status"
        self.A_COL = ['id','cycle','sid','file_name','status','fhr_s','fhr_e','age']
        self.A_TYP = ['integer','text','text','text','text','integer','integer','integer']
        self.A_COL_ADD = ['cycle','sid','file_name','status','fhr_s','fhr_e','age']
        self.A_COL_S1 = ['cycle','sid']
        self.A_COL_S2 = ['cycle','sid','file_name']
        self.A_COL_S3 = ['cycle','file_name']
        self.A_COL_S4 = ['age']
        self.A_COL_U1 = ['fhr_s','fhr_e','age']
        self.A_COL_U2 = ['status']

        # Variables for the graphic_status table
        self.G_TBL = "graphic_status"
        self.G_COL = ['id','module','ensid','cycle','sid','domain','name','lead_time','status','date_modified','location','file_name',\
                      'main_file1','main_file2','main_file3','main_file4','comp_file1','comp_file2','comp_file3','comp_file4']
        self.G_TYP = ['integer','text','text','text','text','text','text','integer','text','timestamp','text','text','text',\
                      'text','text','text','text','text','text','text']
        self.G_COL1 = ['module','ensid','cycle','sid','domain','lead_time','name','status','date_modified','location','file_name',\
                       'main_file1','main_file2','main_file3','main_file4','comp_file1','comp_file2','comp_file3','comp_file4']
        self.G_COL2 = ['module','ensid','cycle','sid','domain','lead_time','name','status','date_modified','location','file_name']
        self.G_COL3 = ['module','ensid','cycle','sid','domain','lead_time','name']
        self.G_COL4 = ['main_file1','main_file2','main_file3','main_file4']
        self.G_COL5 = ['comp_file1','comp_file2','comp_file3','comp_file4']
        self.G_COL6 = ['module','ensid','cycle','sid','domain','name']
        self.G_COL7 = ['module','ensid','cycle','sid','domain','lead_time']


    ###########################################################
    def assign_main_data(self,MOD,CY,DM,ST,TC=None):
        """Assign data for the 'main_status' table
        @param self:  the instance of the class (no need to pass this)
        @param MOD:  the GPLOT module
        @param CY:   the GPLOT cycle
        @param DM:   the GPLOT domain
        @param ST:   the status for this particular task
        @kwarg TC:   the storm ID, could be None
        """
        if TC is None or TC == '00L':
            self.M_DATA1 = [(MOD,CY,DM,'null',ST,datetime.datetime.now())]
            self.M_VAL_S1 = [MOD,CY,DM,'null']
        else:
            self.M_DATA1 = [(MOD,CY,DM,TC,ST,datetime.datetime.now())]
            self.M_VAL_S1 = [MOD,CY,DM,TC]


    ###########################################################
    def retrieve_main_status(self,**kwargs):
        """Retrieve (or create) the GPLOT status for this task.
        @param self: the instance of the class (no need to pass this)
        @kwargs:     extra keywords are allowed
        """

        # Check for the logging object
        L = kwargs.get('logger',Main_Logger('Database'))

        try:
            S = select_rows([self.M_TBL],[self.COL_STATUS],DFILE=self.DB_FILE,C2=self.M_COL_S1,V2=self.M_VAL_S1)
            if S.empty:
                raise Exception
        except Exception:
            L.logger.info("Creating an entry in the database table '"+self.M_TBL+"'.")
            add_table_row(self.M_TBL,self.M_DATA1,CNAME=self.M_COL_ADD,DFILE=self.DB_FILE)
            S = select_rows([self.M_TBL],[self.COL_STATUS],DFILE=self.DB_FILE,C2=self.M_COL_S1,V2=self.M_VAL_S1)
            print(select_rows([self.M_TBL],["*"],DFILE=self.DB_FILE,C2=self.M_COL_S1,V2=self.M_VAL_S1))
        self.M_STATUS = S[self.COL_STATUS].tolist()[0]


    ###########################################################
    def assign_atcf_data(self,CY,TC,ST,A1=[[None]],A2=[[None]],FHR_S=None,FHR_E=None,AGE=None):
        """Assign data for the 'atcf_status' table
        @param self:  the instance of the class (no need to pass this)
        @param CY:    the GPLOT cycle, could be an array
        @param TC:    the storm ID
        @param ST:    the status for this particular task
        @kwarg A1:    the list of ATCF files for this cycle
        @kwarg A2:    the list of ATCF files for this storm
        @kwarg FHR_S: the starting forecast lead time
        @kwarg FHR_E: the ending forecast lead time
        @kwarg AGE:   the age of the ATCF file(s) in seconds
        """

        # Determine the file name and status
        if list(TC)[0] == '00L':
            if A1[0][0] is None:
                FN = ['null']
                ST2 = ['not started']
            else:
                FN = A1[0]
                ST2 = ST
        else:
            if A2[0][0] is None:
                FN = ['null']
                ST2 = ['not started']
            else:
                FN = A2[0]
                ST2 = ST

        # Determine what to do if the lead time(s) and
        # age are missing
        if FHR_S is None or FHR_E is None:  FHR_S = -999
        if FHR_E is None or FHR_S == -999:  FHR_E = -999
        if AGE is None:                     AGE = -999

        # Make sure CY/TC/ST are lists.
        if type(CY) != list:     CY = [CY]
        if type(TC) != list:     TC = [TC]
        if type(ST) != list:     ST = [ST]
        if type(FHR_S) != list:  FHR_S = [FHR_S]
        if type(FHR_E) != list:  FHR_E = [FHR_E]
        if type(AGE) != list:    AGE = [AGE]

        # Make sure CY/TC/ST have the same length as FN
        if len(CY) < len(FN) and len(CY) == 1:        CY = [CY[0]]*len(FN)
        if len(TC) < len(FN) and len(TC) == 1:        TC = [TC[0]]*len(FN)
        if len(ST) < len(FN) and len(ST) == 1:        ST = [ST[0]]*len(FN)
        if len(FHR_S) < len(FN) and len(FHR_S) == 1:  FHR_S = [FHR_S[0]]*len(FN)
        if len(FHR_E) < len(FN) and len(FHR_E) == 1:  FHR_E = [FHR_E[0]]*len(FN)
        if len(AGE) < len(FN) and len(AGE) == 1:      AGE = [AGE[0]]*len(FN)

        # Create new lists to add to and query the database
        self.A_DATA1,self.A_DATA2 = [],[]
        self.A_VAL_S4,self.A_VAL_S5,self.A_VAL_S6 = [],[],[]
        self.A_VAL_S1 = [CY[0],TC[0]]
        self.A_VAL_S2 = [CY[0],TC[0],FN[0]]
        self.A_VAL_S3 = [CY[0],TC[0],AGE[0]]
        for (F,C,T,S,F1,F2,A) in zip(FN,CY,TC,ST,FHR_S,FHR_E,AGE):
            #print([C,T,F,S,F1,F2,A])
            self.A_DATA1.append([C,T,F,S,F1,F2,A])
            self.A_DATA2.append([F1,F2,A])
            self.A_VAL_S4.append([C,T,F])
            self.A_VAL_S5.append([C,F])
            self.A_VAL_S6.append([A])
            


    ###########################################################
    def retrieve_atcf_status(self,UPDATE=True,**kwargs):
        """Retrieve (or create) the GPLOT status for this ATCF.
        @param self:   the instance of the class (no need to pass this)
        @kwarg UPDATE: logical to update the database
        @kwargs:       extra keywords are allowed
        """

        # Check for the logging object
        L = kwargs.get('logger',Main_Logger('Database'))

        self.A_STATUS = []
        for (R,N) in zip(self.A_DATA1,range(len(self.A_DATA1))):
            try:
                S = select_rows([self.A_TBL],[self.COL_STATUS],DFILE=self.DB_FILE,C2=self.A_COL_S2,V2=self.A_VAL_S4[N],**kwargs)
                if S.empty:
                    raise Exception
            except Exception:
                L.logger.info("Creating an entry in the database table '"+self.A_TBL+"'.")
                add_table_row(self.A_TBL,[R],CNAME=self.A_COL_ADD,DFILE=self.DB_FILE,**kwargs)
                S = select_rows([self.A_TBL],[self.COL_STATUS],DFILE=self.DB_FILE,C2=self.A_COL_S1,V2=self.A_VAL_S1,**kwargs)

            # Extract the value from the DateFrame into a list
            self.A_STATUS = self.A_STATUS + S[self.COL_STATUS].tolist()

            # Update the table as long as its a real storm
            if UPDATE and R[1] != '00L':
                update_table(self.A_TBL,self.A_COL_U1,self.A_DATA2[N],C2=self.A_COL_S3,V2=self.A_VAL_S5[N],DFILE=self.DB_FILE,**kwargs)
            #print(select_rows([self.A_TBL],["*"],DFILE=self.DB_FILE,C2=self.A_COL_S3,V2=self.A_VAL_S5[N]))
            #print(select_rows([self.A_TBL],["*"],DFILE=self.DB_FILE,C2=['cycle'],V2=['2018080906']))

        print(select_rows([self.A_TBL],["*"],DFILE=self.DB_FILE,C2=self.A_COL_S1,V2=self.A_VAL_S1))


    ###########################################################
    def assign_graphic_data(self,MOD,EN,CY,TC,DM,GR,GPOUT):
        """Assign data for the 'graphic_status' table
        @param self:  the instance of the class (no need to pass this)
        @param MOD:   the GPLOT module
        @param EN:    the ensemble ID
        @param CY:    the GPLOT cycle, could be an array
        @param TC:    the storm ID
        @param DM:    the GPLOT domain
        @param GR:    the graphic name
        @param GPOUT: the base GPLOT output directory
        """

        if EN == 99:  EN = 'null'
 
        self.MOD = MOD
        self.EN = EN
        self.CY = CY
        self.TC = TC
        self.DM = DM
        self.GR = GR
        self.GPOUT = GPOUT

        self.FNAME = GR+'.'+CY+'.'+DM


    ###########################################################
    def retrieve_graphic_status(self,NML,FILES,UPDATE=True,**kwargs):
        """Retrieve (or create) the GPLOT status for this graphic.
        @param self:   the instance of the class (no need to pass this)
        @param NML:    the master namelist object
        @param FILES:  the list of file names found for this task
        @kwarg UPDATE: logical to update the database
        @kwargs:       extra keywords are allowed
        @return S:     the status of this particular GPLOT ATCF
        """

        # Check for the logging object
        L = kwargs.get('logger',Main_Logger('Database'))

        # Don't assign a Storm ID if the graphic is not storm-centered
        if not NML.IS_STORMCEN:  self.TC = 'null'

        # Update the status of the current job (module, ensid, cycle, storm, domain, graphic)
        # Must loop over all forecast hours.
        self.G_STATUS = []
        self.G_ACTIVE = False
        for FH in range(NML.HR_INIT,NML.HR_FNL+NML.DT,NML.DT):
            # Determine if any found file match
            FMT = gpu.string_format_int(NML.FILE_HFMT)
            FH2 = FMT.format(FH)
            RE = r'^.*'+self.CY+'.*'+NML.FILE_FHR+FH2+'.*$'
            MATCH = []
            for F in FILES[0]:  MATCH = MATCH+gpu.list_find(F,RE)
            if not MATCH:  continue

            # Assign the data
            FULLNAME = self.FNAME+'.'+NML.FILE_FHR+FH2
            self.G_VAL1 = [self.MOD,self.EN,self.CY,self.TC,self.DM,FH,self.GR]
            self.G_VAL2 = [self.MOD,self.EN,self.CY,self.TC,self.DM,self.GR]
            self.G_DATA1 = [self.MOD,self.EN,self.CY,self.TC,self.DM,FH,self.GR,'started',datetime.datetime.now(),\
                            self.GPOUT,FULLNAME,'null','null','null','null','null','null','null','null']

            # Attempt to retreive the status, and, if successful, update the table entry.
            # If it doesn't exist, create the table entry.
            try:
                S = select_rows([self.G_TBL],[self.COL_STATUS],DFILE=self.DB_FILE,C2=self.G_COL3,V2=self.G_VAL1,logger=L)
                if S.empty:  raise Exception
                L.logger.debug("Found the status for this graphic.")
            except Exception:
                for (M,N) in zip(MATCH,range(len(MATCH))):  self.G_DATA1[11+N] = M

                if NML.DO_COMPARE:
                    MATCH2 = []
                    for F in FILES[1]:  MATCH2 = MATCH2+gpu.list_find(F,RE)
                    if MATCH2:
                        for (M,N) in zip(MATCH2,range(len(MATCH2))):  self.G_DATA1[15+N] = M
                add_table_row(self.G_TBL,[tuple(self.G_DATA1)],CNAME=self.G_COL1,DFILE=self.DB_FILE)
                S = select_rows([self.G_TBL],[self.COL_STATUS],DFILE=self.DB_FILE,\
                                C2=self.G_COL3,V2=self.G_VAL1,logger=L)
            else:
                if UPDATE:
                    for (M,N) in zip(MATCH,range(len(MATCH))):
                        update_table(self.G_TBL,[self.G_COL4[N]],[M],C2=self.G_COL3,V2=self.G_VAL1,DFILE=self.DB_FILE,logger=L)
                    if NML.DO_COMPARE:
                        for F in FILES[1]:  MATCH2 = MATCH2+gpu.list_find(F,RE)
                        if MATCH2:
                            for (M,N) in zip(MATCH2,range(len(MATCH2))):
                                update_table(self.G_TBL,[self.G_COL5[N]],[M],C2=self.G_COL3,V2=self.G_VAL1,DFILE=self.DB_FILE,logger=L)
            self.G_STATUS = self.G_STATUS+S[self.COL_STATUS].tolist()
        #print(select_rows(self.G_TBL,self.G_COL2,DFILE=self.DB_FILE,C2=self.G_COL6,V2=self.G_VAL2,logger=L))

        # Check if any graphic statuses are not complete.
        if [i for i in self.G_STATUS if i in self.ACTIVE_STATUS]:  self.G_ACTIVE = True


    ###########################################################
    def update_tbl_by_age(self,AGE=650000,**kwargs):
        """Update table entries by age
        @param self: the instance of the class (no need to pass this)
        @kwarg AGE:  the threshold age for active/inactive
        @kwargs:     extra keywords are allowed
        """
        L = kwargs.get('logger',Main_Logger('Database'))
        update_table(self.A_TBL,self.A_COL_U2,['inactive'],C2=self.A_COL_S4,O2=['>'],V2=[AGE],DFILE=self.DB_FILE,logger=L)
        update_table(self.A_TBL,self.A_COL_U2,['active'],C2=self.A_COL_S4,O2=['<'],V2=[AGE],DFILE=self.DB_FILE,logger=L)

    ###########################################################
    def initiailize_tbl(self,**kwargs):
        """Initiailize the database status tables.
        @param self: the instance of the class (no need to pass this)
        @kwargs:     extra keywords are allowed
        """
        L = kwargs.get('logger',Main_Logger('Database'))
        create_table(self.M_TBL,self.M_COL,self.M_TYP,DFILE=self.DB_FILE,RM_TBL=False,close=True,logger=L)
        create_table(self.A_TBL,self.A_COL,self.A_TYP,DFILE=self.DB_FILE,close=True,RM_TBL=False,logger=L)
        create_table(self.G_TBL,self.G_COL,self.G_TYP,DFILE=self.DB_FILE,close=True,RM_TBL=True,logger=L)



###########################################################
#def add_table_row(TBL,HNAME,DATA,CONN=None,DFILE=None):
def add_table_row(TBL,DATA,CNAME=None,CONN=None,DFILE=None,close=True,**kwargs):
    """Add a row(s) to a table in the database
    @param TBL:   the database table
    @param HNAME: header names for this table
    @param DATA:  row(s) of data to be appended to the table
    @kwarg CONN:  the connection object
    @kwarg DFILE: the full path to the db file
    @kwarg close: logical about closing the db connection
    @kwargs:      extra keywords are allowed
    @return c.lastrowid: the generated id for this transaction
    """

    # Check for the logging object
    L = kwargs.get('logger',Main_Logger('Database'))

    # Create the connection (if necessary)
    CONN = check_connection(CONN=CONN,DFILE=DFILE)

    # Define helpful variables
    NROW = len(DATA[0])
    VALS = ','.join(("?")*NROW)

    # Create the table entry based on function input
    #table = "INSERT INTO "+TBL+"("+','.join(HNAME)+") VALUES("+VALS+")"
    if CNAME is None:
        sqlstr = "INSERT INTO "+str(TBL)+" VALUES ("+str(VALS)+")"
    else:
        COLS = ','.join(CNAME)
        sqlstr = "INSERT INTO "+str(TBL)+"("+COLS+") VALUES ("+str(VALS)+")"
    # Add data to the table
    if CONN is not None:
        try:
            c = CONN.cursor()
            if NROW > 1:
                #c.executemany(sqlstr+";",DATA)
                c.executemany(sqlstr,DATA)
            else:
                c.execute(sqlstr,DATA)
            CONN.commit()
        except Error as e:
            print(e)
    else:
        L.logger.warning("Database connection was not found.")
        L.logger.warning("Nothing added to table "+TBL)

    # Close the connection if requested
    if close:
        close_connection(CONN)
    
    # return the generated id for this transaction
    return(c.lastrowid);



###########################################################
def check_connection(CONN=None,DFILE=None,report=False,\
                     destroy=False,**kwargs):
    """Check on the connection object and create it if
       necessary.
    @kwarg CONN:    the connection object or None
    @kwarg DFILE:   the full path to the db file
    @kwarg report:  an option to report useful info
    @kwarg destroy: an option to destroy the connection
                    object if it exists.
    @kwargs:        Optional keywords are allowed.
    @return CONN: the connection objection
    """

    # Setup the logging object
    L = kwargs.get('logger',Main_Logger('Database'))

    # Check for the database file name.
    # The default is None, and it must be supplied.
    if DFILE is None:
        L.logger.info("Database file path not supplied. Bounce.")
        if CONN is None:
            L.logger.warning("The connection object CONN is also missing. This could be a problem.")
        return(CONN);

    # Check if reporting info about the file and connection
    if report:
        if os.path.exists(DFILE) and CONN is not None:
            L.logger.info("The database file already exists and you are connected to it.")
            L.logger.info(DFILE)
        elif not os.path.exists(DFILE):
            L.logger.info("The database file does not exist.")
        elif CONN is None:
            L.logger.info("The connection object does not exist.")
        else:
            L.logger.info("Something unexpected has happened.")

    # Check for the secret destroy sequence!
    if destroy:
        if os.path.exists(DFILE):  os.remove(DFILE)
        CONN=None

    # Create the connection object
    if CONN is None:
        CONN = create_connection(DFILE)

    # Return the connection object
    return(CONN);



###########################################################
def close_connection(CONN):
    """Close the connection to an sqlite database
    @CONN: the connection object
    """
    CONN.close()



###########################################################
def create_connection(DBFILE,mem=False,close=False,quiet=True,**kwargs):
    """
    Create a connection to a SQLite database.
    @param DBFILE: the db file path
    @kwarg mem:    Logical for memory usage
    @kwarg close:  Logical for closing connection.
    @kwarg quiet:  logical arg for printing information
    @kwargs:       optional keywords are allowed.
    @return CONN:  Connection object or None
    """

    # Setup the logging object
    L = kwargs.get('logger',Main_Logger('Database'))

    CONN = None;
    try:
        if mem:
            CONN = sq3.connect(':memory:')
        else:
            CONN = sq3.connect(DBFILE,
                               detect_types=sq3.PARSE_DECLTYPES | sq3.PARSE_COLNAMES)
            if not quiet:
                L.logger.info("The SQLite database version: "+sq3.version)
    except Error as e:
        L.logger.error(e)
    finally:
        if CONN and close:
            close_connection(CONN)
        return(CONN);



###########################################################
def create_table(TBL,HNAME,HTYPE,CONN=None,DFILE=None,\
                 HDEF=None,close=True,RM_TBL=False,**kwargs):
    """This function creates a database table.
    @param TBL:    the name of the SQL table
    @param HNAME:  a list of table headers
    @param HTYPE:  the type of each table column
    @kwarg CONN:   connection object. set to None to create here
    @kwarg DFILE:  the database full file path
    @kwarg HDEF:   The default values each column
    @kwarg close:  logical about closing the db connection
    @kwarg RM_TBL: logical to delete the table first
    @kwargs:       optional keywords are allowed.
    """
    #dbfile = r"/lfs1/projects/hur-aoml/Ghassan.Alaka/SCRATCH/pythonsqlite.db"

    # Setup the logging object
    L = kwargs.get('logger',Main_Logger('Database'))

    # Delete the table first, if required.
    if RM_TBL:
        delete_table(TBL,DFILE=DFILE,**kwargs)

    # Create the connection (if necessary)
    CONN = check_connection(CONN=CONN,DFILE=DFILE)

    # Correct size of HDEF if None
    if HDEF is None:
        HDEF = [None]*len(HNAME)
        for (H,N) in zip(HTYPE,range(len(HTYPE))):
            if H == "text":       HDEF[N] = "NOT NULL"
            elif H == "integer":  HDEF[N] = "NOT NULL"
            else:                 HDEF[N] = None

    # Check that HNAME/HTYPE/HDEF all have the same length
    if len(HNAME)!=len(HTYPE) or len(HNAME)!=len(HDEF):
        L.logger.error("HNAME,HTYPE,HDEF must all be the same length.")
        sys.exit(2)

    # Build the table entry
    sqlstr = "CREATE TABLE IF NOT EXISTS "+TBL+" (\n"
    for (H,T,D,N) in zip(HNAME,HTYPE,HDEF,range(len(HNAME))):
        
        # Next, always start with the header name (H)
        # followed by the type (T)
        sqlstr = sqlstr+"  "+str(H)+" "+str(T)
        if N == 0:
            if H == 'id':  sqlstr = sqlstr+" PRIMARY KEY AUTOINCREMENT"
            else:          sqlstr = sqlstr+" PRIMARY KEY"
        elif D is not None:
            sqlstr = sqlstr+" "+str(D)

        # Check if comma is necessary
        if N == len(HNAME)-1:  sqlstr = sqlstr+"\n"
        else:                  sqlstr = sqlstr+",\n"

    # Close out the table creation call
    sqlstr = sqlstr+"); "
    
    # Create the table. If something goes wrong,
    # it will print an error.
    if CONN is not None:
        try:
            c = CONN.cursor()
            c.execute(sqlstr)
            CONN.commit()
        except Error as e:
            L.logger.error(e)
    else:
        L.logger.warning("Database connection was not found.")
        L.logger.warning("Table "+TBL+" has not been created.")

    # Close the connection if requested
    if close:  close_connection(CONN)



###########################################################
def db_name(DIR,EXPT,quiet=True,**kwargs):
    """Return the database file name.
    @param DIR:    prefix of the complete directory
    @param EXPT:   the GPLOT experiment name
    @kwarg quiet:  logical arg for printing information
    @kwargs:       optional keywords are allowed
    @return FNAME: the full path to the db file
    """

    # Setup the logging object
    L = kwargs.get('logger',Main_Logger('Database'))

    if os.path.isdir(DIR):
        FNAME = DIR+"/db/pysqlite."+EXPT+".db"
        try:
            os.mkdir(DIR+"/db")
        except FileExistsError:
            if not quiet:
                L.logger.warning(DIR+"/db already exists.")
    else:
        L.logger.error("Database directory can't be created --> "+DIR)
        sys.exit(2)
    return(FNAME);




###########################################################
def db_retrieve(TBL,EXPT):
    """Retrieve the database file name from a GPLOT table.
    @param TBL:      the path to the GPLOT table
    @param EXPT:     the GPLOT experiment name
    @return DB_FILE: the database file name
    """
    DB_FILE = None
    if os.path.exists(TBL):
        with open(TBL, "r") as f:
            for line in f.readlines():
                if EXPT in line:
                    DB_FILE = line.split(',')[1].strip()
                    break
            f.close()
    return(DB_FILE);




###########################################################
def delete_all_rows(TBL,CONN=None,DBFILE=None,**kwargs):
    """Delete all rows in a given table. Return the number of rows deleted.
    @param TBL:   the name of the SQL table
    @kwarg CONN:  connection object. set to None to create here
    @kwarg DFILE: the database full file path
    @kwargs:       optional keywords are allowed
    @return NROW: the number of deleted rows
    """

    # Setup the logging object
    L = kwargs.get('logger',Main_Logger('Database'))

    # Create the connection (if necessary)
    CONN = check_connection(CONN=CONN,DFILE=DFILE)

    # Build the SQL execute statement
    sqlstr = "DELETE FROM "+TBL

    # Delete all rows
    if CONN is not None:
        try:
            c = CONN.cursor()
            NROW = c.execute(sqlstr).rowcount
        except Error as e:
            L.logger.error(e)
    else:
        L.logger.warning("Database connection was not found.")
        L.logger.warning("Table "+TBL+" rows have not been deleted.")

    # Return the number of rows
    return(NROW);



###########################################################
def delete_table(TBL,CONN=None,DFILE=None,close=True,**kwargs):
    """Delete a table
    @param TBL:   the name of the SQL table
    @kwarg CONN:  connection object. set to None to create here
    @kwarg DFILE: the database full file path
    @kwarg close: logical about closing the db connection
    @kwargs:      optional keywords are allowed
    """

    # Setup the logging object
    L = kwargs.get('logger',Main_Logger('Database'))

    # Create the connection (if necessary)
    CONN = check_connection(CONN=CONN,DFILE=DFILE)

    # Build the SQL execute statement
    sqlstr = "drop table if exists "+TBL

    # Delete the table. If something goes wrong, print an error.
    if CONN is not None:
        try:
            c = CONN.cursor()
            c.execute(sqlstr)
            CONN.commit()
        except Error as e:
            L.logger.error(e)
    else:
        L.logger.warning("Database connection was not found.")
        L.logger.warning("Table "+TBL+" has not been deleted.")

    # Close the connection if requested
    if close:
        close_connection(CONN)



###########################################################
def list_tables(CONN=None,DFILE=None):
    """List all tables in the db file
    @kwarg CONN:  the connection object or None
    @kwarg DFILE: full path to the db file
    """

    # Check or create the connection object
    CONN = check_connection(CONN=CONN,DFILE=DFILE)

    # Build the select statement
    sqlstr = 'SELECT name from sqlite_master where type= "table"'

    # Actually execute the data query
    c = CONN.cursor()
    c.execute(sqlstr)
    TABLES = c.fetchall()

    # Return the list of table names
    return(TABLES);



###########################################################
def select_rows(TBL,C1,CONN=None,DFILE=None,C2=None,V2=None,quiet=True,close=True,**kwargs):
    """Query specific rows in the table based on criteria.
    @param TBL:   the table name(s), could be an array
    @param C1:    the column to search, could be an array
    @kwarg CONN:  the connection object or None
    @kwarg DFILE: full path to the db file
    @kwarg C2:    the column to filter, could be an array
    @kwarg V2:    the criteria by which to filter, could be an array
    @kwarg quiet: Print some extra information if set to False
    @kwarg close: Logical about closing the db connection
    @kwargs:      optional keywords are allowed
    @return DATA: a 2-d array of the data
    """

    # Setup the logging object
    L = kwargs.get('logger',Main_Logger('Database'))

    # Checks
    if C2 is not None:
        if len(C2) != len(V2):
            L.logger.error("C2 and V2 must match to build selection statements.")
            sys.exit(2)

    # Check or create the connection object
    CONN = check_connection(CONN=CONN,DFILE=DFILE)

    # Build the select statement, including what columns to return
    # To select all columns, COL1 = "*"
    sqlstr = "SELECT"
    for (C,N) in zip(C1,range(len(C1))):
        if N == 0:
            sqlstr = sqlstr+" "+C
        elif N > 0:
            sqlstr = sqlstr+", "+C

    # Add the table(s) from which to search
    sqlstr = sqlstr+"\nFROM"
    if type(TBL) == list:
        for T in list(TBL):
            sqlstr = sqlstr+" "+T
    else:
        sqlstr = sqlstr+" "+TBL

    # Add in search criteria, if applicable
    if C2 is not None:
        sqlstr = sqlstr+"\nWHERE"
        for (C,V,N) in zip(C2,V2,range(len(C2))):
            V = set_db_type(V)
            if N == 0:
                sqlstr = sqlstr+" "+C+"="+V
            elif N > 0:
                sqlstr = sqlstr+" AND "+C+"="+V

    # End with a semicolon for read_sql_query()
    sqlstr = sqlstr+";"

    # Read the data into a pandas DataFrame
    DATA = pd.read_sql_query(sqlstr,CONN)

    # Report the rows if required
    if not quiet:
        for (D,N) in zip(DATA,range(len(DATA))):
            L.logger.info(D)

    # Close the connection if requested
    if close:
        close_connection(CONN)

    # Return ROWS
    return(DATA);



###########################################################
def set_db_type(V):
    """Set the variable type when writing to database
    @param V: the variable
    @return V
    """
    # Correct V based on its type
    if type(V) == str:
        V = "'"+V+"'"
    elif type(V) in [int,np.int64,np.int32,float]:
        V = str(V)
    if V is None:
        V = 'null'

    return(V);




###########################################################
def update_table(TBL,C1,V1,C2=[None],O2=[None],V2=[None],CONN=None,DFILE=None,close=True,**kwargs):
    """This function creates a database table.
    @param TBL:   the name of the SQL table
    @param C1:    a list of columns to set
    @param V1:    a list of values to set
    @kwarg C2:    a list of columns to filter by
    @kwarg O2:    a list of operators to filter by
    @kwarg V2:    a list of values to filter by
    @kwarg CONN:  connection object. set to None to create here
    @kwarg DFILE: the database full file path
    @kwarg close: Logical about closing the db connection
    @kwargs:      optional keywords are allowed
    """
    #dbfile = r"/lfs1/projects/hur-aoml/Ghassan.Alaka/SCRATCH/pythonsqlite.db"

    # Setup the logging object
    L = kwargs.get('logger',Main_Logger('Database'))

    # Check that C1/V1 & C2/V2 lengths match
    if len(C1) != len(V1):
        L.logger.error("The length of C1 ("+str(len(C1))+") must match V1 ("+str(len(V1))+").")
        sys.exit(2)
    if C2[0] is not None and V2[0] is not None:
      if len(C2) != len(V2):
        L.logger.error("The length of C2 ("+str(len(C2))+") must match V2 ("+str(len(V2))+").")
        sys.exit(2)
    if C2[0] is not None and O2[0] is not None:
      if len(C2) != len(O2):
        L.logger.error("The length of C2 ("+str(len(C2))+") must match O2 ("+str(len(O2))+").")
        sys.exit(2)

    # Create the connection (if necessary)
    CONN = check_connection(CONN=CONN,DFILE=DFILE)

    # Build the table entry
    sqlstr = "UPDATE "+TBL+"\nSET "

    # Build the list of items to update
    for (C,V,N) in zip(C1,V1,range(len(C1))):
        V = set_db_type(V)    # Correct V based on its type

        # Next, build the list of data to update
        if N == 0:   sqlstr = sqlstr+C+" = "+V
        elif N > 0:  sqlstr = sqlstr+",\n    "+C+" = "+V

    # Build the select statement, adding the columns that will be filtered
    # Note that this is optional and is skipped when C2=V2=None
    if C2[0] is not None and V2[0] is not None:
        if O2[0] is None:  O2 = ["="]*len(C2)
        for (C,O,V,N) in zip(C2,O2,V2,range(len(C2))):
            V = set_db_type(V)    # Correct V based on its type
    
            # Next, built the list of filter criteria
            if N == 0:   sqlstr = sqlstr+"\nWHERE "+C+" "+O+" "+V
            elif N > 0:  sqlstr = sqlstr+" AND "+C+" "+O+" "+V

    sqlstr = sqlstr+";"

    # Actually execute the data query
    c = CONN.cursor()
    c.execute(sqlstr)
    CONN.commit()

    # Close the connection if requested
    if close:  close_connection(CONN)



#if __name__ == '__main__':
	#create_connection_mem()
	#create_connection(r"/lfs1/project/hur-aoml/Ghassan.Alaka/SCRATCH/test.db")
	#main()


