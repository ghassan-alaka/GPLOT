#!python

# Import necessary modules
import itertools as it
import os
import sys
import sqlite3 as sq3
from sqlite3 import Error

"""
Package database: Python functionality for databases. This script is not
                  meant to be called directly. Rather, it should be loaded
                  in appropriate python code.

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

Example call: None (Don't Call)

Modification Log:


"""

__version__ = '0.1.0';




###########################################################
def add_table_row(TNAME,HNAME,DATA,CONN=None):
    """Add a row(s) to a table in the database
    @param TNAME: the database table
    @param HNAME: header names for this table
    @param DATA:  row(s) of data to be appended to the table
    @param CONN:  the connection object.
                  default CONN=None
    @return c.lastrowid: the generated id for this transaction
    """

    # Use the secret destroy sequence!
    if CONN == "destroy":
        os.remove(DFILE)
        CONN=None

    # Create the CONN connection
    if CONN == None:
        CONN = create_connection(DFILE)

    # Define helpful variables
    NROW = len(DATA)
    VALS = ','.join(("?")*len(DATA[0]))

    # Create the table entry based on function input
    table = "INSERT INTO "+TNAME+"("+','.join(HNAME)+") VALUES("+VALS+")"

    # Create a cursor object
    c = CONN.cursor()

    # Actually add the data to the
    if NROW > 1:
        c.executemany(table+";",DATA)
    else:
        c.execute(table,DATA)

    # Commit the changes to the db
    CONN.commit()

    # return the generated id for this transaction
    return(c.lastrowid);



###########################################################
def create_connection(DBFILE,mem=False,close=False):
    """
    Create a connection to a SQLite database.
    @param DBFILE: Connection object
    @param mem:    Logical for memory usage
    @param close   Logical for closing connection.
    @return CONN:  Connection object or None
    """
    CONN = None;
    try:
        if mem:
            CONN = sq3.connect(':memory:')
        else:
            CONN = sq3.connect(DBFILE)
            print(sq3.version)
    except Error as e:
        print(e)
    finally:
        if CONN and close:
            CONN.close()
        return(CONN);



###########################################################
def create_table(DFILE,TNAME,HNAME,HTYPE,CONN=None,HDEF=None,close=False):
    """This function creates a database table.
    @param DFILE: the database full file path
    @param TNAME: the name of the SQL table
    @param HNAME: a list of table headers
    @param HTYPE: the type of each table column
    @param CONN:  connection object. set to None to create here
    @param HDEF:  The default values each column
    @param close: Logical about closing the db connection
    """
    #dbfile = r"/lfs1/projects/hur-aoml/Ghassan.Alaka/SCRATCH/pythonsqlite.db"

    # Use the secret destroy sequence!
    if CONN == "destroy":
        os.remove(DFILE)
        CONN=None        

    # Create the CONN connection
    if CONN == None:
        CONN = create_connection(DFILE)

    # Correct size of HDEF if None
    if HDEF is None:
        HDEF = [None]*len(HNAME)
        for (H,N) in zip(HTYPE,range(len(HTYPE))):
            if H == "text":
                HDEF[N] = "NOT NULL"
            elif H == "integer":
                HDEF[N] = "NOT NULL"
            else:
                HDEF[N] = None

    # Check that HNAME/HTYPE/HDEF all have the same length
    if len(HNAME)!=len(HTYPE) or len(HNAME)!=len(HDEF):
        print("ERROR: HNAME,HTYPE,HDEF must all be the same length.")
        sys.exit(2)

    # Build the table entry
    table = "CREATE TABLE IF NOT EXISTS "+TNAME+" (\n"
    for (H,T,D,N) in zip(HNAME,HTYPE,HDEF,range(len(HNAME))):
        
        # Next, always start with the header name (H)
        # followed by the type (T)
        table = table+"  "+str(H)+" "+str(T)
        if N == 0:
            table = table+" PRIMARY KEY"
        elif D is not None:
            table = table+" "+str(D)

        # Check if comma is necessary
        if N == len(HNAME)-1:
            table = table+"\n"
        else:
            table = table+",\n"

    # Close out the table creation call
    table = table+"); "
    print(table)

    # Create the table. If something goes wrong,
    # it will print an error.
    if CONN is not None:
        try:
            c = CONN.cursor()
            c.execute(table)
        except Error as e:
            print(e)
    else:
        print("WARNING: Database connection was not found.")
        print("WARNING: Table "+TNAME+" has not been created.")

    # Close the connection if requested
    if close:
        CONN.close()



###########################################################
def db_name(IDIR,EXPT):
    """Return the database file name.
    @param IDIR: prefix of the complete directory
    @param EXPT: the GPLOT experiment name
    @return FNAME: the full path to the db file
    """

    FNAME = IDIR+"/db/pysqlite."+EXPT+".db"
    return(FNAME);




#if __name__ == '__main__':
	#create_connection_mem()
	#create_connection(r"/lfs1/project/hur-aoml/Ghassan.Alaka/SCRATCH/test.db")
	#main()


