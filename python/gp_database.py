#!/usr/bin/env python

# Import necessary modules
import os, sys
import sys
import itertools as it
import pandas as pd
import sqlite3 as sq3
from sqlite3 import Error

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
Last Modified: February 27, 2020

Example call: For Internal Calls Only

Modification Log:
2020-Feb-14:  Added several functions to improve table creation in a database
2020-Feb-27:  GJA changed the file name from database.py to gp_database.py

"""

__version__ = '0.2.0';




###########################################################
#def add_table_row(TBL,HNAME,DATA,CONN=None,DFILE=None):
def add_table_row(TBL,DATA,CONN=None,DFILE=None,close=True):
    """Add a row(s) to a table in the database
    @param TBL:   the database table
    @param HNAME: header names for this table
    @param DATA:  row(s) of data to be appended to the table
    @kwarg CONN:  the connection object
    @kwarg DFILE: the full path to the db file
    @kwarg close: Logical about closing the db connection
    @return c.lastrowid: the generated id for this transaction
    """

    # Create the connection (if necessary)
    CONN = check_connection(CONN=CONN,DFILE=DFILE)

    # Define helpful variables
    NROW = len(DATA[0])
    VALS = ','.join(("?")*NROW)
    # Create the table entry based on function input
    #table = "INSERT INTO "+TBL+"("+','.join(HNAME)+") VALUES("+VALS+")"
    sqlstr = "INSERT INTO "+str(TBL)+" VALUES ("+str(VALS)+")"
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
        print("WARNING: Database connection was not found.")
        print("WARNING: Nothing added to table "+TBL)

    # Close the connection if requested
    #if close:
    #    close_connection(CONN)
    
    # return the generated id for this transaction
    return(c.lastrowid);



###########################################################
def check_connection(CONN=None,DFILE=None,report=False,destroy=False):
    """Check on the connection object and create it if
       necessary.
    @kwarg CONN:    the connection object or None
    @kwarg DFILE:   the full path to the db file
    @kwarg report:  an option to report useful info
    @kwarg destroy: an option to destroy the connection
                    object if it exists.
    @return CONN: the connection objection
    """

    # Check for the database file name.
    # The default is None, and it must be supplied.
    if DFILE is None:
        print("MSG: Database file path not supplied. Bounce.")
        if CONN is None:
            print("WARNING: The connection object CONN is also missing. This could be a problem.")
        return(CONN);

    # Check if reporting info about the file and connection
    if report:
        if os.path.exists(DFILE) and CONN is not None:
            print("MSG: the database file already exists and you are connected to it.")
            print("MSG: "+DFILE)
        elif not os.path.exists(DFILE):
            print("MSG: The database file does not exist.")
        elif CONN is None:
            print("MSG: The connection object does not exist.")
        else:
            print("MSG: Something unexpected is happening.")

    # Check for the secret destroy sequence!
    if destroy:
        if os.path.exists(DFILE):
            os.remove(DFILE)
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
def create_connection(DBFILE,mem=False,close=False,quiet=True):
    """
    Create a connection to a SQLite database.
    @param DBFILE: the db file path
    @kwarg mem:    Logical for memory usage
    @kwarg close:  Logical for closing connection.
    @kwarg quiet:  logical arg for printing information
    @return CONN:  Connection object or None
    """
    CONN = None;
    try:
        if mem:
            CONN = sq3.connect(':memory:')
        else:
            CONN = sq3.connect(DBFILE)
            if not quiet:
                print("MSG: The SQLite database version: "+sq3.version)
    except Error as e:
        print(e)
    finally:
        if CONN and close:
            close_connection(CONN)
        return(CONN);



###########################################################
def create_table(TBL,HNAME,HTYPE,CONN=None,DFILE=None,HDEF=None,close=True):
    """This function creates a database table.
    @param TBL:   the name of the SQL table
    @param HNAME: a list of table headers
    @param HTYPE: the type of each table column
    @kwarg CONN:  connection object. set to None to create here
    @kwarg DFILE: the database full file path
    @kwarg HDEF:  The default values each column
    @kwarg close: Logical about closing the db connection
    """
    #dbfile = r"/lfs1/projects/hur-aoml/Ghassan.Alaka/SCRATCH/pythonsqlite.db"

    # Create the connection (if necessary)
    CONN = check_connection(CONN=CONN,DFILE=DFILE)

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
    sqlstr = "CREATE TABLE IF NOT EXISTS "+TBL+" (\n"
    for (H,T,D,N) in zip(HNAME,HTYPE,HDEF,range(len(HNAME))):
        
        # Next, always start with the header name (H)
        # followed by the type (T)
        sqlstr = sqlstr+"  "+str(H)+" "+str(T)
        if N == 0:
            if H == 'id':
                sqlstr = sqlstr+" PRIMARY KEY AUTOINCREMENT"
            else:
                sqlstr = sqlstr+" PRIMARY KEY"
        elif D is not None:
            sqlstr = sqlstr+" "+str(D)

        # Check if comma is necessary
        if N == len(HNAME)-1:
            sqlstr = sqlstr+"\n"
        else:
            sqlstr = sqlstr+",\n"

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
            print(e)
    else:
        print("WARNING: Database connection was not found.")
        print("WARNING: Table "+TBL+" has not been created.")

    # Close the connection if requested
    if close:
        close_connection(CONN)



###########################################################
def db_name(DIR,EXPT,quiet=True):
    """Return the database file name.
    @param DIR:    prefix of the complete directory
    @param EXPT:   the GPLOT experiment name
    @kwarg quiet:  logical arg for printing information
    @return FNAME: the full path to the db file
    """
    if os.path.isdir(DIR):
        FNAME = DIR+"/db/pysqlite."+EXPT+".db"
        try:
            os.mkdir(DIR+"/db")
        except FileExistsError:
            if not quiet:
                print("WARNING: "+DIR+"/db already exists.")
    else:
        print("ERROR: Database directory can't be created --> "+DIR)
        sys.exit(2)
    return(FNAME);



###########################################################
def delete_all_rows(TBL,CONN=None,DBFILE=None):
    """Delete all rows in a given table. Return the number of rows deleted.
    @param TBL:   the name of the SQL table
    @kwarg CONN:  connection object. set to None to create here
    @kwarg DFILE: the database full file path
    @return NROW: the number of deleted rows
    """

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
            print(e)
    else:
        print("WARNING: Database connection was not found.")
        print("WARNING: Table "+TBL+" rows have not been deleted.")

    # Return the number of rows
    return(NROW);



###########################################################
def delete_table(TBL,CONN=None,DFILE=None,close=True):
    """Delete a table
    @param TBL:   the name of the SQL table
    @kwarg CONN:  connection object. set to None to create here
    @kwarg DFILE: the database full file path
    @kwarg close: Logical about closing the db connection
    """

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
            print(e)
    else:
        print("WARNING: Database connection was not found.")
        print("WARNING: Table "+TBL+" has not been deleted.")

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
def select_rows(TBL,COL1,CONN=None,DFILE=None,COL2=None,CRIT=None,quiet=True,close=True):
    """Query specific rows in the table based on criteria.
    @param TBL:   the table name(s), could be an array
    @param COL1:  the column to search, could be an array
    @kwarg CONN:  the connection object or None
    @kwarg DFILE: full path to the db file
    @kwarg COL2:  the column to filter, could be an array
    @kwarg CRIT:  the criteria by which to filter, could be an array
    @kwarg quiet: Print some extra information if set to False
    @kwarg close: Logical about closing the db connection
    @return DATA: a 2-d array of the data
    """
    # Checks
    if COL2 is not None:
        if len(COL2) != len(CRIT):
            print("ERROR: COLM and CRIT must match to build selection statements.")
            sys.exit(2)

    # Check or create the connection object
    CONN = check_connection(CONN=CONN,DFILE=DFILE)

    # Build the select statement, including what columns to return
    # To select all columns, COL1 = "*"
    sqlstr = "SELECT"
    for (C,N) in zip(COL1,range(len(COL1))):
        if N < len(COL1)-1:
            sqlstr = sqlstr+" "+C+","
        else:
            sqlstr = sqlstr+" "+C

    # Add the table(s) from which to search
    sqlstr = sqlstr+" FROM"
    if type(TBL) == list:
        for T in list(TBL):
            sqlstr = sqlstr+" "+T
    else:
        sqlstr = sqlstr+" "+TBL

    # Add in search criteria, if applicable
    if COL2 is not None:
        sqlstr = sqlstr+" WHERE"
        for (C,N) in zip(COL2,range(len(COL2))):
            if N < len(COL2)-1:
                sqlstr = sqlstr+" "+C+"=? AND"
            else:
                sqlstr = sqlstr+" "+C+"=?"

    DATA = pd.read_sql_query(sqlstr,CONN)

    # Report the rows if required
    if not quiet:
        for (D,N) in zip(DATA,range(len(DATA))):
            print(D)

    # Close the connection if requested
    if close:
        close_connection(CONN)

    # Return ROWS
    return(DATA);



###########################################################
def update_table(TBL,HNAME,COLM,DATA,CONN=None,DFILE=None,close=True):
    """This function creates a database table.
    @param TBL:   the name of the SQL table
    @param HNAME: a list of table headers
    @param COLM:  the column to filter, could be an array
    @param DATA:  the data and the criteria
    @kwarg CONN:  connection object. set to None to create here
    @kwarg DFILE: the database full file path
    @kwarg close: Logical about closing the db connection
    """
    #dbfile = r"/lfs1/projects/hur-aoml/Ghassan.Alaka/SCRATCH/pythonsqlite.db"

    # Create the connection (if necessary)
    CONN = check_connection(CONN=CONN,DFILE=DFILE)

    # Build the table entry
    sqlstr = "UPDATE "+TBL+" "
    for (H,N) in zip(HNAME,range(len(HNAME))):

        # Next, always start with the header name (H)
        # followed by the type (T)
        sqlstr = sqlstr+" SET "
        if N != len(HNAME)-1:
            sqlstr = sqlstr+" "+H+" = ? ,"
        else:
            sqlstr = sqlstr+" "+H+" ?"

    # Build the select statement, adding the columns that will be filtered
    for (C,N) in zip(COLM,len(COLM)):
        if N == 0:
            sqlstr = sqlstr+" WHERE "+C+" = ?"
        elif N > 0:
            sqlstr = sqlstr+" AND "+C+" = ?"

    # Actually execute the data query
    c = CONN.cursor()
    c.execute(sqlstr,(DATA))
    CONN.commit()

    # Close the connection if requested
    if close:
        close_connection(CONN)



#if __name__ == '__main__':
	#create_connection_mem()
	#create_connection(r"/lfs1/project/hur-aoml/Ghassan.Alaka/SCRATCH/test.db")
	#main()


