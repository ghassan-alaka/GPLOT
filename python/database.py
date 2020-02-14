#!python

# Import necessary modules
import os
import sys
import subprocess
import sqlite3
from sqlite3 import Error

"""
Package database: Python functionality for databases.

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

Example call: python ./database.py

Modification Log:


"""

__version__ = '0.1.0';




def create_connection_mem():
	"""
	Create a batabase connection to a SQLite database that resides
	in the memory.
	:param conn: Connection object
	:return: Connection object or None
	"""
	conn = None;
	try:
		conn = sqlite3.connect(':memory:')
		print(sqlite3.version)
	except Error as e:
		print(e)
	#finally:
	#	if conn:
	#		conn.close()

	return conn


def create_connection(db_file):
	"""
	Create a batabase connection to a SQLite database.
	:param conn: Connection object
	:return: Connection object or None
	"""
	conn = None;
	try:
		conn = sqlite3.connect(db_file)
		print(sqlite3.version)
	except Error as e:
		print(e)
	#finally:
	#        if conn:
	#                conn.close()

	return conn


def create_table(conn, create_tbl_sql):
	"""
	Create a table from the create_tbl_sql statement.
	:param conn: Connection object
	:param create_tbl_sql: a CREATE TABLE statement
	"""
	try:
		c = conn.cursor()
		c.execute(create_tbl_sql)
	except Error as e:
		print(e)


def create_nml_table(CONN,DFILE,TNAME,HNAME,HTYPE):
    """This function creates a project entry based on the 
       namelist.
    @param CONN:  connection object
    @param DFILE: the database full file path
    @param TNAME: the name of the SQL table
    @param HNAME: a list of table headers
    @param HTYPE: the type of each table column
    """
    #dbfile = r"/lfs1/projects/hur-aoml/Ghassan.Alaka/SCRATCH/pythonsqlite.db"

        
    table = "CREATE TABLE IF NOT EXISTS "+TNAME+""" (
               entry text NOT NULL,
               value text,
            ); """

     # create tables
    if conn is not None:
        create_table(conn, table)
    else:
        print("ERROR: Cannot create the database connection.")


def add_nml_table_row(CONN,DBKEY,ROW):
    """This function adds a row to a table in the database
       for namelist entries.
    @param CONN: the connection object
    @param DBKEY: The database table that we're adding to
    @param ROW:   The row of data that will be appended to the table
    """

    table = """INSERT INTO """+DBKEY+"""(entry,value)
               VALUES(?,?)"""

    c = CONN.cursor()
    c.execute(table,ROW)
    return c.lastrowid



#if __name__ == '__main__':
	#create_connection_mem()
	#create_connection(r"/lfs1/project/hur-aoml/Ghassan.Alaka/SCRATCH/test.db")
	#main()


