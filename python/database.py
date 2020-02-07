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


def main():
	database = r"/lfs1/projects/hur-aoml/Ghassan.Alaka/SCRATCH/pythonsqlite.db"

	sql_test_table = """ CREATE TABLE IF NOT EXISTS projects (
				id integer PRIMARY KEY,
                                name text NOT NULL,
                                begin_date text,
                                end_date text
                         ); """

	# create a database connection
	conn = create_connection(database)

	# create tables
	if conn is not None:
		create_table(conn, sql_test_table)
	else:
		print("ERROR: Cannot create the database connection.")



if __name__ == '__main__':
	#create_connection_mem()
	#create_connection(r"/lfs1/project/hur-aoml/Ghassan.Alaka/SCRATCH/test.db")
	main()


