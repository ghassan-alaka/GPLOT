#!/usr/bin/env python

# Import necessary modules
import os, sys
import logging
#import numpy as np
#import itertools as it
#import pandas as pd
#import sqlite3 as sq3
#from sqlite3 import Error
#sq3.register_adapter(np.int64, lambda val: int(val))
#sq3.register_adapter(np.int32, lambda val: int(val))

"""
GPLOT Package log
Python functionality for logging. This script is not
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
Date Created:  March 19, 2020
Last Modified: 

Example call: For Internal Calls Only

Modification Log:

"""

__version__ = '0.1.0';


###########################################################
class Main_Logger:
    """This is the primary log class for GPLOT.

    """

    ###########################################################
    def __init__(self,NAME,LEVEL='debug',FMT=None):
        """This runs every time Log is assigned.
        @param self:  the instance of the class (no need to pass this)
        @param NAME:  the name of the log object
        @kwarg LEVEL: the log level
        @kwarg FMT:   the log statement format
        """

        # Set a default format
        if FMT is None:
            FMT = '%(asctime)s %(name)s (%(filename)s:%(lineno)d) %(levelname)s:  %(message)s'

        # Add basic configuration settings for the logger
        if FMT is not None:
            logging.basicConfig(format=FMT)

        # Create the logging object
        self.logger = logging.getLogger(NAME)

        # Set the logger level. Default is debug
        if LEVEL is not None:
            self.logger.setLevel(self.set_level(LEVEL))


    ###########################################################
    def add_file_handler(self,FN,FMT=None,LEVEL=None):
        """Create a file handler object
        @param self:  the instance of the class (no need to pass this)
        @param FN:    the log file name
        @kwarg FMT:   the log statement format
        @kwarg LEVEL: the log level
        """
        # Create the file handler object
        self.FHANDLER = logging.FileHandler(FN)

        # Set the logger level. Default is debug
        if LEVEL is not None:
            self.FHANDLER.setLevel(self.set_level(LEVEL))

        # Set formatting, if required
        if FMT is not None:
            self.FHANDLER.setFormatter(self.set_formatter(FMT))

        # Add the file handler to the logging object
        self.logger.addHandler(self.FHANDLER)


    ###########################################################
    def add_stream_handler(self,FMT=None,LEVEL=None):
        """Create a stream handler object
        @param self:  the instance of the class (no need to pass this)
        @kwarg FMT:   the log statement format
        @kwarg LEVEL: the log level
        """
        # Create the file handler object
        self.SHANDLER = logging.StreamHandler()

        # Set the logger level. Default is debug
        if LEVEL is not None:
            self.SHANDLER.setLevel(self.set_level(LEVEL))

        # Set formatting, if required
        if FMT is not None:
            self.SHANDLER.setFormatter(self.set_formatter(FMT))

        # Add the stream handler to the logging object
        self.logger.addHandler(self.SHANDLER)


    ###########################################################
    def set_formatter(self,FMT):
        """Create a formatter object
        @param self: the instance of the class (no need to pass this)
        @param FMT:  the log file name
        """
        # Create the formatter object
        self.FORMATTER = logging.Formatter(FMT)
        return(self.FORMATTER);


    ###########################################################
    def set_level(self,LEVEL):
        """Create a formatter object
        @param self:  the instance of the class (no need to pass this)
        @param LEVEL: the log level
        """
        if LEVEL.lower() == 'info':        return(logging.INFO);
        elif LEVEL.lower() == 'debug':     return(logging.DEBUG);
        elif LEVEL.lower() == 'warning':   return(logging.WARNING);
        elif LEVEL.lower() == 'error':     return(logging.ERROR);
        elif LEVEL.lower() == 'critical':  return(logging.CRITICAL);
        else:                              return(logging.DEBUG);


    ###########################################################
    def log_info(self,MSG='Just checking in.'):
        """
        @param self: the instance of the class (no need to pass this)
        @kwarg MSG:  the log message
        """
        self.logger.info(MSG)


    ###########################################################
    def log_debug(self,MSG='Debugging the code.'):
        """Log a debug statement
        @param self: the instance of the class (no need to pass this)
        @kwarg MSG:  the log message
        """
        self.logger.debug(MSG)


    ###########################################################
    def log_warning(self,MSG='She called out a warning.'):
        """Log a warning statement
        @param self: the instance of the class (no need to pass this)
        @kwarg MSG:  the log message
        """
        self.logger.warning(MSG)


    ###########################################################
    def log_error(self,MSG='Uh oh... an error!'):
        """Log a error statement
        @param self: the instance of the class (no need to pass this)
        @kwarg MSG:  the log message
        """
        self.logger.error(MSG)


    ###########################################################
    def log_critical(self,MSG='This is really very bad.'):
        """Log a critical statement
        @param self: the instance of the class (no need to pass this)
        @kwarg MSG:  the log message
        """
        self.logger.critical(MSG)
