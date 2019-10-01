# -*- coding: utf-8 -*-
"""
Created on Tue Jul  2 10:07:05 2019

@author: WB521633
"""

# Setup -----------------------------------------------------------------------
# Setup Project Filepath
import getpass
if getpass.getuser() == 'WB521633': project_file_path = 'C:/Users/wb521633/Dropbox/World Bank/IEs/CrashMap-Nairobi'
if getpass.getuser() == 'WB521633': code_file_path = 'C:/Users/wb521633/Documents/Github/CrashMap-Nairobi/Code'

if getpass.getuser() == 'robmarty': project_file_path = '/Users/robmarty/Dropbox/World Bank/IEs/CrashMap-Nairobi'
if getpass.getuser() == 'robmarty': code_file_path = '/Users/robmarty/Documents/Github/CrashMap-Nairobi/Code'

# Load Packages
import json, re


os.chdir(r'' + code_file_path + '/Functions and Packages/GOSTNets/GOSTNets')
sys.path.append(r'' + code_file_path + '/Functions and Packages/GOSTNets/GOSTNets')
import GOSTnet as gn
import LoadOSM as losm

os.chdir(r'' + code_file_path + '/Twitter Geocode Algorithm/other_algorithms/LNEx/LNEx')
sys.path.append(r'' + code_file_path + '/Twitter Geocode Algorithm/other_algorithms/LNEx/LNEx')
import LNEx as lnex
