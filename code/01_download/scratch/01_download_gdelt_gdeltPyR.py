# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

# https://linwoodc3.github.io/gdeltPyR/

# this is the version that is the most recent and still works
#
# $ conda activate pip_env
# $ conda install pip git
# $ pip install spyder-kernels3
# $ pip install pyarrow
# $ pip install feather-format # needed pyarrow first

# highlight the console and hit Ctrl . to restart kernel

import os # so we can change directory
#import pyarrow
#import feather
# $pip install git+https://github.com/linwoodc3/gdeltPyR


from concurrent.futures import ProcessPoolExecutor
import pandas as pd


# set up gdeltpyr for version 2
gd = gdelt.gdelt(version=2)

# multiprocess the query
e = ProcessPoolExecutor()

# generic function to pull and write data to disk based on date
def getter(x):
    try:
        date = x.strftime('%Y%m%d')
        d = gd.Search(date, coverage=True)
        d = d[d['EventBaseCode'].str.contains("^(?:15|16|17|18|19|20)")]
        d.to_csv("{}_gdeltdata.csv".format(date),encoding='utf-8',index=False)
    except:
        pass

# now pull the data; this will take a long time
results = list(e.map(getter,pd.date_range('2022 Feb 10','2022 Feb 11')))


os.getcwd() # C:\\Users\\jilli

#data_raw_gkg = 'D:/data/01_raw/GDELT/gkg/gkg'
#os.chdir(data_raw_gkg)

#os.getcwd()

gd2 = gdelt.gdelt(version =2)

# help: 
gd2.Search?
# pull events table, range, output to 

# need "feather" to output data as an R dataframe
#pip install feather

results = gd.Search('2015 Dec 2', table = 'gkg', coverage = False)

