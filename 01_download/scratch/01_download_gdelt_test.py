# -*- coding: utf-8 -*-
"""
Created on Sun May 28 16:39:49 2023

@author: jilli
"""

# in environment env

import feather
import pyarrow
import gdelt

gd = gdelt.gdelt()

gd.Search?

events = gd.Search(['2017 May 23'],table='events',output='df',normcols=True,coverage=False)

