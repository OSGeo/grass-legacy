#!/usr/bin/env python

# must be executed in the GRASS env
# test case for Spearfish location


import python_grass6
# don't confuse loc (the instance from python_grass6) with GRASS LOCATION:
loc=python_grass6

rname = 'elevation.dem'
mapset = 'PERMANENT'

loc.G_gisinit('')
loc.G_find_cell2(rname,'')

print mapset

print 'prints 0 if map was found'

print 'roads:'
print loc.G_raster_map_type('roads',mapset)

print 'elevation.dem:'
print loc.G_raster_map_type(rname,mapset)

