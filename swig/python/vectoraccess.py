#!/usr/bin/python

# run within GRASS Spearfish session

import python_grass6 as g6lib

input = 'soils'
mapset = 'PERMANENT'

# initialize
g6lib.G_gisinit('')

# define map structure
map = g6lib.Map_info()

# define open level (level 2: topology)
g6lib.Vect_set_open_level (2)

# open existing map
g6lib.Vect_open_old(map, input, mapset)

# query
print 'Vect is 3D: ', g6lib.Vect_is_3d (map)
print 'Vect DB links: ', g6lib.Vect_get_num_dblinks(map)
print 'Map Scale:  1:', g6lib.Vect_get_scale(map)
print 'Number of lines:', g6lib.Vect_get_num_lines(map)

# close map
g6lib.Vect_close(map)
## end of the python script

