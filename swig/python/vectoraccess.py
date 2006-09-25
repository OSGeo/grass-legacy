#!/usr/bin/python

# run within GRASS Spearfish session

import python_grass6 as g6lib

input = 'soils'
mapset = 'PERMANENT'

g6lib.G_gisinit('')
map = g6lib.Map_info()
g6lib.Vect_open_old(map, input, mapset)
g6lib.Vect_set_open_level (2)

print 'Vect is 3D: ', g6lib.Vect_is_3d (map)
print 'Vect DB links: ', g6lib.Vect_get_num_dblinks(map)
print 'Map Scale:  1:', g6lib.Vect_get_scale(map)
print 'Number of lines:', g6lib.Vect_get_num_lines(map)

g6lib.Vect_close(map)
## end of the python script

