MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Module.make

PGM = getModTime

#############################################################################
#
# MODULE:   	Grass Tools
# AUTHOR(S):	Original author unknown - probably CERL
#   	    	Justin Hickey - Thailand - jhickey@hpcc.nectec.or.th
# PURPOSE:  	To create the getModTime program.
# COPYRIGHT:    (C) 2000 by the GRASS Development Team
#
#               This program is free software under the GNU General Public
#   	    	License (>=v2). Read the file COPYING that comes with GRASS
#   	    	for details.
#
#############################################################################

default: $(PGM)

$(PGM): main.c
	$(CC) -c $<
	$(CC) -o $(ETC)/$(PGM) $<

clean:
	\rm -f $(ETC)/$(PGM) main.o
