# cat the COPYING file, add a c line-break \n at each line end
# and remove the unix newline. 
COPYING=`cat ./../../COPYING | sed -f sed.script | tr -d '\012'`
PGM=g.version

MODULE_TOPDIR = ../..

EXTRA_CFLAGS=-DGRASS_VERSION_NUMBER=\"'$(GRASS_VERSION_NUMBER)'\" -DGRASS_VERSION_DATE=\"'$(GRASS_VERSION_DATE)'\" -DGRASS_VERSION_UPDATE_PKG=\"'$(GRASS_VERSION_UPDATE_PKG)'\" -DCOPYING="\"$(COPYING)\""

LIBES     = $(GISLIB)
DEPENDENCIES= $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd COPYING

COPYING:
	cat ./../../COPYING | sed -f sed.script | tr -d '\012' > $(GRASS_VERSION_FILE)
