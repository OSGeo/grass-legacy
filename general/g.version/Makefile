# cat the COPYING file, add a c line-break \n at each line end
# and remove the unix newline. 
COPYING=`cat ./../../COPYING | sed -f sed.script | tr -d '\012'`
PGM=g.version

MODULE_TOPDIR = ../..

EXTRA_CFLAGS=-DVERSION_NUMBER=\"'$(VERSION_NUMBER)'\" -DVERSION_DATE=\"'$(VERSION_DATE)'\" -DVERSION_UPDATE_PKG=\"'$(VERSION_UPDATE_PKG)'\" -DCOPYING="\"$(COPYING)\""

LIBES     = $(GISLIB)
DEPENDENCIES= $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd COPYING

COPYING:
	cat ./../../COPYING | sed -f sed.script | tr -d '\012' > $(VERSION_FILE)
