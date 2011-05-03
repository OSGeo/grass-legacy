MODULE_TOPDIR = ../../..

EXTRA_LIBS=$(GISLIB) $(MATHLIB)

LIB_NAME = grass_rli.$(GRASS_VERSION_NUMBER)

LIB_OBJS = daemon.o list.o ipc.o worker.o GenericCell.o	avl.o avlID.o

DEPENDENCIES = $(GISDEP) 

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib
	$(INSTALL_DATA) description.html $(GISBASE)/docs/html/r.li.daemon.html
