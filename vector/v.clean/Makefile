
MODULE_TOPDIR = ../..

PGM = v.clean

LIBES = $(VECTLIB) $(GISLIB)
DEPENDENCIES= $(VECTDEP) $(GISDEP)
EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd ctest

ctest:
	$(MAKE) -C test

clean:
	-rm -rf $(OBJDIR)
	$(MAKE) -C test clean
