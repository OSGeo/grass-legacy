MODULE_TOPDIR = ../../..

EXTRA_LIBS = $(GISLIB) $(BITMAPLIB) $(DBMILIB) $(GMATHLIB) $(LINKMLIB) $(XDRLIB) $(SITESLIB) $(VECTLIB)
DEPENDENCIES = $(GISDEP) $(DBMIDEP) $(GMATLIBDEP) $(LINKMDEP) $(XDRDEP) $(SITESDEP) $(VECTDEP)
EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)

LIB_NAME = $(SIM_LIBNAME)

LIB_OBJS = input.o random.o hydro.o output.o erod.o

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: $(ARCH_INCDIR)/waterglobs.h lib

$(ARCH_INCDIR)/waterglobs.h: waterglobs.h
	cp waterglobs.h $(ARCH_INCDIR)/waterglobs.h
