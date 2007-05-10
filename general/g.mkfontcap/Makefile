
MODULE_TOPDIR = ../..

PGM = g.mkfontcap
CAPFILE = $(ETC)/fontcap

LIBES     = $(GISLIB) $(FTLIB)
DEPENDENCIES = $(GISDEP)
EXTRA_CFLAGS = $(FTINC)

include $(MODULE_TOPDIR)/include/Make/Module.make

*.o: local_proto.h

default: cmd $(CAPFILE)

$(CAPFILE): $(PROG)
	GISRC=$(RUN_GISBASE)/demolocation/.grassrc${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR} \
	GISBASE=$(RUN_GISBASE) \
	$(LD_LIBRARY_PATH_VAR)="$(ARCH_LIBDIR):$($(LD_LIBRARY_PATH_VAR))" \
	$< -o
