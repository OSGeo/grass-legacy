MODULE_TOPDIR = ../..

LIB_NAME = $(GPROJ_LIBNAME)

EXTRA_LIBS=$(GISLIB) $(PROJLIB)
EXTRA_CFLAGS = $(PICFLAGS)
EXTRA_INC = $(PROJINC)

LIB_OBJS = get_proj.o do_proj.o convert.o datum.o ellipse.o

NAD_TABLES = FL.lla MD.lla TN.lla WI.lla WO.lla alaska.lla conus.lla \
             hawaii.lla prvi.lla stgeorge.lla stlrnc.lla stpaul.lla

NT_TABLES = ntv1_can.dat nzgd2kgrid0005.gsb

NAD_DIR = $(GISBASE)/etc/nad
NAD2BIN = nad2bin

include $(MODULE_TOPDIR)/include/Make/Lib.make 

default: lib $(NAD_DIR)

$(NAD_DIR): $(NAD_TABLES) $(NT_TABLES) $(NAD2BIN)
	@ if [ ! -d $@ ] ; then mkdir $@ ; fi
	for i in $(NAD_TABLES) ; do $(NAD2BIN) < $$i $@/`echo $$i | sed 's/.lla//'`; done
	cp -f $(NT_TABLES) $@
	@ touch $@

# Note for future improvement:
# nad2bin should be compiled from source if we are cross compiling and then the
# above NAD_TABLES files created during installation rather than compilation
$(NAD2BIN): #
