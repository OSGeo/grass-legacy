MODULE_TOPDIR = ../..

LIB_NAME = $(GIS_LIBNAME)
EXTRA_LIBS = $(XDRLIB) $(DATETIMELIB) $(INTLLIB)

include $(MODULE_TOPDIR)/include/Make/Lib.make
include $(MODULE_TOPDIR)/include/Make/Doxygen.make

#compile if LFS Large File Support present:
ifneq ($(USE_LARGEFILES),)
	EXTRA_CFLAGS = -D_FILE_OFFSET_BITS=64
endif

default: lib \
	$(FMODE_OBJ) \
	$(ETC)/ellipse.table \
	$(ETC)/datum.table \
	$(ETC)/datumtransform.table \
	$(ETC)/projections \
	$(ETC)/FIPS.code \
	$(ETC)/state27 \
	$(ETC)/state83 \
	$(ETC)/gui.tcl

$(FMODE_OBJ): fmode.dat
	@test -d $(OBJDIR) || mkdir $(OBJDIR)
	cat fmode.dat > $(OBJDIR)/fmode.c
	$(CC) $(CFLAGS) $(INC) -o $(FMODE_OBJ) -c $(OBJDIR)/fmode.c

$(ETC)/ellipse.table: ellipse.table
	rm -f $@
	$(INSTALL) -m 644 $? $@

$(ETC)/datum.table: datum.table
	rm -f $@
	$(INSTALL) -m 644 $? $@

$(ETC)/datumtransform.table: datumtransform.table
	rm -f $@
	$(INSTALL) -m 644 $? $@

$(ETC)/FIPS.code: FIPS.code
	rm -f $@
	$(INSTALL) -m 644 $? $@

$(ETC)/state27: state27
	rm -f $@
	$(INSTALL) -m 644 $? $@

$(ETC)/state83: state83
	rm -f $@
	$(INSTALL) -m 644 $? $@

$(ETC)/projections: projections
	rm -f $@
	$(INSTALL) -m 644 $? $@

$(ETC)/gui.tcl: gui.tcl
	rm -f $@
	$(INSTALL) -m 644 $? $@

#doxygen:
DOXNAME=gis

$(OBJDIR)/area_ellipse.o: pi.h
$(OBJDIR)/area_sphere.o: pi.h
$(OBJDIR)/auto_mask.o: G.h
$(OBJDIR)/closecell.o: G.h
$(OBJDIR)/format.o: G.h
$(OBJDIR)/geodesic.o: pi.h
$(OBJDIR)/geodist.o: pi.h
$(OBJDIR)/get_row.o: G.h
$(OBJDIR)/get_window.o: G.h
$(OBJDIR)/gisinit.o: G.h
$(OBJDIR)/maskfd.o: G.h
$(OBJDIR)/opencell.o: G.h
$(OBJDIR)/put_row.o: G.h
$(OBJDIR)/radii.o: pi.h
$(OBJDIR)/rhumbline.o: pi.h
$(OBJDIR)/set_window.o: G.h
$(OBJDIR)/window_map.o: G.h
