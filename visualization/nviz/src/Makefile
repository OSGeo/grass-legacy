# $Id$
# Makefile for building nvwish
# Requires Tk 8.x and Tcl 8.x

MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

# These should be set by the doconfigure script
NVWISH_VER=NVWISH2.2

OGL_LIBS = $(OPENGLULIB) $(OPENGLLIB) $(OPENGL32) $(XLIB) -lXmu -lXext $(MATHLIB) $(DLLIB) $(XDRLIB) $(XEXTRALIBS) $(XLIBPATH)

SURFLIB = $(OGSFLIB)

# Stuff for mkdspf - not yet implemented
# DSPLIB = $(LIBDIR)/libes/dspf/libdsp3.a
# DSPINC = -I$(LIBDIR)/libes/dspf

EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS= ${XCFLAGS} ${TCLINCDIR} ${TKINCDIR} \
	$(INC) \
	-D_NO_PROTO -D__STDC__ $(TIFFINCPATH) $(DSPINC) \
	$(VECT_CFLAGS)


IMGLIB= $(IMAGELIB)

XTRA_LDFLAGS=$(DFLAGS) $(TCLTKLIBPATH) $(TCLTKLIBS) $(OGL_LIBS)

OBJS = nvizAppInit.o change_view.o draw.o exag.o glwrappers.o \
	init_commands.o lights.o map_obj.o misc.o nviz_init.o \
	position.o quick_draw.o anim_support.o cutplane_obj.o \
	script_support.o do_zoom.o \
	label.o nvizMain.o togl.o togl_cb.o\
	query_vect.o \
	volume.o
# mkdspf_calc_linefax.o mkdspf_init_file.o mkdspf_main.o \
# mkdspf_fill_cfax.o mkdspf_iso_surface.o

ARCH_OBJS := $(foreach obj,$(OBJS),$(OBJDIR)/$(obj)) 

PGM=nviz

default: $(BIN)/$(PGM)

#gsf: 
#	cd $(SURF_INCDIR) ; $(GMAKE)
#	touch nviz_init.c

$(BIN)/$(PGM): $(ARCH_OBJS) $(VECTDEP) $(BITMAPDEP) $(LINKMDEP) $(FORMDEP) $(DBMIDEP $(GISDEP)
	$(CC) $(LDFLAGS) -o $@ $(ARCH_OBJS) $(SURFLIB) $(IMGLIB) \
	$(BITMAPLIB) $(LINKMLIB) $(VECTLIB) $(SITESLIB) $(FORMLIB) $(DBMILIB) \
	$(GISLIB) $(MATHLIB) $(XDRLIB) \
	$(DATETIMELIB) $(XTRA_LDFLAGS) -ltiff $(XLIB) $(G3DLIB)
