#########################################################################
# these define the various directories which contain GRASS programs
# or files used by GRASS programs
BIN             = $(GISBASE)/bin
ETC             = $(GISBASE)/etc
BIN_INTER       = $(ETC)/bin/inter
BIN_CMD         = $(ETC)/bin/cmd
TXT             = $(GISBASE)/txt
MAN1            = $(GISBASE)/man/1
MAN2            = $(GISBASE)/man/2
MAN3            = $(GISBASE)/man/3
MAN4            = $(GISBASE)/man/4
MAN5            = $(GISBASE)/man/5
MAN6            = $(GISBASE)/man/6
HELP            = $(GISBASE)/man/help
HTML            = $(GISBASE)/documents
SCRIPTS         = $(GISBASE)/scripts
LOCALE          = $(GISBASE)/locale

# other
CFLAGS      = -I$(OBJARCH) -I$(CURDIR) -I$(INCLUDE_DIR) -I$(CONFIG_DIR) $(EXTRA_CFLAGS) $(COMPILE_FLAGS) $(USE_TERMIO)
CXXFLAGS    = -I$(OBJARCH) -I$(CURDIR) -I$(INCLUDE_DIR) -I$(CONFIG_DIR) $(EXTRA_CXXFLAGS) $(COMPILE_FLAGS_CXX) $(USE_TERMIO)
LDFLAGS     = -L$(LIBDIR) $(LINK_FLAGS)
MANROFF     = #
LIBRULE_ST  = $(STLIB_LD) $@ $?; $(RANLIB) $@
LIBRULE     = $(LIBRULE_ST)
ifeq ($(GRASS_LIBRARY_TYPE),shlib)
LIB_RUNTIME_DIR = ${LIBDIR}
LDFLAGS    += $(GRASS_SHLIB_LD_EXTRAS) $(LD_SEARCH_FLAGS)
LIBRULE_SH  = $(SHLIB_LD) -o $@ ${LDFLAGS} $^ $(SLIBDEPS)
SLIBRULE    = $(LIBRULE_SH)
CFLAGS     += $(SHLIB_CFLAGS)
else
SLIBRULE    = $(LIBRULE_ST)
endif

# various source directories and libraries
LIBDIR      = $(DSTDIR)/src/libes
INCLUDE_DIR = $(SRC)/include
CONFIG_DIR  = $(DSTDIR)/src/include

# libraries
DEPGISLIB      = $(LIBDIR)/$(LIB_PREFIX)grass_gis$(LIB_SUFFIX)
GISLIB         = -lgrass_gis $(DATETIMELIB) $(XDRLIB) $(ZLIB) $(MATHLIB) $(SOCKLIB) $(INTLLIB)

DEPVASKLIB     = $(LIBDIR)/$(LIB_PREFIX)grass_vask$(LIB_SUFFIX)
VASKLIB        = -lgrass_vask $(CURSES)

DEPEDITLIB     = $(LIBDIR)/$(LIB_PREFIX)grass_gedit$(LIB_SUFFIX)
EDITLIB        = -lgrass_gedit $(VASKLIB) $(GISLIB)

DEPG3DLIB      = $(LIBDIR)/$(LIB_PREFIX)grass_g3d$(LIB_SUFFIX)
G3DLIB         = -lgrass_g3d $(GISLIB)

DEPICONLIB     = $(LIBDIR)/$(LIB_PREFIX)grass_icon$(LIB_SUFFIX)
ICONLIB        = -lgrass_icon $(GISLIB)

DEPLOCKLIB     = $(LIBDIR)/$(LIB_PREFIX)grass_lock$(LIB_SUFFIX)
LOCKLIB        = -lgrass_lock

DEPIMAGERYLIB  = $(LIBDIR)/$(LIB_PREFIX)grass_I$(LIB_SUFFIX)
IMAGERYLIB     = -lgrass_I $(VASKLIB) $(GMATHLIB) $(GISLIB)

DEPROWIOLIB    = $(LIBDIR)/$(LIB_PREFIX)grass_rowio$(LIB_SUFFIX)
ROWIOLIB       = -lgrass_rowio

DEPCOORCNVLIB  = $(LIBDIR)/$(LIB_PREFIX)grass_coorcnv$(LIB_SUFFIX)
COORCNVLIB     = -lgrass_coorcnv $(GISLIB)

DEPSEGMENTLIB  = $(LIBDIR)/$(LIB_PREFIX)grass_segment$(LIB_SUFFIX)
SEGMENTLIB     = -lgrass_segment $(GISLIB)

DEPGPROJLIB    = $(LIBDIR)/$(LIB_PREFIX)grass_gproj$(LIB_SUFFIX)
GPROJLIB       = -lgrass_gproj $(PROJLIB) $(GISLIB) $(MATHLIB)

DEPBTREELIB    = $(LIBDIR)/$(LIB_PREFIX)grass_btree$(LIB_SUFFIX)
BTREELIB       = -lgrass_btree

DEPIBTREELIB   = $(LIBDIR)/$(LIB_PREFIX)grass_ibtree$(LIB_SUFFIX)
IBTREELIB      = -lgrass_ibtree

DEPGMATHLIB    = $(LIBDIR)/$(LIB_PREFIX)grass_gmath$(LIB_SUFFIX)
GMATHLIB       = -lgrass_gmath $(GISLIB) $(FFTWLIB) $(LAPACKLIB) $(BLASLIB) $(MATHLIB)

DEPDLGLIB      = $(LIBDIR)/$(LIB_PREFIX)grass_dlg$(LIB_SUFFIX)
DLGLIB         = -lgrass_dlg $(GISLIB)

DEPRASTERLIB   = $(LIBDIR)/$(LIB_PREFIX)grass_raster$(LIB_SUFFIX)
RASTERLIB      = -lgrass_raster $(GISLIB)

DEPDISPLAYLIB  = $(LIBDIR)/$(LIB_PREFIX)grass_display$(LIB_SUFFIX)
DISPLAYLIB     = -lgrass_display $(RASTERLIB) $(GISLIB)

DEPD_LIB       = $(LIBDIR)/$(LIB_PREFIX)grass_D$(LIB_SUFFIX)
D_LIB          = -lgrass_D $(DISPLAYLIB) $(RASTERLIB) $(GISLIB)

DEPDATETIMELIB = $(LIBDIR)/$(LIB_PREFIX)grass_datetime$(LIB_SUFFIX)
DATETIMELIB    = -lgrass_datetime

DEPDRIVERLIB   = $(LIBDIR)/$(STLIB_PREFIX)grass_driver$(STLIB_SUFFIX)
DRIVERLIB      = -lgrass_driver $(GISLIB) $(MATHLIB)

DEPLINKMLIB    = $(LIBDIR)/$(LIB_PREFIX)grass_linkm$(LIB_SUFFIX)
LINKMLIB       = -lgrass_linkm

DEPBITMAPLIB   = $(LIBDIR)/$(LIB_PREFIX)grass_bitmap$(LIB_SUFFIX)
BITMAPLIB      = -lgrass_bitmap $(LINKMLIB)

DEPDIGLIB      = $(LIBDIR)/$(LIB_PREFIX)grass_dig$(LIB_SUFFIX)
DIGLIB         = -lgrass_dig

DEPDIG_ATTLIB  = $(LIBDIR)/$(LIB_PREFIX)grass_dig_atts$(LIB_SUFFIX)
DIG_ATTLIB     = -lgrass_dig_atts $(GISLIB)

DEPVECTLIB     = $(LIBDIR)/$(LIB_PREFIX)grass_vect_combined$(LIB_SUFFIX)
VECTLIB        = -lgrass_vect_combined $(DIG_ATTLIB) $(LINKMLIB) $(GISLIB)

# Cannot be made shared
DEPDBMILIB     = $(LIBDIR)/$(STLIB_PREFIX)dbmi$(STLIB_SUFFIX)
DBMILIB        = -ldbmi

DEPIMAGESUPLIB = $(LIBDIR)/$(LIB_PREFIX)grass_image_sup$(LIB_SUFFIX)
IMAGESUPLIB    = -lgrass_image_sup

# triangulation libraries

DEPSOSLIB      = $(LIBDIR)/$(LIB_PREFIX)grass_sos$(LIB_SUFFIX)
SOSLIB         = -lgrass_sos

DEPLIALIB      = $(LIBDIR)/$(LIB_PREFIX)grass_lia$(LIB_SUFFIX)
LIALIB         = -lgrass_lia

DEPOPTRILIB    = $(LIBDIR)/$(LIB_PREFIX)grass_optri$(LIB_SUFFIX)
OPTRILIB       = -lgrass_optri

DEPBASICLIB    = $(LIBDIR)/$(LIB_PREFIX)grass_basic$(LIB_SUFFIX)
BASICLIB       = -lgrass_basic

DEPGEOMLIB     = $(DEPOPTRILIB) $(DEPSOSLIB) $(DEPLIALIB) $(DEPBASICLIB)
GEOMLIB        = $(OPTRILIB) $(SOSLIB) $(LIALIB) $(BASICLIB)

DEPXDISPLAYLIB = $(LIBDIR)/$(LIB_PREFIX)grass_Xdisplay$(LIB_SUFFIX)
XDISPLAYLIB    = -lgrass_Xdisplay

#########################################################################
