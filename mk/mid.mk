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

# other
CFLAGS      = -I$(INCLUDE_DIR) -I$(CONFIG_DIR) $(COMPILE_FLAGS) $(EXTRA_CFLAGS) $(USE_TERMIO)
LDFLAGS     = -L$(LIBDIR) $(LINK_FLAGS)
MANROFF     = # 
LIBRULE_ST  = ar ruv $@ $?; $(RANLIB) $@
LIBRULE     = $(LIBRULE_ST)
SLIBRULE    = $(LIBRULE_ST)

# various source directories and libraries
LIBDIR      = $(DSTDIR)/src/libes
INCLUDE_DIR = $(SRC)/include
CONFIG_DIR  = $(DSTDIR)/src/include

# libraries
DEPGISLIB      = $(LIBDIR)/libgis.a
GISLIB         = -lgis

DEPVASKLIB     = $(LIBDIR)/libvask.a
VASKLIB        = -lvask

DEPEDITLIB     = $(LIBDIR)/libedit.a
EDITLIB        = -ledit

DEPG3DLIB      = $(LIBDIR)/libg3d.a
G3DLIB         = -lg3d

DEPICONLIB     = $(LIBDIR)/libicon.a
ICONLIB        = -licon

DEPLOCKLIB     = $(LIBDIR)/liblock.a
LOCKLIB        = -llock

DEPIMAGERYLIB  = $(LIBDIR)/libI.a
IMAGERYLIB     = -lI

DEPROWIOLIB    = $(LIBDIR)/librowio.a
ROWIOLIB       = -lrowio

DEPCOORCNVLIB  = $(LIBDIR)/libcoorcnv.a
COORCNVLIB     = -lcoorcnv

DEPSEGMENTLIB  = $(LIBDIR)/libsegment.a
SEGMENTLIB     = -lsegment

DEPGPROJLIB    = $(LIBDIR)/libproj.a
GPROJLIB       = -lproj

DEPBTREELIB    = $(LIBDIR)/libbtree.a
BTREELIB       = -lbtree

DEPIBTREELIB   = $(LIBDIR)/libibtree.a
IBTREELIB      = -libtree

DEPGMATHLIB    = $(LIBDIR)/libgmath.a
GMATHLIB       = -lgmath

DEPDLGLIB      = $(LIBDIR)/libdlg.a
DLGLIB         = -ldlg

DEPRASTERLIB   = $(LIBDIR)/libraster.a
RASTERLIB      = -lraster

DEPDISPLAYLIB  = $(LIBDIR)/libdisplay.a
DISPLAYLIB     = -ldisplay

DEPD_LIB       = $(LIBDIR)/libD.a
D_LIB          = -lD

DEPDATETIMELIB = $(LIBDIR)/libdatetime.a
DATETIMELIB    = -ldatetime

DEPDRIVERLIB   = $(LIBDIR)/libdriver.a
DRIVERLIB      = -ldriver

DEPLINKMLIB    = $(LIBDIR)/liblinkm.a
LINKMLIB       = -llinkm

DEPBITMAPLIB   = $(LIBDIR)/libbitmap.a
BITMAPLIB      = -lbitmap

DEPDIGLIB      = $(LIBDIR)/libdig.a
DIGLIB         = -ldig

DEPDIG2LIB     = $(LIBDIR)/libdig2.a
DIG2LIB        = -ldig2

DEPVECTLIB_REAL= $(LIBDIR)/libvect.a
VECTLIB_REAL   = -lvect

DEPDIG_ATTLIB  = $(LIBDIR)/libdig_atts.a
DIG_ATTLIB     = -ldig_atts

DEPVECTLIB     = $(DEPVECTLIB_REAL) $(DEPDIG2LIB)
VECTLIB        = $(VECTLIB_REAL) $(DIG2LIB)

# triangulation libraries

DEPSOSLIB      = $(LIBDIR)/libsos.a
SOSLIB         = -lsos

DEPLIALIB      = $(LIBDIR)/liblia.a
LIALIB         = -llia

DEPOPTRILIB    = $(LIBDIR)/liboptri.a
OPTRILIB       = -loptri

DEPBASICLIB    = $(LIBDIR)/libbasic.a
BASICLIB       = -lbasic

DEPGEOMLIB     = $(DEPOPTRILIB) $(DEPSOSLIB) $(DEPLIALIB) $(DEPBASICLIB)
GEOMLIB        = $(OPTRILIB) $(SOSLIB) $(LIALIB) $(BASICLIB)

DEPXDISPLAYLIB = $(LIBDIR)/libXdisplay.a
XDISPLAYLIB    = -lXdisplay

#########################################################################
