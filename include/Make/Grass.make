# to force make to use /bin/sh
SHELL           = /bin/sh

#########################################################################
#                         Variable names
# xxxINCDIR  directory(ies) including header files (example: /usr/include)
# xxxINC  cc option(s) for include directory (example: -I/usr/inlude)
# xxxLIBDIR  directory(ies) containing library (example: /usr/lib)
# xxxLIBPATH cc option for library directory (example: -L/usr/lib)
# xxx_LIBNAME library name (example: gis)
# xxxLIB full static library path 
#        (example: /home/abc/grass51/dist.i686-pc-linux-gnu/lib/libgis.a)
# xxxDEP dependency
# 
# GRASS_xxx GRASS specific (without ARCH_xxx)
#
# ARCH_xxx platform specific dirs (without GRASS_xxx)
#
# _xxx  GRASS_xxx + ARCH_xxx
#
# ALLxxx all known for GRASS make system
#
#########################################################################


# GRASS global directories and constants
# platform specific dirs
ARCH_DISTDIR	= $(GRASS_HOME)/dist.$(ARCH)
ARCH_BINDIR     = $(GRASS_HOME)/bin.$(ARCH)

# include dirs
GRASS_INCDIR    = $(GRASS_HOME)/include
ARCH_INCDIR     = $(ARCH_DISTDIR)/include

GRASS_INC	= -I$(GRASS_INCDIR)
ARCH_INC	= -I$(ARCH_INCDIR)
INC		= $(GRASS_INC) $(ARCH_INC)
ALLINC		= $(INC) $(PNGINC) $(ODBCINC) $(PQINCPATH)
		# and so on

# libraries
GRASS_LIBDIR    = $(GRASS_HOME)/lib
ARCH_LIBDIR     = $(ARCH_DISTDIR)/lib

ARCH_LIBPATH	= -L$(ARCH_LIBDIR)
LIBPATH		= $(ARCH_LIBPATH)
ALLLIBPATH	= $(LIBPATH) $(PNGLIB) $(ODBCLIB) $(PQLIBPATH)
		# and so on

# object dir
OBJDIR		= OBJ.$(ARCH)

#########################################################################
# these define the various directories which contain GRASS programs
# or files used by GRASS programs
GISBASE		= $(ARCH_DISTDIR)
BIN             = $(ARCH_DISTDIR)/bin
ETC             = $(ARCH_DISTDIR)/etc
BIN_INTER       = $(ETC)/bin/inter
BIN_CMD         = $(ETC)/bin/cmd
DRIVERDIR       = $(ARCH_DISTDIR)/driver
DBDRIVERDIR     = $(ARCH_DISTDIR)/driver/db

FONTDIR         = $(ARCH_DISTDIR)/fonts

VERSION_MAJOR   = 5
VERSION_MINOR   = 1
VERSION_NUMBER  = $(VERSION_MAJOR).$(VERSION_MINOR)
VERSION_DATE    = January 2001
VERSION_NAME    = 51

##################### other #############################################
CFLAGS      =  $(COMPILE_FLAGS) $(INC) $(USE_TERMIO)
LDFLAGS     =  $(LD_FLAGS) $(LIBPATH) $(USE_TERMIO)

##################### library names #####################################
VASK_LIBNAME	= vask

GIS_LIBNAME	 = gis
G3D_LIBNAME	 = g3d
ICON_LIBNAME     = icon
LOCK_LIBNAME     = lock
IMAGERY_LIBNAME  = I
ROWIO_LIBNAME    = rowio
COORCNV_LIBNAME  = coorcnv
SEGMENT_LIBNAME  = segment
GPROJ_LIBNAME    = proj
BTREE_LIBNAME    = btree
IBTREE_LIBNAME   = ibtree
GMATH_LIBNAME    = gmath
DLG_LIBNAME      = dlg
RASTER_LIBNAME   = raster
DISPLAY_LIBNAME  = display
D__LIBNAME       = D
DATETIME_LIBNAME = datetime
DRIVER_LIBNAME   = driver
LINKM_LIBNAME    = inkm
BITMAP_LIBNAME   = bitmap
XGI_LIBNAME	 = Xgi
XGD_LIBNAME	 = Xgd
XPM_LIBNAME	 = Xpm

DIG_LIBNAME      = dig
DIG2_LIBNAME     = dig2
VECTR_LIBNAME    = vect

# triangulation libraries
SOS_LIBNAME      = sos
LIA_LIBNAME      = lia
OPTRI_LIBNAME    = optri
BASIC_LIBNAME    = basic

XDISPLAY_LIBNAME = Xdisplay

DBMI_LIBNAME     = dbmi
SQLP_LIBNAME     = sqlp
DBSTUBS_LIBNAME  = dbstubs

SHAPE_LIBNAME    = shape

##################### library options ###################################
VASKLIB     = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(VASK_LIBNAME).$(STLIB_SUFFIX)

GISLIB      = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(GIS_LIBNAME).$(STLIB_SUFFIX)
G3DLIB      = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(G3D_LIBNAME).$(STLIB_SUFFIX)
ICONLIB     = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(ICON_LIBNAME).$(STLIB_SUFFIX)
LOCKLIB     = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(LOCK_LIBNAME).$(STLIB_SUFFIX)
IMAGERYLIB  = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(IMAGERY_LIBNAME).$(STLIB_SUFFIX)
ROWIOLIB    = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(ROWIO_LIBNAME).$(STLIB_SUFFIX)
COORCNVLIB  = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(COORCNV_LIBNAME).$(STLIB_SUFFIX)
SEGMENTLIB  = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(SEGMENT_LIBNAME).$(STLIB_SUFFIX)
GPROJLIB    = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(GPROJ_LIBNAME).$(STLIB_SUFFIX)
BTREELIB    = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(BTREE_LIBNAME).$(STLIB_SUFFIX)
IBTREELIB   = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(IBTREE_LIBNAME).$(STLIB_SUFFIX)
GMATHLIB    = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(GMATH_LIBNAME).$(STLIB_SUFFIX)
DLGLIB      = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(DLG_LIBNAME).$(STLIB_SUFFIX)
RASTERLIB   = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(RASTER_LIBNAME).$(STLIB_SUFFIX)
DISPLAYLIB  = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(DISPLAY_LIBNAME).$(STLIB_SUFFIX)
D_LIB       = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(D__LIBNAME).$(STLIB_SUFFIX)
DATETIMELIB = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(DATETIME_LIBNAME).$(STLIB_SUFFIX)
DRIVERLIB   = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(DRIVER_LIBNAME).$(STLIB_SUFFIX)
LINKMLIB    = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(LINKM_LIBNAME).$(STLIB_SUFFIX)
BITMAPLIB   = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(BITMAP_LIBNAME).$(STLIB_SUFFIX)
XGILIB	    = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(XGI_LIBNAME).$(STLIB_SUFFIX)
XGDLIB	    = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(XGD_LIBNAME).$(STLIB_SUFFIX)
XPMLIB	    = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(XPM_LIBNAME).$(STLIB_SUFFIX)

DIGLIB      = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(DIG_LIBNAME).$(STLIB_SUFFIX)
DIG2LIB     = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(DIG2_LIBNAME).$(STLIB_SUFFIX)
VECTRLIB    = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(VECTR_LIBNAME).$(STLIB_SUFFIX)
VECTLIB     = $(VECTRLIB) $(DIG2LIB) $(SHAPELIB) $(PQLIB)

# triangulation libraries
SOSLIB      = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(SOS_LIBNAME).$(STLIB_SUFFIX)
LIALIB      = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(LIA_LIBNAME).$(STLIB_SUFFIX)
OPTRILIB    = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(OPTRI_LIBNAME).$(STLIB_SUFFIX)
BASICLIB    = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(BASIC_LIBNAME).$(STLIB_SUFFIX)
GEOMLIB     = $(OPTRILIB) $(SOSLIB) $(LIALIB) $(BASICLIB)		

XDISPLAYLIB = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(XDISPLAY_LIBNAME).$(STLIB_SUFFIX)

DBMILIB     = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(DBMI_LIBNAME).$(STLIB_SUFFIX)
SQLPLIB     = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(SQLP_LIBNAME).$(STLIB_SUFFIX)
DBSTUBSLIB  = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(DBSTUBS_LIBNAME).$(STLIB_SUFFIX)

SHAPELIB    = $(ARCH_LIBDIR)/$(STLIB_PREFIX)$(SHAPE_LIBNAME).$(STLIB_SUFFIX)

##################### dependencies ######################################
VECTDEP     =  $(GRASS_INCDIR)/Vect.h $(GRASS_INCDIR)/V_.h \
	       $(GRASS_INCDIR)/vect/dig_defines.h $(GRASS_INCDIR)/vect/dig_head.h \
               $(GRASS_INCDIR)/vect/dig_macros.h $(GRASS_INCDIR)/vect/dig_structs.h \
               $(GRASS_INCDIR)/vect/dig_externs.h $(GRASS_INCDIR)/vect/dig_globs.h

##################### rules #############################################
# first found target
first: pre default

# create platform dirs 
pre: $(ARCH_BINDIR) $(ARCH_INCDIR) $(ARCH_LIBDIR) \
	$(BIN) $(ETC) $(BIN_CMD) $(BIN_INTER) \
	$(DRIVERDIR) $(DBDRIVERDIR) $(FONTDIR)
	
$(ARCH_BINDIR):
	mkdir -p $(ARCH_BINDIR)

$(ARCH_INCDIR):
	mkdir -p $(ARCH_INCDIR)

$(ARCH_LIBDIR):
	mkdir -p $(ARCH_LIBDIR)

$(BIN):
	mkdir -p $(BIN)

$(ETC):
	mkdir -p $(ETC)

$(BIN_CMD):
	mkdir -p $(BIN_CMD)

$(BIN_INTER):
	mkdir -p $(BIN_INTER)

$(DRIVERDIR):
	mkdir -p $(DRIVERDIR)

$(DBDRIVERDIR):
	mkdir -p $(DBDRIVERDIR)

$(FONTDIR):
	mkdir -p $(FONTDIR)

