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

#generate static (ST) or shared (SH)
LIB_PREFIX=$(STLIB_PREFIX)
LIB_SUFFIX=$(STLIB_SUFFIX)
#LIB_PREFIX=$(SHLIB_PREFIX)
#LIB_SUFFIX=$(SHLIB_SUFFIX)


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
VERSION_RELEASE = 0
VERSION_NUMBER  = $(VERSION_MAJOR).$(VERSION_MINOR).$(VERSION_RELEASE)
VERSION_DATE    = November 2002
VERSION_NAME    = $(VERSION_MAJOR)$(VERSION_MINOR)

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
LINKM_LIBNAME    = linkm
BITMAP_LIBNAME   = bitmap
EDIT_LIBNAME     = edit
XGI_LIBNAME	 = Xgi
XGD_LIBNAME	 = Xgd
XPM_LIBNAME	 = Xpm

IMAGE_LIBNAME	 = image

DIG_LIBNAME      = dig
DIG2_LIBNAME     = dig2
VECTR_LIBNAME    = vect

# Interpolation
INTERPDATA_LIBNAME  = interpdata
INTERPFL_LIBNAME = interpfl
QTREE_LIBNAME = qtree

OGSF_LIBNAME     = ogsf

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
GRAPH_LIBNAME    = dgl
RTREE_LIBNAME    = rtree

##################### library options ###################################
VASKLIB     = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(VASK_LIBNAME).$(LIB_SUFFIX)

GISLIB      = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(GIS_LIBNAME).$(LIB_SUFFIX)
G3DLIB      = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(G3D_LIBNAME).$(LIB_SUFFIX)
ICONLIB     = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(ICON_LIBNAME).$(LIB_SUFFIX)
LOCKLIB     = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(LOCK_LIBNAME).$(LIB_SUFFIX)
IMAGERYLIB  = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(IMAGERY_LIBNAME).$(LIB_SUFFIX)
ROWIOLIB    = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(ROWIO_LIBNAME).$(LIB_SUFFIX)
COORCNVLIB  = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(COORCNV_LIBNAME).$(LIB_SUFFIX)
SEGMENTLIB  = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(SEGMENT_LIBNAME).$(LIB_SUFFIX)
GPROJLIB    = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(GPROJ_LIBNAME).$(LIB_SUFFIX)
BTREELIB    = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(BTREE_LIBNAME).$(LIB_SUFFIX)
IBTREELIB   = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(IBTREE_LIBNAME).$(LIB_SUFFIX)
GMATHLIB    = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(GMATH_LIBNAME).$(LIB_SUFFIX)
DLGLIB      = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(DLG_LIBNAME).$(LIB_SUFFIX)
RASTERLIB   = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(RASTER_LIBNAME).$(LIB_SUFFIX)
DISPLAYLIB  = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(DISPLAY_LIBNAME).$(LIB_SUFFIX)
D_LIB       = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(D__LIBNAME).$(LIB_SUFFIX)
DATETIMELIB = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(DATETIME_LIBNAME).$(LIB_SUFFIX)
DRIVERLIB   = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(DRIVER_LIBNAME).$(LIB_SUFFIX)
LINKMLIB    = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(LINKM_LIBNAME).$(LIB_SUFFIX)
BITMAPLIB   = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(BITMAP_LIBNAME).$(LIB_SUFFIX)
EDITLIB     = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(EDIT_LIBNAME).$(LIB_SUFFIX)
XGILIB	    = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(XGI_LIBNAME).$(LIB_SUFFIX)
XGDLIB	    = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(XGD_LIBNAME).$(LIB_SUFFIX)
XPMLIB	    = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(XPM_LIBNAME).$(LIB_SUFFIX)

IMAGELIB    = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(IMAGE_LIBNAME).$(LIB_SUFFIX)

DIGLIB      = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(DIG_LIBNAME).$(LIB_SUFFIX)
DIG2LIB     = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(DIG2_LIBNAME).$(LIB_SUFFIX)
VECTRLIB    = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(VECTR_LIBNAME).$(LIB_SUFFIX)
VECTLIB     = $(VECTRLIB) $(DIG2LIB) $(SHAPELIB) $(PQLIB) $(BTREELIB) $(RTREELIB)

# Interpolation
INTERPDATALIB = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(INTERPDATA_LIBNAME).$(LIB_SUFFIX)
INTERPFLLIB   = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(INTERPFL_LIBNAME).$(LIB_SUFFIX)
QTREELIB      = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(QTREE_LIBNAME).$(LIB_SUFFIX)

OGSFLIB      = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(OGSF_LIBNAME).$(LIB_SUFFIX)

# triangulation libraries
SOSLIB      = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(SOS_LIBNAME).$(LIB_SUFFIX)
LIALIB      = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(LIA_LIBNAME).$(LIB_SUFFIX)
OPTRILIB    = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(OPTRI_LIBNAME).$(LIB_SUFFIX)
BASICLIB    = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(BASIC_LIBNAME).$(LIB_SUFFIX)
GEOMLIB     = $(OPTRILIB) $(SOSLIB) $(LIALIB) $(BASICLIB)		

XDISPLAYLIB = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(XDISPLAY_LIBNAME).$(LIB_SUFFIX)

DBMILIB     = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(DBMI_LIBNAME).$(LIB_SUFFIX)
SQLPLIB     = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(SQLP_LIBNAME).$(LIB_SUFFIX)
DBSTUBSLIB  = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(DBSTUBS_LIBNAME).$(LIB_SUFFIX)

SHAPELIB    = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(SHAPE_LIBNAME).$(LIB_SUFFIX)
GRAPHLIB    = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(GRAPH_LIBNAME).$(LIB_SUFFIX)
RTREELIB    = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(RTREE_LIBNAME).$(LIB_SUFFIX)

##################### dependencies ######################################
VECTDEP     =  $(GRASS_INCDIR)/Vect.h $(GRASS_INCDIR)/V_.h \
	       $(GRASS_INCDIR)/vect/dig_defines.h \
               $(GRASS_INCDIR)/vect/dig_macros.h $(GRASS_INCDIR)/vect/dig_structs.h \
               $(GRASS_INCDIR)/vect/dig_externs.h $(GRASS_INCDIR)/vect/dig_globs.h \
               $(VECTRLIB) $(DIG2LIB) $(SHAPELIB) $(BTREELIB) $(RTREELIB)

##################### driver names #####################################

DBF_DRIVERNAME=dbf
DBFDRIVER  = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(DBF_DRIVERNAME).$(LIB_SUFFIX)
ODBC_DRIVERNAME=dbf
ODBCDRIVER  = $(ARCH_LIBDIR)/$(LIB_PREFIX)$(ODBC_DRIVERNAME).$(LIB_SUFFIX)

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

