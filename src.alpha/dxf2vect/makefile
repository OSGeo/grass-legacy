GMAKE=/home/grass3.1/src/CMD/Gmake
# SUN
# set these definitions according to your system requirements

GIS                 = /home/grass3.1
GISDBASE            = /data/zorro
UNIX_BIN            = /usr/local/bin
DEFAULT_LOCATION    = spearfish

OS                  = BERK
COMPILE_FLAGS       = -O
DIGIT_FLAGS         =
LDFLAGS             = -s
TERMLIB             = -ltermlib
CURSES		    = -lcurses $(TERMLIB)
MATHLIB             = -lm
CLEAR               = ok
AR                  = ar ruv $@ $?; ranlib $@

LATLON              = /home/users/shapiro/LATLON
LATLONLIB           = $(LATLON)/src/libes/latlonlib.a
LATLONBIN           = $(LATLON)/bin
###########################################################
CFLAGS      = $(COMPILE_FLAGS) -I$(LIBDIR) -D$(OS) $(EXTRA_CFLAGS)
GMAKE       = $(GIS)/src/CMD/Gmake
MAKEALL     = $(GIS)/src/CMD/Makeall $(GMAKE)
MANROFF     = tbl -TX $(GIS)/src/man.help/man.header $? | nroff -Tlp | col -b > $@
MAN1        = $(GIS)/man/1
MAN2        = $(GIS)/man/2
HELP        = $(GIS)/man/help

BIN         = $(GIS)/bin
ETC         = $(GIS)/etc
SRC         = $(GIS)/src
TXT         = $(GIS)/txt
LIBDIR      = $(GIS)/src/libes
GISLIB      = $(LIBDIR)/libgis.a
VASKLIB     = $(LIBDIR)/libvask.a
LOCKLIB     = $(LIBDIR)/liblock.a
IMAGERYLIB  = $(LIBDIR)/libI.a
ROWIOLIB    = $(LIBDIR)/librowio.a
SEGMENTLIB  = $(LIBDIR)/libsegment.a
DLGLIB      = $(LIBDIR)/libdlg.a
VASK        = $(VASKLIB) $(CURSES)
RASTERLIB   = $(SRC)/D/libes/rasterlib.a
DISPLAYLIB  = $(SRC)/D/libes/displaylib.a
D_LIB       = $(SRC)/D/libes/Dlib.a

DIGLIB      = $(SRC)/mapdev/lib/libdig.a
DIG_ATTLIB  = $(LIBDIR)/libdig_atts.a
###########################################################
PGM = dxf2vect
BIN = .

SOURCES =	\
		dxf_extents.c \
		dxf_line.c \
		dxf_point.c \
		dxf_polyline.c \
		dxf_ext.c \
		dxf_entities.c \
		dxf_lines.c \
		dxf_init.c \
		dxf_header.c \
		dxf_layer.c \
		dxf_which.c \
		main.c

LIST =	\
	dxf_extents.o \
	dxf_line.o \
	dxf_point.o \
	dxf_polyline.o \
	dxf_ext.o \
	dxf_entities.o \
	dxf_lines.o \
	dxf_init.o \
	dxf_header.o \
	dxf_layer.o \
	dxf_which.o \
	main.o


all:	$(BIN)/$(PGM)

$(BIN)/$(PGM): $(LIST)
	$(CC) $(LDFLAGS) $(LIST) $(TERMLIB) $(MATHLIB) -o $@

saber_src:
	#load $(SOURCES)

saber_obj:
	#load $(LIST)
###########################################################
# the tags file created by "make ctags" is great for
# editting to look for function definitions
# vi -t <function name>

ctags:
	ctags *.[ch]
	sed 's/\?/\//g' tags > tags.tmp
	mv tags.tmp tags

# this next rule builds .s files from .o files preserving all the CFLAGS
# to make obj.s simply enter "make obj.s"

.c.s:
	$(CC) $(CFLAGS) -S $<
