GMAKE=/home/grass3.1/src/CMD/Gmake
# SUN
# set these definitions according to your system requirements

GIS                 = /home/grass3.1
GISDBASE            = /home/grass3.1/data
UNIX_BIN            = /usr/local/bin
DEFAULT_LOCATION    = spearfish

OS                  = BERK
COMPILE_FLAGS       = -O
DIGIT_FLAGS         =
LDFLAGS             = -s
TERMLIB             = -ltermlib
CURSES              = -lcurses $(TERMLIB)
MATHLIB             = -lm
CLEAR               = ok
AR                  = ar ruv $@ $?; ranlib $@

LATLONLIB = $(LIBDIR)/libll.a
LATLONBIN = $(GIS)/bin.ll
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

VASKLIB     = $(LIBDIR)/libvask.a
VASK        = $(VASKLIB) $(CURSES)

GISLIB      = $(LIBDIR)/libgis.a
LOCKLIB     = $(LIBDIR)/liblock.a
IMAGERYLIB  = $(LIBDIR)/libI.a
ROWIOLIB    = $(LIBDIR)/librowio.a
SEGMENTLIB  = $(LIBDIR)/libsegment.a
BTREELIB    = $(LIBDIR)/libbtree.a
COORCNVLIB  = $(LIBDIR)/libcoorcnv.a
RASTERLIB   = $(LIBDIR)/libraster.a
DISPLAYLIB  = $(LIBDIR)/libdisplay.a
D_LIB       = $(LIBDIR)/libD.a

DLGLIB      = $(LIBDIR)/libdlg.a
DIGLIB      = $(LIBDIR)/libdig.a
DIG_ATTLIB  = $(LIBDIR)/libdig_atts.a

###########################################################
EXTRA_CFLAGS	= -I../lib


SRC = Vcontour.c \
	line_center.c

OBJ = Vcontour.o \
	line_center.o 

all:  $(BIN)/Vcontour $(BIN)/Vstate
	



$(BIN)/Vcontour: $(OBJ)
	$(CC) $(LDFLAGS) -o $@ $(OBJ) $(GISLIB) $(DIGLIB) $(MATHLIB) $(DIG_ATTLIB)

$(BIN)/Vstate: stat.o
	$(CC) $(LDFLAGS) -o $@ stat.o $(GISLIB) $(DIGLIB) $(MATHLIB) $(DIG_ATTLIB)

saber_src:
	#load $(SRC) $(CFLAGS) $(DIGLIB) $(LIBTRANS) $(DIG_ATTLIB) $(DISPLAYLIB) $(RASTERLIB) $(VASKLIB) $(CURSES) $(MATHLIB) $(GISLIB) /usr/lib/libc.a

Vcontour.o:  corridor.c

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
