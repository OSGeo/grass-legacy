VERSION_NUMBER=3.2
VERSION_DATE=Winter 1990
VERSION_FILE=/home/grass3/CMD/VERSION
SRC=/home/grass3/src
OBJARCH=OBJ.sun3
LIBARCH=LIB.sun3


GISBASE             = /home/grass3/sun3
UNIX_BIN            = /usr/local/bin

DEFAULT_DATABASE    = /data/mass3
DEFAULT_LOCATION    = spearfish

COMPILE_FLAGS       = -O -f68881
LDFLAGS             = -s
EXTRA_FLAGS         = -I$(DIG_INCLUDE)

TERMLIB             = -ltermlib
CURSES		    = -lcurses $(TERMLIB)
MATHLIB             = -lm
LIBRULE             = ar ruv $@ $?; ranlib $@

USE_TERMIO          =
USE_MTIO            = -DUSE_MTIO
DIGITFLAGS          = -DUSE_SETREUID -DUSE_SETEUID -DUSE_SETPRIORITY
CAN_CLEAR           = -DCAN_CLEAR
GMAKE = /usr/local/bin/gmake
#########################################################################
# these define the various directories which contain GRASS programs
# or files used by GRASS programs
BIN         = $(GISBASE)/bin
ETC         = $(GISBASE)/etc
TXT         = $(GISBASE)/txt
MAN1        = $(GISBASE)/man/1
MAN2        = $(GISBASE)/man/2
MAN3        = $(GISBASE)/man/3
HELP        = $(GISBASE)/man/help

# other
CFLAGS      = $(COMPILE_FLAGS) -I$(LIBDIR) $(USE_TERMIO) $(EXTRA_CFLAGS)
AR          = $(GMAKE) -makeparentdir $@; $(LIBRULE)
MANROFF     = tbl -TX $(SRC)/man.help/man.version $(SRC)/man.help/man.header $? | nroff -Tlp | col -b > $@
MAKEALL     = $(GMAKE) -all

# various source directories and libraries
LIBDIR      = $(SRC)/libes

VASKLIB     = $(LIBDIR)/$(LIBARCH)/libvask.a
VASK        = $(VASKLIB) $(CURSES)

GISLIB      = $(LIBDIR)/$(LIBARCH)/libgis.a
LOCKLIB     = $(LIBDIR)/$(LIBARCH)/liblock.a
IMAGERYLIB  = $(LIBDIR)/$(LIBARCH)/libI.a
ROWIOLIB    = $(LIBDIR)/$(LIBARCH)/librowio.a
COORCNVLIB  = $(LIBDIR)/$(LIBARCH)/libcoorcnv.a
SEGMENTLIB  = $(LIBDIR)/$(LIBARCH)/libsegment.a
BTREELIB    = $(LIBDIR)/$(LIBARCH)/libbtree.a
DLGLIB      = $(LIBDIR)/$(LIBARCH)/libdlg.a
RASTERLIB   = $(LIBDIR)/$(LIBARCH)/libraster.a
DISPLAYLIB  = $(LIBDIR)/$(LIBARCH)/libdisplay.a
D_LIB       = $(LIBDIR)/$(LIBARCH)/libD.a

DIGLIB      = $(LIBDIR)/$(LIBARCH)/libdig.a
DIG_ATTLIB  = $(LIBDIR)/$(LIBARCH)/libdig_atts.a

XDISPLAYLIB = $(LIBDIR)/$(LIBARCH)/libXdisplay.a
#########################################################################
PGM = mkcontour 
EXTRA_CFLAGS = 	-I$(DIG_INCLUDE)

SOURCES =	\
		drawline.c \
		build_contours.c \
		read_data.c \
		init_functions.c \
		main.c

OBJ =	\
	$(OBJARCH)/drawline.o \
	$(OBJARCH)/build_contours.o \
	$(OBJARCH)/read_data.o \
	$(OBJARCH)/init_functions.o \
	$(OBJARCH)/main.o


all:	$(BIN)/$(PGM)

$(BIN)/$(PGM): $(OBJ)
	$(CC) $(LDFLAGS) $(EXTRA_FLAGS) $(OBJ) $(GISLIB) $(DIGLIB) $(TERMLIB) $(MATHLIB) -o $@

saber_src:
	#load $(CFLAGS) $(SOURCES) /lib/libc.a $(GISLIB) $(DIGLIB) $(TERMLIB) $(MATHLIB)

saber_obj:
	#load $(OBJ)

$(OBJARCH)/main.o: main.c
	rm -f $@
	$(CC) $(CFLAGS) -c main.c
	mv main.o $@
$(OBJARCH)/init_functions.o: init_functions.c
	rm -f $@
	$(CC) $(CFLAGS) -c init_functions.c
	mv init_functions.o $@
$(OBJARCH)/read_data.o: read_data.c
	rm -f $@
	$(CC) $(CFLAGS) -c read_data.c
	mv read_data.o $@
$(OBJARCH)/build_contours.o: build_contours.c
	rm -f $@
	$(CC) $(CFLAGS) -c build_contours.c
	mv build_contours.o $@
$(OBJARCH)/drawline.o: drawline.c
	rm -f $@
	$(CC) $(CFLAGS) -c drawline.c
	mv drawline.o $@
#########################################################################
# the tags file created by "make ctags" is great for
# editing to look for function definitions
# vi -t <function name>

ctags:
	ctags *.[ch]
	sed 's/\?/\//g' tags > tags.tmp
	mv tags.tmp tags
