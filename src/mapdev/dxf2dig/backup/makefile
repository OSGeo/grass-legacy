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
PGM = dxf2dig

SOURCES =	\
		add_extents.c \
		add_line.c \
		add_point.c \
		add_polyline.c \
		check_ext.c \
		entities.c \
		find_lines.c \
		init_chars.c \
		make_header.c \
		close_layer.c \
		open_layer.c \
		reopen_layer.c \
		which_layer.c \
		label_box.c \
		main.c

OBJ =	\
	$(OBJARCH)/add_extents.o \
	$(OBJARCH)/add_line.o \
	$(OBJARCH)/add_point.o \
	$(OBJARCH)/add_polyline.o \
	$(OBJARCH)/check_ext.o \
	$(OBJARCH)/entities.o \
	$(OBJARCH)/find_lines.o \
	$(OBJARCH)/init_chars.o \
	$(OBJARCH)/make_header.o \
	$(OBJARCH)/close_layer.o \
	$(OBJARCH)/open_layer.o \
	$(OBJARCH)/reopen_layer.o \
	$(OBJARCH)/which_layer.o \
	$(OBJARCH)/label_box.o \
	$(OBJARCH)/main.o


all:	$(BIN)/$(PGM)

$(BIN)/$(PGM): $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) $(GISLIB) $(TERMLIB) $(MATHLIB) -o $@

saber_src:
	#load $(CFLAGS) $(SOURCES) /lib/libc.a $(GISLIB) $(TERMLIB) $(MATHLIB)

saber_obj:
	#load $(OBJ)

$(OBJARCH)/add_extents.o: add_extents.c
	rm -f $@
	$(CC) $(CFLAGS) -c add_extents.c
	mv add_extents.o $@
$(OBJARCH)/add_line.o: add_line.c
	rm -f $@
	$(CC) $(CFLAGS) -c add_line.c
	mv add_line.o $@
$(OBJARCH)/add_point.o: add_point.c
	rm -f $@
	$(CC) $(CFLAGS) -c add_point.c
	mv add_point.o $@
$(OBJARCH)/add_polyline.o: add_polyline.c
	rm -f $@
	$(CC) $(CFLAGS) -c add_polyline.c
	mv add_polyline.o $@
$(OBJARCH)/check_ext.o: check_ext.c
	rm -f $@
	$(CC) $(CFLAGS) -c check_ext.c
	mv check_ext.o $@
$(OBJARCH)/entities.o: entities.c
	rm -f $@
	$(CC) $(CFLAGS) -c entities.c
	mv entities.o $@
$(OBJARCH)/find_lines.o: find_lines.c
	rm -f $@
	$(CC) $(CFLAGS) -c find_lines.c
	mv find_lines.o $@
$(OBJARCH)/init_chars.o: init_chars.c
	rm -f $@
	$(CC) $(CFLAGS) -c init_chars.c
	mv init_chars.o $@
$(OBJARCH)/label_box.o: label_box.c
	rm -f $@
	$(CC) $(CFLAGS) -c label_box.c
	mv label_box.o $@
$(OBJARCH)/main.o: main.c
	rm -f $@
	$(CC) $(CFLAGS) -c main.c
	mv main.o $@
$(OBJARCH)/make_header.o: make_header.c
	rm -f $@
	$(CC) $(CFLAGS) -c make_header.c
	mv make_header.o $@
$(OBJARCH)/close_layer.o: close_layer.c
	rm -f $@
	$(CC) $(CFLAGS) -c close_layer.c
	mv close_layer.o $@
$(OBJARCH)/open_layer.o: open_layer.c
	rm -f $@
	$(CC) $(CFLAGS) -c open_layer.c
	mv open_layer.o $@
$(OBJARCH)/reopen_layer.o: reopen_layer.c
	rm -f $@
	$(CC) $(CFLAGS) -c reopen_layer.c
	mv reopen_layer.o $@
$(OBJARCH)/which_layer.o: which_layer.c
	rm -f $@
	$(CC) $(CFLAGS) -c which_layer.c
	mv which_layer.o $@
#########################################################################
# the tags file created by "make ctags" is great for
# editing to look for function definitions
# vi -t <function name>

ctags:
	ctags *.[ch]
	sed 's/\?/\//g' tags > tags.tmp
	mv tags.tmp tags
