GMAKE=/regis/grass3/src/CMD/Gmake
# SUN
# set these definitions according to your system requirements

GIS                 = /regis/grass3
GISDBASE            = /mapper/grass/data
UNIX_BIN            = /usr/tmp
DEFAULT_LOCATION    = sfbay

OS                  = BERK
COMPILE_FLAGS       = -O
DIGIT_FLAGS         =
LDFLAGS             = -s
TERMLIB             = -ltermlib
CURSES              = -lcurses $(TERMLIB)
MATHLIB             = -lm
CLEAR               = ok
AR                  = ar ruv $@ $?; ranlib $@

#local additions for gnu cc
#CC                  = /usr/local/gnu/bin/gcc
#COMPILE_FLAGS       = -O -traditional -m68881 -fstrength-reduce -finline-functions 
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
RASTERLIB   = $(SRC)/D/libes/libraster.a
DISPLAYLIB  = $(SRC)/D/libes/libdisplay.a
D_LIB       = $(SRC)/D/libes/libD.a

DIGLIB      = $(SRC)/mapdev/lib/libdig.a
DIG_ATTLIB  = $(LIBDIR)/libdig_atts.a
###########################################################
# %W% %G%

LIBRARY = ../libsegment.a
HEADER = ../segment.h

OBJ = \
	address.o\
	flush.o\
	format.o\
	get.o\
	get_row.o\
	init.o\
        inmem.o\
	pagein.o\
	pageout.o\
	put.o\
	put_row.o\
	release.o\
	seek.o\
        select.o\
	setup.o


all: $(LIBRARY) $(HEADER)

$(LIBRARY): $(OBJ)
	$(AR)

$(OBJ): segment.h

$(HEADER): segment.h
	rm -f $@
	cp $? $@
	chmod 0644 $@
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
