GMAKE=/usr/grass3/src/CMD/Gmake
# SUN
# set these definitions according to your system requirements

GIS                 = /usr/grass3
GISDBASE            = /usr/grass3/data
UNIX_BIN            = /usr/local/bin
DEFAULT_LOCATION    = spearfish

OS                  = BERK
COMPILE_FLAGS       = -O
DIGIT_FLAGS         =
LDFLAGS             = -s
TERMLIB             = -ltermlib
MATHLIB             = -lm
CLEAR               = ok
AR                  = ar ruv $@ $?; ranlib $@
###########################################################
CFLAGS      = $(COMPILE_FLAGS) -I$(LIBDIR) -D$(OS) $(EXTRA_CFLAGS)
GMAKE       = $(GIS)/src/CMD/Gmake
MAKEALL     = set - *; for d do test -f $$d/Gmakefile && $(GMAKE) $$d; done; exit 0
MANROFF     = tbl -TX $(GIS)/src/man.help/man.header $? | nroff -Tlp | col -b > $@
MAN1        = $(GIS)/man/1
MAN2        = $(GIS)/man/2
HELP        = $(GIS)/man/help

BIN         = $(GIS)/bin
ETC         = $(GIS)/etc
SRC         = $(GIS)/src
LIBDIR      = $(GIS)/src/libes
GISLIB      = $(LIBDIR)/libgis.a
VASKLIB     = $(LIBDIR)/libvask.a
LOCKLIB     = $(LIBDIR)/liblock.a
IMAGERYLIB  = $(LIBDIR)/libI.a
SEGMENTLIB  = $(LIBDIR)/libsegment.a
DLGLIB      = $(LIBDIR)/libdlg.a
CURSES      = -lcurses $(TERMLIB)
VASK        = $(VASKLIB) $(CURSES)
RASTERLIB   = $(SRC)/D/libes/rasterlib.a
DISPLAYLIB  = $(SRC)/D/libes/displaylib.a
D_LIB       = $(SRC)/D/libes/Dlib.a

DIGLIB      = $(SRC)/mapdev/lib/libdig.a
###########################################################
# %W% %G%

XDISPLAYLIB = $(SRC)/D/libes/Xdisplaylib.a
INCLUDE = /usr/include
XLIB = /usr/lib
DAVESPATH = $(SRC)/mapdev/lib
DIGPATH = $(SRC)/mapdev/libes

DIGLIB = $(SRC)/mapdev/libes
DAVESLIB = $(DAVESPATH)/libdig.a

EXTRA_CFLAGS = -I$(DAVESPATH) -I$(DIGLIB)


PGM = Xvect

SOURCES =  main.c command.c p_lines.c p_coors.c

OBJECTS =  main.o command.o p_lines.o p_coors.o

$(BIN)/$(PGM):  $(OBJECTS)   $(DISPLAYLIB) $(RASTERLIB) $(GISLIB)  $X
	cc $(LDFLAGS) $(OBJECTS) -I$(INCLUDE) -I$(LIBDIR)  $(DISPLAYLIB) $(XDISPLAYLIB) $(RASTERLIB) $(DAVESLIB) $(GISLIB)  -o $@ -L$(XLIB)   -lXaw -lXt -lX11 -lm

.c.o: $*.c
	cc -I$(INCLUDE) -I$(DAVESPATH) -I$(LIBDIR) -c $*.c

$(GISLIB): #
$(RASTERLIB): #
$(DISPLAYLIB): #

command.o:      options.h
main.o: options.h

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
	cc $(CFLAGS) -S $<
