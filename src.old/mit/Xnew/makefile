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
CURSES      = -lcurses $(TERMLIB)
VASK        = $(VASKLIB) $(CURSES)
RASTERLIB   = $(SRC)/D/libes/rasterlib.a
DISPLAYLIB  = $(SRC)/D/libes/displaylib.a
D_LIB       = $(SRC)/D/libes/Dlib.a

DIGLIB      = $(SRC)/mapdev/lib/libdig.a
###########################################################
# %W%  %G%
XDISPLAYLIB = ../../Xdisplaylib.a
XLIB = /usr/lib

PGM = Xnew
OFILES =  main.o make_clrs.o


$(BIN)/$(PGM): $(OFILES) $(GISLIB) $(DISPLAYLIB) $(RASTERLIB) $X
	$(CC) $(LDFLAGS) -o $@ $(OFILES) $(D_LIB) $(DISPLAYLIB) $(XDISPLAYLIB) ${RASTERLIB} $(GISLIB) $(VASK) -L$(XLIB) -lX11



$(GISLIB):	#
$(DISPLAYLIB):	#
$(RASTERLIB):	#

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
