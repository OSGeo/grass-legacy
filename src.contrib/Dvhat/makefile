GMAKE=/u3/grass3.1/src/CMD/Gmake
# MASSCOMP with floating point processor
# set these definitions according to your system requirements

GIS                 = /u3/grass3.1
GISDBASE            = /u2/grass3/data
UNIX_BIN            = /u2/local/bin
DEFAULT_LOCATION    = spearfish

OS                  = SYSV
COMPILE_FLAGS       = -O -f
DIGIT_FLAGS         = -DMASSCOMP
LDFLAGS             = -s
TERMLIB             = -ltermlib
CURSES              = -loldcurses $(TERMLIB)
MATHLIB             = -lm
CLEAR               = ok
AR                  = ar ruv $@ $?; ranlib $@
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
#  %W%  %G%

PGM = Dvhat
OFILES = \
	main.o \
	opencell.o\
	show.o\
	what.o


all: $(BIN)/$(PGM)

$(BIN)/$(PGM): $(OFILES) $(DISPLAYLIB) $(RASTERLIB) $(GISLIB)
	$(CC) -o $@ $(LDFLAGS) $(OFILES) $(DIGLIB) $(DISPLAYLIB) $(RASTERLIB) $(GISLIB) $(CURSES)

$(OFILES): what.h

$(RASTERLIB): #
$(DISPLAYLIB): #
$(GISLIB): #
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
