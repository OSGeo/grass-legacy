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
CURSES              = -lcurses $(TERMLIB)
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
###########################################################
#  %W%  %G%
UNIX_BIN = $(GRAPH)/macros

LIBDIG		= $(SRC)/mapdev/lib/libdig.a

EXTRA_CFLAGS	= -I$(SRC)/mapdev/lib   -I$(SRC)/mapdev/libes




all:  $(UNIX_BIN)/make.line \
	$(UNIX_BIN)/redo.line


$(UNIX_BIN)/make.line: make.line
	cp make.line $(UNIX_BIN)/make.line

$(UNIX_BIN)/redo.line: redo.line
	cp redo.line $(UNIX_BIN)/redo.line

make.line:#
redo.line:#
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
