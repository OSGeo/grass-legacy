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
OFILES = att_cat.o


all:  $(UNIX_BIN)/sites.to.geo $(UNIX_BIN)/make.symbol $(UNIX_BIN)/att.to.geo\
	$(UNIX_BIN)/make.label $(UNIX_BIN)/redo.label $(UNIX_BIN)/cat.to.geo

$(UNIX_BIN)/sites.to.geo: sites.to.geo
	cp sites.to.geo $(UNIX_BIN)/sites.to.geo

$(UNIX_BIN)/att.to.geo: att.to.geo
	cp att.to.geo $(UNIX_BIN)/att.to.geo

$(UNIX_BIN)/make.symbol: make.symbol
	cp make.symbol $(UNIX_BIN)/make.symbol

$(UNIX_BIN)/make.label: make.label
	cp make.label $(UNIX_BIN)/make.label

$(UNIX_BIN)/redo.label: redo.symbol
	cp redo.symbol $(UNIX_BIN)/redo.symbol
	cp  $(UNIX_BIN)/redo.symbol $(UNIX_BIN)/redo.label

$(UNIX_BIN)/cat.to.geo: cat.to.geo $(UNIX_BIN)/att_cat
	cp cat.to.geo $(UNIX_BIN)/cat.to.geo

$(UNIX_BIN)/att_cat: $(OFILES)
	$(CC) -o $@ $(OFILES)
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
