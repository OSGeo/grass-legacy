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


###########################################################
#  %W%  %G%


UNIX_BIN = $(GRAPH)/macros


all:  $(UNIX_BIN)/MAPGEN $(UNIX_BIN)/show.sh $(UNIX_BIN)/plot.sh \
	$(UNIX_BIN)/create.pat $(UNIX_BIN)/CHECK.MAP $(UNIX_BIN)/redo\
	$(UNIX_BIN)/redo.misc $(UNIX_BIN)/show.font\
	$(UNIX_BIN)/tput $(UNIX_BIN)/show.char $(UNIX_BIN)/etc \
	$(UNIX_BIN)/rm.line $(UNIX_BIN)/rm.fill $(UNIX_BIN)/rm.legend \
	$(UNIX_BIN)/rm.label $(UNIX_BIN)/rm.symbol $(UNIX_BIN)/rm.misc\
	$(UNIX_BIN)/copy.map

$(UNIX_BIN)/MAPGEN: MAPGEN
	cp MAPGEN $(UNIX_BIN)/MAPGEN

$(UNIX_BIN)/show.sh: show.sh
	cp show.sh $(UNIX_BIN)/show.sh

$(UNIX_BIN)/show.font: show.font
	cp show.font $(UNIX_BIN)/show.font

$(UNIX_BIN)/show.char: show.char $(UNIX_BIN)/to_char
	cp show.char $(UNIX_BIN)/show.char

$(UNIX_BIN)/to_char: to_char.o
	$(CC) $(LDFLAGS) -o $@ to_char.o

$(UNIX_BIN)/plot.sh: plot.sh
	cp plot.sh $(UNIX_BIN)/plot.sh

$(UNIX_BIN)/create.pat: create.pat
	cp create.pat $(UNIX_BIN)/create.pat

$(UNIX_BIN)/CHECK.MAP: CHECK.MAP
	cp CHECK.MAP $(UNIX_BIN)/CHECK.MAP

$(UNIX_BIN)/redo.misc: redo.misc
	cp redo.misc $(UNIX_BIN)/redo.misc

$(UNIX_BIN)/redo: redo
	cp redo $(UNIX_BIN)/redo

$(UNIX_BIN)/tput: tput
	cp tput $(UNIX_BIN)/tput

$(UNIX_BIN)/rm.misc: rm.layer ren.layer
	cp rm.layer $(UNIX_BIN)/rm.misc
	cp ren.layer $(UNIX_BIN)/ren.misc

$(UNIX_BIN)/rm.line: rm.layer ren.layer
	cp rm.layer $(UNIX_BIN)/rm.line
	cp ren.layer $(UNIX_BIN)/ren.line

$(UNIX_BIN)/rm.fill: rm.layer ren.layer
	cp rm.layer $(UNIX_BIN)/rm.fill
	cp ren.layer $(UNIX_BIN)/ren.fill

$(UNIX_BIN)/rm.label: rm.layer ren.layer
	cp rm.layer $(UNIX_BIN)/rm.label
	cp ren.layer $(UNIX_BIN)/ren.label

$(UNIX_BIN)/rm.symbol: rm.layer ren.layer
	cp rm.layer $(UNIX_BIN)/rm.symbol
	cp ren.layer $(UNIX_BIN)/ren.symbol

$(UNIX_BIN)/rm.legend: rm.layer ren.layer
	cp rm.layer $(UNIX_BIN)/rm.legend
	cp ren.layer $(UNIX_BIN)/ren.legend

$(UNIX_BIN)/copy.map: copy.map
	cp copy.map $(UNIX_BIN)/copy.map

$(UNIX_BIN)/etc: etc
	rm -r ${GRAPH}/etc
	find etc -print | cpio -oc | (cd ${GRAPH}; cpio -idumcv)
	./make.font

etc:#
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
