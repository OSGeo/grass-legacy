GMAKE=/mit/crlprojects/grass/src/CMD/Gmake
# SUN
# set these definitions according to your system requirements

GIS                 = /mit/crlprojects/grass
GISDBASE            = /mit/crlprojects/grass/data
UNIX_BIN            = /mit/crlprojects/grass/bin
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
OBJ =\
	Color.o\
	Erase.o\
	Font.o\
	Get_t_box.o\
	Move_abs.o\
	Move_rel.o\
	Plydts_abs.o\
	Raster_def.o\
	Reset_clrs.o\
	Set_window.o\
	Text.o\
	Text_size.o\
	clip.o\
	color_supp.o\
	connect.o\
	font.o\
	RGB.o\
	text.o

all: driverlib.a

driverlib.a: $(OBJ)
	$(AR)

Clr_table.o: colors.h
Color.o: colors.h
Cont_abs.o: driver.h
Cont_rel.o: driver.h
Get_t_box.o: driver.h
Move_abs.o: driver.h
Move_rel.o: driver.h
Plylne_rel.o: driver.h
Polygn_rel.o: driver.h
Raster.o: driver.h
Reset_clrs.o: colors.h
SWITCHER.o: graph.h
SWITCHER.o: driver.h
Text.o: driver.h
Text_size.o: driver.h
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
