GMAKE=/grass/grass3/src/CMD/Gmake
#
# GRASS3 makehead file for the OKIstation7300
#
GIS                 = /grass/grass3
GISDBASE            = /grass/grass3/data
UNIX_BIN            = /grass/grass3/bin
DEFAULT_LOCATION    = spearfish
OS                  = SYSV
LD_FLAGS            = -s 
COMPILE_FLAGS       = -O
DIGIT_FLAGS         = -DATT
TERMLIB             = /usr/ucblib/libbtermlib.a 
CURSES              = /usr/ucblib/libcurses.a $(TERMLIB)
MATHLIB             = -lm
CLEAR               = no
AR                  = ar rc $@ `lorder $(OBJ) | tsort`
CC                  = /usr/ucb/cc
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
#EXTRA_CFLAGS = -gg

OBJ =\
	Color.o\
	Erase.o\
	Font.o\
	Get_t_box.o\
	Move_abs.o\
	Move_rel.o\
	Num_colors.o\
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

Color.o: colors.h
Get_t_box.o: driver.h
Move_abs.o: driver.h
Move_rel.o: driver.h
Raster.o: driver.h
Reset_clrs.o: colors.h
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
