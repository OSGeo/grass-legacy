GMAKE=/usr2/grass/src/CMD/Gmake
#
# For Interactive UNIX/386ix
#
# set these definitions according to your system requirements

GIS                 = /usr2/grass4
GISDBASE            = /usr2/grass4/data
UNIX_BIN            = /usr2/grass4/bin
DEFAULT_LOCATION    = spearfish

OS                  = SYSV
COMPILE_FLAGS       = -O
DIGIT_FLAGS         = -DATT
LDFLAGS             =
TERMLIB             =
CURSES              = -lcurses $(TERMLIB)
MATHLIB             = -lm
CLEAR               = no
AR                  = ar rc $@ `lorder $(OBJ) | tsort`
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
NAME=hpd500c
SRC=$(GIS)/src
LIBDIR=$(GIS)/src/libes/LIB
LOCKLIB=$(LIBDIR)/liblock.a
LIBS = $(SRC)/paint/Interface/driverlib/LIB/lib.a $(LOCKLIB)
ETC=$(GIS)/etc
PAINT = $(ETC)/paint
LIST =\
    main.o \
    alpha.o \
    clear.o \
    colors.o \
    cym.o \
    data.o \
    dither.o \
    esc.o \
    finish.o \
    formfeed.o \
    init.o \
    npixels.o \
    pictsize.o \
    raster.o \
    rle.o \
    text.o

all: $(PAINT)/driver/$(NAME) $(PAINT)/driver.sh/$(NAME)

$(PAINT)/driver/$(NAME): $(LIST) $(LIBS)
	$(CC) $(LDFLAGS) $(LIST) $(LIBS) -o $@
$(PAINT)/driver.sh/$(NAME): DRIVER.sh
	cp $? $@
	chmod +x $@

$(LIST): P.h
$(LIBS): #
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
