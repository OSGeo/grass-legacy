GMAKE=/files/grass3.1/src/CMD/Gmake
# set these definitions according to your system requirements

GIS                 = /files/grass3.1
GISDBASE            = /files/grass3.1/data
UNIX_BIN            = /files/grass3.1/local/bin
DEFAULT_LOCATION    = spearfish

#OS                  = SYSV
OS                  = BERK
COMPILE_FLAGS       = -O
LDFLAGS             = -s
DIGIT_FLAGS         =
#DIGIT_FLAGS         = -DATT
#DIGIT_FLAGS         = -DMASSCOMP
MATHLIB             = -lm
TERMLIB             = -ltermlib
CURSES              = -lcurses $(TERMLIB)
#CURSES              = -loldcurses $(TERMLIB)
CLEAR               = ok
#CLEAR               = no
AR                  = ar ruv $@ $?; ranlib $@
#AR                  = ar rc $@ `lorder $(OBJ) | tsort`
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
COORCNVLIB=$(LIBDIR)/libcoorcnv.a
EXTRA_CFLAGS=-I$(SRC)/mapdev/lib

OFILES = \
	main.o \
	where_am_i.o\
	area_perim.o\
	perimeter.o\
	clip.o

Darea: $(BIN)/Darea

$(BIN)/Darea: $(OFILES) $(DISPLAYLIB) $(RASTERLIB) $(COORCNVLIB) $(GISLIB)
	cc -o $@ $(LDFLAGS) $(OFILES) $(DISPLAYLIB) $(RASTERLIB) $(COORCNVLIB) $(GISLIB) $(DIGLIB) $(MATHLIB)

$(RASTERLIB): #
$(DISPLAYLIB): #
$(COORCNVLIB): #
$(GISLIB): #
$(DIGLIB): #
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
