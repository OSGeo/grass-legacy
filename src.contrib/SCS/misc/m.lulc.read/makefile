GMAKE=/ncc1/grass/grass3.1a/src/CMD/Gmake
# ATT (Vanilla system 5 unix)
# set these definitions according to your system requirements

GIS                 = /ncc1/grass/grass3.1a
SRC                 = /ncc1/grass/grass3.1a/src
GISDBASE            = /ncc1/grass/grass3.1a/data
UNIX_BIN            = /ncc1/grass/grass3.1a/local/bin
DEFAULT_LOCATION    = spearfish

OS                  = BERK
COMPILE_FLAGS       = -O -DCERL_PORTABLE
DIGIT_FLAGS         = 
LDFLAGS             = -s
TERMLIB             = -ltermlib
CURSES              = -lcurses $(TERMLIB)
MATHLIB             = -lm
CLEAR               = no
AR                  = ar ruv $@ $?;ranlib $@
SHAREDLIB           =
###########################################################
CFLAGS      = $(COMPILE_FLAGS) -I$(LIBDIR) -D$(OS) $(EXTRA_CFLAGS)
GMAKE       = $(SRC)/CMD/Gmake
MAKEALL     = $(SRC)/CMD/Makeall $(GMAKE)
MANROFF     = tbl -TX $(GISs)/src/man.help/man.header $? | nroff -Tlp | col -b > $@
MAN1        = $(GIS)/man/1
MAN2        = $(GIS)/man/2
HELP        = $(GIS)/man/help

BIN         = $(GIS)/bin
ETC         = $(GIS)/etc
TXT         = $(GIS)/txt
LIBDIR      = $(SRC)/libes
GISLIB      = $(LIBDIR)/libgis.a
VASKLIB     = $(LIBDIR)/libvask.a
LOCKLIB     = $(LIBDIR)/liblock.a
IMAGERYLIB  = $(LIBDIR)/libI.a
ROWIOLIB    = $(LIBDIR)/librowio.a
SEGMENTLIB  = $(LIBDIR)/libsegment.a
DLGLIB      = $(LIBDIR)/libdlg.a
COORLIB     = $(LIBDIR)/libcoorcnv.a
VASK        = $(VASKLIB) $(CURSES)
RASTERLIB   = $(SRC)/D/libes/rasterlib.a
DISPLAYLIB  = $(SRC)/D/libes/displaylib.a
D_LIB       = $(SRC)/D/libes/Dlib.a

DIGLIB      = $(SRC)/mapdev/lib/libdig.a
DIG_ATTLIB  = $(LIBDIR)/libdig_atts.a
###########################################################
OBJ = main.o header.o

$(MAN2)/Mlulc.read: $(BIN)/Mlulc.read
	cp manual $@

$(BIN)/Mlulc.read: $(OBJ) 
	$(CC) $(LDFLAGS) -o $@ $(OBJ)


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
