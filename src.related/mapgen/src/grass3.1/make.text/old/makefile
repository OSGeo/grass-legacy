GMAKE=/usr/grass3.1/src/CMD/Gmake
# ATT (Vanilla system 5 unix)
# set these definitions according to your system requirements

GIS                 = /usr/grass3
GISs                = /usr/grass3.1
GISDBASE            = /usr
UNIX_BIN            = /usr/grass3/local/bin
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
GMAKE       = $(GISs)/src/CMD/Gmake
MAKEALL     = $(GISs)/src/CMD/Makeall $(GMAKE)
MANROFF     = tbl -TX $(GISs)/src/man.help/man.header $? | nroff -Tlp | col -b > $@
MAN1        = $(GIS)/man/1
MAN2        = $(GIS)/man/2
HELP        = $(GIS)/man/help

BIN         = $(GIS)/bin
ETC         = $(GIS)/etc
SRC         = $(GISs)/src
TXT         = $(GIS)/txt
LIBDIR      = $(GISs)/src/libes
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

CFILE = make_text.c

all:  $(UNIX_BIN)/make_text $(UNIX_BIN)/make.text

$(UNIX_BIN)/make_text: $(CFILE)
	$(CC) -o $@ $(CFILE)

$(UNIX_BIN)/make.text: make.text
	cp make.text $(UNIX_BIN)/make.text
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
