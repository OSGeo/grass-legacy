GMAKE=/usr3/grass3.1/src/CMD/Gmake
# set these definitions according to your system requirements

GIS                 = /usr3/grass3.1
GISDBASE            = /usr3/grass3.1/data
UNIX_BIN            = /usr/local/bin
DEFAULT_LOCATION    = spearfish

OS                  = SYSV
COMPILE_FLAGS       = -O
LDFLAGS             = -s
DIGIT_FLAGS         = -DATT
MATHLIB             = -lm
TERMLIB             = -ltermlib
CURSES              = $(TERMLIB)
CLEAR               = no
AR                  = ar rc $@ `lorder $(OBJ) | tsort`

SHELL = /bin/sh
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

GRAPH =  -ltools_s -lm
IGRAPHLIB =  ../../

OFILES =  \
	curses.o \
	get_env.o \
	main.o \
	modify.o \
	pd_menu.o \
	print.o \
	store_env.o \
	tty.o 

$(BIN)/Digraph: $(OFILES) 
	cc $(LDFLAGS) -o $@ $(OFILES) $(GRAPH)  $(GISLIB) -I$(LIBDIR) -I$(IGRAPHLIB) $(CURSES)

get_env.o:
	cc get_env.c  $(LDFLAGS) -c $(GRAPH) $(GISLIB) -I$(LIBDIR) -I$(IGRAPHLIB)

pd_menu.o:
	cc pd_menu.c  $(LDFLAGS) -c $(GRAPH) $(GISLIB) -I$(LIBDIR) -I$(IGRAPHLIB)


get_env.o:	../../igraphics.h
pd_menu.o:	../../igraphics.h

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
