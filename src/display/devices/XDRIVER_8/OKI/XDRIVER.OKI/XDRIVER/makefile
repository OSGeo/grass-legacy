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
PGM=XDRIVER
#EXTRA_CFLAGS = 

CC = /usr/ucb/cc

LIB = ../../lib/driverlib.a $(D_LIB) -L/usr/lib/X11

OFILES =  \
	Box_abs.o\
	Can_do.o\
	Color.o\
	Cont_abs.o\
	Clr_table.o\
	Draw_line.o\
	Get_w_box.o\
	Get_w_line.o\
	Get_w_pnt.o\
	Graph_Clse.o\
	Graph_Set.o\
	Panel.o\
	Plylne_abs.o\
	Polygn_abs.o\
	Returns.o\
	Serve_Xevent.o\
	SWITCHER.o\
	alloc.c\
	command_pend.o\
	Raster.o

$(GIS)/driver/$(PGM): $(OFILES) $(LIB)
	$(CC) $(LDFLAGS) $(OFILES) /usr/ccs/lib/libc.a -lucb $(LIB) -DSYSV -lXaw -lXmu -lXt -lXm  -lnsl -lsocket -lns -lX11 -lm
	mv a.out $@

$(LIB): #
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
