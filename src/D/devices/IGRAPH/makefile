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
# %W%  %G%

GRAPH =  -ltools_s -lm
DLIB =  ../lib

#  Note that the following files had to be copyed from ../lib and changed.
#  Clr_table.c, Erase.c, Reset_clrs.o

#  New files: 
#  polyfill.c, Mouse.c, Band_lines.c

OFILES =  \
	Band_lines.o\
	Can_do.o\
	Clr_table.o\
	Color.o\
	Draw_line.o\
	Erase.o\
	Get_w_box.o\
	Get_w_line.o\
	Get_w_pnt.o\
	Graph_Clse.o\
	Graph_Set.o\
	Mouse.o\
	Panel.o\
	polyfill.o\
	Polygn_abs.o\
	Put_env.o \
	Raster_box.o\
	Raster_chr.o\
	Raster_def.o\
	Raster_int.o\
	Raster_over.o\
	Reset_clr.o \
	Reset_clrs.o \
	Returns.o \
	Set_env.o \
	SWITCHER.o \
	window_act.o

$(GIS)/driver/IGRAPH: IGRAPH
	mv IGRAPH $@
	$(GMAKE) igraph_progs

IGRAPH: $(OFILES) ../lib/driverlib.a
	cc $(LDFLAGS) -o $@ $(OFILES) ../lib/driverlib.a $(GRAPH)  $(GISLIB) -I$(DLIB) -I$(LIBDIR)


Reset_clrs.o:
	cc Reset_clrs.c  $(LDFLAGS) -c ../lib/driverlib.a $(GRAPH) -I$(DLIB)

Color.o:
	cc Color.c  $(LDFLAGS) -c ../lib/driverlib.a $(GRAPH) -I$(DLIB)
	
Clr_table.o:
	cc Clr_table.c  $(LDFLAGS) -c ../lib/driverlib.a $(GRAPH) -I$(DLIB)
	
Raster_def.o:
	cc Raster_def.c  $(LDFLAGS) -c ../lib/driverlib.a $(GRAPH) -I$(DLIB)
	
Raster_over.o:
	cc Raster_over.c  $(LDFLAGS) -c ../lib/driverlib.a $(GRAPH) -I$(DLIB)
	
SWITCHER.o:
	cc SWITCHER.c  $(LDFLAGS) -c ../lib/driverlib.a $(GRAPH) -I$(DLIB)

window_act.o:
	cc window_act.c  $(LDFLAGS) -c ../lib/driverlib.a $(GRAPH) -I$(DLIB)
	

Clr_table.o:	../lib/colors.h
Color.o:	../lib/colors.h
Graph_Set.o:	igraphics.h  ../lib/colors.h
Reset_clr.o:	igraphics.h
Reset_clrs.o:	igraphics.h  ../lib/colors.h
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
