GMAKE=/files/home/users/grass/src/CMD/Gmake
# SUN
# set these definitions according to your system requirements

GIS                 = /files/home/users/grass
GISDBASE            = /files/home/users/grass/data
UNIX_BIN            = /files/home/users/grass
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
EXTRA_CFLAGS = -I../../lib.ortho
IMAGERYLIB2  = ../../lib.ortho/libI.a

 
LIST = main.o\
	ask_files.o\
	ask_files1.o\
	ask_files2.o\
	ask_wind.o\
        ask_elev.o\
	get_wind.o\
	aver_z.o\
	cp.o\
        ps_cp.o\
	compress.o\
	conv.o\
	env.o\
	exec.o\
	equ.o\
	mail.o\
	matrix.o\
	perform.o\
	rectify.o\
	report.o\
	target.o\
	write.o

LIBES = $(IMAGERYLIB2) $(IMAGERYLIB) $(GISLIB) $(SRC)/D/libes/Dlib.a $(DISPLAYLIB) $(RASTERLIB) $(SEGMENTLIB) $(VASK)
$(ETC)/imagery/i.block.rectify: $(LIST) $(LIBES)
	$(CC) $(EXTRA_CFLAGS) $(LDFLAGS) -o $@ $(LIST) $(LIBES) $(CURSES) $(MATHLIB)
$(LIST): defs.h global.h rowcol.h

rowcol.h: rowcol
	rm -f $@
	rowcol > $@

rowcol: rowcol.o
	$(CC) $(LDFLAGS) -o $@ rowcol.o

$(LIBES): # in case libraries change











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
