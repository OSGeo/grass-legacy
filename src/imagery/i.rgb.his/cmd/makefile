GMAKE=/grass.src/3.1/src/CMD/Gmake
# SUN
# set these definitions according to your system requirements

GIS                 = /grass.src/3.1
GISDBASE            = /data/zorro
UNIX_BIN            = /usr/local/bin
DEFAULT_LOCATION    = spearfish

OS                  = BERK
COMPILE_FLAGS       = -O
DIGIT_FLAGS         =
LDFLAGS             = -s
TERMLIB             = -ltermlib
CURSES		    = -lcurses $(TERMLIB)
MATHLIB             = -lm
CLEAR               = ok
AR                  = ar ruv $@ $?; ranlib $@

LATLON              = /home/users/shapiro/LATLON
LATLONLIB           = $(LATLON)/src/libes/latlonlib.a
LATLONBIN           = $(LATLON)/bin
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

#COMPILE_FLAGS= -g
#LDFLAGS= -g

R2HOBJS = r2hmain.o rgb2his.o command.o openfiles.o closefiles.o

H2ROBJS = h2rmain.o his2rgb.o command.o openfiles.o closefiles.o

LIBES= $(GISLIB)

all:	$(BIN)/Grgb.to.his \
	$(MAN2)/Grgb.to.his \
	$(HELP)/Grgb.to.his \
	$(BIN)/Ghis.to.rgb \
	$(MAN2)/Ghis.to.rgb \
	$(HELP)/Ghis.to.rgb

$(BIN)/Grgb.to.his: $(R2HOBJS) $(LIBES)
	$(CC) $(LDFLAGS) -o $@ $(R2HOBJS) $(LIBES)

$(MAN2)/Grgb.to.his: Grgbhis.man
	$(MANROFF)

$(HELP)/Grgb.to.his: Grgbhis.help
	cp Grgbhis.help $@

$(BIN)/Ghis.to.rgb: $(H2ROBJS) $(LIBES)
	$(CC) $(LDFLAGS) -o $@ $(H2ROBJS) $(LIBES)

$(MAN2)/Ghis.to.rgb: Ghisrgb.man
	$(MANROFF)

$(HELP)/Ghis.to.rgb: Ghisrgb.help
	cp Ghisrgb.help $@

$(R2HOBJS): globals.h

$(H2ROBJS): globals.h

$(LIBES): #
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
