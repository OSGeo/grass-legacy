VERSION_NUMBER=4.2
VERSION_DATE=Spr-Sum 1993
VERSION_FILE=/GRASS.src/4.2/src/CMD/VERSION
SRC=/GRASS.src/4.2/src
OBJARCH=OBJ.sun4
LIBARCH=LIB.sun4

CC                  = cc
ARCH                = sun4

GISBASE             = /GRASS.bin/4.2/sun4
UNIX_BIN            = /usr/local/bin

DEFAULT_DATABASE    = /data/foghorn/4.0
DEFAULT_LOCATION    = spearfish

COMPILE_FLAGS       = -g
LDFLAGS             = 

XCFLAGS             = -D_NO_PROTO
XLDFLAGS            =
XINCPATH            =
XMINCPATH           =
XLIBPATH            = -L/usr/lib
XTLIBPATH           = -L/usr/lib
XMLIBPATH           = -L/usr/lib
XLIB                = -lX11
XTLIB               = -lXt
XMLIB               = -lXm
XEXTRALIBS          =

TERMLIB             = -ltermlib
CURSES              = -lcurses $(TERMLIB)
MATHLIB             = -lm

#                   LIBRULE = ar ruv $@ $?
#                   LIBRULE = ar ruv $@ $?; ranlib $@
#                   LIBRULE = ar ruv $@ $?; ar ts $@
#                   LIBRULE = ar rc $@ `lorder $(OBJ) | tsort`
LIBRULE             = ar ruv $@ $?; ranlib $@

USE_TERMIO          =
USE_MTIO            = -DUSE_MTIO
USE_FTIME           = -DUSE_FTIME
DIGITFLAGS          = -DUSE_SETREUID -DUSE_SETEUID -DUSE_SETPRIORITY
VECTLIBFLAGS        =
GETHOSTNAME         = -DGETHOSTNAME_OK
GMAKE = /usr/local/bin/gmake4.2
# to force make to use /bin/sh
SHELL           = /bin/sh
#########################################################################
# these define the various directories which contain GRASS programs
# or files used by GRASS programs
BIN             = $(GISBASE)/bin
ETC             = $(GISBASE)/etc
GARDEN_BIN      = $(GISBASE)/garden/bin
GARDEN_ETC      = $(GISBASE)/garden/etc
BIN_MAIN_INTER  = $(ETC)/bin/main/inter
BIN_MAIN_CMD    = $(ETC)/bin/main/cmd
BIN_ALPHA_INTER = $(ETC)/bin/main/inter
BIN_ALPHA_CMD   = $(ETC)/bin/main/cmd
BIN_CONTRIB_INTER = $(ETC)/bin/contrib/inter
BIN_CONTRIB_CMD   = $(ETC)/bin/contrib/cmd
TXT             = $(GISBASE)/txt
MAN1            = $(GISBASE)/man/1
MAN2            = $(GISBASE)/man/2
MAN3            = $(GISBASE)/man/3
MAN4            = $(GISBASE)/man/4
MAN5            = $(GISBASE)/man/5
MAN6            = $(GISBASE)/man/6
HELP            = $(GISBASE)/man/help

# other
CFLAGS      =  $(COMPILE_FLAGS) $(EXTRA_CFLAGS) -I$(INCLUDE_DIR) $(USE_TERMIO)
AR          = $(GMAKE) -makeparentdir $@; $(LIBRULE)
#MANROFF     = tbl -TX $(SRC)/man.help/man.version $(SRC)/man.help/man.header $? | nroff -Tlp | col -b > $@
MANROFF     = # 
MAKEALL     = $(GMAKE) -all

# various source directories and libraries
SRC_X       = $(SRC)/xgrass
LIBDIR      = $(SRC)/libes
X_LIBDIR     = $(SRC_X)/libes
INCLUDE_DIR = $(SRC)/include
DIG_LIBDIR  = $(SRC)/mapdev/libes
DIG_INCLUDE  = $(SRC)/mapdev/lib
VECT_INCLUDE = -I$(SRC)/mapdev/Vlib -I$(SRC)/mapdev/diglib

VASKLIB     = $(LIBDIR)/$(LIBARCH)/libvask.a
VASK        = $(VASKLIB) $(CURSES)

GISLIB      = $(LIBDIR)/$(LIBARCH)/libgis.a
ICONLIB     = $(LIBDIR)/$(LIBARCH)/libicon.a
LOCKLIB     = $(LIBDIR)/$(LIBARCH)/liblock.a
IMAGERYLIB  = $(LIBDIR)/$(LIBARCH)/libI.a
ROWIOLIB    = $(LIBDIR)/$(LIBARCH)/librowio.a
COORCNVLIB  = $(LIBDIR)/$(LIBARCH)/libcoorcnv.a
SEGMENTLIB  = $(LIBDIR)/$(LIBARCH)/libsegment.a
GPROJLIB    = $(LIBDIR)/$(LIBARCH)/libproj.a
BTREELIB    = $(LIBDIR)/$(LIBARCH)/libbtree.a
IBTREELIB   = $(LIBDIR)/$(LIBARCH)/libibtree.a
DLGLIB      = $(LIBDIR)/$(LIBARCH)/libdlg.a
RASTERLIB   = $(LIBDIR)/$(LIBARCH)/libraster.a
DISPLAYLIB  = $(LIBDIR)/$(LIBARCH)/libdisplay.a
D_LIB       = $(LIBDIR)/$(LIBARCH)/libD.a
DRIVERLIB   = $(SRC)/display/devices/lib/$(LIBARCH)/driverlib.a
LINKMLIB    = $(LIBDIR)/$(LIBARCH)/liblinkm.a
BITMAPLIB    = $(LIBDIR)/$(LIBARCH)/libbitmap.a
XGILIB	    = $(X_LIBDIR)/$(LIBARCH)/libXgi.a
XGDLIB	    = $(X_LIBDIR)/$(LIBARCH)/libXgd.a
XPMLIB	    = $(X_LIBDIR)/$(LIBARCH)/libXpm.a


DIGLIB      = $(LIBDIR)/$(LIBARCH)/libdig.a
DIG2LIB     = $(LIBDIR)/$(LIBARCH)/libdig2.a
VECTLIB_REAL= $(LIBDIR)/$(LIBARCH)/libvect.a
VECTLIB     = $(VECTLIB_REAL) $(DIG2LIB)
DIG_ATTLIB  = $(LIBDIR)/$(LIBARCH)/libdig_atts.a

XDISPLAYLIB = $(LIBDIR)/$(LIBARCH)/libXdisplay.a
#########################################################################
EXTRA_CFLAGS = $(XCFLAGS) $(XINCPATH) $(XMINCPATH)

BOGUS = $(VECTLIB) $(GISLIB) $(DISPLAYLIB)

XXX = $(XMLIBPATH) $(XMLIB) $(XLIBPATH) -lXext $(XTLIBPATH) $(XTLIB) \
 $(XLIB)

YYY = -ll $(XEXTRALIBS) -lm

LIBS = $(BOGUS) $(XGDLIB) $(XGILIB) $(XXX) $(XPMLIB) $(BOGUS) $(YYY)

SABERLIBS = $(XPMLIB) $(BOGUS) $(XGDLIB) $(XGILIB) $(XXX) $(BOGUS) $(YYY)

all:: $(BIN)/xgdisplay 

clean:
	rm -f lex.c grammar.c y.tab.h

OBJS = \
	$(OBJARCH)/args.o \
	$(OBJARCH)/band.o \
	$(OBJARCH)/barattr.o \
	$(OBJARCH)/barbox.o \
	$(OBJARCH)/barscale.o \
	$(OBJARCH)/chlist.o \
	$(OBJARCH)/client.o \
	$(OBJARCH)/coloropt.o \
	$(OBJARCH)/crtecolor.o \
	$(OBJARCH)/drawgrid.o \
	$(OBJARCH)/exit.o \
	$(OBJARCH)/font.o \
	$(OBJARCH)/getcat.o \
	$(OBJARCH)/getdatafh.o \
	$(OBJARCH)/grammar.o \
	$(OBJARCH)/grid.o \
	$(OBJARCH)/gridbox.o \
	$(OBJARCH)/gridgadg.o \
	$(OBJARCH)/handler.o \
	$(OBJARCH)/highlight.o \
	$(OBJARCH)/layout.o \
	$(OBJARCH)/labelbox.o \
	$(OBJARCH)/legend.o \
	$(OBJARCH)/lex.o \
	$(OBJARCH)/linewgadg.o \
	$(OBJARCH)/menubar.o \
	$(OBJARCH)/menucb.o \
	$(OBJARCH)/main.o \
        $(OBJARCH)/mode.o \
        $(OBJARCH)/objbutton.o \
        $(OBJARCH)/object.o \
        $(OBJARCH)/popup.o \
	$(OBJARCH)/qrastpl.o \
	$(OBJARCH)/qrasttext.o \
	$(OBJARCH)/queryraster.o \
	$(OBJARCH)/redrwarea.o \
        $(OBJARCH)/ruler.o \
        $(OBJARCH)/scroll.o \
	$(OBJARCH)/selpixpl.o \
	$(OBJARCH)/sitegadg.o \
	$(OBJARCH)/siteicon.o \
	$(OBJARCH)/stdsite.o \
	$(OBJARCH)/stdsitepl.o \
        $(OBJARCH)/toolbox.o \
        $(OBJARCH)/units.o \
	$(OBJARCH)/upbarbox.o \
	$(OBJARCH)/upgridbox.o \
	$(OBJARCH)/uprastpl.o \
	$(OBJARCH)/xwd.o


$(BIN)/xgdisplay: $(OBJS) $(XGILIB) $(XGDLIB)
	$(CC) -o $@ $(LDFLAGS) $(XLDFLAGS) $(EXTRA_LDFLAGS) $(OBJS) $(LIBS)

$(XGILIB): #

$(XGDLIB): #

lex.c: y.tab.h

y.tab.h: grammar.c

grammar.c: grammar.y
	rm -f grammar.c
	yacc -d grammar.y
	mv y.tab.c grammar.c

saber_src:
	#load *.c ../../src/libes/Xgd/*.c -Bstatic $(CFLAGS) $(EXTRA_CFLAGS) $(VECT_INCLUDE) $(SABERLIBS) $(EXTRA_LDFLAGS)

checkin:
	@(if test -d RCS; then \
	echo "attempting checkin..."; \
	else \
	echo "oops...first time?...making RCS directory"; mkdir -p RCS; \
	fi; \
	for i in $(RCS) ;\
	do (echo Working on $$i; EXISTS="0"; \
	if test -f RCS/$$i,v; then  \
	EXISTS="1" ; \
	fi; \
	if test "$$EXISTS" = "1"; then \
	ci -l0.0.0 -f -m"auto checkin: `date`" -t./ci.txt $$i; \
	chmod 664 $$i; \
	else \
	ci -l0.0 -f -m"auto checkin: `date`" -t./ci.txt $$i; \
	chmod 664 $$i; \
	fi;);  \
	done);

$(OBJARCH)/args.o: args.c
	rm -f $@
	$(CC) $(CFLAGS) -c args.c
	mv args.o $@
$(OBJARCH)/band.o: band.c
	rm -f $@
	$(CC) $(CFLAGS) -c band.c
	mv band.o $@
$(OBJARCH)/barattr.o: barattr.c
	rm -f $@
	$(CC) $(CFLAGS) -c barattr.c
	mv barattr.o $@
$(OBJARCH)/barbox.o: barbox.c
	rm -f $@
	$(CC) $(CFLAGS) -c barbox.c
	mv barbox.o $@
$(OBJARCH)/barscale.o: barscale.c
	rm -f $@
	$(CC) $(CFLAGS) -c barscale.c
	mv barscale.o $@
$(OBJARCH)/chlist.o: chlist.c
	rm -f $@
	$(CC) $(CFLAGS) -c chlist.c
	mv chlist.o $@
$(OBJARCH)/client.o: client.c
	rm -f $@
	$(CC) $(CFLAGS) -c client.c
	mv client.o $@
$(OBJARCH)/coloropt.o: coloropt.c
	rm -f $@
	$(CC) $(CFLAGS) -c coloropt.c
	mv coloropt.o $@
$(OBJARCH)/crtecolor.o: crtecolor.c
	rm -f $@
	$(CC) $(CFLAGS) -c crtecolor.c
	mv crtecolor.o $@
$(OBJARCH)/drawgrid.o: drawgrid.c
	rm -f $@
	$(CC) $(CFLAGS) -c drawgrid.c
	mv drawgrid.o $@
$(OBJARCH)/exit.o: exit.c
	rm -f $@
	$(CC) $(CFLAGS) -c exit.c
	mv exit.o $@
$(OBJARCH)/font.o: font.c
	rm -f $@
	$(CC) $(CFLAGS) -c font.c
	mv font.o $@
$(OBJARCH)/getcat.o: getcat.c
	rm -f $@
	$(CC) $(CFLAGS) -c getcat.c
	mv getcat.o $@
$(OBJARCH)/getdatafh.o: getdatafh.c
	rm -f $@
	$(CC) $(CFLAGS) -c getdatafh.c
	mv getdatafh.o $@
$(OBJARCH)/grammar.o: grammar.c
	rm -f $@
	$(CC) $(CFLAGS) -c grammar.c
	mv grammar.o $@
$(OBJARCH)/grid.o: grid.c
	rm -f $@
	$(CC) $(CFLAGS) -c grid.c
	mv grid.o $@
$(OBJARCH)/gridbox.o: gridbox.c
	rm -f $@
	$(CC) $(CFLAGS) -c gridbox.c
	mv gridbox.o $@
$(OBJARCH)/gridgadg.o: gridgadg.c
	rm -f $@
	$(CC) $(CFLAGS) -c gridgadg.c
	mv gridgadg.o $@
$(OBJARCH)/handler.o: handler.c
	rm -f $@
	$(CC) $(CFLAGS) -c handler.c
	mv handler.o $@
$(OBJARCH)/highlight.o: highlight.c
	rm -f $@
	$(CC) $(CFLAGS) -c highlight.c
	mv highlight.o $@
$(OBJARCH)/labelbox.o: labelbox.c
	rm -f $@
	$(CC) $(CFLAGS) -c labelbox.c
	mv labelbox.o $@
$(OBJARCH)/layout.o: layout.c
	rm -f $@
	$(CC) $(CFLAGS) -c layout.c
	mv layout.o $@
$(OBJARCH)/legend.o: legend.c
	rm -f $@
	$(CC) $(CFLAGS) -c legend.c
	mv legend.o $@
$(OBJARCH)/lex.o: lex.c
	rm -f $@
	$(CC) $(CFLAGS) -c lex.c
	mv lex.o $@
$(OBJARCH)/linewgadg.o: linewgadg.c
	rm -f $@
	$(CC) $(CFLAGS) -c linewgadg.c
	mv linewgadg.o $@
$(OBJARCH)/main.o: main.c
	rm -f $@
	$(CC) $(CFLAGS) -c main.c
	mv main.o $@
$(OBJARCH)/menubar.o: menubar.c
	rm -f $@
	$(CC) $(CFLAGS) -c menubar.c
	mv menubar.o $@
$(OBJARCH)/menucb.o: menucb.c
	rm -f $@
	$(CC) $(CFLAGS) -c menucb.c
	mv menucb.o $@
$(OBJARCH)/mode.o: mode.c
	rm -f $@
	$(CC) $(CFLAGS) -c mode.c
	mv mode.o $@
$(OBJARCH)/objbutton.o: objbutton.c
	rm -f $@
	$(CC) $(CFLAGS) -c objbutton.c
	mv objbutton.o $@
$(OBJARCH)/object.o: object.c
	rm -f $@
	$(CC) $(CFLAGS) -c object.c
	mv object.o $@
$(OBJARCH)/popup.o: popup.c
	rm -f $@
	$(CC) $(CFLAGS) -c popup.c
	mv popup.o $@
$(OBJARCH)/qrastpl.o: qrastpl.c
	rm -f $@
	$(CC) $(CFLAGS) -c qrastpl.c
	mv qrastpl.o $@
$(OBJARCH)/qrasttext.o: qrasttext.c
	rm -f $@
	$(CC) $(CFLAGS) -c qrasttext.c
	mv qrasttext.o $@
$(OBJARCH)/queryraster.o: queryraster.c
	rm -f $@
	$(CC) $(CFLAGS) -c queryraster.c
	mv queryraster.o $@
$(OBJARCH)/redrwarea.o: redrwarea.c
	rm -f $@
	$(CC) $(CFLAGS) -c redrwarea.c
	mv redrwarea.o $@
$(OBJARCH)/ruler.o: ruler.c
	rm -f $@
	$(CC) $(CFLAGS) -c ruler.c
	mv ruler.o $@
$(OBJARCH)/scroll.o: scroll.c
	rm -f $@
	$(CC) $(CFLAGS) -c scroll.c
	mv scroll.o $@
$(OBJARCH)/selpixpl.o: selpixpl.c
	rm -f $@
	$(CC) $(CFLAGS) -c selpixpl.c
	mv selpixpl.o $@
$(OBJARCH)/sitegadg.o: sitegadg.c
	rm -f $@
	$(CC) $(CFLAGS) -c sitegadg.c
	mv sitegadg.o $@
$(OBJARCH)/siteicon.o: siteicon.c
	rm -f $@
	$(CC) $(CFLAGS) -c siteicon.c
	mv siteicon.o $@
$(OBJARCH)/stdsite.o: stdsite.c
	rm -f $@
	$(CC) $(CFLAGS) -c stdsite.c
	mv stdsite.o $@
$(OBJARCH)/stdsitepl.o: stdsitepl.c
	rm -f $@
	$(CC) $(CFLAGS) -c stdsitepl.c
	mv stdsitepl.o $@
$(OBJARCH)/toolbox.o: toolbox.c
	rm -f $@
	$(CC) $(CFLAGS) -c toolbox.c
	mv toolbox.o $@
$(OBJARCH)/units.o: units.c
	rm -f $@
	$(CC) $(CFLAGS) -c units.c
	mv units.o $@
$(OBJARCH)/upbarbox.o: upbarbox.c
	rm -f $@
	$(CC) $(CFLAGS) -c upbarbox.c
	mv upbarbox.o $@
$(OBJARCH)/upgridbox.o: upgridbox.c
	rm -f $@
	$(CC) $(CFLAGS) -c upgridbox.c
	mv upgridbox.o $@
$(OBJARCH)/uprastpl.o: uprastpl.c
	rm -f $@
	$(CC) $(CFLAGS) -c uprastpl.c
	mv uprastpl.o $@
$(OBJARCH)/xwd.o: xwd.c
	rm -f $@
	$(CC) $(CFLAGS) -c xwd.c
	mv xwd.o $@
#########################################################################
# the tags file created by "make ctags" is great for
# editing to look for function definitions
# vi -t <function name>

ctags:
	ctags *.[ch]
	sed 's/\?/\//g' tags > tags.tmp
	mv tags.tmp tags
tags: ctags ; #
