GMAKE=/usr/grass3/src/CMD/Gmake
# SUN
# set these definitions according to your system requirements

GIS                 = /usr/grass3
GISDBASE            = /usr/grass3/data
UNIX_BIN            = /usr/local/bin
DEFAULT_LOCATION    = spearfish

OS                  = BERK
COMPILE_FLAGS       = -O
DIGIT_FLAGS         =
LDFLAGS             = -s
TERMLIB             = -ltermlib
MATHLIB             = -lm
CLEAR               = ok
AR                  = ar ruv $@ $?; ranlib $@
###########################################################
CFLAGS      = $(COMPILE_FLAGS) -I$(LIBDIR) -D$(OS) $(EXTRA_CFLAGS)
GMAKE       = $(GIS)/src/CMD/Gmake
MAKEALL     = set - *; for d do test -f $$d/Gmakefile && $(GMAKE) $$d; done; exit 0
MANROFF     = tbl -TX $(GIS)/src/man.help/man.header $? | nroff -Tlp | col -b > $@
MAN1        = $(GIS)/man/1
MAN2        = $(GIS)/man/2
HELP        = $(GIS)/man/help

BIN         = $(GIS)/bin
ETC         = $(GIS)/etc
SRC         = $(GIS)/src
LIBDIR      = $(GIS)/src/libes
GISLIB      = $(LIBDIR)/libgis.a
VASKLIB     = $(LIBDIR)/libvask.a
LOCKLIB     = $(LIBDIR)/liblock.a
IMAGERYLIB  = $(LIBDIR)/libI.a
SEGMENTLIB  = $(LIBDIR)/libsegment.a
DLGLIB      = $(LIBDIR)/libdlg.a
CURSES      = -lcurses $(TERMLIB)
VASK        = $(VASKLIB) $(CURSES)
RASTERLIB   = $(SRC)/D/libes/rasterlib.a
DISPLAYLIB  = $(SRC)/D/libes/displaylib.a
D_LIB       = $(SRC)/D/libes/Dlib.a

DIGLIB      = $(SRC)/mapdev/lib/libdig.a
###########################################################
# %W% %G%

PGM = bnoise

LIBES = $(GISLIB) $(VASK) $(MATHLIB)

FFLAGS	= -O -w66 

LCDN	= lcdn.o map.o  forma.o point.o putxr.o pgrid.o plot.o   \
            base.o fileopen.o second.o scattr.o stopp.o locatr.o \
            shift.o readtb.o calcnr.o readin.o bdset.o getjn.o  \
			rand.o scatpl.o date.o bounds.o

TAB	= tabgen.o

GUN	= makegun.o

EDIT	= editor.o second.o module.o tricky.o newwc.o \
            eform.o eplot.o emap.o ebase.o eloc.o ebound.o \
            escat.o epoint.o epgrid.o

NASA	= nasamain.o phase1.o phase2.o phase3.o phas3b.o \
            second.o plot.o dash.o tick.o label.o place.o \
            symbol.o slsmth.o ctline.o plots.o cpst.o grdr.o \
            gpst.o apst.o ypp.o xpp.o center.o theta.o \
            phas3a.o date.o getjn.o pnt.o convt.o single.o \
            adash.o sarc.o arcseg.o aroff.o ardash.o artick.o \
            atic.o phase4.o angle.o skip.o crd.o celchk.o \
            track.o offst.o conseg.o arsing.o number.o cell.o \
            axis.o search.o

all:  $(ETC)/bnoise.exe

$(ETC)/bnoise.exe: $(LCDN)
	f77 $(FFLAGS) -o $@ $(LCDN)

#editor: $(EDIT)
#	f77 $(FFLAGS) -o editor.exe $(EDIT)

#gun: makegun.f
#	f77 $(FFLAGS) -o makegun.exe makegun.f

#nasa: $(NASA)
#	f77 $(FFLAGS) -o nasa.exe $(NASA)

#tab: $(TAB)
#	f77 $(FFLAGS) -o tabgen.exe $(TAB)

lcdn.o:  lcdn.f
	f77 -c $(FFLAGS) lcdn.f

map.o:  map.f
	f77 -c $(FFLAGS) map.f

forma.o:  forma.f
	f77 -c $(FFLAGS) forma.f

point.o:  point.f
	f77 -c $(FFLAGS) point.f

putxr.o:  putxr.f
	f77 -c $(FFLAGS) putxr.f

pgrid.o:  pgrid.f
	f77 -c $(FFLAGS) pgrid.f

plot.o:  plot.f
	f77 -c $(FFLAGS) plot.f

base.o:  base.f
	f77 -c $(FFLAGS) base.f

fileopen.o:  fileopen.f
	f77 -c $(FFLAGS) fileopen.f

second.o:  second.f
	f77 -c $(FFLAGS) second.f

scattr.o:  scattr.f
	f77 -c $(FFLAGS) scattr.f

stopp.o:  stopp.f
	f77 -c $(FFLAGS) stopp.f

locatr.o:  locatr.f
	f77 -c $(FFLAGS) locatr.f

shift.o:  shift.f
	f77 -c $(FFLAGS) shift.f

readtb.o:  readtb.f
	f77 -c $(FFLAGS) readtb.f

calcnr.o:  calcnr.f
	f77 -c $(FFLAGS) calcnr.f

readin.o:  readin.f
	f77 -c $(FFLAGS) readin.f

bdset.o:  bdset.f
	f77 -c $(FFLAGS) bdset.f

getjn.o:  getjn.f
	f77 -c $(FFLAGS) getjn.f

rand.o:  rand.f
	f77 -c $(FFLAGS) rand.f

scatpl.o:  scatpl.f
	f77 -c $(FFLAGS) scatpl.f

date.o:  date.f
	f77 -c $(FFLAGS) date.f

bounds.o:  bounds.f
	f77 -c $(FFLAGS) bounds.f
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
	cc $(CFLAGS) -S $<
