# @(#)Makefile	OEMG v.3.4
# Basic make system control for program plotter and
# libraries.  For first time setup do:
#	make install
#
GRAPH=/usr/mapgen

INCL = $(GRAPH)/src/include

LDFLAGS = -s

OPT = -O

CFLAGS = $(OPT) -I$(INCL) -D_GENVB=\"GRAPHB\"

LIB = $(GRAPH)/src/lib/libgraph.a

DLIB = libdev.a

LLIB = libplt.a

.PRECIOUS:	$(DLIB) $(LIB) $(LLIB)

# to test new graphic driver with plotter do:
# make T=new_name.o
T = 

# program compile
plotter: plotter.o devlist.o $(T) libdev.a libplt.a
	cc $(LDFLAGS) -o plotter plotter.o devlist.o $(T) libdev.a libplt.a  -lm
	cp plotter $(GRAPH)/bin

plotter.o devlist.o : $(INCL)/graphics.h plotter.h $(INCL)/grerror.h

#---------------------------------------
# modules for plotter

GRPP.o = $(LLIB)(pltparse.o) $(LLIB)(clip.o) $(LLIB)(post.o) \
	$(LLIB)(setfont.o) $(LLIB)(urequest.o) $(LLIB)(setpen.o) \
	$(LLIB)(lines.o) $(LLIB)(setsizes.o)

GLRP.o = $(LLIB)(vtostr.o) $(LLIB)(lrnd.o)

libplt: $(GRPP.o) $(GLRP.o)
	$(RANLIB) $(LLIB)

$(GRPP.o) : $(INCL)/graphics.h plotter.h $(INCL)/grerror.h

#---------------------------------------
# application libraries

LISTH = $(LIB)(plotopen.o) $(LIB)(pxyxmit.o) $(LIB)(plotreq.o)

LIST = $(LIB)(p2bopen.o) $(LIB)(vtostr.o)

libgraph:	$(LISTH) $(LIST)
	$(RANLIB) $(LIB)

$(LISTH): $(INCL)/graphics.h

#---------------------------------------
# device drivers

DEVS = $(DLIB)(Dbumdev.o) $(DLIB)(Ddebug.o) $(DLIB)(Ddmp60.o) \
	$(DLIB)(Dranger.o) $(DLIB)(Dpstscr.o) $(DLIB)(D4014.o) \
	$(DLIB)(Dkong.o) $(DLIB)(Dextdev.o) $(DLIB)(Dc906.o) \
	$(DLIB)(Dc970.o) $(DLIB)(Dgerber.o) $(DLIB)(Dhp7475.o) \
	$(DLIB)(Dhp7586.o) $(DLIB)(Dingena.o) $(DLIB)(Dhpgl.o) \
	$(DLIB)(Dc1077.o) $(DLIB)(DGRASS.o) $(DLIB)(Dpsqms.o)

libdev:	$(DEVS)
	$(RANLIB) $(DLIB)

$(DEVS) : $(INCL)/graphics.h plotter.h


$(DLIB)(bumdev.o) : $(INCL)/grerror.h

#---------------------------------------
# debug external driver
debug:	extdebug.o
	cc $(LDFLAGS) -o extdebug extdebug.o

install:
	make	libgraph libdev libplt plotter debug
	- mv	plotter extdebug $(GRAPH)/bin

# external type drivers, touch calcomp.c so that CFLAGS will compile
#	proper version.
# dummy.o (FORTRAN routine) is ersatz driver for testing
calcomps:
#	touch s.calcomp.c
#	make -f Make_Cal CAL_LIB=dummy.o C_NAME=c1077 CFLAGS='-O -DC1077'
#	touch s.calcomp.c
#	make -f Make_Cal CAL_LIB=dummy.o C_NAME=dp8 CFLAGS='-O -DDP8'
#	touch s.calcomp.c
#	make -f Make_Cal CAL_LIB=dummy.o C_NAME=apple CFLAGS='-O -DAPPLE'
#	touch s.calcomp.c
#	make -f Make_Cal CAL_LIB=dummy.o C_NAME=imogen CFLAGS='-O -DIMOGEN'
	- rm -f calcomp.o
	make -f Make_Cal CAL_LIB=/usr/local/lib/libcal.a C_NAME=c5800 CFLAGS='-O -DC5800'

