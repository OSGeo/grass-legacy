# @(#)Makefile	USGS v.4.8
# Basic make system control for program plotter and
# libraries.  For first time setup do:
#	make install
#
# tunables --------------------------------------
# compile/load options:
LDFLAGS = -s
OPT = -O
CC = cc
# environment name for control
CFLAGS = $(OPT) -D_GENVB=\"GRAPHB\" -I. -I$(INCL)
# end tunables ----------------------------------

GLIB = $(MLIB)/libgraph.a

DLIB = $(MLIB)/libdev.a

LLIB = $(MLIB)/libplt.a

.PRECIOUS:	$(DLIB) $(GLIB) $(LLIB)

# to test new graphic driver with plotter do:
# make T=new_name.o
T = 

# program compile
plotter: plotter.o devlist.o $(T) $(DLIB) $(LLIB)
	cc $(LDFLAGS) -o plotter plotter.o devlist.o $(T) $(DLIB) $(LLIB) -lm

plotter.o devlist.o : $(INCL)/graphics.h $(INCL)/plotter.h

#---------------------------------------
# modules for plotter

GRPP.o = $(LLIB)(pltparse.o) $(LLIB)(clip.o) $(LLIB)(post.o) \
	$(LLIB)(setfont.o) $(LLIB)(urequest.o) $(LLIB)(setpen.o) \
	$(LLIB)(lines.o) $(LLIB)(setsizes.o)

GLRP.o = $(LLIB)(vtostr.o) $(LLIB)(lrnd.o) $(LLIB)(wpopen.o)

libplt: $(GRPP.o) $(GLRP.o)
	if [ -f /usr/bin/ranlib -o -f /bin/ranlib ] ; then ranlib $(LLIB) ;fi

$(GRPP.o) : $(INCL)/graphics.h $(INCL)/plotter.h

#---------------------------------------
# application libraries

LISTH = $(GLIB)(plotopen.o) $(GLIB)(pxyxmit.o) $(GLIB)(plotreq.o)

LIST = $(GLIB)(p2bopen.o) $(GLIB)(vtostr.o)

libgraph:	$(LISTH) $(LIST)
	if [ -f /usr/bin/ranlib -o -f /bin/ranlib ] ; then ranlib $(GLIB) ;fi

$(LISTH): $(INCL)/graphics.h

#---------------------------------------
# device drivers

DEVS = $(DLIB)(Dbumdev.o) $(DLIB)(Ddebug.o) \
	$(DLIB)(Dranger.o) $(DLIB)(Dpsqms.o) $(DLIB)(D4014.o) \
	$(DLIB)(Dkong.o) $(DLIB)(Dextdev.o) $(DLIB)(Dc906.o) \
	$(DLIB)(Dc970.o) $(DLIB)(Dgerber.o) $(DLIB)(Dhp7475.o) \
	$(DLIB)(Dhp7586.o) $(DLIB)(Dingena.o) $(DLIB)(Dhpgl.o) \
	$(DLIB)(Ddmp60.o) $(DLIB)(Dc1077.o) $(DLIB)(Dcalhpgl.o)

libdev:	$(DEVS)
	if [ -f /usr/bin/ranlib -o -f /bin/ranlib ] ; then ranlib $(DLIB) ;fi

$(DEVS) : $(INCL)/graphics.h $(INCL)/plotter.h


$(DLIB)(bumdev.o) : $(INCL)/graphics.h

#---------------------------------------
# debug external driver
debug:	extdebug.o
	cc $(LDFLAGS) -o extdebug extdebug.o

install: libgraph libdev libplt plotter debug
	mv	plotter extdebug $(BIN)
	if [ -f /usr/bin/ranlib -o -f /bin/ranlib ];then ranlib $(GLIB);fi
	- rm	-f *.o

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
#	- rm -f calcomp.o
#	make -f Make_Cal CAL_LIB=$(LIB) C_NAME=c5800 CFLAGS='-O -DC5800' BIN=$(BIN)

