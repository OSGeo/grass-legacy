# @(#)Makefile	AMG v.1.13
# Cartographic projection system - UNIX V
# For complete PROJ generation:
#	make install
# The following must be set in installers environment
# GRAPH		( = /usr/local/bin		<-- installers choice )
# RANLIB	( = echo or :			<-- for typical System V )
#

# to test new module with proj do:
#	make T=new.o
T =

BIN = $(GRAPH)/bin

OPT = -O

CFLAGS = $(OPT)

LDFLAGS = -s

# don't kill either libs on break or kill process
.PRECIOUS: libproj.a libsupp.a

# basic program compilation
proj: proj.o $(T) prjlst.o libproj.a libsupp.a
	cc $(LDFLAGS) -o proj proj.o $(T) prjlst.o libproj.a libsupp.a -lm

proj.o prjlst.o: projects.h

# support library
SUPPA =	libsupp.a(phi1_.o) libsupp.a(phi2_.o) libsupp.a(phi3_.o) \
	libsupp.a(tsfn_.o) libsupp.a(phi4_.o) libsupp.a(comset.o) \
	libsupp.a(param.o)

SUPPB =	libsupp.a(enfn_.o) libsupp.a(msfn_.o) libsupp.a(mufn_.o) \
	libsupp.a(qsfn_.o) libsupp.a(dmstor.o) libsupp.a(rtodms.o) \
	libsupp.a(adjlon.o) libsupp.a(authlat.o)

libsupp: $(SUPPA) $(SUPPB)
	$(RANLIB) libsupp.a

# projection library
PLIB =	libproj.a(poly.o), libproj.a(aea.o), libproj.a(nsper.o), \
	libproj.a(merc.o), libproj.a(mill.o), libproj.a(omerc.o), \
	libproj.a(ortho.o), libproj.a(sinu.o), libproj.a(gnom.o), \
	libproj.a(aeqd.o), libproj.a(tmerc.o), libproj.a(stere.o), \
	libproj.a(laea.o), libproj.a(vandg.o), libproj.a(eqc.o), \
	libproj.a(lcc.o), libproj.a(eqdc.o), libproj.a(bipc.o), \
	libproj.a(bonne.o), libproj.a(moll.o), libproj.a(eck4.o), \
	libproj.a(eck6.o), libproj.a(cea.o), libproj.a(cass.o)

libproj: $(PLIB)
	$(RANLIB) libproj.a

# these refer to
$(SUPPA) $(PLIB): projects.h

install:
	make libproj libsupp proj
	- mv proj $(BIN)/proj
	- rm -f *.o

