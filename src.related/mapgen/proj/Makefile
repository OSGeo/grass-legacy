# @(#)Makefile	USGS v.3.7
# Cartographic projection system - UNIX V
# For complete PROJ generation:
#	make install
#
# to test new module with proj do:
#	make T=new.o
T =
# Tunables - modify for local needs ----------------------------

# if forward projections desired, include following
FORWARD = -DFOR_CODE

# if inverse projections desired, include following
INVERSE = -DINV_CODE

# optimizer flag
# for cc compiler
OPT = -O
# for gcc compiler (for now)
#OPT = -O -fwritable-strings -fstrength-reduce -traditional

# basic compiler
CC = cc
# gcc compiler
#CC = gcc

# loader flags
LDFLAGS = -s

# end of tunables ----------------------------------------------

CFLAGS = $(OPT) $(FORWARD) $(INVERSE)
LS = $(MLIB)/libsupp.a
LP = $(MLIB)/libproj.a
# don't kill either libs on break or kill process
.PRECIOUS: $(LP) $(LS)

HEAD = proj.o SetProj.o $(T)

# basic program compilation
proj: $(HEAD) $(LS) $(LP)
	- rm -f invproj
	$(CC) $(LDFLAGS) -o proj $(HEAD) $(LP) $(LS) -lm
	ln proj invproj

$(HEAD): projects.h

# fast Chebyshev proj
fproj:	fproj.o $(LS)
	$(CC) $(LDFLAGS) -o fproj fproj.o $(LS) -lm

# geodesic computations
geod:	geod.o $(LS)
	$(CC) $(LDFLAGS) -o geod geod.o $(LS) -lm

# support library
SUPPB = $(LS)(emess.o) $(LS)(dmstor.o) $(LS)(rtodms.o) $(LS)(load_opt.o) \
	$(LS)(bchgen.o) $(LS)(bchload.o)

SUPPA = $(LS)(ProjSetup.o) $(LS)(SetEllips.o) $(LS)(SuppFuncs.o) \
	$(LS)(gen_cheby.o) $(LS)(ELL_set.o)

SUPPC =	$(LS)(geod_forwd.o) $(LS)(geod_invrs.o) $(LS)(geod_setup.o)

support: $(SUPPA) $(SUPPB) $(SUPPC)
	if [ -f /usr/bin/ranlib -o -f /bin/ranlib ] ; then ranlib $(LS) ; fi

# projection library
PLIB =	$(LP)(UVpoly.o) $(LP)(UVaea.o) $(LP)(UVnsper.o) $(LP)(UVmerc.o) \
	$(LP)(XYpconic.o) $(LP)(XYrpoly.o) $(LP)(XYgoode.o) $(LP)(UVcollg.o) \
	$(LP)(UVmill.o) $(LP)(UVomerc.o) $(LP)(UVortho.o) $(LP)(UVsinu.o) \
	$(LP)(UVgnom.o) $(LP)(UVaeqd.o) $(LP)(UVtmerc.o) $(LP)(UVstere.o) \
	$(LP)(UVlaea.o) $(LP)(UVvandg.o) $(LP)(UVeqc.o) $(LP)(UVlcc.o) \
	$(LP)(UVeqdc.o) $(LP)(UVbipc.o) $(LP)(UVbonne.o) $(LP)(UVmoll.o) \
	$(LP)(UVeck4.o) $(LP)(UVeck6.o) $(LP)(UVcea.o) $(LP)(UVcass.o) \
	$(LP)(UVocea.o) $(LP)(UVtcea.o) $(LP)(UVparab.o) $(LP)(XYhammer.o) \
	$(LP)(UVgall.o) $(LP)(UVeck5.o) $(LP)(UVwink1.o) $(LP)(XYaugust.o) \
	$(LP)(UVhataea.o) $(LP)(UVmbtfpq.o) $(LP)(UVmbtfps.o) $(LP)(UVputp2.o) \
	$(LP)(UVquan.o) $(LP)(UVmbtfpp.o) $(LP)(XYdense.o) $(LP)(UVputp5.o) \
	$(LP)(UVeck1.o) $(LP)(UVeck2.o) $(LP)(UVloxim.o) $(LP)(UVeck3.o) \
	$(LP)(UVcc.o) $(LP)(XYboggs.o) $(LP)(UVrobin.o) $(LP)(XYaitoff.o) \
	$(LP)(XYairy.o) $(LP)(XYbacon.o) $(LP)(XYeisen.o) $(LP)(XYfourn.o) \
	$(LP)(XYlagrng.o) $(LP)(XYnicol.o) $(LP)(XYvandg2.o) \
	$(LP)(UVquau.o) $(LP)(XYtcc.o) $(LP)(XYvandg4.o) $(LP)(XYwag7.o) \
	$(LP)(phi1_.o) $(LP)(phi2_.o) $(LP)(phi4_.o)

projs:	$(PLIB)
	if [ -f /usr/bin/ranlib -o -f /bin/ranlib ] ; then ranlib $(LP) ; fi

# these refer to
$(SUPPA) $(SUPPC) $(PLIB): projects.h

geod.o $(SUPPC): geodesic.h

install:
	make projs support proj fproj geod nad
	- mv proj $(BIN)/proj
	- rm -f $(BIN)/invproj
	- ln $(BIN)/proj $(BIN)/invproj
	- mv fproj $(BIN)/fproj
	- mv geod $(BIN)/geod
	- rm -f $(BIN)/invgeod
	- ln $(BIN)/geod $(BIN)/invgeod
	- rm -f *.o

# install spcs files
nad:
	- cp state?? $(GRAPH)/etc
	(echo "#!/bin/sh; state_base=$(GRAPH)/etc" ; cat nad27 ) >$(BIN)/nad27
	- chmod +x $(BIN)/nad27
	- rm -f $(BIN)/for27
	- rm -f $(BIN)/for83
	- rm -f $(BIN)/inv27
	- rm -f $(BIN)/inv83
	- rm -f $(BIN)/nad83
	- ln $(BIN)/nad27 $(BIN)/nad83
	- ln $(BIN)/nad27 $(BIN)/for83
	- ln $(BIN)/nad27 $(BIN)/inv83
	- ln $(BIN)/nad27 $(BIN)/for27
	- ln $(BIN)/nad27 $(BIN)/inv27

# cleanup this directory
clean:
	- rm -f *.o proj invproj fproj geod invgeod *.a

