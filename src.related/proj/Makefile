# @(#)Makefile	USGS v.3.4
# Cartographic projection system - UNIX V
# For complete PROJ generation:
#	make install
# The following must be set in installers environment
# RANLIB	( = echo or :			<-- for typical System V )
#
# to test new module with proj do:
#	make T=new.o
T =
# Tunables - modify for local needs ----------------------------

# install executable destination
BIN = /usr/local/bin

# install tables destination (nad27 and nad83)
TABLES = /usr/local/lib

# if forward projections desired, include following
FORWARD = -DFOR_CODE

# if inverse projections desired, include following
INVERSE = -DINV_CODE

# optimizer flag
OPT = -O

# basic compiler
CC = cc

# loader flags
LDFLAGS = -s

# end of tunables ----------------------------------------------

CFLAGS = $(OPT) $(FORWARD) $(INVERSE)
LS = libsupp.a
LP = libproj.a
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

# support library
SUPPB = $(LS)(emess.o) $(LS)(dmstor.o) $(LS)(rtodms.o) $(LS)(load_opt.o) \
	$(LS)(bchgen.o) $(LS)(bchload.o)

SUPPA = $(LS)(ProjSetup.o) $(LS)(SetEllips.o) $(LS)(SuppFuncs.o) \
	$(LS)(gen_cheby.o)

support: $(SUPPA) $(SUPPB)
	$(RANLIB) $(LS)

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
	$(RANLIB) $(LP)

# these refer to
$(SUPPA) $(PLIB): projects.h

install:
	make projs support proj fproj
	- mv proj $(BIN)/proj
	- rm -f $(BIN)/invproj
	- ln $(BIN)/proj $(BIN)/invproj
	- mv fproj $(BIN)/fproj
	- rm -f *.o

# install spcs files
nad:
	- cp state?? $(TABLES)
	- chmod +x nad27
	(echo ": ; state_base=$(TABLES)" ; cat nad27 ) >$(BIN)/nad27
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
	- rm -f *.o proj invproj fproj *.a

