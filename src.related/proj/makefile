
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
RANLIB = ranlib
AR = ar
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
SUPPB = emess.o dmstor.o rtodms.o load_opt.o \
	bchgen.o bchload.o

SUPPA = ProjSetup.o SetEllips.o SuppFuncs.o \
	gen_cheby.o

$(LS): $(SUPPA) $(SUPPB)
	$(AR) rcv $(LS) $(SUPPA) $(SUPPB)
	$(RANLIB) $(LS)

# projection library
PLIB =	UVpoly.o UVaea.o UVnsper.o UVmerc.o \
	XYpconic.o XYrpoly.o XYgoode.o UVcollg.o \
	UVmill.o UVomerc.o UVortho.o UVsinu.o \
	UVgnom.o UVaeqd.o UVtmerc.o UVstere.o \
	UVlaea.o UVvandg.o UVeqc.o UVlcc.o \
	UVeqdc.o UVbipc.o UVbonne.o UVmoll.o \
	UVeck4.o UVeck6.o UVcea.o UVcass.o \
	UVocea.o UVtcea.o UVparab.o XYhammer.o \
	UVgall.o UVeck5.o UVwink1.o XYaugust.o \
	UVhataea.o UVmbtfpq.o UVmbtfps.o UVputp2.o \
	UVquan.o UVmbtfpp.o XYdense.o UVputp5.o \
	UVeck1.o UVeck2.o UVloxim.o UVeck3.o \
	UVcc.o XYboggs.o UVrobin.o XYaitoff.o \
	XYairy.o XYbacon.o XYeisen.o XYfourn.o \
	XYlagrng.o XYnicol.o XYvandg2.o \
	UVquau.o XYtcc.o XYvandg4.o XYwag7.o \
	phi1_.o phi2_.o phi4_.o

$(LP):	$(PLIB)
	$(AR) rcv $(LP) $(PLIB)
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

