# This file was generated automatically by configure.  Do not edit.
#VPATH = .
links = 
host_alias = i586-unknown-linux
host_cpu = i586
host_vendor = unknown
host_os = linux
host_canonical = i586-unknown-linux
target_alias = i586-unknown-linux
target_cpu = i586
target_vendor = unknown
target_os = linux
target_canonical = i586-unknown-linux
host_makefile_frag = ../../config/mh-linux
# ****************************************************************************
# * NCSA HDF                                                                 *
# * Software Development Group                                               *
# * National Center for Supercomputing Applications                          *
# * University of Illinois at Urbana-Champaign                               *
# * 605 E. Springfield, Champaign IL 61820                                   *
# *                                                                          *
# * For conditions of distribution and use, see the accompanying             *
# * hdf/COPYING file.                                                        *
# *                                                                          *
# ****************************************************************************
#
# $Id: Makefile,v 1.1 1999-12-29 15:12:53 markus Exp $
#
# ##################################################################
#
#                          ABOUT THIS MAKEFILE
#
# This makefile creates HDF test programs.  There are three categories
# of test programs:
#
#              application layer C test programs
#              application layer FORTRAN test programs
#              lower level C test programs
#
# The application layer C test programs are:
#
#          trig    (8-bit, 24-bit raster image sets and palette interface)
#          tsdnt  (scientific data sets, data only)
#          tsdnnt (same as tdfsd_nt with native number types)
#          tsdmms (scientific data sets, data, max/min and scales)
#          tsdnmms(same as tdfsd_mms with native number types)
#          tsdstr (data and dimension strings: label, unit, format and coordsys)
#          tan     (annotations for HDF objects)
#          tanfile (annotations for HDF files)
#          tstubs  (emulation of old lower level i/o routines)
#
# The application layer FORTRAN test programs are:
#
#          tr8F     (8-bit raster image sets)
#          t24F     (24-bit raster image sets)
#          tpF      (Palettes)
#          tsdntF   (scientific data sets, data only)
#          tsdnntF  (same as tdfsd_nntF except all data types are native
#                           machine number types)
#          tsdmmsF  (scientific data sets, data, max/min and scales)
#          tsdnmmsF (same as tdfsd_mmsF with native number types)
#          tsdstrF  (data and dimenstrings: label, unit, format and coordsys)
#          tanF     (annotations for HDF objects)
#          tanfileF (annotations for HDF files)
#          tstubsF  (emulation of old lower level i/o routines)
#
# Vset test programs:
#
#          tvset.c  (the basic vgroup and vdata creation routines)
#          tvsfpack.c (pack/unpack field values into/from a vdata buf)
#          egfhi.f  (Fortran version of egchi.c)
#          tvsetf.f (Fortran version of tvset.c)
#          tvattrf.f (Fortran version of tvattr.c)
# The lower level C test programs are:
#
#              vers.c     (get version string from an HDF file)
#              thfile     (basic i/o)
#              terr       (error handling)
#              thblocks   (linked blocks)
#              thextelt   (i/o involving external files)
#              tvers      (storing and retrieving version strings)
#              thlinkage  (linkage of functions)
#
# Hyperslab C test programs
#              slabw      (write data as 5 slabs )
#              slab1w     (write data as first 3 of 5 slabs )
#              slab2w     (write data as last 2 of 5 slabs )
#              slab3w     (write data as 24 slabs i.e. each element )
#              slab4w     (write data as 1 slab )
#              slabwf     (write data as 5 slabs )
#
# Hyperslab Fortran test programs
#              slabw      (write data as 5 slabs )
#              slab1wf    (write data as first 3 of 5 slabs )
#              slab2wf    (write data as last 2 of 5 slabs )
#              slab3wf    (write data as 24 slabs i.e. each element )
#              slab4wf    (write data as 1 slab )
#
# ##################################################################
#
#                      HOW TO USE THIS MAKEFILE
#
# You need libdf.a to compile any of these test programs.  You will
# also need selected header files as described below.
#
# To compile the application layer C test programs, enter "make ctests".
# If there are test programs that you do not want to compile, do this
# by removing them from the lines "COBJS=" and "CSRCS=" below.
# You need access to the header files indicated in the "CHDRS=" line
# below to compile these programs.
#
# To compile the application layer FORTRAN test programs, enter
# "make ftests".  You need access to the header files indicated in
# the "FHDRS=" line below to compile these programs.
# If there are test programs that you do not want to compile, do this
# by removing them from the lines "FOBJS=" and "FSRCS=" below.
#
# To compile the lower level C test programs, enter "make htests".
# You need access to the header files indicated in the "HHDRS=" line
# below to compile these programs.  If there are test programs that
# you do not want to compile, do this by removing them from the lines
# "HOBJS=" and "HSRCS=" below.
#
# To remove all object files created by this makefile, enter "make clean"
#
# ##################################################################
#
#                       PORTING INSTRUCTIONS
#
# You must modify the lines containing the symbol "==>" to make
# this makefile compatible with your platform.
#
# ##################################################################

### Start of system configuration section. ####
# Set shell so we know what it is
SHELL = /bin/sh

# 'srcdir' Gets set to directory this Makefile is found in
srcdir = .
TOP_SRCDIR = ../..

prefix = $(TOP_SRCDIR)/NewHDF

exec_prefix = $(prefix)
tooldir = $(exec_prefix)/i586-unknown-linux

program_transform_name = 

datadir = $(prefix)/lib
mandir = $(prefix)/man
man1dir = $(mandir)/man1
man2dir = $(mandir)/man2
man3dir = $(mandir)/man3
man4dir = $(mandir)/man4
man5dir = $(mandir)/man5
man6dir = $(mandir)/man6
man7dir = $(mandir)/man7
man8dir = $(mandir)/man8
man9dir = $(mandir)/man9
docdir = $(datadir)/doc

INSTALL = $${srcroot}/install.sh -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL) -m 644

# /usr/local/bin/m4 (GNU version?)
M4   = /usr/local/bin/m4
# /usr/local/bin/perl (GNU version?)
PERL = /usr/local/bin/perl

# Name of achive randomizer, usually ranlib (use 'touch' if non-existant)
RANLIB = ranlib

# Name of library archiver and flags to send, default 'AR=ar', 'ARFLAGS=r'
AR = ar
ARFLAGS = r

# Name of remove utility, default 'RM=/bin/rm', 'RMFLAGS=-f'
RM = /bin/rm
RMFLAGS = -f

#Compilier defaults overriden by top level make
AS = as
CC = gcc
CFLAGS = -ansi
FC = f77
FFLAGS=

# Set for document generation
MAKEINFO = makeinfo
TEXI2DVI = texi2dvi

# Directory in which to install scripts.
bindir = $(exec_prefix)/bin

# Directory in which to install library files.
libdir = $(prefix)/lib

# Directory in which to install documentation info files.
infodir = $(prefix)/info

#### ----- host and target specific makefile fragments come in here.-------
# $Id: Makefile,v 1.1 1999-12-29 15:12:53 markus Exp $

# You can override the following variables here
#
# Sections in Host makefile fragments
# ----------------------------------:
# 1. General Macros for HDF
# 2. Macros for Pablo Instrumentation
# 3. Macros for File Cache
# 4. General macros for NETCDF
# 4.1 XDR Macros for NETCDF
# 4.2 libsrc Macros for NETCDF
# 4.3 Port Macros for NETCDF
#

# ------------ General Macros for HDF --------------------
# Version of the library
PACKAGE	 = HDF
VERSION  = 4.0
#
# Compiliers:
# For gcc version
CC=gcc
CFLAGS=-ansi -D_BSD_SOURCE

# Fortran compilier
#FC = f77
#FC = NONE
FC = g77
FFLAGS = 

# Name of achive randomizer, usually ranlib (use 'true' if non-existant)
RANLIB = ranlib

# Name of library archiver and flags to send, default 'AR=ar', 'ARFLAGS=r'
AR = ar
ARFLAGS = r

# Name of remove utility, default 'RM=/bin/rm', 'RMFLAGS=-f'
RM = /bin/rm
RMFLAGS = -f

# Extra libraries to be include like '-lm' for fabs()
# e.g with naitve HP-ANSI compilier
#LIBSX = -lm

# ------------ Macros for Pablo Instrumentation  --------------------
# Uncomment the following lines to create a Pablo Instrumentation
# version of the HDF core library called 'libdf-inst.a'
# See the documentation in the directory 'hdf/pablo' for further 
# information about Pablo and what platforms it is supported on
# before enabling. 
# You need to set 'PABLO_INCDIR' to the Pablo distribution 
# include directory to get to files 'IOTrace.h', 'IOTrace_SD.h' and others.

#PABLO_FLAGS  = -DHAVE_PABLO
#PABLO_INCDIR = /usr/local/include/pablo-5.1
#PABLO_INCLUDE = -I$(PABLO_INCDIR)

# ------------ Macros for File Cache(fmpool) ------
# Uncomment the following lines to enable shared memory file buffer pool
# version of the HDF core library libdf.a. Please read the
# documentation before enabling this feature.

#FMPOOL_FLAGS  = -DHAVE_FMPOOL

# ------------ General Macros for NETCDF --------------------
# Operating system, Used in fortran directory to generate fortran
# wrappers. Possible values are osf, aix, hpux, irix, sunos, ultrix
# unicos, convex, Linux, freebsd
OS	= Linux

CPP	      = $(CC) -E

# for endianess, for little endian byte order need -DSWAP
# for FreeBSD and Linux(anny x86 UNIX)
SWAP          = -DSWAP
#SWAP          = 

# for 32bit 'network long' integer, possible value -DNETLONG=int
NETLONG       = 

# additional CFLAGS
CFLAGS_NETCDF = 

# Additional flags for preproccesor. Some no longer used because
#   library now requires ANSI compilier.
#
# for no function prototypes add -DNO_HAVE_PROTOTYPES
# for no strerror()          add -DNO_STRERROR
# for no variadic function support add -DNO_STDARG
#
# They all require -DHDF and -DNDEBUG 
# Any special preprocessor requirements go here
#
# for Unicos 
# CPPFLAGS_HDF  = $(FMPOOL_FLAGS) -DNDEBUG -DHDF -DBIG_SHORTS -DBIG_LONGS
#
# for OSF(dec alpha) 
# CPPFLAGS_HDF  = $(FMPOOL_FLAGS) -DNDEBUG -DHDF -DBIG_LONGS -std1
#
# for IRIX6(64-bit) 
# CPPFLAGS_HDF  = $(FMPOOL_FLAGS) -DNDEBUG -DHDF -DBIG_LONGS
#
# for AIX
# CPPFLAGS_HDF  = $(FMPOOL_FLAGS) -DNDEBUG -DHDF -D_ALL_SOURCE
#
# for Sunos and Linux
CPPFLAGS_HDF  = $(FMPOOL_FLAGS) -DNDEBUG -DHDF -DNO_STRERROR
#
# for IRIX6.0 (32-bit mode). If you prefer other 32 bit option like -n32, 
#  replace '-32' below
# CPPFLAGS_HDF  = -32 $(FMPOOL_FLAGS) -DNDEBUG -DHDF
#
# normal
#CPPFLAGS_HDF  = $(FMPOOL_FLAGS) -DNDEBUG -DHDF

#
# Unix commands/utilities
#
# neqn(1)
NEQN	= neqn
# tbl(1)
TBL	= tbl
# which(1)
WHICH	= which
# lex(1)
#LEX	= lex    # GNU flex?
LEX	= flex    # GNU flex?
# yacc(1)
YACC	= yacc   # GNU bison?

#diff(1)
DIFF    = diff   # GNU diff?
DIFF_FLAGS = -w

# Other Macros
NCDUMP	= 
NCGEN	= 
FTPDIR	= 
VERSION_NETCDF	= 2.3.2

# ------------ XDR Macros for NETCDF --------------------
# This is where the location of system XDR library includes
# and location of the library are set
#
# Location of <xdr.h> if you are using the system
# one, Else you need to comment it out
CPP_XDR		= -I/usr/include/rpc

# If XDR library not present on the system then you need
# uncomment the following and comment the one below it
#XDR_LIBOBJS    = xdr.o xdrfloat.o xdrstdio.o xdrarray.o
XDR_LIBOBJS     =

# Location of library, sometimes the sunos requires -lsun
LD_XDR		= 

#
XDR_INSTALL_DEPS =

# ------------ libsrc Macros for NETCDF --------------------
# Possible values are xdrposix and xdrstdio
# This sets which version of the XDR interface to use.
XDRFILE		= xdrposix

# ------------ Port Macros for NETCDF --------------------
# Major and Minor numbe of NETCDF library version
MAJOR_NO	= 2
MINOR_NO	= 3
PORT_CFORTRAN	= 
NEED_FORTC	= 
FORTC		= 

# possible values are limits.h, float.h, stddef.h, stdlib.h,
# string.h time.h signal.h unistd.h
PORT_HEADERS	= 
PORT_SUBDIRS    =
LIBOBJS		= uddummy.o 

### ------------------- end of makefile fragments -------------------------

# ############# End of system configuration section. ###############

HDF_FLAGS       = \
        CC="$(CC)" \
        CFLAGS="$(CFLAGS)" \
        FC="$(FC)" \
        FFLAGS="$(FFLAGS)" \
        RANLIB="$(RANLIB)" \
        AR="$(AR)" \
        ARFLAGS="$(ARFLAGS)" \
        RM="$(RM)" \
        RMFLAGS="$(RMFLAGS)" \
        MACHINE="$(MACHINE)" 

LOCAL_MACROS  = CC="$(CC)" \
	CFLAGS="$(CFLAGS)" \
	CPP="$(CPP)" \
	CPPFLAGS="$(CPPFLAGS)" \
	exec_prefix="$(exec_prefix)" \
	FC="$(FC)" \
	prefix="$(prefix)"

# ==> where is your HDF library (libdf.a) located?
# ==> where are your hdf.h, hdfi.h and vg.h files located?
HDFINC= $(srcdir)/../src
HDFLIB=$(srcdir)/../src
HDFSRC=$(srcdir)/../src
HDFBIN=$(srcdir)/../bin

# ==> where is your JPEG library (libjpeg.a) located?
JPEGLIB=$(srcdir)/../jpeg
JPEGINC=$(srcdir)/../jpeg

# ==> where is your PABLO include files
PABLO_HINC= $(srcdir)/../pablo

# ==> specify where to find the FMPOOL include and source files 
FMPOOL_INC=$(srcdir)/../fmpool
FMPOOL_SRC=$(srcdir)/../fmpool

# ==> where is your zlib library (libzlib.a) located?
ZLIBLIB=$(srcdir)/../zlib
ZLIBINC=$(srcdir)/../zlib

ALL_CFLAGS = $(CFLAGS) $(FMPOOL_FLAGS) -I$(HDFINC) -I$(JPEGINC) -I$(ZLIBINC)

MANIFEST = $(DISTFILES)

DISTFILES = Makefile.in bitio.dat litend.dat nbit.dat tmgr.dat win32tst.mak \
	$(CSRCS) $(FSRCS) $(CHDRS) gentest.c

CHDRS= tproto.h tutils.h

CSRCS= 	rig.c sdstr.c blocks.c an.c anfile.c extelt.c file.c file1.c vers.c \
	sdmms.c sdnmms.c slab.c litend.c tvset.c comp.c bitio.c tree.c macros.c \
	conv.c nbit.c man.c mgr.c testhdf.c tbv.c tvsfpack.c chunks.c tvattr.c \
    buffer.c

COBJS= rig.o sdstr.o blocks.o an.o anfile.o extelt.o file.o file1.o vers.o \
	sdmms.o sdnmms.o slab.o litend.o tvset.o comp.o bitio.o tree.o macros.o \
	conv.o nbit.o man.o mgr.o testhdf.o tbv.o tvsfpack.o chunks.o tvattr.o \
    buffer.o

FSRCS= fortest.c fortestF.f forsupff.f forsupf.c \
	manf.f mgrf.f slabwf.f t24f.f tanf.f tanfilef.f tpf.f tr8f.f \
	tsdmmsf.f tsdnmmsf.f tsdnntf.f tsdntf.f tsdstrf.f tstubsf.f \
        tvsetf.f, tvattrf.f

FOBJS = fortestF.o forsupff.o forsupf.o \
	manf.o mgrf.o slabwf.o t24f.o tanf.o tanfilef.o tpf.o tr8f.o \
	tsdmmsf.o tsdnmmsf.o tsdnntf.o tsdntf.o tsdstrf.o tstubsf.o  \
        tvsetf.o tvattrf.o

#CFORSUPSRCS = forsupf.c

FTESTS = fortest fortestF

#CFORSUPOBJS = forsupf.o

TESTF = fortest.o

# directory used by extelt test.
TESTDIR = testdir

all::		FORCE
	@case "$(FC)" in \
	NONE*|none*) \
	    $(MAKE) $(MFLAGS) $(HDF_FLAGS) nofortran ;; \
	*) \
	    $(MAKE) $(MFLAGS) $(HDF_FLAGS) rall ; \
	esac

rall::         FORCE
	@if test -z "$(FMPOOL_FLAGS)"; then \
	  $(MAKE) $(MFLAGS) $(HDF_FLAGS) testhdf $(FTESTS) gentest; \
	else		 \
	  $(MAKE) $(MFLAGS) $(HDF_FLAGS) FMPOOL_FLAGS="$(FMPOOL_FLAGS) -I$(FMPOOL_INC)" testhdf $(FTESTS) gentest; \
	fi

nofortran::         FORCE
	@if test -z "$(FMPOOL_FLAGS)"; then \
	  $(MAKE) $(MFLAGS) $(HDF_FLAGS) testhdf gentest; \
	else		 \
	  $(MAKE) $(MFLAGS) $(HDF_FLAGS) FMPOOL_FLAGS="$(FMPOOL_FLAGS) -I$(FMPOOL_INC)" testhdf gentest; \
	fi

rebuild:
	@$(MAKE) all $(HDF_FLAGS) FRC=force_rebuild

rebuildnofortran:
	@$(MAKE) nofortran $(MFLAGS) $(HDF_FLAGS) FRC=force_rebuild

# Leave this target blank with no actions, the rebuild macros above depend on it
force_rebuild:

debug: ptesthdf

$(HDFLIB)/libdf.a:
#	cd $(HDFLIB); echo Making \`$(TARG)\' in `pwd`; \        
#	$(MAKE) $(MFLAGS) $(HDF_FLAGS) $(TARG); 

$(JPEGLIB)/libjpeg.a:
#	cd $(JPEGLIB); echo Making \`$(TARG)\' in `pwd`; \        
#	$(MAKE) $(MFLAGS) $(HDF_FLAGS) $(TARG); 

$(ZLIBLIB)/libz.a:
#	cd $(JPEGLIB); echo Making \`$(TARG)\' in `pwd`; \        
#	$(MAKE) $(MFLAGS) $(HDF_FLAGS) $(TARG); 

#.c.o: 
#	 $(CC) $(CFLAGS) -c -I$(HDFINC) $*.c 
#
.c.o: 
	$(CC) $(ALL_CFLAGS) -c $*.c 

.f.o:
	$(FC) $(FFLAGS) -c $*.f 

$(COBJS) $(TESTF) $(FTESTS): $(FRC)

$(FOBJS): fortest.inc

#$(FTESTS): $(FOBJS) $(CFORSUPOBJS) $(FFORSUPOBJS) $(HDFLIB)/libdf.a $(JPEGLIB)/libjpeg.a $(ZLIBLIB)/libz.a
#	$(FC) $(FFLAGS) $@.o -o $@ $(CFORSUPOBJS) $(FFORSUPOBJS) $(HDFLIB)/libdf.a $(JPEGLIB)/libjpeg.a $(ZLIBLIB)/libz.a

#$(FTESTS): $(FRC)

test-hdf::		FORCE
	@case "$(FC)" in \
	NONE*|none*) \
	    $(MAKE) $(MFLAGS) $(HDF_FLAGS) test-hdfnofortran ;; \
	*) \
	    $(MAKE) $(MFLAGS) $(HDF_FLAGS) atest-hdf ; \
	esac

atest-hdf: testhdf $(FTESTS)
	-$(RM) $(RMFLAGS) *.hdf 
	./testhdf
	-$(RM) $(RMFLAGS) *.hdf 
	./fortest

test-hdfnofortran: testhdf
	-$(RM) $(RMFLAGS) *.hdf 
	./testhdf

test-hdffortran: $(FTESTS) fortest.sav
	-$(RM) $(RMFLAGS) *.hdf 
	./fortest > fortest.out
	@cmp fortest.out fortest.sav && echo "*** Fortran tests passed ***";

qtesthdf: $(COBJS) $(HDFLIB)/libdf.a
	quantify $(CC) $(ALL_CFLAGS) -o qtesthdf $(COBJS) $(HDFSRC)/libdf.a $(JPEGLIB)/libjpeg.a $(ZLIBLIB)/libz.a -lm

ptesthdf: $(COBJS) $(HDFLIB)/libdf.a
	purify $(CC) $(ALL_CFLAGS) -o ptesthdf $(COBJS) $(HDFSRC)/libdf.a $(JPEGLIB)/libjpeg.a $(ZLIBLIB)/libz.a -lm

testhdf.tc: $(COBJS) $(HDFLIB)/libdf.a
	proof $(CC) $(ALL_CFLAGS) -o testhdf.tc $(COBJS) $(HDFLIB)/libdf.a $(JPEGLIB)/libjpeg.a $(ZLIBLIB)/libz.a -lm

testhdf: $(COBJS) $(HDFLIB)/libdf.a
	$(CC) $(ALL_CFLAGS) -o testhdf $(COBJS) $(HDFLIB)/libdf.a $(JPEGLIB)/libjpeg.a $(ZLIBLIB)/libz.a -lm
	test -d $(TESTDIR) || mkdir $(TESTDIR)

fortestF: $(FOBJS) $(HDFLIB)/libdf.a
	$(FC) $(FFLAGS) -o $@ $(FOBJS) $(HDFLIB)/libdf.a $(JPEGLIB)/libjpeg.a $(ZLIBLIB)/libz.a -lm

fortest: $(HDFLIB)/libdf.a fortest.c $(CHDRS)
	$(CC) $(ALL_CFLAGS) -o fortest fortest.c $(HDFLIB)/libdf.a $(JPEGLIB)/libjpeg.a $(ZLIBLIB)/libz.a -lm

gentest: gentest.c $(HDFLIB)/libdf.a
	$(CC) $(ALL_CFLAGS) gentest.c -o gentest $(HDFLIB)/libdf.a $(JPEGLIB)/libjpeg.a $(ZLIBLIB)/libz.a -lm

ftests: $(FTESTS)

clean: 
	-$(RM) $(RMFLAGS) $(COBJS) testhdf $(FOBJS) $(FTESTS) \
	core *.hdf ptesthdf qtesthdf gentest *.o fortest.arg $(TESTDIR)/*

distclean: 
	-$(RM) $(RMFLAGS) $(COBJS) testhdf $(FOBJS) $(FTESTS) \
	core *.hdf ptesthdf qtesthdf gentest *.o fortest.arg $(TESTDIR)/* \
	config.status Makefile

depend: 
	makedepend -I$(HDFINC) *.c

saber:	$(CSRCS)
	 #load $(ALL_CFLAGS) $(CSRCS) 

# for distribution creation
distdir = $(TOP_SRCDIR)/$(PACKAGE)-$(VERSION)/$(curdir)
curdir = hdf/test
dist: $(DISTFILES)
	@for file in $(DISTFILES); do \
	  ln $(srcdir)/$$file $(distdir)/$$file 2> /dev/null \
	    || { echo copying $$file instead; \
	    cp -p $(srcdir)/$$file $(distdir)/$$file;}; \
	done

# The following dummy target is useful as a dependency to ensure that a
# rule is always executed.
#
FORCE:

help:
	@echo ""
	@echo "Make supports the following targets in the 'tests' directory"
	@echo "make help    - prints this usage section"
	@echo "make all     - (DEFAULT) makes tests"
	@echo "make nofortran - makes HDF tests excluding fortran tests"
	@echo "make testhdf   - makes HDF C test program "
	@echo "make fortest   - makes HDF Fortran test program "
	@echo "make test-hdf          - run HDF tests " 
	@echo "make test-hdfnofortran - run HDF tests except the fortran test " 
	@echo "make test-hdffortran   - run HDF fortran tests only " 
	@echo "make clean     - cleans up tests (both C and Fortran)"
	@echo "make distclean - cleans up HDF tests including"
	@echo "                 configuration(Makefiles,config.status,..)"
	@echo ""

# DO NOT DELETE THIS LINE -- make depend depends on it.
