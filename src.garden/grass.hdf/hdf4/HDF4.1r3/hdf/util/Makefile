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
# $Id: Makefile,v 1.1 1999-12-29 15:12:55 markus Exp $
#
# ##################################################################
#

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

INSTALL = ${TOP_SRCDIR}/install-sh -c
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

# The echo command.
ECHO = echo

# Directory in which to install scripts.
bindir = $(exec_prefix)/bin

# Directory in which to install library files.
libdir = $(prefix)/lib

# Directory in which to install include files.
includedir = $(prefix)/include

# Directory in which to install documentation info files.
infodir = $(prefix)/info

#### ----- host and target specific makefile fragments come in here.-------
# $Id: Makefile,v 1.1 1999-12-29 15:12:55 markus Exp $

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
HDFINC=$(srcdir)/../src
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

# ==> where is your zlib library (libz.a) located?
ZLIBLIB=$(srcdir)/../zlib
ZLIBINC=$(srcdir)/../zlib

#Where are the utility includes
UTILINC=$(srcdir)

ALL_CFLAGS = $(CFLAGS) $(FMPOOL_FLAGS) -I$(HDFINC) -I$(JPEGINC) -I$(ZLIBINC) -I$(UTILINC)

OTHERDIRS = fixatr testfiles

MANIFEST = $(DISTFILES)
DISTFILES = Makefile.in README.TST $(UTILSRCS) $(UTILHDRS) $(FIXATRS) $(TESTFILES)

# Utilities
UTILSRCS = hdf24to8.c hdfcomp.c hdfls.c hdfpack.c hdftopal.c hdftor8.c paltohdf.c \
        r8tohdf.c ristosds.c vmake.c vshow.c hdf8to24.c hdf2jpeg.c jpeg2hdf.c fp2hdf.c \
	hdfunpac.c he_main.c he_cntrl.c he_disp.c he_file.c fptest.c

UTILHDRS = he.h he_proto.h

FIXATRS = fixatr/README fixatr/command.for fixatr/common.for fixatr/fixatr.cld \
	fixatr/fixatr.hlp fixatr/makefix.com fixatr/parse.mar fixatr/rformat.for


TESTFILES = testfiles/README testfiles/head.r24.Z testfiles/jpeg_img.jpg \
	testfiles/ntcheck.hdf testfiles/palette.raw testfiles/storm110.hdf \
	testfiles/storm110.raw testfiles/storm120.hdf testfiles/storm120.raw \
	testfiles/storm130.hdf testfiles/storm130.raw testfiles/storm140.raw \
	testfiles/test.cdf testfiles/test.hdf

UTILS = hdf24to8 hdfcomp hdfls hdfpack hdftopal hdftor8 paltohdf \
        r8tohdf ristosds  vmake vshow hdf8to24 hdf2jpeg jpeg2hdf fp2hdf \
	hdfunpac

HEOBJ = he_main.o he_cntrl.o he_disp.o he_file.o

PROGS = $(UTILS) hdfed

#all: hdfed utils fp2hdf fptest
all::         FORCE
	@if test -z "$(FMPOOL_FLAGS)"; then \
	  $(MAKE) $(MFLAGS) $(HDF_FLAGS) hdfed utils fp2hdf fptest ; \
	else		 \
	  $(MAKE) $(MFLAGS) $(HDF_FLAGS) FMPOOL_FLAGS="$(FMPOOL_FLAGS) -I$(FMPOOL_INC)" hdfed utils fp2hdf fptest ; \
	fi

#nofortran: hdfed utils fp2hdf fptest
nofortran::         FORCE
	@if test -z "$(FMPOOL_FLAGS)"; then \
	  $(MAKE) $(MFLAGS) $(HDF_FLAGS) hdfed utils fp2hdf fptest ; \
	else		 \
	  $(MAKE) $(MFLAGS) $(HDF_FLAGS) FMPOOL_FLAGS="$(FMPOOL_FLAGS) -I$(FMPOOL_INC)" hdfed utils fp2hdf fptest ; \
	fi

rebuild:
	@$(MAKE) all $(HDF_FLAGS) FRC=force_rebuild

rebuildnofortran:
	@$(MAKE) nofortran $(MFLAGS) $(HDF_FLAGS) FRC=force_rebuild

# Leave this target blank with no actions, the rebuild macros above depend on it
force_rebuild:

install: install-prog

install-prog: all
	for p in $(PROGS); do \
	    $(INSTALL_PROGRAM) \
		`pwd`/$$p $(bindir)/`$(ECHO) $$p`; \
	done

test-utils:
	./testutil.sh $(target_canonical)

#.c.o: 
#	 $(CC) $(CFLAGS) -c -I$(HDFINC) $*.c 
#
#.c.o: 
#	 $(CC) $(CFLAGS) -c -I$(HDFINC) -I$(UTILINC) -I$(JPEGINC) -I$(ZLIBINC) $*.c 

.c.o: he.h he_proto.h $(HDFLIB)/libdf.a $(FRC)
	$(CC) $(ALL_CFLAGS) -c $<

.c: $(HDFLIB)/libdf.a
	$(CC) $(ALL_CFLAGS) $< -o $@ $(HDFLIB)/libdf.a $(JPEGLIB)/libjpeg.a $(ZLIBLIB)/libz.a $(LIBSX)

.f: $(HDFLIB)/libdf.a 
	$(FC) $(FFLAGS) $< $(HDFLIB)/libdf.a -o $@ $(JPEGLIB)/libjpeg.a $(ZLIBLIB)/libz.a

hdfed: he.h he_proto.h $(HEOBJ) $(HDFLIB)/libdf.a $(FRC)
	$(CC) $(ALL_CFLAGS) $(HEOBJ) $(HDFLIB)/libdf.a -o hdfed $(JPEGLIB)/libjpeg.a $(ZLIBLIB)/libz.a

utils: $(UTILS) $(HDFLIB)/libdf.a $(FRC)

$(HEOBJ): $(FRC)

$(HDFLIB)/libdf.a:

$(JPEGLIB)/libjpeg.a:

$(ZLIBLIB)/libz.a:

clean:
	-$(RM) $(RMFLAGS) *.o $(UTILS) hdfed fptest
	-$(RM) $(RMFLAGS) o* ctx* cb* fptestf 

distclean:
	-$(RM) $(RMFLAGS) *.o $(UTILS) hdfed fptest config.status Makefile
	-$(RM) $(RMFLAGS) o* ctx* cb* fptestf
depend: 
	 makedepend -I$(HDFINC) -I$(UTILINC) *.c

saber:	$(SRCS)
	 #load $(ALL_CFLAGS) -D$(MACHINE) $(SRCS) 

# for distribution creation
distdir = $(TOP_SRCDIR)/$(PACKAGE)-$(VERSION)/$(curdir)
curdir = hdf/util
dist: $(DISTFILES)
	for subdir in $(OTHERDIRS); do \
	  mkdir $(distdir)/$$subdir || exit 1; \
	done
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
	@echo "Make supports the following targets in the 'utilities' directory"
	@echo "make help    - prints this usage section"
	@echo "make all     - (DEFAULT) makes utilities"
	@echo "make hdfed   - makes hdfed utilitiy"
	@echo "make test-utils  - tests utilities"
	@echo "make install     - install HDF utilites, "
	@echo "make clean     - cleans up tests (both C and Fortran)"
	@echo "make distclean - cleans up HDF tests including"
	@echo "                 configuration(Makefiles,config.status,..)"
	@echo ""

# DO NOT DELETE THIS LINE -- make depend depends on it.
