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
# ***************************************************************************
#
# $Id: Makefile,v 1.1 1999-12-29 15:12:51 markus Exp $
#
# ##################################################################
#
#           MAKEFILE for creating the Pablo Instrumentation code
#
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

# Directory in which to install scripts.
bindir = $(exec_prefix)/bin

# Directory in which to install library files.
libdir = $(prefix)/lib

# Directory in which to install include files.
includedir = $(prefix)/include

# Directory in which to install documentation info files.
infodir = $(prefix)/info

# Pablo Instrumentation Library support files
PCHDRS=ProcIDs.h ProcMasks.h
PCSRCS=PabloHDF.c PabloHDF_RT.c PabloHDF_SDDF.c
PCOBJS=PabloHDF.o PabloHDF_RT.o PabloHDF_SDDF.o
PFINCS=PabloHDF.inc ProcMasks.inc
PFSRCS=PabloHDFff.f
PFOBJS=PabloHDFff.o
POBJS=PabloHDF.o PabloHDF_RT.o PabloHDF_SDDF.o PabloHDFff.o
PSRCS= $(PCHDRS) $(PCSRCS) $(PFINCS) $(PFSRCS)

TARGETLIB = libdf-inst.a

PABLO_INCDIR = .
PABLO_INCLUDE= -I$(PABLO_INCDIR)

#### ----- host and target specific makefile fragments come in here.-------
# $Id: Makefile,v 1.1 1999-12-29 15:12:51 markus Exp $

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
        FMPOOL_FLAGS="$(FMPOOL_FLAGS)" \
        MACHINE="$(MACHINE)" 

LOCAL_MACROS  = CC="$(CC)" \
	CFLAGS="$(CFLAGS)" \
	CPP="$(CPP)" \
	CPPFLAGS="$(CPPFLAGS)" \
	exec_prefix="$(exec_prefix)" \
	FC="$(FC)" \
	prefix="$(prefix)"

# ==> specify directories where to find inludes and library
#     HDF library, and HDF utilities
HDFINC= $(srcdir)/../src
HDFLIB= $(srcdir)/../src
HDFBIN= $(srcdir)/../bin
HDFSRC= $(srcdir)/../src

# ==> specify where to find the JPEG include files and library
JPEGINC=$(srcdir)/../jpeg
JPEGLIB=$(srcdir)/../jpeg

# ==> specify where to find the FMPOOL include and source files 
FMPOOL_INC=$(srcdir)/../fmpool
FMPOOL_SRC=$(srcdir)/../fmpool

# ==> specify where to find the gzip zlib include files and library
ZLIBINC=$(srcdir)/../zlib
ZLIBLIB=$(srcdir)/../zlib

ALL_CFLAGS= $(CFLAGS) $(PABLO_FLAGS) $(FMPOOL_FLAGS) -I. $(PABLO_INCLUDE) -I$(HDFINC) -I$(JPEGINC) -I$(ZLIBINC) -I$(FMPOOL_INC)

LINTFLAGS=
SHAR=shar
ARCHIVE=pablo.shar

MANIFEST = $(DISTFILES)
DISTFILES = Makefile.in $(PSRCS) 

# to extract all the object files out of libdf.a 
HOBJS=`ar xv $(HDFLIB)/libdf.a | sed 's/x -//'`

PINCS = $(PABLO_INCLUDES)

#
#include the HDF source file definitions
#
include $(HDFSRC)/src.inc

.SUFFIXES: .c.o .f.o 

# This rule causes ansi2knr to be invoked.
#@ISANSICOM@.c.o:
#@ISANSICOM@	./ansi2knr $(srcdir)/$*.c T$*.c
#@ISANSICOM@	$(CC) $(CFLAGS) -c T$*.c
#@ISANSICOM@	$(RM) $(RMFLAGS) T$*.c $*.o
#@ISANSICOM@	$(MV) T$*.o $*.o

#ansi2knr: ansi2knr.c
#	$(CC) $(CFLAGS) $(ANSI2KNRFLAGS) -o ansi2knr ansi2knr.c

.c.o: 
	$(CC) $(CPPFLAGS) $(ALL_CFLAGS) -c $(HDFSRC)/$*.c

.f.o:
	$(FC) $(FFLAGS) -c $(HDFSRC)/$*.f

.PHONY : all nofortran clean distclean debug saber lint

#all::

all::         FORCE
	@if test -z "$(PABLO_FLAGS)"; then \
	  : true; \
	else		 \
	  $(MAKE) $(MFLAGS) $(HDF_FLAGS) pablohdf; \
	fi

pablohdf::		FORCE
	@case "$(FC)" in \
	NONE*|none*) \
	    $(MAKE) $(MFLAGS) $(HDF_FLAGS) pablonofortran;; \
	*) \
	    $(MAKE) $(MFLAGS) $(HDF_FLAGS) libdf-inst.a;; \
	esac

libdf-inst.a: $(COBJS) $(FOBJS) $(PCOBJS) $(PFOBJS)
	-$(RM) $(RMFLAGS) libdf-inst.a
	$(AR) $(ARFLAGS) libdf-inst.a $(COBJS) $(FOBJS) $(PCOBJS) $(PFOBJS)
	$(RANLIB) libdf-inst.a

pablonofortran: $(COBJS) $(PCOBJS) 
	-$(RM) $(RMFLAGS) libdf-inst.a
	$(AR) $(ARFLAGS) libdf-inst.a $(COBJS) $(PCOBJS)
	$(RANLIB) libdf-inst.a


nofortran: all

rebuild:
	@$(MAKE) all $(HDF_FLAGS) FRC=force_rebuild

rebuildnofortran:
	@$(MAKE) nofortran $(MFLAGS) $(HDF_FLAGS) FRC=force_rebuild

# Leave this target blank with no actions, the rebuild macros above depend on it
force_rebuild:

$(FOBJS): $(FRC)

debug: libdf-inst.a

saber:
	#load -D$(MACHINE) $(CSRCS) $(TSRCS) -lc

install::  FORCE
	@if test -z "$(PABLO_FLAGS)"; then \
	  : true; \
	else		 \
	  $(MAKE) $(MFLAGS) $(HDF_FLAGS) pinstall-lib; \
	fi

install-lib:: install

install-includes:: FORCE
	@if test -z "$(PABLO_FLAGS)"; then \
	  : true; \
	else		 \
	  $(MAKE) $(MFLAGS) $(HDF_FLAGS) pinstall-headers; \
	fi

pinstall-lib: pinstall-headers
	$(INSTALL_DATA) $(TARGETLIB) $(libdir)/$(TARGETLIB).n
	( cd $(libdir) ; $(RANLIB) $(libdir)/$(TARGETLIB).n )
	mv -f $(libdir)/$(TARGETLIB).n $(libdir)/$(TARGETLIB)

pinstall-headers:
	for p in $(PINCS); do \
	    $(INSTALL_DATA) \
		`pwd`/$$p $(includedir)/$$p; \
	done

depend:
	makedepend -fMakefile -- $(CSRCS)

clean:
	-$(RM) $(RMFLAGS) *.o  libdf-inst.a

distclean:
	-$(RM) $(RMFLAGS) *.o  libdf-inst.a config.status Makefile

lint: $(CSRCS) 
	$(LINT) $(LINTFLAGS) $(CSRCS)

TAGS: $(CSRCS) $(CHDRS)
	etags $(CSRCS) $(CHDRS)

# for distribution creation
distdir = $(TOP_SRCDIR)/$(PACKAGE)-$(VERSION)/$(curdir)
curdir = hdf/src
dist: $(DISTFILES)
	@for file in $(DISTFILES); do \
	  ln $(srcdir)/$$file $(distdir)/$$file 2> /dev/null \
	    || { echo copying $$file instead; \
	    cp -p $(srcdir)/$$file $(distdir)/$$file;}; \
	done

# for auto remaking, see toplevel Makefile
#Makefile: $(TOP_SRCDIR)/config.status Makefile.in
#	CONFIG_FILES=$@ CONFIG_HEADERS= $(TOP_SRCDIR)/config.status

# Tell versions [3.59,3.63) of GNU make not to export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:


# The following dummy target is useful as a dependency to ensure that a
# rule is always executed.
#
FORCE:

help:
	@echo ""
	@echo "Make supports the following targets"
	@echo "make help    - prints this usage section"
	@echo "make all     - (DEFAULT) makes Pablo stubs"
	@echo "make pablohdf       - makes Pablo stubs "
	@echo "make pablonofortran - makes Pablo stubs excluding fortran support"
	@echo "make rebuild      - rebuilds Pablo stubs "
	@echo "make rebuildnofortran - rebuilds Pablo stubs excluding"
	@echo "                        fortran support "
	@echo "make install          - install Pablo includes, "
	@echo "make clean     - cleans up Pablo stubs (*.o,..)"
	@echo "make distclean - cleans up Pablo stubs including"
	@echo "                 configuration(Makefiles,config.status,..)"
	@echo ""


# DO NOT DELETE THIS LINE -- make depend depends on it.
PABLO_INCLUDES = HDFentryNames.h HDFidList.h ProcIDs.h HDFTrace.h ProcMasks.h \
		 ProcMasks.inc

HDFentryNames.h: HDFentries.txt
	sort $? | sed "s/.*/\"&\",/" > $@

HDFidList.h: HDFentries.txt
	sort $? | sed "s/.*/ID_&,/" > $@

ProcIDs.h: HDFidList.h

PabloHDF.o: PabloHDF.c $(PABLO_INCLUDES)
	$(CC) $(CPPFLAGS) $(ALL_CFLAGS) -c $<
PabloHDF_RT.o: PabloHDF_RT.c $(PABLO_INCLUDES)
	$(CC) $(CPPFLAGS) $(ALL_CFLAGS) -c $<
PabloHDF_SDDF.o: PabloHDF_SDDF.c $(PABLO_INCLUDES)
	$(CC) $(CPPFLAGS) $(ALL_CFLAGS) -c $<
PabloHDFff.o: PabloHDFff.f ProcIDs.h
	$(FC) $(FFLAGS) -c $<
 
#get all the source dependecies.....
# the dependencies don't work well
include depend

