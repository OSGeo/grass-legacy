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
host_makefile_frag = ../config/mh-linux
# Makefile for HDF
#
### Start of system configuration section. ####
# Set shell so we know what it is
SHELL = /bin/sh

# 'srcdir' Gets set to directory this Makefile is found in
srcdir = .
TOP_SRCDIR = ..

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

#defaults
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
# $Id: Makefile,v 1.1 1999-12-29 15:12:38 markus Exp $

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

PACKAGE	 = HDF
VERSION  = 4.0

MANIFEST = $(DISTFILES)
DISTFILES = Makefile.in COPYING build.inc

SUBDIRS= src jpeg zlib fmpool pablo test util

HDFINC= $(srcdir)/src
HDFLIB= $(srcdir)/src
HDFBIN= $(srcdir)/bin
PABLO_HINC= $(srcdir)/pablo
FMPOOL_INC= $(srcdir)/fmpool

#
#
# Flags to recursively send
#

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
        PABLO_FLAGS="$(PABLO_FLAGS)" \
        PABLO_INCLUDE="$(PABLO_INCLUDE)" \
        MACHINE="$(MACHINE)" 

LOCAL_MACROS  = CC="$(CC)" \
	CFLAGS="$(CFLAGS)" \
	CPP="$(CPP)" \
	CPPFLAGS="$(CPPFLAGS)" \
	exec_prefix="$(exec_prefix)" \
	FC="$(FC)" \
        FFLAGS="$(FFLAGS)" \
	prefix="$(prefix)"

#
#
# General rules
#
all:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libjpeg.a \
          SUBDIRS="jpeg" subd  
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libz.a \
          SUBDIRS="zlib" subd  
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="src pablo fmpool util test" subd

allnofortran:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libjpeg.a \
          SUBDIRS="jpeg" subd  
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libz.a \
          SUBDIRS="zlib" subd  
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=nofortran \
          SUBDIRS="src pablo fmpool util test" subd

rebuild rebuildnofortran:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libjpeg.a \
          SUBDIRS="jpeg" subd 
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libz.a \
          SUBDIRS="zlib" subd 
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="src pablo fmpool util test" subd

libnofortran:
	@$(MAKE) $(MYFLAGS) $(HDF_FLAGS) TARG=libjpeg.a \
	  SUBDIRS="jpeg" subd 
	@$(MAKE) $(MYFLAGS) $(HDF_FLAGS) TARG=libz.a \
	  SUBDIRS="zlib" subd 
	@$(MAKE) $(MYFLAGS) $(HDF_FLAGS) TARG=nofortran \
	  SUBDIRS="src pablo fmpool" subd

libnostub: libnofortran

allnostub: allnofortran

libs:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libjpeg.a \
          SUBDIRS="jpeg" subd
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libz.a \
          SUBDIRS="zlib" subd
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=all \
          SUBDIRS="src pablo fmpool" subd

libdf:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=all \
          SUBDIRS="src" subd

libjpeg:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libjpeg.a \
          SUBDIRS="jpeg" subd

libz:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libz.a \
          SUBDIRS="zlib" subd

utilities:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libjpeg.a \
          SUBDIRS="jpeg" subd
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libz.a \
          SUBDIRS="zlib" subd
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=all \
          SUBDIRS="src pablo fmpool util" subd

hdf-test:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libjpeg.a \
          SUBDIRS="jpeg" subd
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libz.a \
          SUBDIRS="zlib" subd
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=all \
          SUBDIRS="src pablo fmpool test" subd

hdf-testnofortran:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libjpeg.a \
          SUBDIRS="jpeg" subd
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libz.a \
          SUBDIRS="zlib" subd
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=nofortran \
          SUBDIRS="src pablo fmpool test" subd

testnofortran:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libjpeg.a \
          SUBDIRS="jpeg" subd 
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libz.a \
          SUBDIRS="zlib" subd 
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=nofortran \
          SUBDIRS="src pablo fmpool test" subd

test-hdf:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=test-hdf \
          SUBDIRS="test" subd

test-hdfnofortran:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=test-hdfnofortran \
          SUBDIRS="test" subd

test-hdffortran:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=test-hdffortran \
          SUBDIRS="test" subd

test-utils:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=test-utils \
          SUBDIRS="util" subd

install:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=install-lib \
          SUBDIRS="src jpeg zlib pablo fmpool" subd
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=install \
          SUBDIRS="util" subd

install-libs:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=install-lib \
          SUBDIRS="src jpeg zlib pablo fmpool" subd

install-includes:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=install-includes \
          SUBDIRS="src jpeg zlib pablo fmpool" subd

install-utils:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=install \
          SUBDIRS="util" subd

perfs:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libjpeg.a \
          SUBDIRS="jpeg" subd
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libz.a \
          SUBDIRS="zlib" subd
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=all \
          SUBDIRS="src perf fmpool" subd

#install:      $(INSTALL_DIRS)
#	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@  \
#          SUBDIRS="src util test" subd

debug:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libjpeg.a \
          SUBDIRS="jpeg" subd 
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=libz.a \
          SUBDIRS="zlib" subd 
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=debug \
          SUBDIRS="src fmpool test" subd

saber:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=saber \
          SUBDIRS="src jpeg zlib test" subd

clean:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@  \
          SUBDIRS="src jpeg zlib pablo fmpool util test" subd
	-$(RM) $(RMFLAGS) core *.log

distclean:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@  \
          SUBDIRS="src jpeg zlib pablo fmpool util test" subd
	-$(RM) $(RMFLAGS) core *.log
	-$(RM) -rf bin lib include config.status Makefile

# for recusive make in subdirs
subd:
	@for dir in $(SUBDIRS); do \
          (cd $$dir; echo Making \`$(TARG)\' in `pwd`; \
              $(MAKE) $(MFLAGS) $(HDF_FLAGS) $(TARG)); \
	done

# for distribution creation
distdir = $(TOP_SRCDIR)/$(PACKAGE)-$(VERSION)/$(curdir)
curdir = hdf
dist: $(DISTFILES)
	@for file in $(DISTFILES); do \
	  ln $(srcdir)/$$file $(distdir)/$$file 2> /dev/null \
	    || { echo copying $$file instead; \
	    cp -p $(srcdir)/$$file $(distdir)/$$file;}; \
	done
	for subdir in $(SUBDIRS); do \
	  echo making $@ in $$subdir; \
	  mkdir $(distdir)/$$subdir || exit 1; \
	  (cd $$subdir && $(MAKE) $@) || exit 1; \
	done

# for auto remaking, see toplevel Makefile
#Makefile: $(TOP_SRCDIR)/config.status Makefile.in
#	CONFIG_FILES=$@ CONFIG_HEADERS= $(TOP_SRCDIR)/config.status

# Tell versions [3.59,3.63) of GNU make not to export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:


help:
	@echo ""
	@echo "Make supports the following targets"
	@echo "make help    - prints this usage section"
	@echo "make all     - (DEFAULT) makes HDF package"
	@echo "make allnofortran - makes HDF package excluding fortran support"
	@echo "make libnofortran - makes HDF library excluding fortran support"
	@echo "make testnofortran - makes HDF library and tests"
	@echo "                     excluding fortran support"
	@echo "make hdf-test      - makes HDF library and tests"
	@echo "make utilities     - makes HDF library and utilities"
	@echo "make libs          - makes HDF and IJG JPEG libraries only"
	@echo "make libdf         - makes HDF library "
	@echo "make libjpeg       - makes IJG JPEG library "
	@echo "make libz          - makes GZIP deflate library "
	@echo "make test-hdf          - run HDF library tests " 
	@echo "make test-hdfnofortran - run HDF library tests except the fortran test " 
	@echo "make test-hdffortran   - run HDF library fortran tests only " 
	@echo "make test-utils        - run HDF utilities test"
	@echo "make install           - install HDF library, "
	@echo "                         utilites, and includes " 
	@echo "make install-libs      - install HDF library and includes " 
	@echo "make install-includes  - install HDF includes " 
	@echo "make install-utils     - install HDF utilities " 
	@echo "make clean     - cleans up HDF packages(*.o,libdf.a,..)"
	@echo "make distclean - cleans up HDF packages including"
	@echo "                 configuration(Makefiles,config.status,..)"
	@echo ""


