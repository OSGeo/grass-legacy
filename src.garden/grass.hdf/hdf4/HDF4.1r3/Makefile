# This file was generated automatically by configure.  Do not edit.
#VPATH = .
links = mfhdf/libsrc/netcdf.h mfhdf/fortran/jackets.c mfhdf/fortran/netcdf.inc mfhdf/fortran/ftest.f
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
host_makefile_frag = config/mh-linux
# $Id: Makefile,v 1.1 1999-12-29 15:12:38 markus Exp $
#
# Makefile to build HDF & netcdf libraries and utilities...etc
#
# NOTE: This Makefile.in is used by the Cygnus 'configure' script to
#       generate the top level Makefile.

# ############# Start of system configuration section. #############

# 'srcdir' Gets set to directory this Makefile is found in
srcdir = .
TOP_SRCDIR = .

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

SHELL = /bin/sh

INSTALL = ${TOP_SRCDIR}/install-sh -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL) -m 644

# /usr/local/bin/m4 (GNU version?)
M4   = /usr/local/bin/m4
# /usr/local/bin/perl (GNU version?)
PERL = /usr/local/bin/perl

# Name of achive randomizer, usually ranlib (use 'true' if non-existant)
RANLIB = ranlib

# Name of library archiver and flags to send, default 'AR=ar', 'ARFLAGS=r'
AR = ar
ARFLAGS = rc

# Name of remove utility, default 'RM=/bin/rm', 'RMFLAGS=-f'
RM = /bin/rm
RMFLAGS = -f

AS = as
CC = gcc
CFLAGS = -ansi -O
FC = f77
FFLAGS= -O

# Set for document generation
MAKEINFO = makeinfo
TEXI2DVI = texi2dvi

# Directory in which to install binaries
bindir = $(exec_prefix)/bin

# Directory in which to install header files.
includedir = $(prefix)/include

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
DISTFILES = INSTALL README \
	Makefile.in configure configure.in config.guess config.sub \
	install-sh mkinstalldirs move-if-change \
	config/mh-aix config/mh-alpha config/mh-convex \
	config/mh-decstation config/mh-fbsd config/mh-hpux config/mh-irix32 \
	config/mh-irix4 config/mh-irix5 config/mh-irix6 config/mh-linux \
	config/mh-solaris config/mh-solarisx86 config/mh-sun config/mh-unicos

HDFDIR    = hdf
NETCDFDIR = mfhdf
MANDIR = man
SUBDIRS= $(HDFDIR) $(NETCDFDIR) $(MANDIR)
OTHERDIRS= config

# Location where the HDF include files are located
HDFINC= `pwd`/hdf/src

# Location where the HDF library is located
HDFLIB= `pwd`/hdf/src

# ==> where is your PABLO include files
PABLO_HINC= `pwd`/hdf/pablo

# ==> where is your FMPOOL include files
FMPOOL_INC= `pwd`/hdf/fmpool

#
# Flags to recursively send to make to overide ones inserted by Configure
#

HDF_FLAGS       = \
        CC="$(CC)" \
        CFLAGS="$(CFLAGS)" \
        ACDEFS="$(ACDEFS)" \
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
        MACHINE="$(MACHINE)" \
        HDFINC="$(HDFINC)" \
        HDFLIB="$(HDFLIB)" 


NETCDF_FLAGS       = \
        CC="$(CC)" \
        CFLAGS="$(CFLAGS)" \
        ACDEFS="$(ACDEFS)" \
        FC="$(FC)" \
        FFLAGS="$(FFLAGS)" \
        RANLIB="$(RANLIB)" \
        AR="$(AR)" \
        ARFLAGS="$(ARFLAGS)" \
        RM="$(RM)" \
        RMFLAGS="$(RMFLAGS)" \
        FMPOOL_FLAGS="$(FMPOOL_FLAGS)" \
        MACHINE="$(MACHINE)" \
        HDFINC="$(HDFINC)" \
        HDFLIB="$(HDFLIB)" 

LOCAL_MACROS  = CC="$(CC)" \
	CFLAGS="$(CFLAGS)" \
	CPP="$(CPP)" \
	CPPFLAGS="$(CPPFLAGS)" \
	exec_prefix="$(exec_prefix)" \
	FC="$(FC)" \
	FFLAGS="$(FFLAGS)" \
	prefix="$(prefix)"

#
#General rules
#

all:	
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="$(HDFDIR)" subd
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG=$@ \
          SUBDIRS="$(NETCDFDIR)" subdn

HDF:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG="all" \
          SUBDIRS="$(HDFDIR)" subd

NETCDF:
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG="all" \
          SUBDIRS="$(NETCDFDIR)" subdn

test:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG="test-hdf" \
          SUBDIRS="$(HDFDIR)" subd
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=test-utils \
          SUBDIRS="$(HDFDIR)" subd
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG=$@ \
          SUBDIRS="$(NETCDFDIR)" subdn

allnofortran:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=allnofortran \
          SUBDIRS="$(HDFDIR)" subd
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG=all \
          SUBDIRS="$(NETCDFDIR)" subdn

testnofortran:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=test-hdfnofortran \
          SUBDIRS="$(HDFDIR)" subd
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG=test \
          SUBDIRS="$(NETCDFDIR)" subdn

test-hdf:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="$(HDFDIR)" subd
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=test-utils \
          SUBDIRS="$(HDFDIR)" subd

test-hdf-libs:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="$(HDFDIR)" subd

test-hdf-utils:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=test-utils \
          SUBDIRS="$(HDFDIR)" subd

test-mfhdf:
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG=test \
          SUBDIRS="$(NETCDFDIR)" subd

hdf-test:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="$(HDFDIR)" subd

mfhdf-test:
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG=$@ \
          SUBDIRS="$(NETCDFDIR)" subd

hdf-allnofortran: 
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG="allnofortran" \
          SUBDIRS="$(HDFDIR)" subd

hdf-libnofortran: 
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG="libnofortran" \
          SUBDIRS="$(HDFDIR)" subd

test-hdfnofortran: 
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG="test-hdfnofortran" \
          SUBDIRS="$(HDFDIR)" subd

hdf-testnofortran:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG="testnofortran" \
          SUBDIRS="$(HDFDIR)" subd

hdf-libnostub: 
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG="libnostub" \
          SUBDIRS="$(HDFDIR)" subd

hdf-allnostub: 
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG="allnostub" \
          SUBDIRS="$(HDFDIR)" subd

hdf-utilities: 
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG="utilities" \
          SUBDIRS="$(HDFDIR)" subd

hdf-debug: 
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG="debug" \
          SUBDIRS="$(HDFDIR)" subd

libs: 
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="$(HDFDIR)" subd
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG="libmfhdf" \
          SUBDIRS="$(NETCDFDIR)" subdn

hdf-libs: 
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG="libs" \
          SUBDIRS="$(HDFDIR)" subd

libdf: 
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="$(HDFDIR)" subd

libjpeg: 
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="$(HDFDIR)" subd

libz: 
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="$(HDFDIR)" subd

libmfhdf:
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG=$@ \
          SUBDIRS="$(NETCDFDIR)" subdn

install:	
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="$(HDFDIR)" subd
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=install \
          SUBDIRS="$(MANDIR)" subd
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG=install \
          SUBDIRS="$(NETCDFDIR)" subd

install-libs:	
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="$(HDFDIR)" subd
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG=$@ \
          SUBDIRS="$(NETCDFDIR)" subd

install-includes:	
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="$(HDFDIR)" subd
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG=$@ \
          SUBDIRS="$(NETCDFDIR)" subd

install-utils:	
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="$(HDFDIR)" subd
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG=$@ \
          SUBDIRS="$(NETCDFDIR)" subd

install-hdf:	
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=install \
          SUBDIRS="$(HDFDIR)" subd

install-hdf-libs:	
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=install-libs \
          SUBDIRS="$(HDFDIR)" subd

install-hdf-utils:	
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=install-utils \
          SUBDIRS="$(HDFDIR)" subd

install-mfhdf:	
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG=install \
          SUBDIRS="$(NETCDFDIR)" subd

install-mfhdf-libs:	
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG=install-libs \
          SUBDIRS="$(NETCDFDIR)" subd

install-mfhdf-utils:	
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG=install-utils \
          SUBDIRS="$(NETCDFDIR)" subd

install-man:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=install \
          SUBDIRS="$(MANDIR)" subd
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG=$@ \
          SUBDIRS="$(NETCDFDIR)" subd

install-hdf-man:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=install \
          SUBDIRS="$(MANDIR)" subd

install-mfhdf-man:
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG=$@ \
          SUBDIRS="$(NETCDFDIR)" subd
hdfclean:		
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG="clean" \
          SUBDIRS="$(HDFDIR)" subd

mfhdfclean:		
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG="clean" \
          SUBDIRS="$(NETCDFDIR)" subd

clean:		
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="$(HDFDIR)" subd
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="$(MANDIR)" subd
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG=$@ \
          SUBDIRS="$(NETCDFDIR)" subdn

hdfdistclean:		
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG="distclean" \
          SUBDIRS="$(HDFDIR)" subd

mfhdfdistclean:		
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG="distclean" \
          SUBDIRS="$(NETCDFDIR)" subd

distclean:	
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="$(HDFDIR)" subd
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="$(MANDIR)" subd
	@$(MAKE) $(MFLAGS) $(NETCDF_FLAGS) TARG=$@ \
          SUBDIRS="$(NETCDFDIR)" subdn
	-$(RM) $(RMFLAGS) config.status Makefile

help:
	@echo ""
	@echo "Make supports the following targets"
	@echo "make help    - prints this usage section"
	@echo "make all     - (DEFAULT) makes both HDF and netCDF packages"
	@echo "make HDF     - makes HDF library,utilities, and tests"
	@echo "make NETCDF  - makes NETCDF package"
	@echo "make allnofortran - makes HDF package excluding fortran support"
	@echo "                         and entire NETCDF package"
	@echo "make hdf-allnofortran - makes HDF package excluding fortran support"
	@echo "make hdf-libnofortran - makes HDF library excluding fortran support"
	@echo "make hdf-testnofortran - makes HDF library and tests"
	@echo "                         excluding fortran support"
	@echo "make hdf-test          - makes HDF library and tests" 
	@echo "make hdf-utilities     - makes HDF library and utilities"
	@echo "make libs              - makes HDF, IJG JPEG and mfhdf libraries only"
	@echo "make hdf-libs          - makes HDF and IJG JPEG libraries only"
	@echo "make libdf             - makes HDF library"
	@echo "make libjpeg           - makes IJG JPEG library"
	@echo "make libz              - makes GZIP deflate library"
	@echo "make libmfhdf          - makes mfhdf library"
	@echo "make test              - run both HDF and NETCDF tests"
	@echo "make testnofortran     - run HDF tests except the fortran test" 
	@echo "                         and NETCDF tests"
	@echo "make test-mfhdf        - run NETCDF tests" 
	@echo "make test-hdf          - run HDF library and utilities tests" 
	@echo "make test-hdf-libs     - run HDF library tests" 
	@echo "make test-hdf-utils    - run HDF utilities test" 
	@echo "make test-hdfnofortran - run HDF tests except the fortran test" 
	@echo "make install           - install HDF,netCDF libraries,"
	@echo "                         utilites, and includes" 
	@echo "make install-libs      - install HDF,netCDF libraries and includes"
	@echo "make install-includes  - install HDF,netCDF includes"
	@echo "make install-utils     - install HDF,netCDF utilities"
	@echo "make install-man       - install HDF and netCDF man pages"
	@echo "make install-hdf       - install HDF library, utilities and includes"
	@echo "make install-hdf-libs  - install HDF library and includes"
	@echo "make install-hdf-utils - install utilities"
	@echo "make install-hdf-man   - install HDF man pages"
	@echo "make install-mfhdf     - install netCDF library, utilities and includes"
	@echo "make install-mfhdf-libs - install netCDF library and includes"
	@echo "make install-mfhdf-utils - install netCDF utilities"
	@echo "make install-mfhdf-man   - install netCDF man pages"
	@echo "make clean        - cleans up HDF and NETCDF packages(*.o,libdf.a,..)"
	@echo "make distclean    - cleans up HDF and NETCDF packages including"
	@echo "                    configuration(Makefiles,config.status,..)"
	@echo "make hdfclean     - cleans up HDF package(*.o,libdf.a,..et)"
	@echo "make hdfdistclean - cleans up HDF package including"
	@echo "                    configuration(Makefiles,config.status,..)"
	@echo "make mfhdfclean   - cleans up NETCDF package(*.o,libmfhdf.a,..et)"
	@echo "make mfhdfdistclean - cleans up NETCDF package including"
	@echo "                       configuration(Makefiles,config.status,..)"

# target for recursive directory make
subd:
	@for dir in $(SUBDIRS); do \
          (cd $$dir; echo Making \`$(TARG)\' in `pwd`; \
              $(MAKE) $(MFLAGS) $(HDF_FLAGS) $(TARG)); \
	done

subdn:
	@for dir in $(SUBDIRS); do \
          (cd $$dir; echo Making \`$(TARG)\' in `pwd`; \
              $(MAKE) $(MFLAGS) $(NETCDF_FLAGS) $(TARG)); \
	done

# For making distribution
distdir = $(TOP_SRCDIR)/$(PACKAGE)-$(VERSION)
dist: $(DISTFILES)
	rm -rf $(distdir)
	mkdir $(distdir)
	chmod 777 $(distdir)
	for subdir in $(OTHERDIRS); do \
	  mkdir $(distdir)/$$subdir || exit 1; \
	done
	@for file in $(DISTFILES); do \
	  ln $(srcdir)/$$file $(distdir)/$$file 2> /dev/null \
	    || { echo copying $$file instead; \
	    cp -p $(srcdir)/$$file $(distdir);}; \
	done
	for subdir in $(SUBDIRS); do \
	  echo making $@ in $$subdir; \
	  mkdir $(distdir)/$$subdir || exit 1; \
	  (cd $$subdir && $(MAKE) $@) || exit 1; \
	done
	chmod -R a+r $(distdir)
	tar chof $(distdir).tar $(distdir)
	compress $(distdir).tar
	rm -rf $(distdir)

ftp:	$(FTPDIR)/$(PACKAGE)-$(VERSION).tar.Z
	test ! -h $(FTPDIR)/$(PACKAGE).tar.Z && exit 0; \
	cd $(FTPDIR) || exit 1; \
	rm $(PACKAGE).tar.Z || exit 1; \
	ln -s $(PACKAGE)-$(VERSION).tar.Z $(PACKAGE).tar.Z;

$(FTPDIR)/$(PACKAGE)-$(VERSION).tar.Z:	$(PACKAGE)-$(VERSION).tar.Z
	cp $(PACKAGE)-$(VERSION).tar.Z $@ 
	chmod u+rw,g+rw,o=r $@

tar.Z:	$(PACKAGE)-$(VERSION).tar.Z

$(PACKAGE)-$(VERSION).tar.Z:	MANIFEST
	dir=`basename \`pwd\`` \
	&& id=$(PACKAGE)-$(VERSION) \
	&& cd .. \
	&& ln -s $$dir $$id \
	&& tar -chof - `sed "s|^|$$id/|" $$id/MANIFEST` \
	    | compress > $$id/$@ \
	&& rm $$id

MANIFEST:	FORCE_IT
	@echo 1>&2 Creating MANIFEST
	@$(MAKE) -s $(MFLAGS) $(LOCAL_MACROS) MANIFEST.echo > $@

MANIFEST.echo:	FORCE_IT
	@$(MAKE) -s $(MFLAGS) $(LOCAL_MACROS) \
	    MANIFEST=`echo $(MANIFEST) | sed 's/MANIFEST//'` \
	    ensure_manifest > /dev/null
	@echo $(MANIFEST) | fmt -1
	@if [ -n "$(SUBDIRS)" ]; then \
	    subdirs="$(SUBDIRS)"; \
	    for subdir in $$subdirs; do \
		(cd $$subdir && \
	        echo 1>&2 Creating $@ in `pwd` && \
		$(MAKE) $(MFLAGS) $(LOCAL_MACROS) MANIFEST.echo | \
		    sed "s|^|$$subdir/|") || exit 1; \
	    done; \
	else \
	    :; \
	fi

ensure_manifest:	$(MANIFEST)

# For the justification of the following Makefile rules, see node
# `Automatic Remaking' in GNU Autoconf documentation.
#
#Makefile: $(TOP_SRCDIR)/config.status Makefile.in
#	CONFIG_FILES=$@ CONFIG_HEADERS= $(TOP_SRCDIR)/config.status

#config.h: stamp-h

#stamp-h: $(TOP_SRCDIR)/config.status $(srcdir)/config.h.in
#	CONFIG_FILES= CONFIG_HEADERS=config.h $(TOP_SRCDIR)/config.status

#config.status: configure
#	$(TOP_SRCDIR)/config.status --recheck

#configure: configure.in
#	cd $(srcdir) && autoconf

#config.h.in: stamp-h.in

#stamp-h.in: configure.in
#	cd $(srcdir) && autoheader
# Use echo instead of date to avoid spurious conflicts for
# people who use CVS, since stamp-h.in is distributed.
#	echo > $(srcdir)/$@

# Tell versions [3.59,3.63) of GNU make not to export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:

# dummy target as a dependency for forcing a rule to be always executed
FORCE_IT:
