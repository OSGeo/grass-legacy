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
# Makefile for netCDF library
#
# $Id: Makefile,v 1.1 1999-12-29 15:13:10 markus Exp $
# NOTE: This Makefile.in is used by the Cygnus 'configure' script to
#       generate the top level Makefile.

# ############# Start of system configuration section. #############

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

SHELL = /bin/sh

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
ARFLAGS = rc

# Name of remove utility, default 'RM=/bin/rm', 'RMFLAGS=-f'
RM = /bin/rm
RMFLAGS = -f

AS = as
CC = gcc
CFLAGS = -ansi
FC = f77
FFLAGS=

# Set for document generation
MAKEINFO = makeinfo
TEXI2DVI = texi2dvi

# Directory in which to install binaries
bindir = $(exec_prefix)/bin

# Directory in which to install header files.
includedir = $(prefex)/include

# Directory in which to install library files.
libdir = $(prefix)/lib

# Directory in which to install documentation info files.
infodir = $(prefix)/info

# ------------ General Macros for NETCDF --------------------
MFHDFSRC = $(TOP_SRCDIR)/mfhdf/libsrc
INCDIR	 = $(prefix)/include
HDF_LIB  = $(TOP_SRCDIR)/hdf/src/libdf.a
HDF_INC  = $(TOP_SRCDIR)/hdf/src/
PABLO_HINC = $(TOP_SRCDIR)/hdf/pablo
FMPOOL_INC = $(TOP_SRCDIR)/hdf/fmpool
JPEG_LIB = $(TOP_SRCDIR)/hdf/jpeg/libjpeg.a
ZLIB_LIB = $(TOP_SRCDIR)/hdf/zlib/libz.a
NETCDF_INC = $(TOP_SRCDIR)/mfhdf/libsrc
PORT_INC   = $(TOP_SRCDIR)/mfhdf/port
CPP	   = $(CC) -E
CFLAGS_NETCDF = 
CPPFLAGS_HDF  = -DNDEBUG -DHDF $(FMPOOL_FLAGS)
CPP_NETCDF    = -I. -I$(HDF_INC) -I$(NETCDF_INC) -I$(PORT_INC)
CPPFLAGS_LIB	= $(SWAP) $(NETLONG) 
CPPFLAGS_XDR	= $(CPP_XDR) 
CPPFLAGS_NETCDF =  $(CPPFLAGS_XDR) $(CPP_NETCDF) $(CPPFLAGS_LIB) $(CPPFLAGS_HDF) 
PABLO_INCDIR = .
PABLO_INCLUDE= -I$(PABLO_INCDIR)

FFLAGS  =
NCDUMP	= 
NCGEN	= 
NEQN	= neqn
OS	= 
TBL	= tbl
WHICH	= which
FTPDIR	= 
VERSION	= 2.3.2
LEX	= lex
YACC	= yacc
DIFF    = diff
DIFF_FLAGS = -w


#### ----- host and target specific makefile fragments come in here.-------
# $Id: Makefile,v 1.1 1999-12-29 15:13:10 markus Exp $

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

OTHERDIRS = config

PROGRAM		= cdftest
#SWAP    	= @SWAP@
#NETLONG    	= @NETLONG@
#CPPFLAGS	= $(CPPFLAGS_LIB)
#CFLAGS		= $(CFLAGS_LIB)
GARBAGE		= $(PROGRAM) test.cdf cdfout.new *.hdf \
                  hdftest hdfout.new
HEADERS		= netcdf.h mfhdf.h local_nc.h hdf2netcdf.h
#XDRFILE	= @XDRFILE@
#HDF_LIB         = @HDF_LIB@
#JPEG_LIB        = @JPEG_LIB@
CSRCS		= array.c attr.c cdf.c dim.c error.c file.c \
		  globdef.c iarray.c putget.c putgetg.c sharray.c string.c \
		  var.c xdrposix.c xdrstdio.c hdfsds.c mfsd.c nssdc.c
MANIFEST = $(DISTFILES)
DISTFILES	= Makefile.in README \
		  depend \
		  $(CSRCS) cdftest.c \
		  alloc.h testout.sav descrip.mms error.h htons.mar \
		  local_nc.h make.com msoft.mk netcdf.h ntohs.mar \
		  test_cdf.sav hdfout.sav hdftest.c mfhdf.h win32cdf.h hdf2netcdf.h \
		config/netcdf-hpux.h config/netcdf-irix4.h config/netcdf-irix5.h \
		config/netcdf-irix6.h config/netcdf-irix32.h config/netcdf-sun.h \
		config/netcdf-linux.h config/netcdf-fbsd.h config/netcdf-alpha.h \
		config/netcdf-dec.h config/netcdf-aix.h config/netcdf-solaris.h \
		config/netcdf-solarisx86.h config/netcdf-convex.h \
		config/netcdf-unicos.h

LIBOBJS		= array.o attr.o cdf.o dim.o file.o iarray.o error.o \
		  globdef.o putget.o putgetg.o sharray.o string.o var.o \
		  $(XDRFILE).o hdfsds.o mfsd.o nssdc.o
LIBRARY		= mfhdf-inst
DEBUGLIBRARY	= mfhdf_g-inst
PROFILEDLIBRARY	= mfhdf_pg-inst
OBJS		= $(PROGRAM).o
#LD_XDR		= @LD_XDR@
LD_NETCDF	= libmfhdf-inst.a
LIBS		= $(LD_NETCDF) $(LD_XDR) $(HDF_LIB) $(JPEG_LIB) $(ZLIB_LIB)
prefix = $(TOP_SRCDIR)/NewHDF

#all::		library
all::         FORCE
	@if test -z "$(PABLO_FLAGS)"; then \
	   : true; \
	else		 \
	  $(MAKE) $(MFLAGS) $(LOCAL_MACROS) all-inst; \
	fi

all-inst::         FORCE
	@if test -z "$(FMPOOL_FLAGS)"; then \
	  $(MAKE) $(MFLAGS) $(LOCAL_MACROS) library; \
	else		 \
	  $(MAKE) $(MFLAGS) $(LOCAL_MACROS) FMPOOL_FLAGS="$(FMPOOL_FLAGS) -I$(FMPOOL_INC)" library; \
	fi

#
# The formatted file `cdftest_out.new' might differ from the touchstone
# file `testout.sav', but this should be due to rounding errors.
#
# `$(PROGRAM)' creates the binary-file `test.cdf' which should be byte-for-byte
# identical with `test_cdf.sav', modulo small rounding errors.
#
test:		$(PROGRAM) nhdftest
	@ echo "========================="
	@ echo "netCDF formatted tests"
	@ echo "========================="
	./$(PROGRAM) > cdfout.new
	@cmd="$(DIFF) $(DIFF_FLAGS) cdfout.new testout.sav"; \
	    echo $$cmd; \
	    if $$cmd; then \
		echo "*** netCDF passes formatted test ***"; \
	    else \
		echo "*** netCDF fails formatted test ***"; \
		echo "The above differences are OK if small"; \
		exit 0; \
	    fi
#	@cmd="cmp test.cdf test_cdf.sav"; \
#	    echo $$cmd; \
#	    if $$cmd; then \
#		echo "*** netCDF passes binary test ***"; \
#	    else \
#		echo "*** netCDF fails binary test ***"; \
#		exit 1; \
#	    fi

#install::	installed_library installed_headers
install::	installed_library 

#install-includes::	installed_headers

$(PROGRAM):	$(PROGRAM).o library

nhdftest::         FORCE
	@if test -z "$(FMPOOL_FLAGS)"; then \
	  $(MAKE) $(MFLAGS) $(LOCAL_MACROS) hdftest; \
	else		 \
	  $(MAKE) $(MFLAGS) $(LOCAL_MACROS) FMPOOL_FLAGS="$(FMPOOL_FLAGS) -I$(FMPOOL_INC)" hdftest; \
	fi

hdftest:	hdftest.o library        
	$(CC) $(CFLAGS) $(CFLAGS_NETCDF) hdftest.o $(LIBS) -o $@
	@ echo "============================"
	@ echo "HDF-SD C interfaces tests"
	@ echo "============================"
	./hdftest > hdfout.new
	@cmd="$(DIFF) $(DIFF_FLAGS) hdfout.new hdfout.sav"; \
	    echo $$cmd; \
	    if $$cmd; then \
		echo "*** HDF passes formatted test ***"; \
	    else \
		echo "*** HDF fails formatted test ***"; \
		echo "The above differences are OK if small"; \
		exit 0; \
	    fi
debug:
#	rm -f $(COBJS)
	$(MAKE) $(MFLAGS) "CFLAGS=-g $(CFLAGS_NETCDF) " "LIBRARY=$(LIBRARY)_g" library

profiled:
#	rm -f $(COBJS)
	$(MAKE) $(MFLAGS) "CFLAGS=-pg $(CFLAGS_NETCDF) " "LIBRARY=$(LIBRARY)_pg" library

cdftest.oc:	$(CSRCS) cdftest.c
	#load -C $(CPPFLAGS) $(CPPFLAGS_NETCDF) -I/usr/local/lang/SC1.0/ansi_include \
	    $(CSRCS) cdftest.c /usr/local/lang/SC1.0/ansi_lib/libansi.a 

# This is the master.mk.in makefile.  It contains general rules.
#

.SUFFIXES:
.SUFFIXES:	.so .o .i .c .fc .f .F .l .y

# ###########################################################################
# General Macros: (moved to make file fragment )
# ###########################################################################
#CPP		= @CPP@
#NCDUMP		= @NCDUMP@
#NCGEN		= @NCGEN@
#NEQN		= @NEQN@
#OS		= @OS@
#TBL		= @TBL@
#WHICH		= @WHICH@

# ###########################################################################
# Recursive Macros (command-line changes to others aren't supported):
# ###########################################################################
SUBDIR_MACROS = CC="$(CC)" \
	CFLAGS="$(CFLAGS)" \
	CPP="$(CPP)" \
	exec_prefix="$(exec_prefix)" \
	FC="$(FC)" \
	prefix="$(prefix)"

LOCAL_MACROS  = CC="$(CC)" \
	CFLAGS="$(CFLAGS)" \
	CPP="$(CPP)" \
	CPPFLAGS="$(CPPFLAGS)" \
	exec_prefix="$(exec_prefix)" \
	FC="$(FC)" \
	prefix="$(prefix)"

# ###########################################################################
# Recursive make(1)s in Subdirectories:
# ###########################################################################

subdir_dummy_target $(SUBDIR_TARGETS):
	@echo ""
	@dir=`echo $@ | sed 's,/.*,,'`; \
	    target=`echo $@ | sed "s,$$dir/,,"`; \
	    cd $$dir && \
	    echo "making \`$$target' in directory `pwd`" && \
	    echo "" && \
	    $(MAKE) $(MFLAGS) $(SUBDIR_MACROS) \
		$$target || exit 1
	@echo ""
	@echo "returning to directory `pwd`"
	@echo ""


# ###########################################################################
# Compilation:
# ###########################################################################
#FORTC		= @FORTC@

# Compile FORTRAN code.
#
.f.o:
	$(FC) -c $(FFLAGS) $(MFHDFSRC)/$*.f
.F.o:
	ln -s $< $$$$.c \
	&& $(CPP) -DUD_FORTRAN_SOURCE $(CPPFLAGS) $(CPPFLAGS_NETCDF) $$$$.c \
	    | sed '/^# *[0-9]/d' > $*.f \
	|| (rm -f $*.f $$$$.c; exit 1) \
	&& rm $$$$.c
	$(FC) -c $(FFLAGS) $*.f
.F.f:
	ln -s $< $$$$.c \
	&& $(CPP) -DUD_FORTRAN_SOURCE $(CPPFLAGS) $(CPPFLAGS_NETCDF) $$$$.c \
	    | sed '/^# *[0-9]/d' > $@ \
	|| (rm -f $@ $$$$.c; exit 1) \
	&& rm $$$$.c

# Generate FORTRAN-callable C code.
#
.fc.o:
	$(FORTC) -O $(OS) $< > $*.c \
	    || ( rm -f $*.c; exit 1)
	$(CC) -c $(CFLAGS) $(CFLAGS_NETCDF) $(CPPFLAGS) $(CPPFLAGS_NETCDF) $*.c
.fc.c:
	$(FORTC) -O $(OS) $< > $@ \
	    || (rm -f $*.c; exit 1)

# Compile C code.
#
.c.o:
	$(CC) -c $(CPPFLAGS) $(CPPFLAGS_NETCDF) $(CFLAGS) $(CFLAGS_NETCDF) \
	$(PABLO_FLAGS) $(MFHDFSRC)/$*.c $(PABLO_INCLUDE)

# Compile lex(1) code.  This rule is here to ensure the use of CPPFLAGS.
# 
.l.o:
	rm -f $*.c
	$(LEX) -t $< > $*.c
	$(CC) -c $(CPPFLAGS) $(CPPFLAGS_NETCDF) $(CFLAGS) $(CFLAGS_NETCDF) $*.c
	rm $*.c

# C preprocess.  
#   NB: 
#	1. The output `file' is not created but appears on standard output;
#	2. Line numbering information is kept.
#
.c.i:
	$(CPP) $(CPPFLAGS) $(CPPFLAGS_NETCDF) $<


# ###########################################################################
# Libraries:
# ###########################################################################
# RANLIB		= @RANLIB@
LIBDIR		= $(prefix)/lib
LIB_A		= lib$(LIBRARY).a

library::		$(LIB_A)

# The following rule is forced because the library can be created by a
# makefile in another directory prior to the execution of the following rule.
#
$(LIB_A):		$(LIBOBJS) FORCE
	$(AR) rcuv $@ $(LIBOBJS)
	$(RANLIB) $@
#	$(LD) -b -o libmfhdf-inst.sl $(LIBOBJS)

# The following rule is for those makefiles needing to install object
# modules in a library in another directory.  They need only define 
# REMOTE_LIBRARY and LIBOBJS.  The rule is forced for the same reason as the
# $(LIB_A) rule.
#
lib_dummy $(REMOTE_LIBRARY):	$(LIBOBJS) FORCE
	$(AR) rcuv $@ $(LIBOBJS)
	$(RANLIB) $@

installed_libfiles:	FORCE
	@if [ -n "$(LIBFILES)" ]; then \
	    libfiles="$(LIBFILES)"; \
	    for libfile in $$libfiles; do \
		$(MAKE) $(MFLAGS) $(LOCAL_MACROS) \
		    LIBDIR=$(LIBDIR) LIBFILE=$$libfile \
		    installed_libfile || exit 1; \
	    done; \
	else \
	    :; \
	fi

installed_library::     FORCE
	@if test -z "$(PABLO_FLAGS)"; then \
	  : true; \
	else            \
	  $(MAKE) $(MFLAGS) $(LOCAL_MACROS) \
	   LIBDIR=$(LIBDIR) LIBFILE=$(LIB_A) \
	   installed_libfile ; \
	fi
 


# The following target decouples the previous targets from the actual rule;
# thus, eliminating extraneous make(1) messages.
#
installed_libfile:	$(LIBDIR)/$(LIBFILE)

$(LIBDIR)/$(LIBFILE):	$(LIBDIR) $(LIBFILE)
	$(INSTALL_DATA) $(LIBFILE) $@

dummy_libdir $(LIBDIR):
	mkdir -p $@
	chmod u+rwx,g+rws,o=rx $@

LIB_SO		= lib$(LIBRARY).so.$(MAJOR_NO).$(MINOR_NO)
LIB_SA		= lib$(LIBRARY).sa.$(MAJOR_NO).$(MINOR_NO)

#library::		FORCE
#	@so_objs=`echo $(LIBOBJS) | sed 's/\.o/.so/g'`; \
#	if [ -z "$$so_objs" ]; then \
#	    :; \
#	else \
#	    $(MAKE) $(MFLAGS) $(LIB_SO) \
#		SO_OBJS="$$so_objs"; \
#	fi
#	@if [ -z "$(SA_OBJS)" ]; then \
#	    :; \
#	else \
#	    $(MAKE) $(MFLAGS) $(LIB_SA); \
#	fi

$(LIB_SO):	$(SO_OBJS)
	ld -assert pure-text $(SO_OBJS) -o $@
$(LIB_SA):	$(SA_OBJS) FORCE
	$(AR) rcuv $@ $(SA_OBJS)

.fc.so:
	$(FORTC) -O $(OS) $< > $*.c \
	    || (rm -f $*.c; exit 1)
	$(CC) -c -pic $(CFLAGS) $(CFLAGS_NETCDF) $(CPPFLAGS) $(CPPFLAGS_NETCDF) $*.c -o $$$$.o \
	    || (rm -f $$$$.o; exit 1)
	mv $$$$.o $@
.c.so:
	$(CC) -c -pic $(CFLAGS) $(CFLAGS_NETCDF) $(CPPFLAGS) $(CPPFLAGS_NETCDF) $< -o $$$$.o \
	    || (rm -f $$$$.o; exit 1)
	mv $$$$.o $@

#installed_library::	$(LIBDIR)/$(LIB_SO)
#	@if [ -z "$(SA_OBJS)" ]; then \
#	    :; \
#	else \
#	    $(MAKE) $(MFLAGS) $(LIBDIR)/$(LIB_SA); \
#	fi

$(LIBDIR)/$(LIB_SO):	$(LIBDIR) $(LIB_SO)
	$(INSTALL_DATA) $(LIB_SO) $(LIBDIR)/$(LIB_SO)
$(LIBDIR)/$(LIB_SA):	$(LIBDIR) $(LIB_SA)
	$(INSTALL_DATA) $(LIB_SA) $(LIBDIR)/$(LIB_SA)


# ###########################################################################
# Programs:
# ###########################################################################
BINDIR		= $(exec_prefix)/bin

programs:	$(PROGRAMS)
program:
	@if [ -z "$(PROGRAM)" ]; then \
	    : true; \
	else \
	    if [ -z "$(LIBRARY)" ]; then \
		: true; \
	    else \
		$(MAKE) $(MFLAGS) $(LOCAL_MACROS) library; \
	    fi; \
	    $(MAKE) $(MFLAGS) $(LOCAL_MACROS) $(PROGRAM); \
	fi

dummy_prog $(PROGRAM):	$(OBJS)
	@if [ -r $@.f -o -r $@.F ]; then \
	    ld='$(FC) $(FFLAGS)'; \
	else \
	    ld='$(CC) $(CFLAGS) $(CFLAGS_NETCDF) '; \
	fi; \
	cmd="$$ld $(OBJS) $(LIBS) -o $@"; \
	echo $$cmd; \
	$$cmd \
	    || (rm -f $@; exit 1)

installed_programs:	FORCE
	@$(MAKE) $(MFLAGS) $(LOCAL_MACROS) installed_binfiles \
	    BINDIR=$(BINDIR) BINFILES="$(PROGRAMS)"

installed_binfiles:	FORCE
	@if [ -n "$(BINFILES)" ]; then \
	    binfiles="$(BINFILES)"; \
	    for binfile in $$binfiles; do \
		$(MAKE) $(MFLAGS) $(LOCAL_MACROS) \
		    BINDIR=$(BINDIR) BINFILE=$$binfile \
		    installed_binfile || exit 1; \
	    done; \
	else \
	    :; \
	fi

installed_program:	FORCE
	@$(MAKE) $(MFLAGS) $(LOCAL_MACROS) BINDIR=$(BINDIR) BINFILE=$(PROGRAM) \
	    installed_binfile

# The following target decouples the previous targets from the actual rule;
# thus, eliminating extraneous make(1) messages.
#
installed_binfile:	$(BINDIR)/$(BINFILE)

$(BINDIR)/$(BINFILE):	$(BINDIR) $(BINFILE)
	$(INSTALL_PROGRAM) $(BINFILE) $@

dummy_bindir $(BINDIR):
	mkdir -p $@
	chmod u+rwx,g+rws,o=rx $@


# ###########################################################################
# Header files:
# ###########################################################################
installed_headers:	FORCE
	@if [ -n "$(HEADERS)" ]; then \
	    headers="$(HEADERS)"; \
	    for header in $$headers; do \
		$(MAKE) $(MFLAGS) $(LOCAL_MACROS) INCDIR=$(INCDIR) HEADER=$$header \
		    installed_header || exit 1; \
	    done; \
	else \
	    :; \
	fi

# The following target decouples the previous targets from the actual rule;
# thus, eliminating extraneous make(1) messages.
#
installed_header:	$(INCDIR)/$(HEADER)

$(INCDIR)/$(HEADER):	$(INCDIR) $(HEADER)
	$(INSTALL_DATA) $(HEADER) $@

dummy_incdir $(INCDIR):
	mkdir -p $@
	chmod u+rwx,g+rws,o=rx $@


# ###########################################################################
# `etc/' files:
# ###########################################################################
ETCDIR	= $(prefix)/etc

installed_etcfiles:
	@if [ -n "$(ETCFILES)" ]; then \
	    etcfiles="$(ETCFILES)"; \
	    for etcfile in $$etcfiles; do \
		$(MAKE) $(MFLAGS) $(LOCAL_MACROS) ETCDIR=$(ETCDIR) ETCFILE=$$etcfile \
		    installed_etcfile || exit 1; \
	    done; \
	else \
	    :; \
	fi

# The following target decouples the previous targets from the actual rule;
# thus, eliminating extraneous make(1) messages.
#
installed_etcfile:	$(ETCDIR)/$(ETCFILE)

$(ETCDIR)/$(ETCFILE):	$(ETCDIR) $(ETCFILE)
	$(INSTALL_DATA) $(ETCFILE) $@

dummy_etcdir $(ETCDIR):
	mkdir -p $@
	chmod u+rwx,g+rws,o=rx $@


# ###########################################################################
# Manual Pages:
# ###########################################################################
MANDIR	= $(prefix)/man

installed_manuals:	FORCE
	@if [ -n "$(MANUALS)" ]; then \
	    manuals="$(MANUALS)"; \
	    for manual in $$manuals; do \
		mandir=$(MANDIR)/man`echo $$manual | sed 's/.*\.\(.\).*/\1/'`; \
		$(MAKE) $(MFLAGS) $(LOCAL_MACROS) MANDIR=$$mandir MANUAL=$$manual \
		    installed_manual || exit 1; \
	    done; \
	else \
	    :; \
	fi

# The following target decouples the previous targets from the actual rule;
# thus, eliminating extraneous make(1) messages.
#
installed_manual:	$(MANDIR)/$(MANUAL)

# The first sed(1) editing command in the following replaces the expanded
# RCS keyword `$Date ...' in the `.TH' macro line with something more 
# appropriate to a manual page.
#
$(MANDIR)/$(MANUAL):	$(MANDIR) $(MANUAL)
	sed \
	 -e '/^\.TH/s/$$D\ate: \([0-9]*\)\/\([0-9]*\)\/\([0-9]*\).*\$$/\1-\2-\3/'\
	 $(MANUAL) | $(TBL) | $(NEQN) > $@

dummy_mandir $(MANDIR):
	mkdir -p $@
	chmod u+rwx,g+rws,o=rx $@

# ###########################################################################
# TeX-Info Pages:
# ###########################################################################
INFODIR       = $(prefix)/info

installed_infos:      FORCE
	if [ -z "$(MAKEINFO)" ]; then \
		echo 1>&2 "Can't install info(1) pages because no makeinfo(1)"; \
	else \
	  if [ -n "$(INFOS)" ]; then \
	      infos="$(INFOS)"; \
	      for info in $$infos; do \
		$(MAKE) $(MFLAGS) $(LOCAL_MACROS) INFODIR=$(INFODIR) INFO=$$info \
		installed_info || exit 1; \
	      done; \
	   else \
	      :; \
	   fi; \
	fi;
# The following target decouples the previous targets from the actual rule;
# thus, eliminating extraneous make(1) messages.
#
installed_info:       $(INFODIR)/$(INFO)

$(INFODIR)/$(INFO):   $(INFODIR) $(INFO)
	$(INSTALL_DATA) $(INFO) $@

dummy_infodir $(INFODIR):
	mkdir -p $@
	chmod u+rwx,g+rws,o=rx $@

# Rules for generating info pages:
#
.SUFFIXES:    .info .texi .texinfo .txn
.texi.info:
	@if [ -z "$(MAKEINFO)" ]; then \
	  echo 1>&2 "Can't create $@ because no makeinfo(1)"; \
	else \
	  $(MAKEINFO) $< --no-split --output=$@; \
	fi
.texinfo.info:
	@if [ -z "$(MAKEINFO)" ]; then \
	  echo 1>&2 "Can't create $@ because no makeinfo(1)"; \
	else \
	  $(MAKEINFO) $< --no-split --output=$@; \
	fi
.txn.info:
	@if [ -z "$(MAKEINFO)" ]; then \
	  echo 1>&2 "Can't create $@ because no makeinfo(1)"; \
	else \
	  $(MAKEINFO) $< --no-split --output=$@; \
	fi

# Ensure that info page depends on canonical TeX-Info macros.
#
dummy_info $(INFO):   texinfo.tex

# ###########################################################################
# Cleanup:
# ###########################################################################
clean::
	rm -f $(GARBAGE) $(PROGRAM) a.out core *.o *.a *.so.* *.sa.* *.so \
	    *.log

realclean::	distclean
distclean::
	rm -f $(GARBAGE) $(PROGRAM) a.out core *.o *.a *.so.* *.sa.* *.so \
	    *.log Makefile config.status cc_id tags netcdf.h


# ###########################################################################
# Configuration:
# ###########################################################################

# The following target is touched because config.status won't update it if
# it hasn't changed.
#
# The following is commented-out because the HPUX make(1) goes into a loop.
#
#Makefile:	config.status
#	./config.status
#	touch $@

config.status:	cc_id configure
	./configure --no-create

cc_id:		FORCE
	if test -r $@ && test "`cat $@`" = "$(CC)"; then \
	    :; \
	else \
	    echo "$(CC)" > $@; \
	fi

configure:		configure.in
	autoconf


# ###########################################################################
# Dependency maintenance:
# ###########################################################################
.PRECIOUS:	depend
#
# Note: The following rule will only work if the C compiler behaves correctly
# when given the `-M' option.  AIX's cc(1) and SunOS's acc(1) behave
# incorrectly.  --Steve Emmerson 1992-09-24
#
# Furthermore, we do not attempt to maintain dependencies on installed
# header files (e.g. those in $(INCDIR)) for two reasons: 1) the dependency
# list generated at Unidata mightn't correspond to the situation at a client
# site; and 2) clients using broken compilers (c.f. previous note) can't
# regenerate the dependency list.
#
disabled_depend:		FORCE
	-incdir=`echo $(INCDIR) | sed s',/,\\\/,g'`; \
	cc -M $(CPPFLAGS) $(CPPFLAGS_NETCDF) *.c 2> /dev/null | \
	    awk '{ \
		if ($$0 ~ /:/) { \
		    target	= $$1; \
		    if (target ~ /:/) { \
			target	= substr(target,1,length(target)-1); \
			start	= 2; \
		    } else { \
			start	= 3; \
		    } \
		} else { \
		    start	= 1; \
		} \
		for (i = start; i <= NF; ++i) { \
		    if ($$i !~ /^\\/ && \
			$$i !~ /port\// && \
			$$i !~ /^\//) \
			print target ": " $$i \
		} \
	    }' > $@
	tag='### DO NOT DELETE THIS LINE.  make depend DEPENDS ON IT ###'; \
	    sed -e "/^$$tag$$/r depend" \
		-e "/^$$tag$$/q" Makefile > Makefile.new
	mv Makefile.new Makefile


# ###########################################################################
# Distributions:
# ###########################################################################
#FTPDIR		= @FTPDIR@
#VERSION		= @VERSION@

ftp:		$(FTPDIR)/$(PACKAGE)-$(VERSION).tar.Z
	test ! -h $(FTPDIR)/$(PACKAGE).tar.Z && exit 0; \
	cd $(FTPDIR) || exit 1; \
	rm $(PACKAGE).tar.Z || exit 1; \
	ln -s $(PACKAGE)-$(VERSION).tar.Z $(PACKAGE).tar.Z;

$(FTPDIR)/$(PACKAGE)-$(VERSION).tar.Z:	$(PACKAGE)-$(VERSION).tar.Z
	cp $(PACKAGE)-$(VERSION).tar.Z $@ 
	chmod u+rw,g+rw,o=r $@

tar.Z:		$(PACKAGE)-$(VERSION).tar.Z

$(PACKAGE)-$(VERSION).tar.Z:	MANIFEST
	dir=`basename \`pwd\`` \
	&& id=$(PACKAGE)-$(VERSION) \
	&& cd .. \
	&& ln -s $$dir $$id \
	&& tar -chof - `sed "s|^|$$id/|" $$id/MANIFEST` \
	    | compress > $$id/$@ \
	&& rm $$id

MANIFEST:	FORCE
	@echo 1>&2 Creating MANIFEST
	@$(MAKE) -s $(MFLAGS) $(LOCAL_MACROS) MANIFEST.echo > $@

MANIFEST.echo:	FORCE
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

# for distribution creation
distdir = $(TOP_SRCDIR)/$(PACKAGE)-$(VERSION)/$(curdir)
curdir = mfhdf/libsrc
dist: $(DISTFILES)
	for subdir in $(OTHERDIRS); do \
	  mkdir $(distdir)/$$subdir || exit 1; \
	done
	@for file in $(DISTFILES); do \
	  ln $(srcdir)/$$file $(distdir)/$$file 2> /dev/null \
	    || { echo copying $$file instead; \
	    cp -p $(srcdir)/$$file $(distdir)/$$file;}; \
	done

# ###########################################################################
# Miscellaneous:
# ###########################################################################

# The following dummy target is useful as a dependency to ensure that a
# rule is always executed.
#
FORCE:


### Everything after the following line might be overwritten ###
### DO NOT DELETE THIS LINE.  make depend DEPENDS ON IT ###
include depend
