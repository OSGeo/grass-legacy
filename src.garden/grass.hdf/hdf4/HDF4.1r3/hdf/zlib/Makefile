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
# Makefile for zlib
# Copyright (C) 1995-1998 Jean-loup Gailly.
# For conditions of distribution and use, see copyright notice in zlib.h 

# To compile and test, type:
#   ./configure; make test
# The call of configure is optional if you don't have special requirements
# If you wish to build zlib as a shared library, use: ./configure -s

# To install /usr/local/lib/libz.* and /usr/local/include/zlib.h, type:
#    make install
# To install in $HOME instead of /usr/local, use:
#    make install prefix=$HOME

CC=cc

CFLAGS=-O
#CFLAGS=-O -DMAX_WBITS=14 -DMAX_MEM_LEVEL=7
#CFLAGS=-g -DDEBUG
#CFLAGS=-O3 -Wall -Wwrite-strings -Wpointer-arith -Wconversion \
#           -Wstrict-prototypes -Wmissing-prototypes

LDFLAGS=-L. -lz
LDSHARED=$(CC)
CPP=$(CC) -E

VER=1.1.3
LIBS=libz.a
SHAREDLIB=libz.so

AR=ar
ARFLAGS = rc
RANLIB=ranlib
TAR=tar
SHELL = /bin/sh

TOP_SRCDIR = ../..
#prefix = /usr/local
prefix = $(TOP_SRCDIR)/NewHDF
exec_prefix = $(prefix)
libdir = ${exec_prefix}/lib
includedir = ${prefix}/include

# installation program
INSTALL = ${TOP_SRCDIR}/install-sh -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL) -m 644

# ==> specify directories where to find inludes and library
#     HDF library, and HDF utilities
HDFINC= $(srcdir)/../src
HDFLIB= $(srcdir)/../src
HDFBIN= $(srcdir)/../bin

#### makefile fragment from 'config/' goes here ##############
# $Id: Makefile,v 1.1 1999-12-29 15:12:57 markus Exp $

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

## ------------------------ end of makefile fragments--------------------
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

OBJS = adler32.o compress.o crc32.o gzio.o uncompr.o deflate.o trees.o \
       zutil.o inflate.o infblock.o inftrees.o infcodes.o infutil.o inffast.o

OBJA =
# to use the asm code: make OBJA=match.o

TEST_OBJS = example.o minigzip.o

DISTFILES = README FAQ INDEX ChangeLog configure Make*[a-z0-9] *.[ch] *.mms \
  algorithm.txt zlib.3 msdos/Make*[a-z0-9] msdos/zlib.def msdos/zlib.rc \
  nt/Make*[a-z0-9] nt/zlib.dnt amiga/Make*.??? os2/M*.os2 os2/zlib.def \
  contrib/RE*.contrib contrib/*.txt contrib/asm386/*.asm contrib/asm386/*.c \
  contrib/asm386/*.bat contrib/asm386/zlibvc.d?? contrib/asm[56]86/*.?86 \
  contrib/asm[56]86/*.S contrib/iostream/*.cpp \
  contrib/iostream/*.h  contrib/iostream2/*.h contrib/iostream2/*.cpp \
  contrib/untgz/Makefile contrib/untgz/*.c contrib/untgz/*.w32 \
  contrib/minizip/[CM]*[pe] contrib/minizip/*.[ch] contrib/minizip/*.[td]?? \
  contrib/delphi*/*.???

all: example minigzip

test: all
	@LD_LIBRARY_PATH=.:$(LD_LIBRARY_PATH) ; export LD_LIBRARY_PATH; \
	echo hello world | ./minigzip | ./minigzip -d || \
	  echo '		*** minigzip test FAILED ***' ; \
	if ./example; then \
	  echo '		*** zlib test OK ***'; \
	else \
	  echo '		*** zlib test FAILED ***'; \
	fi

libz.a: $(OBJS) $(OBJA)
	$(AR) $(ARFLAGS) $@ $(OBJS) $(OBJA)
	-@ ($(RANLIB) $@ || true) >/dev/null 2>&1

match.o: match.S
	$(CPP) match.S > _match.s
	$(CC) -c _match.s
	mv _match.o match.o
	rm -f _match.s

$(SHAREDLIB).$(VER): $(OBJS)
	$(LDSHARED) -o $@ $(OBJS)
	rm -f $(SHAREDLIB) $(SHAREDLIB).1
	ln -s $@ $(SHAREDLIB)
	ln -s $@ $(SHAREDLIB).1

example: example.o $(LIBS)
	$(CC) $(CFLAGS) -o $@ example.o $(LDFLAGS)

minigzip: minigzip.o $(LIBS)
	$(CC) $(CFLAGS) -o $@ minigzip.o $(LDFLAGS)

install-lib install: $(LIBS)
	-@if [ ! -d $(includedir)  ]; then mkdir $(includedir); fi
	-@if [ ! -d $(libdir) ]; then mkdir $(libdir); fi
	cp zlib.h zconf.h $(includedir)
	chmod 644 $(includedir)/zlib.h $(includedir)/zconf.h
	cp $(LIBS) $(libdir)
	cd $(libdir); chmod 755 $(LIBS)
	-@(cd $(libdir); $(RANLIB) libz.a || true) >/dev/null 2>&1
	cd $(libdir); if test -f $(SHAREDLIB).$(VER); then \
	  rm -f $(SHAREDLIB) $(SHAREDLIB).1; \
	  ln -s $(SHAREDLIB).$(VER) $(SHAREDLIB); \
	  ln -s $(SHAREDLIB).$(VER) $(SHAREDLIB).1; \
	  (ldconfig || true)  >/dev/null 2>&1; \
	fi
# The ranlib in install is needed on NeXTSTEP which checks file times
# ldconfig is for Linux

uninstall:
	cd $(includedir); \
	v=$(VER); \
	if test -f zlib.h; then \
	  v=`sed -n '/VERSION "/s/.*"\(.*\)".*/\1/p' < zlib.h`; \
          rm -f zlib.h zconf.h; \
	fi; \
	cd $(libdir); rm -f libz.a; \
	if test -f $(SHAREDLIB).$$v; then \
	  rm -f $(SHAREDLIB).$$v $(SHAREDLIB) $(SHAREDLIB).1; \
	fi

clean:
	rm -f *.o *~ example minigzip libz.a libz.so* foo.gz so_locations \
	   _match.s maketree

distclean:	clean
	rm -f config.status Makefile

zip:
	mv Makefile Makefile~; cp -p Makefile.in Makefile
	rm -f test.c ztest*.c contrib/minizip/test.zip
	v=`sed -n -e 's/\.//g' -e '/VERSION "/s/.*"\(.*\)".*/\1/p' < zlib.h`;\
	zip -ul9 zlib$$v $(DISTFILES)
	mv Makefile~ Makefile

dist:
	mv Makefile Makefile~; cp -p Makefile.in Makefile
	rm -f test.c ztest*.c contrib/minizip/test.zip
	d=zlib-`sed -n '/VERSION "/s/.*"\(.*\)".*/\1/p' < zlib.h`;\
	rm -f $$d.tar.gz; \
	if test ! -d ../$$d; then rm -f ../$$d; ln -s `pwd` ../$$d; fi; \
	files=""; \
	for f in $(DISTFILES); do files="$$files $$d/$$f"; done; \
	cd ..; \
	GZIP=-9 $(TAR) chofz $$d/$$d.tar.gz $$files; \
	if test ! -d $$d; then rm -f $$d; fi
	mv Makefile~ Makefile

tags:	
	etags *.[ch]

depend:
	makedepend -- $(CFLAGS) -- *.[ch]

# DO NOT DELETE THIS LINE -- make depend depends on it.

adler32.o: zlib.h zconf.h
compress.o: zlib.h zconf.h
crc32.o: zlib.h zconf.h
deflate.o: deflate.h zutil.h zlib.h zconf.h
example.o: zlib.h zconf.h
gzio.o: zutil.h zlib.h zconf.h
infblock.o: infblock.h inftrees.h infcodes.h infutil.h zutil.h zlib.h zconf.h
infcodes.o: zutil.h zlib.h zconf.h
infcodes.o: inftrees.h infblock.h infcodes.h infutil.h inffast.h
inffast.o: zutil.h zlib.h zconf.h inftrees.h
inffast.o: infblock.h infcodes.h infutil.h inffast.h
inflate.o: zutil.h zlib.h zconf.h infblock.h
inftrees.o: zutil.h zlib.h zconf.h inftrees.h
infutil.o: zutil.h zlib.h zconf.h infblock.h inftrees.h infcodes.h infutil.h
minigzip.o:  zlib.h zconf.h 
trees.o: deflate.h zutil.h zlib.h zconf.h trees.h
uncompr.o: zlib.h zconf.h
zutil.o: zutil.h zlib.h zconf.h  
