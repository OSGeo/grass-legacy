# Makefile for pbmplus tools.
#
# Copyright (C) 1989, 1991 by Jef Poskanzer.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice appear in all copies and that both that
# copyright notice and this permission notice appear in supporting
# documentation.  This software is provided "as is" without express or
# implied warranty.

# CONFIGURE: define which of the four sub-packages you want to make.  PBM
# is required; PNM is recommended; and if you want PPM you must have PGM
# too.  This means there are only six possibilities:
SUBDIRS =	pbm pgm ppm pnm
#SUBDIRS =	pbm pgm ppm
#SUBDIRS =	pbm pgm pnm
#SUBDIRS =	pbm pgm
#SUBDIRS =	pbm pnm
#SUBDIRS =	pbm
# Note that when you change this, you have to make the equivalent change
# in pnm/Makefile.

# CONFIGURE: gcc makes things go about 20% faster on some machines, but not
# everyone has it.  Warning: do not use gcc's -finline-functions or
# -fstrength-reduce flags, they can produce incorrect code.  (This is with
# gcc versions 1.35, 1.36, and 1.37, later versions may fix these bugs.)
# Also, on some systems gcc can't compile pnmconvol.
#CC =		cc
#CC =		gcc
CC =		gcc -fcombine-regs -fpcc-struct-return

# CONFIGURE: cc flags go here.
CFLAGS =	-O
#CFLAGS =	-g
#CFLAGS =	-g -O

# CONFIGURE: ld flags go here.  Eunice users may want to use -noshare so that
# the binaries can run standalone.
LDFLAGS =	-s
#LDFLAGS =	
#LDFLAGS =	-noshare

# CONFIGURE: If you have an X11-style rgb color names file, define its
# path here.  This is used by PPM to parse color names into rgb values.
# If you don't have such a file, comment this out and use the alternative
# hex and decimal forms to specify colors (see ppm/pgmtoppm.1 for details).
RGBDEF =	-DRGB_DB=\"/usr/lib/X11/rgb.txt\"

# CONFIGURE: PBMPLUS's support for TIFF files depends on the library from
# Sam Leffler's TIFF Software package -- see the OTHER.SYSTEMS file for a
# full description and access information.  To configure PBMPLUS to use the
# library: first, if necessary, fetch the TIFF Software, unpack it in a
# scratch directory somewhere, and move the libtiff subdirectory right here
# into the PBMPLUS top-level directory.  Configure and "make" in the
# libtiff directory.  Then, uncomment the following five definitions.
# (You may need to tweak the TIFFDEF definition, adding -DBSDTYPES or
# the like.  Look at the libtiff Makefiles if you have problems.)
#
# Also, depending on what version of libtiff you get, you may need the
# patch in LIBTIFF.PATCH.  If you get link errors about TIFFFdOpen,
# apply the patch.
#
# Libtiff is pretty good about portability, but there are some machines
# it has problems on.  If you run into problems, you may wish to contact
# Sam directly, at the address listed in the OTHER.SYSTEMS file.
TIFFDEF =	-DLIBTIFF
TIFFINC =	-I../libtiff
TIFFLIB =	../libtiff/libtiff.a
TIFFBINARIES =	tifftopnm pnmtotiff
TIFFOBJECTS =	tifftopnm.o pnmtotiff.o

# CONFIGURE: Define the directory that you want the binaries copied to.
# If you need scripts and binaries to be in different directories, you
# can set that up too.
INSTALLBINARIES =	/usr/new/pbm
INSTALLSCRIPTS =	$(INSTALLBINARIES)

# CONFIGURE: Define the directories that you want the manual sources copied to,
# plus the suffix you want them to have.
#INSTALLMANUALS1 =	/usr/man/mann
#SUFFIXMANUALS1 =	n
#INSTALLMANUALS3 =	/usr/man/mann
#SUFFIXMANUALS3 =	n
#INSTALLMANUALS5 =	/usr/man/mann
#SUFFIXMANUALS5 =	n
INSTALLMANUALS1 =	/usr/helios/u0/jef/src/pbmbook/pbmman/man1
SUFFIXMANUALS1 =	1
INSTALLMANUALS3 =	/usr/helios/u0/jef/src/pbmbook/pbmman/man3
SUFFIXMANUALS3 =	3
INSTALLMANUALS5 =	/usr/helios/u0/jef/src/pbmbook/pbmman/man5
SUFFIXMANUALS5 =	5

# CONFIGURE: Normally the man pages are installed using "cp".  By changing
# this define you can use something else, for example a script that calls
# compress or pack.
MANCP =			cp

# CONFIGURE: Normally the Makefiles build and install separate binaries for
# each program.  However, on some systems (especially those without shared
# libraries) this can mean a lot of space.  In this case you might try
# building a "merge" instead.  The idea here is to link all the binaries
# together into one huge executable, with a tiny dispatch program as the
# main.  Then the merged binary is installed with file-system links for
# each program it includes.  The dispatch routine can tell which program
# to run by looking at argv[0].  On a Sun3 under SunOS 3.5 the space for
# executables went from 2.9 meg to .36 meg.
#all:		binaries
#install:	installbinaries installmanuals
all:		merge
install:	installmerge installmanuals

# End of configurable definitions.

SHELL =		/bin/sh
MAKE =		make

binaries:
	for i in $(SUBDIRS) ; do \
	    ( cd $$i ; $(MAKE) $(MFLAGS) 'CC=$(CC)' 'CFLAGS=$(CFLAGS)' 'RGBDEF=$(RGBDEF)' 'TIFFDEF=$(TIFFDEF)' 'TIFFINC=$(TIFFINC)' 'TIFFLIB=$(TIFFLIB)' 'TIFFBINARIES=$(TIFFBINARIES)' 'TIFFOBJECTS=$(TIFFOBJECTS)' 'LDFLAGS=$(LDFLAGS)' binaries ); \
	done

merge:
	for i in $(SUBDIRS) ; do \
	    ( cd $$i ; $(MAKE) $(MFLAGS) 'CC=$(CC)' 'CFLAGS=$(CFLAGS)' 'RGBDEF=$(RGBDEF)' 'TIFFDEF=$(TIFFDEF)' 'TIFFINC=$(TIFFINC)' 'TIFFLIB=$(TIFFLIB)' 'TIFFBINARIES=$(TIFFBINARIES)' 'TIFFOBJECTS=$(TIFFOBJECTS)' 'LDFLAGS=$(LDFLAGS)' merge ); \
	done

installbinaries:
	for i in $(SUBDIRS) ; do \
	    ( cd $$i ; $(MAKE) $(MFLAGS) 'CC=$(CC)' 'CFLAGS=$(CFLAGS)' 'RGBDEF=$(RGBDEF)' 'TIFFDEF=$(TIFFDEF)' 'TIFFINC=$(TIFFINC)' 'TIFFLIB=$(TIFFLIB)' 'TIFFBINARIES=$(TIFFBINARIES)' 'TIFFOBJECTS=$(TIFFOBJECTS)' 'LDFLAGS=$(LDFLAGS)' 'INSTALLBINARIES=$(INSTALLBINARIES)' 'INSTALLSCRIPTS=$(INSTALLSCRIPTS)' installbinaries ); \
	done

installmerge:
	for i in $(SUBDIRS) ; do \
	    ( cd $$i ; $(MAKE) $(MFLAGS) 'CC=$(CC)' 'CFLAGS=$(CFLAGS)' 'RGBDEF=$(RGBDEF)' 'TIFFDEF=$(TIFFDEF)' 'TIFFINC=$(TIFFINC)' 'TIFFLIB=$(TIFFLIB)' 'TIFFBINARIES=$(TIFFBINARIES)' 'TIFFOBJECTS=$(TIFFOBJECTS)' 'LDFLAGS=$(LDFLAGS)' 'INSTALLBINARIES=$(INSTALLBINARIES)' 'INSTALLSCRIPTS=$(INSTALLSCRIPTS)' installmerge ); \
	done

installmanuals:
	for i in $(SUBDIRS) ; do \
	    ( cd $$i ; $(MAKE) $(MFLAGS) 'TIFFBINARIES=$(TIFFBINARIES)' 'INSTALLMANUALS1=$(INSTALLMANUALS1)' 'SUFFIXMANUALS1=$(SUFFIXMANUALS1)' 'INSTALLMANUALS3=$(INSTALLMANUALS3)' 'SUFFIXMANUALS3=$(SUFFIXMANUALS3)' 'INSTALLMANUALS5=$(INSTALLMANUALS5)' 'SUFFIXMANUALS5=$(SUFFIXMANUALS5)' 'MANCP=$(MANCP)' installmanuals ); \
	done

clean:
	-rm -f *.shar *.shar?
	for i in $(SUBDIRS) ; do \
	    ( cd $$i ; $(MAKE) $(MFLAGS) clean ); \
	done

# Imakefile stuff.  Ignore if you're not an X11 type.

              TOP = ../../../../../usr/src/new/X11
        MACROFILE = 

  BOOTSTRAPCFLAGS =
        CONFIGSRC = $(TOP)/config
      CURRENT_DIR = ./pbmplus
          UTILSRC = $(TOP)/util
         IMAKESRC = $(CONFIGSRC)
         IRULESRC = $(UTILSRC)/imake.includes
            IMAKE = $(IMAKESRC)/imake
    IMAKE_DEFINES =
        IMAKE_CMD = $(NEWTOP)$(IMAKE) -I$(NEWTOP)$(IRULESRC) $(IMAKE_DEFINES)
     ICONFIGFILES = $(IRULESRC)/Imake.tmpl $(IRULESRC)/Imake.rules \
		        $(IRULESRC)/Project.tmpl $(IRULESRC)/site.def \
		        $(IRULESRC)/$(MACROFILE) $(EXTRA_ICONFIGFILES)
               MV = mv
               RM = rm -f

Makefile:: $(IMAKE)

$(IMAKE):
	@(cd $(IMAKESRC); if [ -f Makefile ]; then \
	echo "checking $@ in $(IMAKESRC) first..."; $(MAKE) all; else \
	echo "bootstrapping $@ from Makefile.ini in $(IMAKESRC) first..."; \
	$(MAKE) -f Makefile.ini BOOTSTRAPCFLAGS=$(BOOTSTRAPCFLAGS); fi; \
	echo "okay, continuing in $(CURRENT_DIR)")

Makefile::
	-@if [ -f Makefile ]; then \
	echo "  $(RM) Makefile.bak; $(MV) Makefile Makefile.bak"; \
	$(RM) Makefile.bak; $(MV) Makefile Makefile.bak; \
	else exit 0; fi
	$(IMAKE_CMD) -DTOPDIR=$(TOP) -DCURDIR=$(CURRENT_DIR)

Makefiles::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
	echo "making Makefiles in $(CURRENT_DIR)/$$i..."; \
	newtop=../ sub=sub;; \
	case "$(TOP)" in \
	/?*) newtop= upprefix= ;; \
	*) upprefix=../ ;; \
	esac; \
	$(MAKE) $${sub}dirMakefiles UPPREFIX=$$upprefix NEWTOP=$$newtop \
	MAKEFILE_SUBDIR=$$i NEW_CURRENT_DIR=$(CURRENT_DIR)/$$i; \
	done

subdirMakefiles:
	$(RM) $(MAKEFILE_SUBDIR)/Makefile.bak
	-@if [ -f $(MAKEFILE_SUBDIR)/Makefile ]; then \
	echo "  $(MV) $(MAKEFILE_SUBDIR)/Makefile $(MAKEFILE_SUBDIR)/Makefile.bak"; \
	$(MV) $(MAKEFILE_SUBDIR)/Makefile $(MAKEFILE_SUBDIR)/Makefile.bak; \
	else exit 0; fi
	cd $(MAKEFILE_SUBDIR); $(IMAKE_CMD) -DTOPDIR=$(UPPREFIX)$(TOP) -DCURDIR=$(NEW_CURRENT_DIR); \
	$(MAKE) $(MFLAGS) Makefiles
