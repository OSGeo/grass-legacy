# Makefile for ppm tools.
#
# Copyright (C) 1989, 1991 by Jef Poskanzer.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice appear in all copies and that both that
# copyright notice and this permission notice appear in supporting
# documentation.  This software is provided "as is" without express or
# implied warranty.

# Default values, usually overridden by top-level Makefile.
CC =		cc
#CC =		gcc -fcombine-regs -fpcc-struct-return
#CFLAGS =	-O
CFLAGS =	-g
#CFLAGS =	-g -O
RGBDEF =	-DRGB_DB=\"/usr/lib/X11/rgb.txt\"
#LDFLAGS =	-s
LDFLAGS =	
INSTALLBINARIES =	/usr/new/pbm
INSTALLSCRIPTS =	$(INSTALLBINARIES)
INSTALLMANUALS1 =	/usr/man/mann
SUFFIXMANUALS1 =	1
INSTALLMANUALS3 =	/usr/man/mann
SUFFIXMANUALS3 =	3
INSTALLMANUALS5 =	/usr/man/mann
SUFFIXMANUALS5 =	5
MANCP =			cp

PGMDIR =	../pgm
INCLUDEPGM =	-I$(PGMDIR)
LIBPGM =	$(PGMDIR)/libpgm.a
DEFPGM =	$(PGMDIR)/pgm.h
DEFLIBPGM =	$(PGMDIR)/libpgm.h

PBMDIR =	../pbm
INCLUDEPBM =	-I$(PBMDIR)
LIBPBM =	$(PBMDIR)/libpbm.a
DEFPBM =	$(PBMDIR)/pbm.h ../pbmplus.h
DEFLIBPBM =	$(PBMDIR)/libpbm.h

SHELL =		/bin/sh
INCLUDE =	$(INCLUDEPGM) $(INCLUDEPBM)
ALLCFLAGS =	$(CFLAGS) $(RGBDEF) $(INCLUDE)
LIBPPM =	libppm.a

PORTBINARIES =	giftoppm gouldtoppm ilbmtoppm imgtoppm mtvtoppm \
		pcxtoppm pgmtoppm pi1toppm picttoppm \
		ppmhist ppmmake ppmquant \
		ppmrelief ppmtogif ppmtoicr ppmtoilbm \
		ppmtopcx ppmtopgm ppmtopi1 ppmtopict ppmtops \
		ppmtopuzz ppmtorgb3 ppmtouil ppmtoxpm \
		qrttoppm rawtoppm rgb3toppm spctoppm \
		sputoppm tgatoppm ximtoppm xpmtoppm
MATHBINARIES =	ppmpat
BINARIES =	$(PORTBINARIES) $(MATHBINARIES)
SCRIPTS =	ppmquantall

OBJECTS =	giftoppm.o gouldtoppm.o ilbmtoppm.o imgtoppm.o mtvtoppm.o \
		pcxtoppm.o pgmtoppm.o pi1toppm.o picttoppm.o \
		ppmhist.o ppmmake.o ppmquant.o \
		ppmrelief.o ppmtogif.o ppmtoicr.o ppmtoilbm.o \
		ppmtopcx.o ppmtopgm.o ppmtopi1.o ppmtopict.o ppmtops.o \
		ppmtopuzz.o ppmtorgb3.o ppmtouil.o ppmtoxpm.o \
		qrttoppm.o rawtoppm.o rgb3toppm.o spctoppm.o \
		sputoppm.o tgatoppm.o ximtoppm.o xpmtoppm.o \
		ppmpat.o

MANUALS1 =	$(BINARIES) $(SCRIPTS)
MANUALS3 =	libppm
MANUALS5 =	ppm


all:		binaries


binaries:	$(BINARIES)

installbinaries:	binaries
	cd $(INSTALLBINARIES) ; rm -f $(BINARIES)
	cp $(BINARIES) $(INSTALLBINARIES)
	cd $(INSTALLSCRIPTS) ; rm -f $(SCRIPTS)
	cp $(SCRIPTS) $(INSTALLSCRIPTS)
	cd $(INSTALLSCRIPTS) ; chmod +x $(SCRIPTS)


merge:		ppmmerge
ppmmerge:	ppmmerge.c $(OBJECTS) $(LIBPPM) $(LIBPGM) $(LIBPBM)
	$(CC) $(ALLCFLAGS) $(LDFLAGS) -o $@ $@.c $(OBJECTS) -lm $(LIBPPM) $(LIBPGM) $(LIBPBM)

installmerge:	installppmmerge
installppmmerge:	ppmmerge
	cd $(INSTALLBINARIES) ; rm -f $(BINARIES)
	cp ppmmerge $(INSTALLBINARIES)
	cd $(INSTALLBINARIES) ; for i in $(BINARIES) ; do ln ppmmerge $$i ; done
	rm $(INSTALLBINARIES)/ppmmerge
	cd $(INSTALLSCRIPTS) ; rm -f $(SCRIPTS)
	cp $(SCRIPTS) $(INSTALLSCRIPTS)
	cd $(INSTALLSCRIPTS) ; chmod +x $(SCRIPTS)


installmanuals:
	for i in $(MANUALS1) ; do $(MANCP) $$i.1 $(INSTALLMANUALS1)/$$i.$(SUFFIXMANUALS1) ; done
	for i in $(MANUALS3) ; do $(MANCP) $$i.3 $(INSTALLMANUALS3)/$$i.$(SUFFIXMANUALS3) ; done
	for i in $(MANUALS5) ; do $(MANCP) $$i.5 $(INSTALLMANUALS5)/$$i.$(SUFFIXMANUALS5) ; done


# Rule for plain programs.
$(PORTBINARIES):	ppm.h $(DEFPGM) $(DEFPBM) $(LIBPPM) $(LIBPGM) $(LIBPBM)
	$(CC) $(ALLCFLAGS) $(LDFLAGS) -o $@ $@.c $(LIBPPM) $(LIBPGM) $(LIBPBM)

# Rule for math-dependent programs.
$(MATHBINARIES):	ppm.h $(DEFPGM) $(DEFPBM) $(LIBPPM) $(LIBPGM) $(LIBPBM)
	$(CC) $(ALLCFLAGS) $(LDFLAGS) -o $@ $@.c -lm $(LIBPPM) $(LIBPGM) $(LIBPBM)

# Rule for objects.
$(OBJECTS):	ppm.h $(DEFPGM) $(DEFPBM)
	$(CC) $(ALLCFLAGS) "-Dmain=$*_main" -c $*.c

# And libraries.
$(LIBPBM):
	cd $(PBMDIR) ; make lib
$(LIBPGM):
	cd $(PGMDIR) ; make lib
lib:		$(LIBPPM)
$(LIBPPM):	libppm1.o libppm2.o libppm3.o libppm4.o libppm5.o
	-rm $(LIBPPM)
	ar rc $(LIBPPM) libppm1.o libppm2.o libppm3.o libppm4.o libppm5.o
	-ranlib $(LIBPPM)

libppm1.o:	ppm.h $(DEFPGM) $(DEFPBM) libppm.h libppm1.c
	$(CC) $(ALLCFLAGS) -c libppm1.c
libppm2.o:	ppm.h $(DEFPGM) $(DEFPBM) libppm.h libppm2.c $(DEFLIBPGM) \
		$(DEFLIBPBM)
	$(CC) $(ALLCFLAGS) -c libppm2.c
libppm3.o:	ppm.h $(DEFPGM) $(DEFPBM) libppm.h libppm3.c
	$(CC) $(ALLCFLAGS) -c libppm3.c
libppm4.o:	ppm.h $(DEFPGM) $(DEFPBM) ppmcmap.h libppm4.c
	$(CC) $(ALLCFLAGS) -c libppm4.c
libppm5.o:	ppm.h $(DEFPGM) $(DEFPBM) ppmdraw.h libppm5.c
	$(CC) $(ALLCFLAGS) -c libppm5.c

# Other dependencies.
giftoppm giftoppm.o:		giftoppm.c
gouldtoppm gouldtoppm.o:	gouldtoppm.c
ilbmtoppm ilbmtoppm.o:		ilbmtoppm.c ilbm.h
imgtoppm imgtoppm.o:		imgtoppm.c
mtvtoppm mtvtoppm.o:		mtvtoppm.c
pcxtoppm pcxtoppm.o:		pcxtoppm.c
pgmtoppm pgmtoppm.o:		pgmtoppm.c
pi1toppm pi1toppm.o:		pi1toppm.c
picttoppm picttoppm.o:		picttoppm.c
ppmhist ppmhist.o:		ppmhist.c ppmcmap.h
ppmmake ppmmake.o:		ppmmake.c
ppmpat ppmpat.o:		ppmpat.c ppmdraw.h
ppmquant ppmquant.o:		ppmquant.c $(PGMDIR)/dithers.h ppmcmap.h
ppmrelief ppmrelief.o:		ppmrelief.c
ppmtogif ppmtogif.o:		ppmtogif.c ppmcmap.h
ppmtoicr ppmtoicr.o:		ppmtoicr.c
ppmtoilbm ppmtoilbm.o:		ppmtoilbm.c ilbm.h ppmcmap.h
ppmtopcx ppmtopcx.o:		ppmtopcx.c
ppmtopgm ppmtopgm.o:		ppmtopgm.c
ppmtopi1 ppmtopi1.o:		ppmtopi1.c
ppmtopict ppmtopict.o:		ppmtopict.c ppmcmap.h
ppmtopuzz ppmtopuzz.o:		ppmtopuzz.c
ppmtops ppmtops.o:		ppmtops.c
ppmtorgb3 ppmtorgb3.o:		ppmtorgb3.c
ppmtouil ppmtouil.o:		ppmtouil.c
ppmtoxpm ppmtoxpm.o:		ppmtoxpm.c
qrttoppm qrttoppm.o:		qrttoppm.c
rawtoppm rawtoppm.o:		rawtoppm.c
rgb3toppm rgb3toppm.o:		rgb3toppm.c
spctoppm spctoppm.o:		spctoppm.c
sputoppm sputoppm.o:		sputoppm.c
tgatoppm tgatoppm.o:		tgatoppm.c tga.h
ximtoppm ximtoppm.o:		ximtoppm.c xim.h
xpmtoppm xpmtoppm.o:		xpmtoppm.c

clean:
	-rm -f *.o *.a *.cat core $(BINARIES) ppmmerge


# Imakefile stuff.  Ignore if you're not an X11 type.

              TOP = ../../../../../../usr/src/new/X11
        MACROFILE = 

  BOOTSTRAPCFLAGS =
        CONFIGSRC = $(TOP)/config
      CURRENT_DIR = ./pbmplus/ppm
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
