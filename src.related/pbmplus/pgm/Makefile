# Makefile for pgm tools.
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
#LDFLAGS =	-s
LDFLAGS =	
INSTALLBINARIES =	/usr/new/pbm
INSTALLMANUALS1 =	/usr/man/man1
SUFFIXMANUALS1 =	1
INSTALLMANUALS3 =	/usr/man/man3
SUFFIXMANUALS3 =	3
INSTALLMANUALS5 =	/usr/man/man5
SUFFIXMANUALS5 =	5
MANCP =			cp

PBMDIR =	../pbm
INCLUDEPBM =	-I$(PBMDIR)
LIBPBM =	$(PBMDIR)/libpbm.a
DEFPBM =	$(PBMDIR)/pbm.h ../pbmplus.h
DEFLIBPBM =	$(PBMDIR)/libpbm.h

SHELL =		/bin/sh
INCLUDE =	$(INCLUDEPBM)
ALLCFLAGS =	$(CFLAGS) $(INCLUDE)
LIBPGM =	libpgm.a

PORTBINARIES =	fitstopgm fstopgm hipstopgm lispmtopgm pgmbentley \
		pgmenhance pgmhist pgmnorm pgmoil pgmramp \
		pgmtopbm pgmtofits pgmtofs pgmtolispm pgmtops \
		psidtopgm rawtopgm
MATHBINARIES =	pgmedge
BINARIES =	$(PORTBINARIES) $(MATHBINARIES)

OBJECTS =	fitstopgm.o fstopgm.o hipstopgm.o lispmtopgm.o pgmbentley.o \
		pgmenhance.o pgmhist.o pgmnorm.o pgmoil.o pgmramp.o \
		pgmtopbm.o pgmtofits.o pgmtofs.o pgmtolispm.o pgmtops.o \
		psidtopgm.o rawtopgm.o pgmedge.o

MANUALS1 =	$(BINARIES)
MANUALS3 =	libpgm
MANUALS5 =	pgm


all:		binaries


binaries:	$(BINARIES)

installbinaries:	binaries
	cd $(INSTALLBINARIES) ; rm -f $(BINARIES)
	cp $(BINARIES) $(INSTALLBINARIES)


merge:		pgmmerge
pgmmerge:	pgmmerge.c $(OBJECTS) $(LIBPGM) $(LIBPBM)
	$(CC) $(ALLCFLAGS) $(LDFLAGS) -o $@ $@.c $(OBJECTS) -lm $(LIBPGM) $(LIBPBM)

installmerge:	installpgmmerge
installpgmmerge:	pgmmerge
	cd $(INSTALLBINARIES) ; rm -f $(BINARIES)
	cp pgmmerge $(INSTALLBINARIES)
	cd $(INSTALLBINARIES) ; for i in $(BINARIES) ; do ln pgmmerge $$i ; done
	rm $(INSTALLBINARIES)/pgmmerge


installmanuals:
	for i in $(MANUALS1) ; do $(MANCP) $$i.1 $(INSTALLMANUALS1)/$$i.$(SUFFIXMANUALS1) ; done
	for i in $(MANUALS3) ; do $(MANCP) $$i.3 $(INSTALLMANUALS3)/$$i.$(SUFFIXMANUALS3) ; done
	for i in $(MANUALS5) ; do $(MANCP) $$i.5 $(INSTALLMANUALS5)/$$i.$(SUFFIXMANUALS5) ; done


# Rule for plain programs.
$(PORTBINARIES):	pgm.h $(DEFPBM) $(LIBPGM) $(LIBPBM)
	$(CC) $(ALLCFLAGS) $(LDFLAGS) -o $@ $@.c $(LIBPGM) $(LIBPBM)

# Rule for math-dependent programs.
$(MATHBINARIES):	pgm.h $(DEFPBM) $(LIBPGM) $(LIBPBM)
	$(CC) $(ALLCFLAGS) $(LDFLAGS) -o $@ $@.c -lm $(LIBPGM) $(LIBPBM)

# Rule for objects.
$(OBJECTS):	pgm.h $(DEFPBM)
	$(CC) $(ALLCFLAGS) "-Dmain=$*_main" -c $*.c

# And libraries.
$(LIBPBM):
	cd $(PBMDIR) ; make lib
lib:		$(LIBPGM)
$(LIBPGM):	libpgm1.o libpgm2.o
	-rm $(LIBPGM)
	ar rc $(LIBPGM) libpgm1.o libpgm2.o
	-ranlib $(LIBPGM)

libpgm1.o:	pgm.h $(DEFPBM) libpgm.h libpgm1.c
	$(CC) $(ALLCFLAGS) -c libpgm1.c
libpgm2.o:	pgm.h $(DEFPBM) libpgm.h libpgm2.c $(DEFLIBPBM)
	$(CC) $(ALLCFLAGS) -c libpgm2.c

# Other dependencies.
fitstopgm fitstopgm.o:		fitstopgm.c
fstopgm fstopgm.o:		fstopgm.c
hipstopgm hipstopgm.o:		hipstopgm.c
lispmtopgm lispmtopgm.o:	lispmtopgm.c
pgmbentley pgmbentley.o:	pgmbentley.c
pgmedge pgmedge.o:		pgmedge.c
pgmenhance pgmenhance.o:	pgmenhance.c
pgmhist pgmhist.o:		pgmhist.c
pgmnorm pgmnorm.o:		pgmnorm.c
pgmoil pgmoil.o:		pgmoil.c
pgmramp pgmramp.o:		pgmramp.c
pgmtopbm pgmtopbm.o:		pgmtopbm.c dithers.h $(DEFPBM)
pgmtofits pgmtofits.o:		pgmtofits.c
pgmtofs pgmtofs.o:		pgmtofs.c
pgmtolispm pgmtolispm.o:	pgmtolispm.c
pgmtops pgmtops.o:		pgmtops.c
psidtopgm psidtopgm.o:		psidtopgm.c
rawtopgm rawtopgm.o:		rawtopgm.c

clean:
	-rm -f *.o *.a *.cat core $(BINARIES) pgmmerge


# Imakefile stuff.  Ignore if you're not an X11 type.

              TOP = ../../../../../../usr/src/new/X11
        MACROFILE = 

  BOOTSTRAPCFLAGS =
        CONFIGSRC = $(TOP)/config
      CURRENT_DIR = ./pbmplus/pgm
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
