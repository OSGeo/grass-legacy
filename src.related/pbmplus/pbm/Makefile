# Makefile for pbm tools.
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

SHELL =		/bin/sh
ALLCFLAGS =	$(CFLAGS)
LIBPBM =	libpbm.a

BINARIES =	brushtopbm cmuwmtopbm g3topbm icontopbm gemtopbm \
		macptopbm mgrtopbm pbmlife pbmmake pbmmask \
		pbmpaste pbmreduce pbmtext pbmto10x pbmtoascii \
		pbmtobbnbg pbmtocmuwm pbmtoepson pbmtog3 \
		pbmtogem pbmtogo pbmtoicon pbmtolj pbmtomacp \
		pbmtomgr pbmtopi3 pbmtoplot pbmtoptx \
		pbmtox10bm pbmtoxbm pbmtoybm pbmtozinc \
		pbmupc pi3topbm xbmtopbm \
		ybmtopbm

OBJECTS =	brushtopbm.o cmuwmtopbm.o g3topbm.o icontopbm.o gemtopbm.o \
		macptopbm.o mgrtopbm.o pbmlife.o pbmmake.o pbmmask.o \
		pbmpaste.o pbmreduce.o pbmtext.o pbmto10x.o pbmtoascii.o \
		pbmtobbnbg.o pbmtocmuwm.o pbmtoepson.o pbmtog3.o \
		pbmtogem.o pbmtogo.o pbmtoicon.o pbmtolj.o pbmtomacp.o \
		pbmtomgr.o pbmtopi3.o pbmtoplot.o pbmtoptx.o \
		pbmtox10bm.o pbmtoxbm.o pbmtoybm.o pbmtozinc.o \
		pbmupc.o pi3topbm.o xbmtopbm.o \
		ybmtopbm.o

MANUALS1 =	$(BINARIES)
MANUALS3 =	libpbm
MANUALS5 =	pbm


all:		binaries


binaries:	$(BINARIES)

installbinaries:	binaries
	cd $(INSTALLBINARIES) ; rm -f $(BINARIES)
	cp $(BINARIES) $(INSTALLBINARIES)


merge:		pbmmerge
pbmmerge:	pbmmerge.c $(OBJECTS) $(LIBPBM)
	$(CC) $(ALLCFLAGS) $(LDFLAGS) -o $@ $@.c $(OBJECTS) $(LIBPBM)

installmerge:	installpbmmerge
installpbmmerge:	pbmmerge
	cd $(INSTALLBINARIES) ; rm -f $(BINARIES)
	cp pbmmerge $(INSTALLBINARIES)
	cd $(INSTALLBINARIES) ; for i in $(BINARIES) ; do ln pbmmerge $$i ; done
	rm $(INSTALLBINARIES)/pbmmerge


installmanuals:
	for i in $(MANUALS1) ; do $(MANCP) $$i.1 $(INSTALLMANUALS1)/$$i.$(SUFFIXMANUALS1) ; done
	for i in $(MANUALS3) ; do $(MANCP) $$i.3 $(INSTALLMANUALS3)/$$i.$(SUFFIXMANUALS3) ; done
	for i in $(MANUALS5) ; do $(MANCP) $$i.5 $(INSTALLMANUALS5)/$$i.$(SUFFIXMANUALS5) ; done


# Rules for plain programs.
$(BINARIES):	pbm.h ../pbmplus.h $(LIBPBM)
	$(CC) $(ALLCFLAGS) $(LDFLAGS) -o $@ $@.c $(LIBPBM)
$(OBJECTS):	pbm.h ../pbmplus.h
	$(CC) $(ALLCFLAGS) "-Dmain=$*_main" -c $*.c

# And library.
lib:		$(LIBPBM)
$(LIBPBM):	libpbm1.o libpbm2.o libpbm3.o libpbm4.o libpbm5.o
	-rm $(LIBPBM)
	ar rc $(LIBPBM) libpbm1.o libpbm2.o libpbm3.o libpbm4.o libpbm5.o
	-ranlib $(LIBPBM)

libpbm1.o:	pbm.h ../pbmplus.h libpbm.h libpbm1.c
	$(CC) $(ALLCFLAGS) -c libpbm1.c
libpbm2.o:	pbm.h ../pbmplus.h libpbm.h libpbm2.c
	$(CC) $(ALLCFLAGS) -c libpbm2.c
libpbm3.o:	pbm.h ../pbmplus.h libpbm.h libpbm3.c
	$(CC) $(ALLCFLAGS) -c libpbm3.c
libpbm4.o:	pbm.h ../pbmplus.h libpbm.h libpbm4.c
	$(CC) $(ALLCFLAGS) -c libpbm4.c
libpbm5.o:	pbm.h ../pbmplus.h pbmfont.h libpbm5.c
	$(CC) $(ALLCFLAGS) -c libpbm5.c

# Other dependencies.
brushtopbm brushtopbm.o:	brushtopbm.c
cmuwmtopbm cmuwmtopbm.o:	cmuwmtopbm.c cmuwm.h
g3topbm g3topbm.o:		g3topbm.c g3.h
icontopbm icontopbm.o:		icontopbm.c
gemtopbm gemtopbm.o:		gemtopbm.c
macptopbm macptopbm.o:		macptopbm.c macp.h
mgrtopbm mgrtopbm.o:		mgrtopbm.c mgr.h
pbmlife pbmlife.o:		pbmlife.c
pbmmake pbmmake.o:		pbmmake.c
pbmmask pbmmask.o:		pbmmask.c
pbmpaste pbmpaste.o:		pbmpaste.c
pbmreduce pbmreduce.o:		pbmreduce.c
pbmtext pbmtext.o:		pbmtext.c pbmfont.h
pbmto10x pbmto10x.o:		pbmto10x.c
pbmtoascii pbmtoascii.o:	pbmtoascii.c
pbmtobbnbg pbmtobbnbg.o:	pbmtobbnbg.c
pbmtocmuwm pbmtocmuwm.o:	pbmtocmuwm.c cmuwm.h
pbmtoepson pbmtoepson.o:	pbmtoepson.c
pbmtog3 pbmtog3.o:		pbmtog3.c g3.h
pbmtogem pbmtogem.o:		pbmtogem.c
pbmtogo pbmtogo.o:		pbmtogo.c
pbmtoicon pbmtoicon.o:		pbmtoicon.c
pbmtolj pbmtolj.o:		pbmtolj.c
pbmtomacp pbmtomacp.o:		pbmtomacp.c macp.h
pbmtomgr pbmtomgr.o:		pbmtomgr.c mgr.h
pbmtopi3 pbmtopi3.o:		pbmtopi3.c
pbmtoplot pbmtoplot.o:		pbmtoplot.c
pbmtoptx pbmtoptx.o:		pbmtoptx.c
pbmtox10bm pbmtox10bm.o:	pbmtox10bm.c
pbmtoxbm pbmtoxbm.o:		pbmtoxbm.c
pbmtoybm pbmtoybm.o:		pbmtoybm.c
pbmtozinc pbmtozinc.o:		pbmtozinc.c
pbmupc pbmupc.o:		pbmupc.c
pi3topbm pi3topbm.o:		pi3topbm.c
xbmtopbm xbmtopbm.o:		xbmtopbm.c
ybmtopbm ybmtopbm.o:		ybmtopbm.c

clean:
	-rm -f *.o *.a *.cat core $(BINARIES) pbmmerge


# Imakefile stuff.  Ignore if you're not an X11 type.

              TOP = ../../../../../../usr/src/new/X11
        MACROFILE = 

  BOOTSTRAPCFLAGS =
        CONFIGSRC = $(TOP)/config
      CURRENT_DIR = ./pbmplus/pbm
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
