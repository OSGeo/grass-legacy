# @(#)Makefile	AMG v.3.5

CFLAGS = -O -I$(GRAPH)/src/graphics/plotter

GLIB = $(MLIB)/libgraph.a

plroff:	plroff.o libTR.a
	cc -o $(BIN)/plroff plroff.o libTR.a $(GLIB)

LIB = libTR.a

SETH = $(LIB)(TRloadfont.o) $(LIB)(TRparse.o) $(LIB)(TRprint.o) \
	$(LIB)(TRsetdev.o)

SET0 = $(LIB)(TRmain.o)

lib:	$(SET0) $(SETH)
	if [ -f /usr/bin/ranlib -o -f /bin/ranlib ] ; then ranlib $(LIB) ; fi

# make null stubs

STUBS =	init setres pause stop trailer loadfont \
	height slant setsize setfont print sprint endline \
	newpage useropt line circle ellipse arc spline

stubs:
	for st in $(STUBS) ; \
		do echo "$$st() {}" >$$st.c ; make slib SMEM=$$st.o ; \
		rm -f $$st.c ; \
	done
	if [ -f /usr/bin/ranlib -o -f /bin/ranlib ] ; then ranlib $(LIB) ; fi

slib:	$(LIB)($(SMEM))

$(SETH): TRdev.h

install: lib stubs plroff
	rm -f plroff.o
	makedev DESC R I B BI S
	mv [A-Z]*.out /usr/lib/font/devGRAPH

