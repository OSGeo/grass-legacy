# "@(#)Makefile	AMG v.3.2";

INCL = $(GRAPH)/src/include

LDFLAGS = -s

OPT = -O

CFLAGS = $(OPT) -I$(INCL) -DGRAPHB=\"GRAPHB\"

LIB = libplot.a

BIN = $(GRAPH)/bin

GLIB = $(LIB) $(GRAPH)/src/lib/libgen.a $(GRAPH)/src/lib/libgraph.a

panal: panal.o $(GLIB)
	cc $(LDFLAGS) -o panal panal.o $(GLIB) -lm 

s_panal:
	make panal
	- mv panal $(BIN)/panal
	- rm -f panal.o

pgrid: pgrid.o $(GLIB)
	cc $(LDFLAGS) -o pgrid pgrid.o $(GLIB) -lm 

s_pgrid:
	make pgrid
	- mv pgrid $(BIN)/pgrid
	- rm -f pgrid.o

pline: pline.o $(GLIB)
	cc $(LDFLAGS) -o pline pline.o $(GLIB) -lm 

s_pline:
	make pline
	- mv pline $(BIN)/pline
	- rm -f pline.o

plegend: plegend.o $(GLIB)
	cc $(LDFLAGS) -o plegend plegend.o $(GLIB) -lm 

s_plegend:
	make plegend
	- mv plegend $(BIN)/plegend
	- rm -f plegend.o

ppoint: ppoint.o $(GLIB)
	cc $(LDFLAGS) -o ppoint ppoint.o $(GLIB) -lm 

s_ppoint:
	make ppoint
	- mv ppoint $(BIN)/ppoint
	- rm -f ppoint.o

preview: preview.o $(GLIB)
	cc $(LDFLAGS) -o preview preview.o $(GLIB) -lm 

s_preview:
	make preview
	- mv preview $(BIN)/preview
	- rm -f preview.o

LIST_H_G = $(LIB)(scalein.o) $(LIB)(setpltr.o) $(LIB)(range.o)

LIST = $(LIB)(draft.o) $(LIB)(lrnd.o)

# general local library
lib: $(LIST_H_G) $(LIST)
	$(RANLIB) $(LIB)

preview.o: $(INCL)/mapgen.h

$(LIST_H_G) ppoint.o pline.o pgrid.o preview.o plegend.o: $(INCL)/plotgen.h

$(LIST_H_G): $(INCL)/graphics.h

all:
	make lib ppoint pline preview plegend pgrid panal

install:
	make lib s_ppoint s_pline s_plegend s_pgrid s_panal s_preview

