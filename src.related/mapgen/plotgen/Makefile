# "@(#)Makefile	AMG v.3.8";
# plotgen graphic programs
OPT = -O

CFLAGS = $(OPT) -I$(INCL)

LIB = $(MLIB)/libplot.a

GLIB = $(LIB) $(MLIB)/libgen.a $(MLIB)/libgraph.a

panal: panal.o $(GLIB)
	$(CC) $(LDFLAGS) -o panal panal.o $(GLIB) -lm 

pgrid: pgrid.o $(GLIB)
	$(CC) $(LDFLAGS) -o pgrid pgrid.o $(GLIB) -lm 

pline: pline.o $(GLIB)
	$(CC) $(LDFLAGS) -o pline pline.o $(GLIB) -lm 

ppoint: ppoint.o $(GLIB)
	$(CC) $(LDFLAGS) -o ppoint ppoint.o $(GLIB) -lm 

preview: preview.o $(GLIB)
	$(CC) $(LDFLAGS) -o preview preview.o $(GLIB) -lm 

LIST_H_G = $(LIB)(scalein.o) $(LIB)(setpltr.o) $(LIB)(range.o)

LIST = $(LIB)(draft.o) $(LIB)(lrnd.o)

# general local library
lib: $(LIST_H_G) $(LIST)
	if [ -f /usr/bin/ranlib -o -f /bin/ranlib ]; then ranlib  $(LIB); fi

preview.o: ../include/mapgen.h

$(LIST_H_G) ppoint.o pline.o pgrid.o preview.o : plotgen.h

$(LIST_H_G): $(INCL)/graphics.h

install: lib ppoint pline pgrid panal preview
	- mv ppoint pline pgrid panal preview $(BIN)
	- /bin/rm -f $(BIN)/prev $(BIN)/zoom
	- ln $(BIN)/preview $(BIN)/zoom
	- ln $(BIN)/preview $(BIN)/prev
	- rm *.o

