# set proper install for local system
LIB=$(MLIB)
CFLAGS = -O -I$(INCL)

LDFLAGS = -s -L$(LIB)

ftab:	ftab.o $(LIB)/libgraph.a
	cc $(LDFLAGS) -o ftab ftab.o -lgraph -lm

cur:	cur.o $(LIB)/libgraph.a
	cc $(LDFLAGS) -o cur cur.o -lgraph -lm

boxes:	boxes.o $(LIB)/libgraph.a
	cc $(LDFLAGS) -o boxes boxes.o -lgraph -lm

install: ftab boxes cur
	mv ftab boxes cur $(BIN)
	rm -f *.o

