prefix=/usr/local/staff/villa/Work/Dev/r.concordance
CC_FLAGS=-g -fno-builtin -fpcc-struct-return -fwritable-strings
LIBDIR=$(prefix)/lib
LINK_PATHS=-L$(LIBDIR)
GISINCDIR=/usr/local/grass/src/include
GISLIBDIR=/usr/local/grass/src/libes/LIB
GISLIB= $(GISLIBDIR)/libgis.a
INCLUDE_PATHS=-I. -I$(LIBDIR)  -I$(GISINCDIR) -I/usr/local/include 
LIB=../lib/libbase.a

SRC_LIST= main.C scan.C

OBJ_LIST= main.o scan.o

.C.o:
	g++ -c $(CC_FLAGS) $(INCLUDE_PATHS) -o $@ $<

.c.o:
	gcc -c $(CC_FLAGS) $(INCLUDE_PATHS) -o $@ $<

all:    ../bin/r.concordance

../bin/r.concordance: $(OBJ_LIST) $(LIB)
	g++ -o r.concordance $(OBJ_LIST) $(LIB) $(GISLIB) -lm 
	mv r.concordance ../bin

clean: 
	rm -f *.o *.a core *\~ *.bak 
