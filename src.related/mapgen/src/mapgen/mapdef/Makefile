# @(#)Makefile	AMG v.3.2

INCL = $(GRAPH)/src/include

OPT = -O

CFLAGS = $(OPT) -I$(INCL)

LDFLAGS = -s

BIN = $(GRAPH)/bin

LIBS = ../libmap.a $(GRAPH)/src/lib/libgen.a

LISTA = mapdef.o genrange.o genscale.o range.o headlst.o pvalue.o adjcoef.o

mapdef: $(LISTA) $(LIBS)
	cc -o mapdef $(LISTA) $(LIBS) -lm

$(LISTA): mapdef.h $(INCL)/mapgen.h

install:
	make mapdef
	- mv mapdef $(BIN)/mapdef
	- rm -f *.o

