# @(#)Makefile	AMG v.3.5
# mapdef make

OPT = -O

CFLAGS = $(OPT) -I$(INCL) -I../

LIBS = $(MLIB)/libmap.a $(MLIB)/libgen.a

LISTA = mapdef.o genrange.o genscale.o range.o headlst.o pvalue.o adjcoef.o

mapdef: $(LISTA) $(LIBS)
	$(CC) -o mapdef $(LISTA) $(LIBS) -lm

$(LISTA): mapdef.h ../mapgen.h

install: mapdef
	- mv mapdef $(BIN)/mapdef
	- /bin/rm -f *.o mapdef core
