# @(#)Makefile	AMG v.3.2
# makefile for bivar program (UNIX V)
# for full load do:
#	make install [S=local_binary]
# check for local modes

LIB = libbieval.a

BIN = $(GRAPH)/bin

OPT = -O

CFLAGS = $(OPT)

LDFLAGS = -s

SETA.o = bivar.o ffct.o eval.o

OTHERS.o = cprod.o apfs.o apll.o setscale.o 

bivar: $(SETA.o) $(OTHERS.o) $(LIB)
	cc $(LDFLAGS) -o bivar $(SETA.o) $(OTHERS.o) $(LIB) -lm

$(SETA.o): bivar.h bieval.h

LIST_H = $(LIB)(bieval.o) $(LIB)(prbicoef.o) $(LIB)(rdbicoef.o)

LIST = $(LIB)(rgroup.o) $(LIB)(pgroup.o) $(LIB)(cnp.o) $(LIB)(mnp.o)

univar: univar.o setscale.o apfs.o apll.o $(LIB)
	cc -o univar univar.o setscale.o apfs.o apll.o $(LIB) -lm

install:
	make lib bivar
	- mv bivar $(BIN)/bivar
	rm -f *.o

load:
	make lib bivar

lib: $(LIST_H) $(LIST)
	$(RANLIB) $(LIB)

$(LIST_H): bieval.h

