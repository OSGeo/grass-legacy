# "@(#)Makefile	AMG v.3.2";

INCL = $(GRAPH)/src/include

BIN = $(GRAPH)/bin

OPT = -O

CFLAGS = $(OPT) -I$(INCL) -D$(OLD)

LDFLAGS = -s

LIBS = ../libmap.a $(GRAPH)/src/lib/libgen.a $(GRAPH)/src/lib/libgraph.a

OLD = NEW

getcoast: getcoast.o $(LIBS)
	cc $(LDFLAGS) -o getcoast getcoast.o $(LIBS) -lm

s_getcoast:
	make getcoast
	- mv getcoast $(BIN)/getcoast
	- rm getcoast.o

coast: coast.o $(LIBS)
	cc $(LDFLAGS) -o coast coast.o $(LIBS) -lm

s_coast:
	make coast
	- mv coast $(BIN)/coast
	- rm coast.o

MLIB = ../libmap.a $(GRAPH)/src/lib/libgen.a

mcoast: mcoast.o compress.o
	cc $(LDFLAGS) -o mcoast mcoast.o compress.o $(MLIB) -lm

s_mcoast:
	make mcoast
	- mv mcoast $(BIN)/mcoast
	- rm mcoast.o

strpcst: strpcst.o
	cc $(LDFLAGS) -o strpcst strpcst.o -lm

s_strpcst:
	make strpcst
	- mv strpcst $(BIN)/strpcst
	- rm strpcst.o

todeg:	todeg.o
	cc $(LDFLAGS) -o todeg todeg.o -lm

s_todeg:
	make todeg
	- mv todeg $(BIN)/todeg
	- rm todeg.o
	- rm -f $(BIN)/tobin
	- ln $(BIN)/todeg $(BIN)/tobin

install:
	make s_coast s_mcoast s_strpcst s_todeg s_getcoast s_todeg

all:
	make coast strpcst mcoast todeg getcoast todeg

getcoast.o coast.o strpcst.o mcoast.o compress.o: coast.h $(INCL)/mapgen.h

