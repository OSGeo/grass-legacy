# "@(#)Makefile	AMG v.3.2";

LIB = libmap.a

INCL = $(GRAPH)/src/include

BIN = $(GRAPH)/bin

OPT = -O

CFLAGS = $(OPT) -I$(INCL) -DGRAPHB=\"GRAPHB\"

LDFLAGS = -s

GLIB = $(LIB) $(GRAPH)/src/lib/libgen.a $(GRAPH)/src/lib/libgraph.a

lines: lines.o $(GLIB)
	cc $(LDFLAGS) -o lines lines.o $(GLIB) -lm

s_lines:
	make lines
	- mv -f lines $(BIN)/lines
	- rm lines.o

points: points.o $(GLIB)
	cc $(LDFLAGS) -o points points.o $(GLIB) -lm

s_points:
	make points
	- mv -f points $(BIN)/points
	- rm points.o

grid: grid.o $(GLIB)
	cc $(LDFLAGS) -o grid grid.o $(GLIB) -lm

s_grid:
	make grid
	- mv -f grid $(BIN)/grid
	- rm grid.o

legend: legend.o $(GLIB)
	cc $(LDFLAGS) -o legend legend.o $(GLIB) -lm

s_legend:
	make legend
	- mv -f legend $(BIN)/legend
	- rm legend.o

fence: fence.o $(GLIB)
	cc $(LDFLAGS) -o fence fence.o $(GLIB) -lm

s_fence:
	make fence
	- mv -f fence $(BIN)/fence
	- rm fence.o

head: head.c
	cc -o head head.c

s_head:
	make head
	- mv -f head $(BIN)/head
	- rm head.o

PROGS = fence.o lines.o points.o legend.o grid.o

LIST_P = $(LIB)(georange.o) $(LIB)(setpltr.o)

LIST_H = $(LIB)(loaddef.o) $(LIB)(mproj.o) $(LIB)(iproj.o) $(LIB)(approj.o) 

lib: $(LIST_P) $(LIST_H)
	$(RANLIB) $(LIB)

$(PROGS) $(LIST_P) $(LIST_H): $(INCL)/mapgen.h

$(LIST_P): $(INCL)/graphics.h

install:
	make lib s_lines s_points s_grid s_legend s_fence s_head

all:
	make lib lines points grid legend fence head

