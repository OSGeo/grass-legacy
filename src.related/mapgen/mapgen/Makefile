# "@(#)Makefile	AMG v.3.5";
# basic mapgen graphics generating programs

LIB = $(MLIB)/libmap.a

OPT = -O

CFLAGS = $(OPT) -I$(INCL)

GLIB = $(LIB) $(MLIB)/libgen.a $(MLIB)/libgraph.a

$(BIN)/lines: lines.o $(GLIB)
	$(CC) $(LDFLAGS) -o $(BIN)/lines lines.o $(GLIB) -lm

$(BIN)/points: points.o $(GLIB)
	$(CC) $(LDFLAGS) -o $(BIN)/points points.o $(GLIB) -lm

$(BIN)/grid: grid.o $(GLIB)
	$(CC) $(LDFLAGS) -o $(BIN)/grid grid.o $(GLIB) -lm

# old version we don't bother with---see ../merge
$(BIN)/legend: legend.o $(GLIB)
	$(CC) $(LDFLAGS) -o $(BIN)/legend legend.o $(GLIB) -lm

$(BIN)/fence: fence.o $(GLIB)
	$(CC) $(LDFLAGS) -o $(BIN)/fence fence.o $(GLIB) -lm

LIST_P = $(LIB)(georange.o) $(LIB)(setpltr.o)

LIST_H = $(LIB)(loaddef.o) $(LIB)(mproj.o) $(LIB)(iproj.o) $(LIB)(approj.o) 

lib: $(LIST_P) $(LIST_H)
	if [ -f /usr/bin/ranlib -o -f /bin/ranlib ]; then ranlib  $(LIB); fi

PROGS = fence.o lines.o points.o legend.o grid.o

$(PROGS) $(LIST_P) $(LIST_H): mapgen.h

$(LIST_P): $(INCL)/graphics.h

install: lib $(BIN)/lines $(BIN)/points $(BIN)/grid $(BIN)/fence $(BIN)/legend
	- /bin/rm -f *.o *.a core lines points grid fence legend

