# @(#)Makefile	OEMG v.1.6
# "merge" programs

OPT = -O

CFLAGS = $(OPT) -I$(GRAPH)/include

SLIB = $(MLIB)/libgen.a $(MLIB)/libgraph.a

LIB = $(MLIB)/libgen.a

LG =	$(LIB)(approj.o) $(LIB)(clip.o) $(LIB)(drawline.o) \
	$(LIB)(invclip.o) $(LIB)(loader.o) $(LIB)(outside.o) \
	$(LIB)(polycheck.o) $(LIB)(polyclip.o) $(LIB)(scalein.o) \
	$(LIB)(setpltr.o) $(LIB)(xwindow.o) $(LIB)(pgdraw.o) \
	$(LIB)(mgdraw.o) $(LIB)(set_line.o)

LN =	$(LIB)(emess.o) $(LIB)(lrnd.o)

legend:	legend.o $(LG) $(LN)
	$(CC) $(LDFLAGS) -o legend legend.o $(LIB) $(SLIB) -lm 

vector:	vector.o $(LG) $(LN)
	$(CC) $(LDFLAGS) -o vector vector.o $(LIB) $(SLIB) -lm 

lib:	$(LG) $(LN)
	if [ -f /usr/bin/ranlib -o -f /bin/ranlib ]; then ranlib  $(LIB); fi

legend.o vector.o $(LG): gen.h

install: lib legend vector
	- mv legend vector $(BIN)
	- /bin/rm -f $(BIN)/plegend
	- ln $(BIN)/legend $(BIN)/plegend
	- rm *.o

