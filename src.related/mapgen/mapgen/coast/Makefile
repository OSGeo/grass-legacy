# "@(#)Makefile	AMG v.3.7";
# coast line programs

OPT = -O

OLD = NEW

CFLAGS = $(OPT) -I../ -I$(INCL) -D$(OLD)

LDFLAGS = -s

LIBS = $(MLIB)/libmap.a $(MLIB)/libgen.a $(MLIB)/libgraph.a
MAPLIB = $(MLIB)/libmap.a $(MLIB)/libgen.a

getcoast: getcoast.o getdir.o $(LIBS)
	$(CC) $(LDFLAGS) -o getcoast getcoast.o getdir.o $(LIBS) -lm -L/usr/local/lib

coast: coast.o getdir.o $(LIBS)
	$(CC) $(LDFLAGS) -o coast coast.o getdir.o $(LIBS) -lm


mcoast: mcoast.o compress.o putdir.o
	$(CC) $(LDFLAGS) -o mcoast mcoast.o compress.o putdir.o $(MAPLIB) -lm

strpcst: strpcst.o
	$(CC) $(LDFLAGS) -o strpcst strpcst.o -lm

todeg:	todeg.o
	$(CC) $(LDFLAGS) -o todeg todeg.o -lm

newdir: newdir.o putdir.o
	$(CC) $(LDFLAGS) -o newdir newdir.o putdir.o

install: coast mcoast strpcst todeg getcoast
	- mv coast mcoast strpcst todeg getcoast $(BIN)
	- rm -f $(BIN)/tobin
	- ln $(BIN)/todeg $(BIN)/tobin
	- rm *.o

getcoast.o coast.o strpcst.o mcoast.o compress.o: coast.h ../mapgen.h

clean:
	- /bin/rm -f coast mcoast strpcst todeg getcoast
