# Makefile for shp2dig test suite

CC= gcc
CFLAGS= -g

INCLUDEDIRS= -I.. -I. -I../btree -I/usr/include
LIBS= -L/usr/lib -lm 

OBJS= shpopen.o dbfopen.o shp2dig.o dbutils.o writelin.o arctest1.o
BTREEDIR= ../btree
BTREE= create.o find.o free.o next.o rewind.o update.o

all: arctest1


arctest1: $(OBJS) $(BTREE)
	$(CC) $(CFLAGS) $(LIBS) -o $@ $^

shpopen.o: ../shpopen.c
	$(CC) $(CFLAGS) $(INCLUDEDIRS) -c ../shpopen.c
dbfopen.o: ../dbfopen.c
	$(CC) $(CFLAGS) $(INCLUDEDIRS) -c ../dbfopen.c
shp2dig.o: ../shp2dig.c
	$(CC) $(CFLAGS) $(INCLUDEDIRS) -c ../shp2dig.c
dbutils.o: ../dbutils.c
	$(CC) $(CFLAGS) $(INCLUDEDIRS) -c ../dbutils.c
writelin.o: ../writelin.c
	$(CC) $(CFLAGS) $(INCLUDEDIRS) -c ../writelin.c
arctest1.o: arctest1.c
	$(CC) $(CFLAGS) $(INCLUDEDIRS) -c arctest1.c
create.o:  ../btree/create.c
	$(CC) $(CFLAGS) $(INCLUDEDIRS) -c ../btree/create.c
find.o: ../btree/find.c
	$(CC) $(CFLAGS) $(INCLUDEDIRS) -c ../btree/find.c
free.o:  ../btree/free.c
	$(CC) $(CFLAGS) $(INCLUDEDIRS) -c ../btree/free.c
next.o:  ../btree/next.c
	$(CC) $(CFLAGS) $(INCLUDEDIRS) -c ../btree/next.c
rewind.o:  ../btree/rewind.c
	$(CC) $(CFLAGS) $(INCLUDEDIRS) -c ../btree/rewind.c
update.o:  ../btree/update.c
	$(CC) $(CFLAGS) $(INCLUDEDIRS) -c ../btree/update.c


clean:
	rm -f *.o arctest1
