# Makefile for shp2dig test suite

CC= gcc
CFLAGS= -g

INCLUDEDIRS= -I.. -I. -I../btree -I/usr/include
LIBS= -L/usr/lib -lm 

OBJS= shpopen.o dbfopen.o shp2dig.o dbutils.o writelin.o gbtree.o arctest1.o

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
gbtree.o: ../gbtree.c
	$(CC) $(CFLAGS) $(INCLUDEDIRS) -c ../gbtree.c
arctest1.o: arctest1.c
	$(CC) $(CFLAGS) $(INCLUDEDIRS) -c arctest1.c


clean:
	rm -f *.o arctest1
