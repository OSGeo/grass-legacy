# $Log: makefile,v $
# Revision 1.2  2000-03-22 20:05:34  markus
# updated oon shapelib path
#
# Revision 1.1  2000/03/22 20:02:28  markus
# contribution by Jan Wagner
#
# Revision 1.4  1999/11/05  08:02:48  jwagner
# added comment for compiling under MS VC
#
# Revision 1.3  1999/09/16  08:45:32  jwagner
# new shapelib revision
#
# Revision 1.2  1999/04/22 15:33:31  jwagner
# Added further tests and utils module.
#
# Revision 1.1  1999/04/21  16:01:52  jwagner
# Initial revision
#

SHAPELIBPATH=../shapelib-1.2.7

CC=gcc
# add -DWITH_STRICMP if you are compiling i.e. with MS VC
CFLAGS=-I$(SHAPELIBPATH) # -DDEBUG

all: gen2shp

utils.o: utils.c utils.h
	$(CC) -c utils.c

gen2shp: gen2shp.c utils.o
	$(CC) $(CFLAGS) -o $@ gen2shp.c  $(SHAPELIBPATH)/shpopen.o\
		$(SHAPELIBPATH)/dbfopen.o utils.o

test: gen2shp
	rm -f pnttest.dbf pnttest.shp pnttest.shx
	gen2shp pnttest points < pnttest.gen
	rm -f arctest.dbf arctest.shp arctest.shx
	gen2shp arctest lines < arctest.gen
	rm -f plytest.dbf plytest.shp plytest.shx
	gen2shp plytest polygons < plytest.gen

clean:
	rm -f gen2shp *.o
	rm -f pnttest.dbf pnttest.shp pnttest.shx
	rm -f arctest.dbf arctest.shp arctest.shx
	rm -f plytest.dbf plytest.shp plytest.shx
