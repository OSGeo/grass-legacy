# Makefile for tester

PURIFYHOME    = /n/tweety/vid2/purify

PURIFY        = $(PURIFYHOME)/purify

CFLAGS= -g  -I.

#CFLAGS= -O -DLITTLE_ENDIEN
LFLAGS=

SRC= movieToVid.c
OBJ= movieToVid.o

default: movieToVid

movieToVid: movieToVid.o
	cc ${CFLAGS} -o movieToVid movieToVid.o

clean:
	rm -f core lintout makeout tags Makefile.bak *.o \
		movieToVid

purify:	$(OBJ)
	$(PURIFY) $(CC) $(OBJ) -o movieToVid.purify

