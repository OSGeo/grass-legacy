# to install executable into $(BINDIR),
# and function library into $(LIBDIR),
#	type "make".

CC	= cc
AR	= ar
CFLAGS	= -fullwarn
OBJS	= hull.o ch.o io.o rand.o pointops.o fg.o
HDRS	= hull.h points.h pointsites.h stormacs.h
SRC	= hull.c ch.c io.c rand.c pointops.c fg.c
PROG	= hull
BINDIR	= ../bin
LIBDIR	= ../lib
LIB	= $(LIBDIR)/lib$(PROG).a



all	: $(PROG) rsites
	cp $(PROG) $(BINDIR)/.
	cp rsites $(BINDIR)/.

$(OBJS) : $(HDRS)

hullmain.o	: $(HDRS)

$(PROG)	: $(OBJS) hullmain.o
	$(CC) $(CFLAGS) $(OBJS) hullmain.o -o $(PROG) -lm
	$(AR) rcv $(LIB) $(OBJS)

rsites	: rsites.c
	$(CC) $(CFLAGS) -o rsites rsites.c -lm

clean	:
	-rm -f $(OBJS) hullmain.o core a.out $(PROG)

