#
#	makefile for panel library applications
#

include ../Makefile.defs

INCLUDEDIR = ../../include
LIBDIR= ../../lib

INCLUDES= -I${INCLUDEDIR}
HEADER= ${INCLUDEDIR}/panel.h
LIB= ${LIBDIR}/libpanel.a
DEBUG= -g -DDEBUG

APPS=	ep cam stoa grav

all:	apps

apps:	${APPS}

.o:	${LIB}
	${CC} ${CFLAGS} ${LDFLAGS} $@.o -o $@ ${LIB} ${LIBS}

xp:	xp.o colormod.o fade.o ${LIB} ${HEADER}
	${CC} ${CFLAGS} -o xp xp.o colormod.o fade.o ${LIB} ${LIBS}

ep:	ep.o colormod.o fade.o ${LIB} ${HEADER}
	${CC} ${CFLAGS} -o ep ep.o colormod.o fade.o ${LIB} ${LIBS}

#ep:	ep.u colormod.o fade.o ${LIB} ${HEADER}
#	cc -O3 -p -o ep ep.u colormod.o fade.o ${LIB} ${LIBS}
#
#ep.u:	ep.c
#	${CC} -O3 ${INCLUDES} -j ep.c
#	${CC} ${CFLAGS} -DDEBUG ${INCLUDES} -c ep.c

cam:	cam.o colormod.o ${LIB} ${HEADER}
	${CC} ${CFLAGS} -o cam cam.o colormod.o ${LIB} ${LIBS}

grav:	grav.o colormod.o ${LIB} ${HEADER}
	${CC} ${CFLAGS} -o grav grav.o colormod.o ${LIB} ${LIBS}

stoa:	stoa.o ${LIB} ${HEADER}
	cc -o stoa stoa.o ${LIB} ${LIBS}

cm:	cm.o matrix.o ss.o pcm.o ${LIB} ${HEADER}
	cc -o cm cm.o matrix.o ss.o pcm.o ${LIB} ${LIBS}

.c.o:
	cc ${CFLAGS} -c $< ${DEBUG} ${INCLUDES}

clean:
	-rm *.o

clobber:	clean
	-rm ${APPS}



