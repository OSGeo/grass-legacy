#
#	makefile for panel library demos
#

include ../Makefile.defs

INCLUDEDIR = ../../include
LIBDIR= ../../lib

INCLUDES= -I${INCLUDEDIR}
HEADER= ${INCLUDEDIR}/panel.h
LIB= ${LIBDIR}/libpanel.a

DEMOSO= demo.o demo2.o demo3.o demo4.o demo5.o \
	demo6.o demo7.o demo8.o demo9.o demo10.o \
	demo11.o demo12.o demo13.o demo14.o demo15.o \
	demo16.o demo17.o demo18.o demo19.o demo20.o demo21.o \
	demo22.o demo23.o demo24.o demo25.o demo26.o demo27.o demo28.o \
	demo29.o demo30.o demo31.o demo32.o demo33.o demo34.o demo35.o \
	demo36.o demo37.o demo38.o demo39.o demo40.o demo41.o demo42.o \
	demo43.o demo44.o demo45.o demo46.o demo47.o demo48.o demo49.o \
	demo50.o demo51.o

# presentation demos
#DEMOSO= demo.o demo12.o demo14.o demo2.o demo25.o demo26.o demo17.o \
	demo27.o demo44.o demo30.o demo31.o demo32.o demo37.o demo29.o

#DEMOSO= demo48.o


DEMOS= ${DEMOSO:.o=}

all:	${DEMOS}

demos:	all

${DEMOS}: ${LIB} ${DEMOSC}
	${CC} -c ${CFLAGS} ${INCLUDES} $@.c
	${CC} ${CFLAGS} ${LDFLAGS} $@.o -o $@ ${LIB} ${LIBS}

demo27: ${LIB} demo27.o
	${CC} ${CFLAGS} ${LDFLAGS} $@.o -o $@ ${LIB} ${LIBS}

demo36: ${LIB} demo36.o
	${CC} ${CFLAGS} ${LDFLAGS} $@.o -o $@ ${LIB} ${LIBS}

demo38: ${LIB} demo38.o
	${CC} ${CFLAGS} ${LDFLAGS} $@.o -o $@ ${LIB} ${LIBS}

demo43: ${LIB} demo43.o
	${CC} ${CFLAGS} ${LDFLAGS} $@.o -o $@ ${LIB} ${LIBS}

demo44: ${LIB} demo44.o
	${CC} ${CFLAGS} ${LDFLAGS} $@.o -o $@ ${LIB} ${LIBS}

.c.o:
	${CC} -c ${CFLAGS} ${INCLUDES} $<

${DEMOSO}:   ${HEADER}

clean:
	-rm ${DEMOSO}

clobber:	clean
	-rm ${DEMOS}


