
include Makefile.defs

LIB= ../lib/libpanel.a

HEADER= ${INCLUDEDIR}/panel.h

INCLUDEDIR= ../include
LIBDIR= ../lib
SRCDIR= ../src
DEMODIR= D.demos

INCLUDES= -I${INCLUDEDIR}

LIBOBJS= ${LIB}(panel.o) ${LIB}(puck.o) ${LIB}(slider.o) ${LIB}(g_gets.o) \
	 ${LIB}(strip.o) ${LIB}(button.o) ${LIB}(typein.o) ${LIB}(typeout.o) \
	 ${LIB}(mouse.o) ${LIB}(slideroid.o) ${LIB}(palette.o) \
	 ${LIB}(dial.o) ${LIB}(script.o) ${LIB}(mslider.o) ${LIB}(menu.o) \
	 ${LIB}(iconmenu.o) ${LIB}(icon.o) ${LIB}(frame.o) ${LIB}(cycle.o) \
	 ${LIB}(submenu.o) ${LIB}(list.o) ${LIB}(scroll.o) \
	 ${LIB}(viewframe.o) ${LIB}(polyframe.o) ${LIB}(mapwfind.o) \
	 ${LIB}(signal.o) ${LIB}(oldtypein.o) ${LIB}(graphframe.o) \
	 $(LIB)(user.o)

LIBSRC=	 panel.c puck.c slider.c g_gets.c \
	 strip.c button.c typein.c typeout.c \
	 mouse.c slideroid.c palette.c \
	 dial.c script.c mslider.c menu.c \
	 iconmenu.c icon.c frame.c cycle.c \
	 submenu.c list.c scroll.c \
	 user.c

lib:	${LIB}

all:	${LIB} demos apps

demos:	${LIB}
	cd D.demos; ${MAKE} -${MAKEFLAGS}

apps:	${LIB}
	cd D.apps; ${MAKE} -${MAKEFLAGS}

.PRECIOUS:  ${LIB}

${LIB}:	${LIBOBJS} ${HEADER}
	${RANLIB} ${LIB}

${LIBOBJS}:
	@ if [ ! -d ${LIBDIR} ] ; then mkdir ${LIBDIR} ; fi
	${CC} -c ${CFLAGS} ${INCLUDES} $*.c
	${LD} -r $*.o
	/bin/mv a.out $*.o
#	${AR} ${ARFLAGS} $@ $*.o
	${AR} ${ARFLAGS} $(LIB) $*.o
	/bin/rm $*.o

.o:	${LIB}
	${CC} ${CFLAGS} ${LDFLAGS} $@.o -o $@ ${LIB} ${LIBS}

.c.o:
	cc ${CFLAGS} -c $< ${INCLUDES}

clean:
	cd ${DEMODIR}; ${MAKE} -${MAKEFLAGS} clean
	-rm -f *.o

clobber:	clean
	cd ${DEMODIR}; ${MAKE} -${MAKEFLAGS} clobber
	-rm ${LIB} ${APPS}

install:
	cp ${LIB} ${LIBDEST}
	${RANLIB} ${LIBDEST}/${LIB}
	cp ${HEADER} ${INCLUDEDEST}

lint:	${LIBSRC}
	${LINT} ${INCLUDES} $?
