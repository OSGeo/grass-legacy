/* lia/Makefile.cpp --- C pre-processor Makefile for the Lia Library. */

/* USAGE: cpp -P ${CPPMK} > ${TMPMK}; make -k TMPMK=${TMPMK} -f ${TMPMK} ... */

/* Targets. */
DEST=   ../lib
H_DEST= ../include

/* Internals and sys deps: CC, LINT, LLOUT, AR, RANLIB, MALLOC, COPT, ... */
#include "Makefile.sys"
INCLS = -I../include
LINTLIBS= ../lib/llib-l_basic.ln
COPT= -O

/* Source and object files. */
MODULES= lia.c aux.c chars.c stack.c pool.c \
         det.c sdet.c sdet_one.c \
         mint.c
H_FILES= lia.h mint.h
H_EXPORT= lia.h
OBJECTS= ${MODULES:.c=.o}

/* Interface. */
normal: a mv;  @echo "Library ready."
debug:  ;      make -k -f ${TMPMK} a COPT="-g -D__DEBUG__" mvd mv
lint:   ;      ${LINT} -D__DEBUG__ ${INCLS} ${MODULES} ${LINTLIBS}
llib:   ln mv;
remove: ;      rm -f ${OBJECTS}
all:    ;      make llib debug remove normal

/* Internal rules. */
ln:     ;               ${LINT} -D__DEBUG__ ${INCLS} ${LLOUT} ${MODULES};
a:      ${OBJECTS};     ${AR} ${OBJECTS}; ${RANLIB}
mvd:    ;               mv lib_OUT.a lib_OUT-g.a
mv:     ${H_FILES};     @ /bin/csh -cf ' \
   echo "Moving targets ..."; \
   if (-e lib_OUT.a)      mv lib_OUT.a     ${DEST}/lib_lia.a; \
   if (-e lib_OUT-g.a)    mv lib_OUT-g.a   ${DEST}/lib_lia-g.a; \
   if (-e llib-l_OUT.ln)  mv llib-l_OUT.ln ${DEST}/llib-l_lia.ln; \
   pushd ${DEST}; chmod a+r *lia.{a,ln}; popd; \
   cp ${H_EXPORT} ${H_DEST}; \
   pushd ${H_DEST}; chmod a+r ${H_EXPORT}; popd \
   '

/* Generic rule how to compile source files. */
.c.o: ${H_FILES}; ${CC} ${COPT} ${INCLS} -c $*.c -o $*.o
