/* basic/Makefile.cpp --- C pre-processor Makefile for the Basic C Library. */

/* USAGE: cpp -P ${CPPMK} > ${TMPMK}; make -k TMPMK=${TMPMK} -f ${TMPMK} ... */

/* Targets. */
DEST=   ../lib
H_DEST= ../include

/* Internals & sys deps: CC, LINT, LLOUT, AR, RANLIB, MALLOC, COPT, ... */
#include "Makefile.sys"
COPT= -O

/* Source, header, and object files. */
MODULES= basic.c malloc.c files.c \
         getarg.c cb.c cb_doprnt.c \
         isort.c qsort.c istaque.c \
         prime.c uhash.c \
         time.c tokenize.c counter.c \
         math2.c 
H_FILES= basic.h
H_EXPORT= basic.h
OBJECTS= ${MODULES:.c=.o}

/* Interface. */
normal: a mv;  @echo "Library ready."
debug:  ;      make -k -f ${TMPMK} a COPT="-g -D__DEBUG__" mvd mv
lint:   ;      ${LINT} -D__DEBUG__ ${MODULES}
llib:   ln mv;
remove: ;      rm -f ${OBJECTS}
all:    ;      make llib debug remove normal

/* Internal rules. */
ln:     ;               ${LINT} -D__DEBUG__ ${LLOUT} ${MODULES};
a:      ${OBJECTS};     ${AR} ${OBJECTS}; ${RANLIB}
mvd:    ;               mv lib_OUT.a lib_OUT-g.a
mv:     ${H_FILES};     @ /bin/csh -cf ' \
   echo "Moving targets ..."; \
   if (-e lib_OUT.a)      mv lib_OUT.a     ${DEST}/lib_basic.a; \
   if (-e lib_OUT-g.a)    mv lib_OUT-g.a   ${DEST}/lib_basic-g.a; \
   if (-e llib-l_OUT.ln)  mv llib-l_OUT.ln ${DEST}/llib-l_basic.ln; \
   pushd ${DEST}; chmod a+r *basic.{a,ln}; popd; \
   cp ${H_EXPORT} ${H_DEST}; \
   pushd ${H_DEST}; chmod a+r ${H_EXPORT}; popd \
   '

/* The rule how to compile source files. */
.c.o: ${H_FILES}; ${CC} ${COPT} -c $*.c -o $*.o
