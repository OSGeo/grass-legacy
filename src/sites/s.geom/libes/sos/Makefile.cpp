/* sos/Makefile.cpp --- C pre-processor Makefile for the SoS Library. */ 

/* USAGE: cpp -P ${CPPMK} > ${TMPMK}; make -k TMPMK=${TMPMK} -f ${TMPMK} ... */

/* Targets. */
DEST=   ../lib
H_DEST= ../include

/* Internals and system dependencies.
   CC, LINT, LLOUT, AR, RANLIB, MALLOC, COPT, ... */
#include "Makefile.sys"
INCLS =  -I../include
LINTLIBS= ../lib/llib-l_lia.ln ../lib/llib-l_basic.ln 
COPT= -O

/* Source and object files. */
MODULES= sos.c minor.c primitive.c \
         lambda3.c lambda3_star.c \
         lambda4.c lambda4_star.c \
         lambda5.c \
         above3.c above3_star.c \
         above4.c positive3.c in_sphere.c \
         rho1.c rho1_num.c \
         rho2.c rho2_num.c rho2_den.c \
         rho3.c rho3_num.c rho3_den.c \
         smaller.c
/* Feel free to move unused primitive modules from MODULES
   to SKIPPED_MODULES to speed up the compilation. */
SKIPPED_MODULES=
H_FILES= sos.h primitive.h internal.h
H_EXPORT= sos.h
OBJECTS= ${MODULES:.c=.o}

/* Interface. */
normal: a mv;  @echo "Library ready."
debug:  ;      make -k -f ${TMPMK} a COPT="-g -D__DEBUG__" mvd mv
lint:   ;      ${LINT} -D__DEBUG__ ${INCLS} ${MODULES} ${LINTLIBS}
llib:   ln mv; 
remove: ;      rm -f ${OBJECTS}
all:    ;      make llib debug remove normal;

/* Internal rules. */
ln:     ;               ${LINT} -D__DEBUG__ ${INCLS} ${LLOUT} ${MODULES} 
a:      ${OBJECTS};     ${AR} ${OBJECTS}; ${RANLIB}
mvd:    ;               mv lib_OUT.a lib_OUT-g.a
mv:     ${H_FILES};     @ /bin/csh -cf ' \
   echo "Moving tragets ..."; \
   if (-e lib_OUT.a)     mv lib_OUT.a     ${DEST}/lib_sos.a; \
   if (-e lib_OUT-g.a)   mv lib_OUT-g.a   ${DEST}/lib_sos-g.a; \
   if (-e llib-l_OUT.ln) mv llib-l_OUT.ln ${DEST}/llib-l_sos.ln; \
   pushd ${DEST}; chmod a+r *lia.{a,ln}; popd; \
   cp ${H_EXPORT} ${H_DEST}; \
   pushd ${H_DEST}; chmod a+r ${H_EXPORT}; popd \
   '
/* Generic rule how to compile source files. */
.c.o: ${H_FILES}; ${CC} ${COPT} ${INCLS} -c $*.c -o $*.o
