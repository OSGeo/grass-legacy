# Generic Makefile using Makefile.{cpp,inc,[tmp]} and the C pre-processor.

# GENERIC MAKE PARAMETERS.  (Not all af them are necessarily defined!)
#       normal : normal compilation (with -O flag)
#       debug  : compile for dbx debugging (with -g flag)
#       lint   : run lint code checker (we're doing C, remember)
#       clear  : remove objectfiles and reset state of program/library
#       llib   : generate lint library
#       new    : (clear + normal)
#       all    : ([llib + clear + debug] + clear + normal)
#       mv
#       a

MK= make
SHELL= /bin/sh
CC= cc
CPP= /usr/lib/cpp
COPT=-O

CPPMK= Makefile.cpp
TMPMK= Makefile.tmp
PREREQU= Makefile.sys ${CPPMK}
REALMK= ${MK} -k TMPMK=${TMPMK} COPT="${COPT}" -f ${TMPMK}

normal:   ;      ${MK} ${TMPMK}; ${REALMK} $@
.DEFAULT: ;      ${MK} ${TMPMK}; ${REALMK} $@
all:      clear; ${MK} ${TMPMK}; ${REALMK} all
new:      clear; ${MK} ${TMPMK}; ${REALMK} normal
clear:    objclear stdclear;
clean:    objclear stdclear;

${TMPMK}: ${PREREQU}; ${CPP} ${CPPMK} > ${TMPMK}
stdclear: ;           rm -f *.tmp lib* core
objclear: ;           ${MK} ${TMPMK}; ${REALMK} remove
  _clear: ;           rm -f *.o
