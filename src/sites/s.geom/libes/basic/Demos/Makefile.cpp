/* basic/Demo/Makefile.cpp */

DEMO= print.c

LIB=     ../../lib
LIBINC=  ../../include

#include "Makefile.sys"

normal:;  ${CC} -I${LIBINC} -D__PROTO__ -D__DEBUG__ -g ${DEMO} \
          -L${LIB} -l_basic ${MALLOC}
lint:; ${LINT} -I${LIBINC} -D__DEBUG__ ${DEMO} ${LIB}/llib-l_basic.ln
lib: ; pushd ..; make a mv; popd; make
