#include "gis.h"

#ifndef GLOBAL
# define GLOBAL extern
# define INIT(x)
#else
# define INIT(x) =(x)
#endif

GLOBAL int nfiles;
GLOBAL int nrows;
GLOBAL int ncols;

GLOBAL int maskfd;
GLOBAL CELL *mask;
GLOBAL int (*get_row)();

GLOBAL char fs[2];
GLOBAL struct Categories *labels INIT(NULL) ;
