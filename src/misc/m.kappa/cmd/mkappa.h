#ifndef GLOBAL
# define GLOBAL extern
# define INIT(x)
#else
# define INIT(x)=x
#endif
 
#include "gis.h"
#include <math.h>

GLOBAL int header;
GLOBAL char *input;
GLOBAL char *output;
GLOBAL char *title;
GLOBAL long **mat;
GLOBAL int ncat;
