#include <stdio.h>


#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL int nrows, ncols ;
GLOBAL int nbytes ;
GLOBAL int ras_row;
GLOBAL int ras_nrows;
GLOBAL unsigned char *BLACK;
GLOBAL unsigned char *YELLOW;
GLOBAL unsigned char *CYAN;
GLOBAL unsigned char *MAGENTA;
GLOBAL int NPIXELS ;
