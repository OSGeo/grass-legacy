#include "imagery.h"

#ifndef GLOBAL
#define GLOBAL extern
#endif

#define THEMATIC_MAPPER_NBANDS 7

GLOBAL char inputname[50];
GLOBAL char groupname[50];
GLOBAL char *title;
GLOBAL int *wantbands;
GLOBAL int verbose;
GLOBAL int rows, cols;
GLOBAL int firstrow, lastrow, firstcol, lastcol;
