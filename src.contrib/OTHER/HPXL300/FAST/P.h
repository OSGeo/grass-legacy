#include <stdio.h>
#ifndef GLOBAL
#define GLOBAL extern
#endif

#define NCOLORS  6

GLOBAL FILE *out;
GLOBAL char buffer[255];
GLOBAL int ncolumns, maxcolumns;
