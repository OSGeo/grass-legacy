/* %W% %G% */
#include <stdio.h>
#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL FILE *out;
GLOBAL int current_row ;

GLOBAL int nrows, ncols;
GLOBAL int ras_row, ras_nrows;	

GLOBAL int punit ;
GLOBAL int iarray[1024];

