#include "imagery.h"

#define BIL 1
#define BSQ1 2
#define BSQ2 3

#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL int tapefd;
GLOBAL int *bandfd;
GLOBAL int *wantband;
GLOBAL int *first;
GLOBAL int nbands;
GLOBAL unsigned char *tapebuf;
GLOBAL int tapebufsize;
GLOBAL CELL *cellbuf;
GLOBAL int skipfiles;
GLOBAL int skiprecords;
GLOBAL int skipbytes;
GLOBAL int bandsize;
GLOBAL int format;
GLOBAL int blocking_factor;
GLOBAL int firstrow, lastrow;
GLOBAL int firstcol, lastcol;
GLOBAL int nrows;
GLOBAL int ncols;

GLOBAL struct Tape_Info tape_info;
