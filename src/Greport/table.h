#include "gis.h"
#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL struct Cell_head	 window;
GLOBAL struct Categories cats;
GLOBAL long count0;
GLOBAL long ncells;
GLOBAL double total0;

GLOBAL int cols,col[3];
GLOBAL int page,lines,tables;
GLOBAL int pnum;
GLOBAL char param[7][2];

extern char	*paramname[7],fill[],topline[],midline[];
extern int	threshold,pagelen;
