/* %W% %G% */
#include "gis.h"
#include <math.h>

#define MAXCOL          1000

CELL *cellbuf;
int cellfd;

struct Cell_head window ;

double	*Ux,*Uy;
double	*Ax,*Ay;

char *tmpname1, *tmpname2;
