#include "gis.h"
#include "glob.h"

/**********************************************************************
isnull(x)

  return 1 if x is null, 0 otherwise

**********************************************************************/

int 
i_isnull (int argc, CELL *argv[], register CELL *cell, register int ncols)
{
    register CELL *a;
    a = argv[0];
    while (ncols-- > 0)
	*cell++ = ISNULL(a++) ? 1 : 0;

    return 0;
}

int 
x_isnull (int argc, double *argv[], register CELL *cell, register int ncols)
{
    register double *a;
    a = argv[0];
    while (ncols-- > 0)
	*cell++ = ISNULL_D(a++) ? 1 : 0;

    return 0;
}

int 
n_isnull (int n, char *name)
{
    if (n == 1) return 1;
    fprintf (stderr, "%s - ",name);
    if (n < 1)
	fprintf (stderr, "no arguments specified. ");
    else
	fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s(x)\n",name);
    return 0;
}
