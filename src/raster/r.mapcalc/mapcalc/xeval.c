#include "glob.h"
/* just copy last arg to cell */
int 
i_eval (int argc, CELL *argv[], register CELL *cell, register int ncols)
{
    register CELL *a;

    a = argv[argc-1];
    for (; ncols-- > 0; a++, cell++)
    {
	if (ISNULL(a))
	    SETNULL(cell);
	else
	    *cell = *a;
    }

    return 0;
}

int 
x_eval (int argc, double *argv[], register double *xcell, int ncols)
{
    register double *a;

    a = argv[argc-1];
    for (; ncols-- > 0; a++, xcell++)
    {
	if (ISNULL_D(a))
	    SETNULL_D(xcell);
	else
	    *xcell = *a;
    }

    return 0;
}

int 
n_eval (int n, char *name)
{
    if (n>0) return 1;
    fprintf (stderr, "%s - ", name);
    fprintf (stderr, "no arguments ");
    fprintf (stderr, "specified. usage: %s(x[,y ...])\n", name);
    return 0;
}
