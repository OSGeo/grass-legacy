#include "gis.h"
/* just copy last arg to cell */
i_eval (argc, argv, cell, ncols)
    CELL *argv[];
    register CELL *cell;
    register int ncols;
{
    register CELL *a;

    a = argv[argc-1];
    while (ncols-- > 0)
	*cell++ = *a++;
}

x_eval (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
{
    register double *a;

    a = argv[argc-1];
    while (ncols-- > 0)
	*cell++ = *a++;
}

n_eval (n,name) char *name;
{
    if (n>0) return 1;
    fprintf (stderr, "%s - ", name);
    fprintf (stderr, "no arguments ");
    fprintf (stderr, "specified. usage: %s(x[,y ...])\n", name);
    return 0;
}
