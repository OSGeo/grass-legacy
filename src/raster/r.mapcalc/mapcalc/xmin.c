#include "gis.h"

i_min (argc, argv, cell, ncols)
    CELL *argv[];
    register CELL *cell;
    register int ncols;
{
    register CELL min,x;
    register int i;

    while (ncols-- > 0)
    {
	min = argv[0][ncols];
	for (i=1;i<argc;i++)
	    if ((x=argv[i][ncols]) < min)
		min = x;
	cell[ncols] = min;
    }
}

x_min (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
{
    register double min,x;
    register int i;

    while (ncols-- > 0)
    {
	min = argv[0][ncols];
	for (i=1;i<argc;i++)
	    if ((x=argv[i][ncols]) < min)
		min = x;
	cell[ncols] = min;
    }
}

n_min (n,name) char *name;
{
    if (n>1) return 1;
    fprintf (stderr, "%s - ", name);
    if (n==0)
	fprintf (stderr, "no arguments ");
    else
	fprintf (stderr, "only one argument ");
    fprintf (stderr, "specified. usage: %s(x,y[,z...])\n", name);
    return 0;
}
