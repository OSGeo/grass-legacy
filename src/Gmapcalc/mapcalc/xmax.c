#include "gis.h"

i_max (argc, argv, cell, ncols)
    CELL *argv[];
    register CELL *cell;
    register int ncols;
{
    register CELL max,x;
    int i;

    while (ncols-- > 0)
    {
	max = argv[0][ncols];
	for (i=1;i<argc;i++)
	    if ((x=argv[i][ncols]) > max)
		max = x;
	cell[ncols] = max;
    }
}

x_max (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
    register int ncols;
{
    register double max,x;
    register int i;

    while (ncols-- > 0)
    {
	max = argv[0][ncols];
	for (i=1;i<argc;i++)
	    if ((x=argv[i][ncols]) > max)
		max = x;
	cell[ncols] = max;
    }
}

n_max (n,name) char *name;
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
