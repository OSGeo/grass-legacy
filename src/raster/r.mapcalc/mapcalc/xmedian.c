#include "gis.h"


i_median (argc, argv, cell, ncols)
    CELL *argv[];
    register CELL *cell;
    register int ncols;
{
    register int i;
    static int alloc = 0;
    static CELL *array = NULL;
    CELL *a1,*a2;
    int icmp();

    if (argc > alloc)
    {
	alloc = argc;
	array = (CELL *) G_realloc (array, alloc * sizeof(CELL));
    }
    a1 = array + (argc-1)/2;
    a2 = array + argc/2;
    while (ncols-- > 0)
    {
	for (i=0;i<argc;i++)
	    array[i] = argv[i][ncols];
	qsort (array, argc, sizeof(CELL), icmp);
	cell[ncols] = (*a1 + *a2)/2;
    }
}

x_median (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
{
    register int i;
    static int alloc = 0;
    static double *array = NULL;
    double *a1,*a2;
    int dcmp();

    if (argc > alloc)
    {
	alloc = argc;
	array = (double *) G_realloc (array, alloc * sizeof(double));
    }
    a1 = array + (argc-1)/2;
    a2 = array + argc/2;
    while (ncols-- > 0)
    {
	for (i=0;i<argc;i++)
	    array[i] = argv[i][ncols];
	qsort (array, argc, sizeof(double), dcmp);
	cell[ncols] = (*a1 + *a2)/2;
    }
}

n_median (n,name) char *name;
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

static
icmp (a,b)
    CELL *a,*b;
{
    return *a - *b;
}
static
dcmp (a,b)
    double *a,*b;
{
    if (*a < *b) return -1;
    if (*a > *b) return 1;
    return 0;
}
