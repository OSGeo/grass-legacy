#include <stdlib.h>
#include "glob.h"

static int icmp (const void *, const void *);
static int dcmp (const void *, const void *);

int i_median (int argc, CELL *argv[], register CELL *cell, register int ncols)
{
    register int i;
    int nv;
    static int alloc = 0;
    static CELL *array = NULL;
    CELL *a1,*a2;

    if (argc > alloc)
    {
	alloc = argc;
	array = (CELL *) G_realloc (array, alloc * sizeof(CELL));
    }
    a1 = array + (argc-1)/2;
    a2 = array + argc/2;
    while (ncols-- > 0)
    {
	nv = 0;
	for (i=0;i<argc && nv==0;i++)
	{
	    if (ISNULL(&argv[i][ncols]))
		nv = 1;
	    else
		array[i] = argv[i][ncols];
	}
	if (nv)
	    SETNULL(&cell[ncols]);
	else
	{
	    qsort (array, argc, sizeof(CELL), icmp);
	    cell[ncols] = (*a1 + *a2)/2;
	}
    }

    return 0;
}

int x_median (int argc, double *argv[], register double *xcell, int ncols)
{
    register int i;
    int nv;
    static int alloc = 0;
    static double *array = NULL;
    double *a1,*a2;

    if (argc > alloc)
    {
	alloc = argc;
	array = (double *) G_realloc (array, alloc * sizeof(double));
    }
    a1 = array + (argc-1)/2;
    a2 = array + argc/2;
    while (ncols-- > 0)
    {
	nv = 0;
	for (i=0;i<argc && nv==0;i++)
	{
	    if (ISNULL_D(&argv[i][ncols]))
		nv = 1;
	    else
		array[i] = argv[i][ncols];
	}
	if (nv)
	    SETNULL_D(&xcell[ncols]);
	else
	{
	    qsort (array, argc, sizeof(double), dcmp);
	    xcell[ncols] = (*a1 + *a2)/2;
	}
    }

    return 0;
}

int n_median (int n, char *name)
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

static int icmp (const void *aa, const void *bb)
{
    const CELL *a = aa, *b = bb;
    return *a - *b;
}

static int dcmp (const void *aa, const void *bb)
{
    const double *a = aa, *b = bb;
    if (*a < *b) return -1;
    if (*a > *b) return 1;
    return 0;
}
