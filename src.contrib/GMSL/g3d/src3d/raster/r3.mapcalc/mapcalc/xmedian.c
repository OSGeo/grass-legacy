#include "glob.h"

x_median (argc, argv, xcell, ncols)
    double *argv[];
    register double *xcell;
{
    register int i;
    int nv;
    static int alloc = 0;
    static double *array = NULL;
    double *a1,*a2;
    int dcmp();

    if (argc > alloc)
    {
	alloc = argc;
	array = (double *) G3d_realloc (array, alloc * sizeof(double));
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
dcmp (a,b)
    double *a,*b;
{
    if (*a < *b) return -1;
    if (*a > *b) return 1;
    return 0;
}
