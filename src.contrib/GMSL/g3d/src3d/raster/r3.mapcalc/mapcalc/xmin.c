#include "glob.h"

x_min (argc, argv, xcell, ncols)
    double *argv[];
    register double *xcell;
    register int ncols;
{
    register double min,x;
    register int i;
    int nv;

    while (ncols-- > 0)
    {
	nv = 0;
	for (i=0;i<argc && nv==0;i++)
	{
	    if (ISNULL_D(&argv[i][ncols]))
		nv=1;
	    else if(i==0)
		min = argv[0][ncols];
	    else if ((x=argv[i][ncols]) < min)
		min = x;
	}
	if(nv)
	    SETNULL_D(&xcell[ncols]);
	else
	    xcell[ncols] = min;
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
