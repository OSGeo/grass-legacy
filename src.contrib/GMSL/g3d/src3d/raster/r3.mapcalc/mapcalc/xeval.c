#include "glob.h"
/* just copy last arg to cell */

x_eval (argc, argv, xcell, ncols)
    double *argv[];
    register double *xcell;
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
}

n_eval (n,name) char *name;
{
    if (n>0) return 1;
    fprintf (stderr, "%s - ", name);
    fprintf (stderr, "no arguments ");
    fprintf (stderr, "specified. usage: %s(x[,y ...])\n", name);
    return 0;
}
