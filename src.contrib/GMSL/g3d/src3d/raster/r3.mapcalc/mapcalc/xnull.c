#include "gis.h"
#include "glob.h"
/****************************************************************
null() null values
****************************************************************/

x_null (argc, argv, xcell, ncols)
    double *argv[];
    register double *xcell;
    register int ncols;
{
    while (ncols-- > 0)
    {
	SETNULL_D(xcell++);
    }
}

n_null(n,name) char *name;
{
    if (n == 0) return 1;
    fprintf (stderr, "%s - ",name);
    fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s()\n",name);
    return 0;
}
