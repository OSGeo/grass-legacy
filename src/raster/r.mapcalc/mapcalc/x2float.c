#include "glob.h"
/**********************************************************************
float(x)

  converts CELL to double
**********************************************************************/

int 
x_2float (int argc, double *argv[], register double *xcell, register int ncols)
{
    register double *a;
    a = argv[0];
    while (ncols-- > 0)
    {
	if(ISNULL_D(a))
	    SETNULL_D(xcell);
	else
	    *xcell = *a;
	xcell++;
	a++;
    }

    return 0;
}

int 
n_2float (int n, char *name)
{
    if (n == 1) return 1;
    fprintf (stderr, "%s - ",name);
    if (n < 1)
	fprintf (stderr, "no arguments specified. ");
    else
	fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s(x)\n",name);
    return 0;
}
