#include "glob.h"
/**********************************************************************
int(x)

   truncates to nearest integer and returns CELL
**********************************************************************/
int 
i_2int (int argc, CELL *argv[], register CELL *cell, register int ncols)
{
    register CELL *a;
    a = argv[0];
    while (ncols-- > 0)
	*cell++ = *a++;

    return 0;
}

int 
x_2int (int argc, double *argv[], register CELL *cell, register int ncols)
{
    register double *a,x;
    a = argv[0];
    while (ncols-- > 0)
    {
	if (ISNULL_D(a))
	{
	    SETNULL(cell);
	}
	else
	{
	    x = *a;
	    if (x > HUGE || x < -HUGE)
	    {
		SETNULL(cell);
		overflow_occurred = 1;
	    }
	    else
		*cell = (CELL) x;
	}
	cell++;
	a++;
    }

    return 0;
}

int 
n_2int (int n, char *name)
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
