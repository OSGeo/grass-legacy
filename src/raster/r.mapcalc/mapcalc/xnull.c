#include "gis.h"
#include "glob.h"
/****************************************************************
null() null values
****************************************************************/

int 
i_null (int argc, CELL *argv[], register CELL *cell, register int ncols)
{
    while (ncols-- > 0)
    {
	SETNULL(cell++);
    }

    return 0;
}

int 
x_null (int argc, double *argv[], register double *xcell, register int ncols)
{
    while (ncols-- > 0)
    {
	SETNULL_D(xcell++);
    }

    return 0;
}

int 
n_null (int n, char *name)
{
    if (n == 0) return 1;
    fprintf (stderr, "%s - ",name);
    fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s()\n",name);
    return 0;
}
