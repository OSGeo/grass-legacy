#include "gis.h"

/**********************************************************************
round(x)

  rounds x to nearest integer.

  if input is CELL (which is an integer already)
  the input argument (argv[0]) is simply copied to the output cell.

  if the input is double, the input is rounded by adding .5 to positive
  numbers, and subtracting .5 from negatives.
**********************************************************************/

i_round (argc, argv, cell, ncols)
    CELL *argv[];
    register CELL *cell;
    register int ncols;
{
    register CELL *a;
    a = argv[0];
    while (ncols-- > 0)
	*cell++ = *a++;
}

x_round (argc, argv, cell, ncols)
    double *argv[];
    register CELL *cell;
    register int ncols;
{
    register double *a;
    a = argv[0];
    while (ncols-- > 0)
	*cell++ = round (*a++);
}

n_round(n,name) char *name;
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
