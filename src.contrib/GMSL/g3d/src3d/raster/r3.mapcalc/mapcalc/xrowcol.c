#include "gis.h"
#include "glob.h"
/****************************************************************
row() row number
col() col number
depth() depth number
****************************************************************/

x_depth (argc, argv, xcell, ncols)
    double *argv[];
    double *xcell;
    register int ncols;
{
    int depth;

    depth = current_depth+1;
    while (ncols-- > 0)
    {
	*xcell++ = (double) depth;
    }
}

n_depth(n,name) char *name;
{
    if (n == 0) return 1;
    fprintf (stderr, "%s - ",name);
    fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s()\n",name);
    return 0;
}

x_row (argc, argv, xcell, ncols)
    double *argv[];
    double *xcell;
    register int ncols;
{
    int row;

    row = current_row+1;
    while (ncols-- > 0)
    {
	*xcell++ = (double) row;
    }
}

n_row(n,name) char *name;
{
    if (n == 0) return 1;
    fprintf (stderr, "%s - ",name);
    fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s()\n",name);
    return 0;
}

x_col (argc, argv, xcell, ncols)
    double *argv[];
    double *xcell;
    register int ncols;
{
    int col;

    col = 1;
    while (ncols-- > 0)
    {
	*xcell++ = (double) col++;
    }
}

n_col(n,name) char *name;
{
    if (n == 0) return 1;
    fprintf (stderr, "%s - ",name);
    fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s()\n",name);
    return 0;
}
