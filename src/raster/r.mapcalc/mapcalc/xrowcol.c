#include "gis.h"
#include "glob.h"
/****************************************************************
row() row number
col() col number

****************************************************************/

int 
i_row (int argc, CELL *argv[], register CELL *cell, register int ncols)
{
    int row;

    row = current_row+1;
    while (ncols-- > 0)
    {
	*cell++ = row;
    }

    return 0;
}

int 
n_row (int n, char *name)
{
    if (n == 0) return 1;
    fprintf (stderr, "%s - ",name);
    fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s()\n",name);
    return 0;
}

int 
i_col (int argc, CELL *argv[], register CELL *cell, register int ncols)
{
    int col;

    col = 1;
    while (ncols-- > 0)
    {
	*cell++ = col++;
    }

    return 0;
}

int 
n_col (int n, char *name)
{
    if (n == 0) return 1;
    fprintf (stderr, "%s - ",name);
    fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s()\n",name);
    return 0;
}
