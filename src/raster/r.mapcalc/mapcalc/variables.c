#include "glob.h"

static struct var {
	char *buf;
	int type;
} *var = NULL;
static int nvars = 0;

/* variables are converted to integers by etc/polish, beginning with 0
 * the list of vars is just buffers to copy to/from stack buffers, as if
 * the variable were a map, plus flag to say if buffer is CELL or double
 */
int 
create_variable (int n)
{
    int size;
    if (n >= nvars)
    {
	var = (struct var *)G_realloc (var, (n+1) * sizeof(struct var));
	if (sizeof(CELL) > sizeof(double))
	    size = sizeof(CELL);
	else
	    size = sizeof(double);
/* variables should be created in increasing order, but just in case */
	while (n >= nvars)
	    var[nvars++].buf = G_malloc (size * G_window_cols());
    }

    return 0;
}

int 
copy_cell_to_variable (CELL *cell, int n, int ncols)
{
    CELL *buf;

    buf = (CELL *) var[n].buf;
    while (ncols-- > 0)
    {
	if(ISNULL(cell))
	    SETNULL(buf);
	else
	    *buf = *cell;
	buf++;
	cell++;
    }
    var[n].type = INTEGER;

    return 0;
}

int 
copy_double_to_variable (double *xcell, int n, int ncols)
{
    double *xbuf;

    xbuf = (double *) var[n].buf;
    while (ncols-- > 0)
    {
	if(ISNULL_D(xcell))
	    SETNULL_D(xbuf);
	else
	    *xbuf = *xcell;
	xbuf++;
	xcell++;
    }
    var[n].type = DOUBLE;

    return 0;
}

int 
copy_variable_to_cell (CELL *cell, int n, int ncols)
{
    CELL *buf;

    buf = (CELL *) var[n].buf;
    while (ncols-- > 0)
    {
	if(ISNULL(buf))
	    SETNULL(cell);
	else
	    *cell = *buf;
	buf++;
	cell++;
    }

    return 0;
}

int 
copy_variable_to_double (double *xcell, int n, int ncols)
{
    double *xbuf;

    xbuf = (double *) var[n].buf;
    while (ncols-- > 0)
    {
	if(ISNULL_D(xbuf))
	    SETNULL_D(xcell);
	else
	    *xcell = *xbuf;
	xbuf++;
	xcell++;
    }

    return 0;
}

int 
variable_is_double (int n)
{
    return var[n].type == DOUBLE;
}
