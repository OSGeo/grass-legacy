#include "glob.h"

static struct var {
	char *buf;
} *var = NULL;
static int nvars = 0;

/* variables are converted to integers by etc/polish, beginning with 0
 * the list of vars is just buffers to copy to/from stack buffers, as if
 * the variable were a map, plus flag to say if buffer is CELL or double
 */
create_variable(n)
{
    int size;
    if (n >= nvars)
    {
	var = (struct var *)G3d_realloc (var, (n+1) * sizeof(struct var));

        size = sizeof(double);

        /* variables should be created in increasing order, but just in case */
	while (n >= nvars)
	    var[nvars++].buf = G3d_malloc (size * /*G_window_cols()*/ current_region.cols);
    }
}

copy_double_to_variable (xcell, n, ncols)
    double *xcell;
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
}

copy_variable_to_double (xcell, n, ncols)
    double *xcell;
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
}
