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
create_variable(n)
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
}

copy_cell_to_variable (cell, n, ncols)
    CELL *cell;
{
    CELL *buf;

    buf = (CELL *) var[n].buf;
    while (ncols-- > 0)
	*buf++ = *cell++;
    var[n].type = INTEGER;
}

copy_double_to_variable (cell, n, ncols)
    double *cell;
{
    double *buf;

    buf = (double *) var[n].buf;
    while (ncols-- > 0)
	*buf++ = *cell++;
    var[n].type = DOUBLE;
}

copy_variable_to_cell (cell, n, ncols)
    CELL *cell;
{
    CELL *buf;

    buf = (CELL *) var[n].buf;
    while (ncols-- > 0)
	*cell++ = *buf++;
}

copy_variable_to_double (cell, n, ncols)
    double *cell;
{
    double *buf;

    buf = (double *) var[n].buf;
    while (ncols-- > 0)
	*cell++ = *buf++;
}

variable_is_double(n)
{
    return var[n].type == DOUBLE;
}
