#include "glob.h"
#include "func_proto.h"
#include "mapcalc.h"

char *get_buffer_from_pool();

int buffer_is_double (int s)
{
    return (execute_stack[s].xcell != NULL) ;
}

int convert_to_double (int s)
{
    CELL *cell;
    double *xcell;

    int n;
    int col;

    if (buffer_is_double(s))
	return 0;
    
    xcell = (double *) get_buffer_from_pool (&n);
    execute_stack[s].xcell = xcell;

    cell = execute_stack[s].cell;

    col = G_window_cols();
    while (col-- > 0)
    {
	if (ISNULL(cell))
	{
	    SETNULL_D(xcell);
	}
	else
	{
	    *xcell = *cell;
	}
	cell++;
	xcell++;
    }

    return_buffer_to_pool (execute_stack[s].n);
    execute_stack[s].cell = NULL;
    execute_stack[s].n = n;

    return 0;
}

int convert_to_cell (int s)
{
    CELL *cell;
    double *xcell;

    int n;
    int col;

    if (!buffer_is_double(s))
	return 0;

    cell = (CELL *) get_buffer_from_pool (&n);
    execute_stack[s].cell = cell;

    xcell = execute_stack[s].xcell;

    col = G_window_cols();
    while (col-- > 0)
    {
	if (ISNULL_D(xcell))
	{
	    SETNULL(cell);
	}
	else
	{
	    *cell = round (*xcell);
	}
	cell++;
	xcell++;
    }

    return_buffer_to_pool (execute_stack[s].n);
    execute_stack[s].xcell = NULL;
    execute_stack[s].n = n;

    return 0;
}
