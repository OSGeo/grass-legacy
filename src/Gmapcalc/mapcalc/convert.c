#include "glob.h"

buffer_is_double (s)
{
    return (execute_stack[s].xcell != NULL) ;
}

convert_to_double (s)
{
    CELL *cell;
    double *xcell;
    char *get_buffer_from_pool();

    int n;
    int col;

    if (buffer_is_double(s))
	return;
    
    xcell = (double *) get_buffer_from_pool (&n);
    execute_stack[s].xcell = xcell;

    cell = execute_stack[s].cell;

    col = G_window_cols();
    while (col-- > 0)
	*xcell++ = *cell++;
    
    return_buffer_to_pool (execute_stack[s].n);
    execute_stack[s].cell = NULL;
    execute_stack[s].n = n;
}

convert_to_cell (s)
{
    CELL *cell;
    double *xcell;

    int n;
    int col;

    if (!buffer_is_double(s))
	return;

    cell = (CELL *) get_buffer_from_pool (&n);
    execute_stack[s].cell = cell;

    xcell = execute_stack[s].xcell;

    col = G_window_cols();
    while (col-- > 0)
	*cell++ = round (*xcell++);

    return_buffer_to_pool (execute_stack[s].n);
    execute_stack[s].xcell = NULL;
    execute_stack[s].n = n;
}
