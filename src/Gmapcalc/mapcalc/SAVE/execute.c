#include "glob.h"
execute(outfd)
{
    int row, nrows, ncols;
    CELL *mask;
    int maskfd;

    nrows = G_window_rows();
    ncols = G_window_cols();

    mask = NULL;
    if((maskfd = G_maskfd()) > 0)
	mask = G_allocate_cell_buf();

    printf ("EXECUTING ... ");
    for (row = 0; row < nrows; row++)
    {
	percent (row, nrows, 5);

	execute_stack_depth = 0;
	if(!evaluate(row,nrows,ncols))
	{
	    fprintf (stderr, "** expression evaluation failure **\n");
	    return 0;
	}
	execute_stack_depth = 0;
    
	if (mask)
	{

	    if (G_get_map_row (maskfd, mask, row) < 0)
		G_fatal_error ("unable to read MASK file");
	    apply_mask (mask, execute_stack[0].cell, ncols);
	}

	if(G_put_map_row (outfd, execute_stack[0].cell) < 0)
	{
	    fprintf (stderr, "OOPS can't write cell file (row %d)\n",row);
	    return 0;
	}
	if (row == 0)
	    min_value = max_value = execute_stack[0].cell[0];
	find_min_max (execute_stack[0].cell, ncols);
	return_buffer_to_pool (execute_stack[0].n);
    }
    percent (row, nrows, 5);
    if (mask)
	free (mask);
    return 1;
}

free_execute_stack()
{
    while (execute_stack_depth-- > 0)
	if (execute_stack[execute_stack_depth].type == MAP)
	    return_buffer_to_pool (execute_stack[execute_stack_depth].n);
    execute_stack_depth = 0;
}

apply_mask(mask, cell, ncols)
    register CELL *mask, *cell;
    register int ncols;
{
    while (ncols-- > 0)
	if (*mask++ == 0)
	    *cell++ = 0;
	else
	    cell++;
}
