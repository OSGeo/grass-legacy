#include "glob.h"
#include "mapcalc.h"

int execute (char *result)
{
    int nrows, ncols;
    int outfd;
    int stat;

    nrows = G_window_rows();
    ncols = G_window_cols();

    configmaps(ncols); /* set up rowio for neighborhoods */

    for (current_row = 0; current_row < nrows; current_row++)
    {
	G_percent (current_row, nrows, 2);

	execute_stack_depth = 0;
	if(!evaluate(nrows,ncols))
	{
	    fprintf (stderr, "\n** expression evaluation failure **\n");
	    return 0;
	}
	execute_stack_depth = 0;
    
	if (current_row == 0)
	{
	    if (buffer_is_double(0))
		outfd = G_open_fp_cell_new (result);
	    else
		outfd = G_open_cell_new (result);
	    if (outfd < 0)
	    {
		fprintf (stderr, "\nOOPS can't create cell file [%s] (file system full?)\n", result);
		return 0;
	    }
	}
	if (buffer_is_double(0))
	    stat = G_put_d_raster_row (outfd, execute_stack[0].xcell);
	else
	    stat = G_put_c_raster_row (outfd, execute_stack[0].cell);
	if(stat < 0)
	{
	    fprintf (stderr, "\nOOPS can't write cell file (row %d)  (file system full?)\n",current_row);
	    G_unopen_cell (outfd);
	    return 0;
	}
	return_buffer_to_pool (execute_stack[0].n);
    }
    G_percent (current_row, nrows, 2);
    G_close_cell (outfd);
    return 1;
}

int 
free_execute_stack (void)
{
    while (execute_stack_depth-- > 0)
	if (execute_stack[execute_stack_depth].type == MAP)
	    return_buffer_to_pool (execute_stack[execute_stack_depth].n);
    execute_stack_depth = 0;

    return 0;
}
