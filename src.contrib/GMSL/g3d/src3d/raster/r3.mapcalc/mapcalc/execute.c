#include "glob.h"

execute(result)
    char *result;
{
    int nrows, ncols, ndepths;
    void *outfd;
    int stat;
    int i;
    double value;
   
    nrows=current_region.rows;
    ncols=current_region.cols;
    ndepths=current_region.depths;

    for (current_depth = 0; current_depth < ndepths; current_depth++) {
      for (current_row = 0; current_row < nrows; current_row++)
      {
        G_percent (current_depth, ndepths, 2);
        execute_stack_depth = 0;
	    if(!evaluate(nrows,ncols,ndepths))
	    {
	      fprintf (stderr, "\n** expression evaluation failure **\n");
	      return 0;
	    }
        execute_stack_depth = 0;

	    if (current_row == 0 && current_depth == 0)
	    {
            outfd = G3d_openCellNew (result, G3D_DOUBLE,  G3D_USE_CACHE_DEFAULT, &current_region);
            if (outfd == NULL) {
		       fprintf (stderr, "\nOOPS can't create cell file [%s]\n", result);
		       return 0;
			}
		}
        for (i=0;i<ncols;i++)
        {
			if (ISNULL_D(&execute_stack[0].xcell[i]))
              G3d_setNullValue (&execute_stack[0].xcell[i], 1, G3D_DOUBLE);
            G3d_putDouble (outfd, i, current_row, current_depth, execute_stack[0].xcell[i]);
        }
		return_buffer_to_pool (execute_stack[0].n);
      }
    }
    G_percent (current_depth, ndepths, 2);

    if (! G3d_closeCell (outfd))
    {
        fprintf (stderr, "\nOOPS can't close cell file [%s]\n", result);
        return 0;
    }
    return 1;
}

free_execute_stack()
{
    while (execute_stack_depth-- > 0)
	if (execute_stack[execute_stack_depth].type == MAP)
	    return_buffer_to_pool (execute_stack[execute_stack_depth].n);
    execute_stack_depth = 0;
}
