/*****************************************************************************/
/***                                                                       ***/
/***                             open_files()                              ***/
/***           Opens input and output raster files for r.example	   ***/
/***                    Jo Wood, V1.0, 13th September, 1994                ***/
/***                                                                       ***/
/*****************************************************************************/

#include "frac.h"

int 
open_files (void)
{
    /* Open new file and set the output file descriptor. */

    if ( (fd_out=G_open_cell_new(rast_out_name)) <0)
    {
        char err[256];
        sprintf(err,"ERROR: Problem opening output file.");
        G_fatal_error(err);
    }

}
