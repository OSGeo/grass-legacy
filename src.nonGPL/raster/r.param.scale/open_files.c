/*****************************************************************************/
/***                                                                       ***/
/***                             open_files()                              ***/
/***   	              Opens input and output raster files.  		   ***/
/***               Jo Wood, Project ASSIST, 24th January 1993              ***/
/***                                                                       ***/
/*****************************************************************************/

#include "param.h"


open_files()
{
    /* Open existing file and set the input file descriptor. */

    if ( (fd_in=G_open_cell_old(rast_in_name,mapset_in)) <0)
    {
        char err[256];
        sprintf(err,"ERROR: Problem opening input file.");
        G_fatal_error(err);
    }

    /* Open new file and set the output file descriptor. */

    if ( (fd_out=G_open_cell_new(rast_out_name)) <0)
    {
        char err[256];
        sprintf(err,"ERROR: Problem opening output file.");
        G_fatal_error(err);
    }
}
