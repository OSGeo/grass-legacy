/**************************************************************************/
/***                                                                    ***/
/***                             close_down()                           ***/
/***       Closes all input and output raster files and frees memory.	***/
/***                   Jo Wood, V1.0, 13th September, 1994	 	***/
/***                                                                    ***/
/**************************************************************************/

#include "frac.h"

close_down()
{
    /* Write output raster file and close connection. */

    G_close_cell(fd_out);
}
