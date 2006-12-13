/**************************************************************************/
/***                                                                    ***/
/***                             close_down()                           ***/
/***       Closes all input and output raster maps and frees memory.	***/
/***                   Jo Wood, V1.0, 13th September, 1994	 	***/
/***                                                                    ***/
/**************************************************************************/

#include "frac.h"

int 
close_down (void)
{
    /* Write output raster map and close connection. */

    G_close_cell(fd_out);
}
