/*****************************************************************************/
/***                                                                       ***/
/***                             close_down()                              ***/
/***   	   Closes all input and output raster files and frees memory.	   ***/
/***               Jo Wood, Project ASSIST, 7th February 1993              ***/
/***                                                                       ***/
/*****************************************************************************/

#include "feature.h"


close_down()
{
    /* Close connection with existing input rasters. */

    G_unopen_cell(fd_feat);
    G_unopen_cell(fd_dem);

    /* Write output raster file and close connection. */

    if(rast_out_name)
    	G_close_cell(fd_out);

}
