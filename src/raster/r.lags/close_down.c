/*****************************************************************************/
/***                                                                       ***/
/***                             close_down()                              ***/
/***   	   Closes all input and output raster files and frees memory.	   ***/
/***               Jo Wood, Project ASSIST, 7th February 1993              ***/
/***                                                                       ***/
/*****************************************************************************/

#include "lags.h"


close_down()
{
    /* Close connection with existing input raster. */

    G_unopen_cell(fd_in);

    /* Write output raster file and close connection. */


    if (measure==MORAN)
    {
    	G_close_cell(fd1_out);
/*
	G_close_cell(fd2_out);
*/
    }

    if (measure==TEXTURAL)
    {
	G_close_cell(fd_con);
	G_close_cell(fd_asmo);
	G_close_cell(fd_ent);
	G_close_cell(fd_asym);
	G_close_cell(fd_idm);
    }
}
