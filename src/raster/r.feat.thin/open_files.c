/*****************************************************************************/
/***                                                                       ***/
/***                             open_files()                              ***/
/***   	              Opens input and output raster files.  		   ***/
/***               Jo Wood, Project ASSIST, 24th January 1993              ***/
/***                                                                       ***/
/*****************************************************************************/

#include "feature.h"


open_files()
{
    /* Open existing file and set the input file descriptor. */

    if ( (fd_feat=G_open_cell_old(feat_in_name,mapset1_in)) <0)
    {
        char err[256];
        sprintf(err,"ERROR: Problem opening feature file.");
        G_fatal_error(err);
    }

    if ( (fd_dem=G_open_cell_old(dem_in_name,mapset2_in)) <0)
    {
        char err[256];
        sprintf(err,"ERROR: Problem opening DEM file.");
        G_fatal_error(err);
    }


    /* Open new file and set the output file descriptor. */

    if(rast_out_name)
    {
    	if ( (fd_out=G_open_cell_new(rast_out_name)) <0)
    	{
            char err[256];
            sprintf(err,"ERROR: Problem opening output file.");
            G_fatal_error(err);
        }
    }


    /* Display site header information */

    if (sites)
    {
	printf("name|Features\n");
	printf("desc|DEM Very Important Points (VIPs - pits, peaks, & passes)\n");
    }

}
