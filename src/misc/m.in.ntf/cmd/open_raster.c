/*****************************************************************************/
/***                                                                       ***/
/***                             open_raster()                             ***/
/***   	              Opens output GRASS raster file.  			   ***/
/***               Jo Wood, Project ASSIST, 29th May 1993                  ***/
/***                                                                       ***/
/*****************************************************************************/

#include "ntf_in.h"

open_raster(xorigin,yorigin,nrows,ncols)
    int	xorigin,yorigin,		/* Origin of SW corner of raster.     	*/
	nrows,ncols;			/* Number of rows and columns in raster.*/
{
    /*--------------------------------------------------------------------------*/
    /*				    INITIALISE					*/
    /*--------------------------------------------------------------------------*/

    struct  Cell_head	region; 	/* Stores GRASS region information.	*/


    /*--------------------------------------------------------------------------*/
    /*		      CHECK OUTPUT RASTER DOES NOT ALREADY EXIST		*/
    /*--------------------------------------------------------------------------*/

    mapset_out = G_mapset();            /* Set output to current mapset.	*/

    if (G_legal_filename(file_out_name)==NULL)
    {
        char err[256];
        sprintf(err,"Illegal GRASS file name. Please try another.");
        G_fatal_error(err);
    }
    else
    {
        if (G_find_cell2(file_out_name,mapset_out) !=NULL)
        {
            char err[256];
            sprintf(err,"Raster map [%s] already exists.\n",file_out_name);
            G_fatal_error(err);
        }
    }


    /*--------------------------------------------------------------------------*/
    /*		        CHECK CURRENT REGION AND CHANGE IF NECESSARY		*/
    /*--------------------------------------------------------------------------*/

    G_get_window(&region);

    if ( (region.ew_res != 50) || (region.ns_res != 50)	||
	 (region.south != yorigin-25.0)			|| 
	 (region.west  != xorigin-25.0) 		||
	 (region.north != yorigin + (50*(nrows-1) + 25))||
	 (region.east  != xorigin + (50*(ncols-1) + 25))  )
    {
        char warn[256];
        sprintf(warn, "Current GRASS region does not match NTF grid.");
        G_warning(warn);

    	region.ew_res = (double) 50.0;
    	region.ns_res = (double) 50.0;
    	region.south  = (double) (yorigin - 25.0);
    	region.west   = (double) (xorigin - 25.0);
	region.north  = (double) (yorigin + (50*(nrows-1) + 25));
	region.east   = (double) (xorigin + (50*(ncols-1) + 25));

    	G_set_window(&region);
    }


    /*--------------------------------------------------------------------------*/
    /*		          OPEN THE NEW RASTER OUTPUT FILE			*/
    /*--------------------------------------------------------------------------*/


    if ( (fd_out=G_open_cell_new(file_out_name)) <0)
    {
        char err[256];
        sprintf(err,"Problem opening output raster file.");
        G_fatal_error(err);
    }

    O_raster = TRUE;			/* Flag to indicate file is open.	*/
}
