/***************************************************************************/
/***                                                                     ***/
/***                             process()                               ***/
/***          Reads in a raster files row by row for processing          ***/
/***                  Jo Wood, V2.1, 18th July, 1995			 ***/
/***                                                                     ***/
/***************************************************************************/

#include "feature.h"

process()
{

    /*------------------------------------------------------------------*/
    /*                              INITIALISE                         	*/
    /*------------------------------------------------------------------*/ 

    CELL        *raster,   	/* Buffers to hold entire raster.	*/
		*row_dem,	/* Stores DEM row at a time.		*/
		*row_out,	/* Output row from raster.		*/
		feature; 	/* Surface feature.			*/

    int         row,col;        /* Counts through each row and column   */
                                /* of the input raster.                 */

    struct Cell_head region;	/* Geo-ref coordinates of raster.	*/
                           

    /*------------------------------------------------------------------*/
    /*                       GET DETAILS OF INPUT RASTER                */
    /*------------------------------------------------------------------*/ 

    row_dem = G_allocate_cell_buf(); /* Allocate row buffer for input.	*/
    row_out = G_allocate_cell_buf(); /* Allocate row buffer for output.	*/
                                        
    nrows = G_window_rows();         /* Find out the number of rows and	*/
    ncols = G_window_cols();         /* columns of the raster view.	*/

    G_get_window(&region);           /* Fill out the region structure	*/
                                     /* (the geographical limits etc.)	*/
                                        

    /*------------------------------------------------------------------*/
    /*   	        READ ENTIRE INPUT RASTER INTO MEMORY		*/
    /*------------------------------------------------------------------*/ 

    raster = (CELL *) G_malloc(ncols*nrows*sizeof(CELL));
                                     /* Reserve enough memory for rast. */

    for(row=0; row<nrows; row++)
        G_get_map_row(fd_feat, (raster + row*ncols),row);


    /*------------------------------------------------------------------*/
    /*      PROCESS INPUT RASTERS AND WRITE OUT RASTER LINE BY LINE	*/
    /*------------------------------------------------------------------*/ 
                                                                             
    for(row=0; row<nrows; row++)
    {
	G_get_map_row(fd_dem,row_dem,row);

				/* Write out corner values if requested */
	if (((row==0) || (row==nrows-1)) && (sites))
	{
	    write_site(region.west  + (0.5*region.ew_res),
		       region.north - ((row+0.5)*region.ns_res),
		       *(row_dem),
		       *(raster + row*ncols));
			
	    write_site(region.east  - (0.5*region.ew_res),
		       region.north - ((row+0.5)*region.ns_res),
		       *(row_dem+ ncols-1),
		       *(raster + row*ncols + ncols-1));
	}

        for(col = 0; col < ncols; col++)
        {
	    /* Process input file */

	    if ((*(raster + row*ncols + col) == PIT) ||
		(*(raster + row*ncols + col) == PEAK)||
		(*(raster + row*ncols + col) == PASS))
	    	centroid(raster,row,col);	/* Erases all but the centroid 
						   of a contiguous block. */
	    else
		*(raster + row*ncols + col) = (CELL)0;

	    /* Create output */

	    feature = *(raster + row*ncols + col);

	    if (rast_out_name)
		*(row_out+col) = feature;

   	    if ((sites) && (feature))
		write_site(     region.west  + ((col+.5)*region.ew_res),
                                region.north - ((row+.5)*region.ns_res),
				*(row_dem+col),
                                feature);	    
        }
	
        if (rast_out_name)
	    G_put_map_row(fd_out,row_out);
    }


    /* Free memory used to hold raster */

    free(raster);
}

