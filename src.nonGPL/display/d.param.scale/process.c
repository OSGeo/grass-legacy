/*****************************************************************************/
/***                                                                       ***/
/***                             process()                                 ***/
/***          Reads in a raster file row by row for processing.		   ***/
/***           Jo Wood, Project ASSIST, V1.0 7th February 1993             ***/
/***                                                                       ***/
/*****************************************************************************/

#include <stdlib.h>
#include "raster.h"
#include "display.h"
#include "param.h"
#include "local_proto.h"

int process (void)
{

    /*--------------------------------------------------------------------------*/
    /*                              INITIALISE					*/
    /*--------------------------------------------------------------------------*/ 


    CELL		*rast_ptr,	/* Buffer large enough to hold DEM. 	*/

			*window_ptr,	/* Stores local terrain window.		*/

			centre;		/* Elevation of central cell in window.	*/

    struct Cell_head	region;		/* Structure to hold region information	*/

    int			nrows,		/* Will store the current number of 	*/
			ncols,		/* rows and columns in the raster.	*/

			row,		/* Counts through each row of raster. 	*/

			scr_x,scr_y,	/* Mouse screen coordinates.		*/
			dem_x,dem_y,	/* Mouse DEM coordinates.		*/

			button,		/* Mouse button pressed.		*/

			wind_row,	/* Counts through each row and column	*/
			wind_col,	/* of the local neighbourhood window.	*/

			mwsize = wsize,	/* Maximum local window size.		*/

			*param_ptr,	/* Stores parameters at each scale.	*/

			*index_ptr;	/* Row permutation vector for LU decomp.*/

    float		**normal_ptr,	/* Cross-products matrix.		*/
			*obs_ptr;	/* Observed vector.			*/

    double		*weight_ptr;	/* Weighting matrix for observed values.*/


    /*--------------------------------------------------------------------------*/
    /*               	   GET RASTER AND WINDOW DETAILS			*/
    /*--------------------------------------------------------------------------*/ 

    G_get_window(&region);		/* Fill out the region structure (the	*/
					/* geographical limits etc.)		*/
					
    nrows = G_window_rows();		/* Find out the number of rows and 	*/
    ncols = G_window_cols();		/* columns of the raster.		*/


    if ((region.ew_res/region.ns_res >= 1.01) || /* If EW and NS resolns are	*/
	(region.ns_res/region.ew_res >= 1.01))   /* >1% different, warn user.	*/
    {
	G_warning("E-W and N-S grid resolutions are different. Taking average.");
	resoln = (region.ns_res + region.ew_res)/2;
    }
    else
	resoln = region.ns_res;



    /*--------------------------------------------------------------------------*/
    /*              RESERVE MEMORY TO HOLD Z VALUES AND MATRICES		*/
    /*--------------------------------------------------------------------------*/ 

					/* Reserver enough memory for raster.	*/
    rast_ptr   = (CELL *) G_malloc(ncols*nrows*sizeof(CELL));

					/* Reserve enough memory for local wind.*/
    window_ptr = (CELL *) G_malloc(wsize*wsize*sizeof(CELL));	

					/* Reserve enough memory for parameters.*/
    param_ptr  = (int *) G_malloc(mwsize*sizeof(int));

					/* Reserve enough memory weights matrix.*/
    weight_ptr = (double *) G_malloc(wsize*wsize*sizeof(double));	

    normal_ptr = matrix(1,6,1,6);	/* Allocate memory for 6*6 matrix	*/
    index_ptr  = ivector(1,6);		/* and for 1D vector holding indices	*/
    obs_ptr    = vector(1,6);		/* and for 1D vector holding observed z */

					/* Read entire raster into memory.	*/
    for (row=0; row<nrows; row++)
    	G_get_map_row(fd_in,rast_ptr + row*ncols,row);



    /* ---------------------------------------------------------------- */
    /* -            CONTROL LOOP BASED ON MOUSE LOCATION              - */
    /* ---------------------------------------------------------------- */

    do
    {
			/* Get array coordinates from mouse location.	*/
	R_get_location_with_pointer(&scr_x,&scr_y,&button);
	dem_x = (int)D_d_to_a_col((double)scr_x);
	dem_y = (int)D_d_to_a_row((double)scr_y);

			/* Only procede if correct mouse button is
			   pressed and mouse is within DEM.		*/
	if ((button == 3) || (dem_x < 1) || (dem_x > ncols-2) || 
	  	  	     (dem_y < 1) || (dem_y > nrows-2))
	    continue;

	for (wsize = 3; wsize <=mwsize; wsize += 2)
	{
			/* Only procede if local window can be 
			   extracted around the mouse location.		*/
	    if ( (dem_x < EDGE) || (dem_x > ncols-EDGE-1) ||
		 (dem_y < EDGE) || (dem_y > nrows-EDGE-1))
		break;


    	    /* -------------------------------------------------------- */
    	    /* -         CALCULATE LEAST SQUARES COEFFICIENTS	      - */
    	    /* -------------------------------------------------------- */
    
    	    /*--- Calculate weighting matrix. ---*/

    	    find_weight(weight_ptr);

    	    /*--- Find normal equations in matrix form. ---*/

    	    find_normal(normal_ptr,weight_ptr);


    	    /*--- Apply LU decomposition to normal equations. ---*/

    	    /* To constrain the quadratic through the central cell, ignore
       	       the calculations involving the coefficient f. Since these are
       	       all in the last row and column of the matrix, simply redimension.*/

    	    if (constrained)
		ludcomp(normal_ptr,5,index_ptr);  
    	    else
    		ludcomp(normal_ptr,6,index_ptr);


    	    /*----------------------------------------------------------*/
    	    /*  PROCESS LOCAL WINDOW AND CALCULATE MORPHOMETRIC PARAM.	*/
    	    /*----------------------------------------------------------*/ 
    
					/* Find central z value */
	    centre = *(rast_ptr + dem_y*ncols + dem_x);

	    for (wind_row=0; wind_row<wsize; wind_row++)
		for (wind_col=0; wind_col<wsize; wind_col++)

					/* Express all window values relative	*/
					/* to the central elevation.		*/
		    *(window_ptr+(wind_row*wsize)+wind_col) = 
			*(rast_ptr+(dem_y+wind_row-EDGE)*ncols + dem_x+wind_col-EDGE)
			- centre;


    	    /*--- Use LU back substitution to solve normal equations. ---*/

	    find_obs(window_ptr,obs_ptr,weight_ptr);

	    if (constrained) 
	    	lubksub(normal_ptr,5,index_ptr,obs_ptr);
	    else
	    	lubksub(normal_ptr,6,index_ptr,obs_ptr);


	    /*--- Calculate terrain parameter based on quad. coefficients. ---*/

	    switch(mparam)
	    {
		case (FEATURE):
		    *(param_ptr + (wsize-3)/2) = feature(obs_ptr);
		    break;

		case (ELEV):
		    *(param_ptr + (wsize-3)/2) = param(mparam,obs_ptr) + centre;
		    break;

		case (RESID):
		case (MORAN):
		case (GEARY):
		    *(param_ptr + (wsize-3)/2) = 
					stats(mparam,obs_ptr,window_ptr,weight_ptr);
		    break;

		default:
		    *(param_ptr + (wsize-3)/2) = param(mparam,obs_ptr);
		break;
	    }
	}				

    	/*----------------------------------------------------------------------*/
    	/*	  	REPORT PARAMETER VALUES AS GRAPH AND TEXT		*/
    	/*----------------------------------------------------------------------*/

	wsize -= 2;			/* Set wsize back to original.		*/
    	disp_graph(scr_x,scr_y,param_ptr,mwsize);

    }
    while (button != 3);



    /*--------------------------------------------------------------------------*/
    /*       FREE MEMORY USED TO STORE RASTER, LOCAL WINDOW AND MATRICES	*/
    /*--------------------------------------------------------------------------*/ 

    free(rast_ptr);
    free(window_ptr);
    free(param_ptr);
    free_matrix(normal_ptr,1,6,1,6);
    free_vector(obs_ptr,1,6);
    free_ivector(index_ptr,1,6);

    return 0;
}
