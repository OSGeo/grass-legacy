/*****************************************************************************/
/***                                                                       ***/
/***                             process()                                 ***/
/***          Reads in a raster file row by row for processing.		   ***/
/***           Jo Wood, Project ASSIST, V1.0 7th February 1993             ***/
/***                                                                       ***/
/*****************************************************************************/

#include "param.h"

process()
{

    /*--------------------------------------------------------------------------*/
    /*                              INITIALISE					*/
    /*--------------------------------------------------------------------------*/ 


    CELL		*row_in,	/* Buffer large enough to hold `wsize' 	*/
			*row_out,	/* raster rows. When GRASS reads in a	*/
					/* raster row, each element is of type	*/
					/* CELL (probably of type int).		*/

			*window_ptr,	/* Stores local terrain window.		*/

			centre;		/* Elevation of central cell in window.	*/

    struct Cell_head	region;		/* Structure to hold region information	*/
    struct Categories	cats;		/* Category file structure for raster	*/

    int			nrows,		/* Will store the current number of 	*/
			ncols,		/* rows and columns in the raster.	*/

			row,col,	/* Counts through each row and column 	*/
					/* of the input raster.			*/

			wind_row,	/* Counts through each row and column	*/
			wind_col,	/* of the local neighbourhood window.	*/

			row_mem,	/* Memory to hold one raster row.	*/

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


    row_in = (CELL *) G_malloc(ncols*sizeof(CELL)*wsize);
					/* Reserve `wsize' rows of memory.	*/

    row_out = G_allocate_cell_buf();	/* Initialise output row buffer. 	*/

    window_ptr = (CELL *) G_malloc(wsize*wsize*sizeof(CELL));	
					/* Reserve enough memory for local wind.*/

    weight_ptr = (double *) G_malloc(wsize*wsize*sizeof(double));	
					/* Reserve enough memory weights matrix.*/

    normal_ptr = matrix(1,6,1,6);	/* Allocate memory for 6*6 matrix	*/
    index_ptr  = ivector(1,6);		/* and for 1D vector holding indices	*/
    obs_ptr    = vector(1,6);		/* and for 1D vector holding observed z */


    /* ---------------------------------------------------------------- */
    /* -            CALCULATE LEAST SQUARES COEFFICIENTS              - */
    /* ---------------------------------------------------------------- */
    
    /*--- Calculate weighting matrix. ---*/

    find_weight(weight_ptr);

    /* Initial coefficients need only be found once since they are 
       constant for any given window size. The only element that 
       changes is the observed vector (RHS of normal equations). */

    /*--- Find normal equations in matrix form. ---*/

    find_normal(normal_ptr,weight_ptr);


    /*--- Apply LU decomposition to normal equations. ---*/

    if (constrained){
	ludcomp(normal_ptr,5,index_ptr);   /* To constrain the quadtratic 
					      through the central cell, ignore 
					      the calculations involving the
					      coefficient f. Since these are 
					      all in the last row and column of
					      the matrix, simply redimension.	*/
	/* disp_matrix(normal_ptr,obs_ptr,obs_ptr,5);
	*/
    }
	
    else{
    	ludcomp(normal_ptr,6,index_ptr);
	/* disp_matrix(normal_ptr,obs_ptr,obs_ptr,6);
	*/
    }


    /*--------------------------------------------------------------------------*/
    /*          PROCESS INPUT RASTER AND WRITE OUT RASTER LINE BY LINE		*/
    /*--------------------------------------------------------------------------*/ 

    G_zero_cell_buf(row_out);	
    for (wind_row=0; wind_row<EDGE; wind_row++)
	G_put_map_row(fd_out,row_out);	/* Write out the edge cells as zeros.	*/
    
    for (wind_row=0; wind_row<wsize-1; wind_row++)
    	G_get_map_row(fd_in,row_in+(wind_row*ncols),wind_row);
					/* Read in enough of the first rows to	*/
					/* allow window to be examined.		*/

    for (row=EDGE; row<(nrows-EDGE); row++)
    { 
	G_percent(row+1,nrows-EDGE,2);

	G_get_map_row(fd_in,row_in+((wsize-1)*ncols),row+EDGE); 

	for (col=EDGE; col<(ncols-EDGE); col++)
	{
					/* Find central z value */
	    centre = *(row_in + EDGE*ncols + col);

	    for (wind_row=0; wind_row<wsize; wind_row++)
		for (wind_col=0; wind_col<wsize; wind_col++)

					/* Express all window values relative	*/
					/* to the central elevation.		*/

		    *(window_ptr+(wind_row*wsize)+wind_col) = 
			*(row_in+(wind_row*ncols)+col+wind_col-EDGE) - centre;



    	    /*--- Use LU back substitution to solve normal equations. ---*/

	    find_obs(window_ptr,obs_ptr,weight_ptr);

    	    			/*	disp_wind(window_ptr); 
					disp_matrix(normal_ptr,obs_ptr,obs_ptr,6);
				*/
	
	    if (constrained) 
	    {
	    	lubksub(normal_ptr,5,index_ptr,obs_ptr);
		/*
	 	   disp_matrix(normal_ptr,obs_ptr,obs_ptr,5);
		*/
	    }

	    else
	    {
	    	lubksub(normal_ptr,6,index_ptr,obs_ptr);
	/*	
	 	  disp_matrix(normal_ptr,obs_ptr,obs_ptr,6);
	*/
		
	    }			

	    /*--- Calculate terrain parameter based on quad. coefficients. ---*/

	    if (mparam == FEATURE)
		*(row_out+col) = feature(obs_ptr);
	    else
	        *(row_out+col) = param(mparam,obs_ptr);

	    if (mparam == ELEV)
		*(row_out+col) += centre;	/* Add central elevation back */
	
	}				
	G_put_map_row(fd_out,row_out);	/* Write the row buffer to the output	*/
					/* raster.				*/
         
					/* 'Shuffle' rows down one, and read in	*/
					/*  one new row.			*/
	for (wind_row=0;wind_row<wsize-1;wind_row++)
	   for (col=0; col<ncols; col++)
            *(row_in+(wind_row*ncols)+col) = *(row_in+((wind_row+1)*ncols)+col);
    }

    G_zero_cell_buf(row_out);	
    for (wind_row=0; wind_row<EDGE; wind_row++)
	G_put_map_row(fd_out,row_out);	/* Write out the edge cells as zeros.	*/

    /*--------------------------------------------------------------------------*/
    /*     FREE MEMORY USED TO STORE RASTER ROWS, LOCAL WINDOW AND MATRICES	*/
    /*--------------------------------------------------------------------------*/ 

    free(row_in);
    free(row_out);
    free(window_ptr);
    free_matrix(normal_ptr,1,6,1,6);
    free_vector(obs_ptr,1,6);
    free_ivector(index_ptr,1,6);
}
