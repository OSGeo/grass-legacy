/*****************************************************************************/
/***                                                                       ***/
/***                             process()                                 ***/
/***          Reads in a raster file row by row for processing.		   ***/
/***           Jo Wood, Project ASSIST, V1.0 7th February 1993             ***/
/***                                                                       ***/
/*****************************************************************************/

#include "moran.h"

process()
{

    /*--------------------------------------------------------------------------*/
    /*                              INITIALISE					*/
    /*--------------------------------------------------------------------------*/ 


    CELL		*row_in,	/* Buffer large enough to hold `wsize' 	*/
			*row_out,	/* raster rows. When GRASS reads in a	*/
					/* raster row, each element is of type	*/
					/* CELL (probably of type int).		*/

			*window,
			moran_local();	/* Function to calcualted local I stat.	*/

    struct Cell_head	region;		/* Structure to hold region information	*/
    struct Categories	cats;		/* Category file structure for raster	*/

    int			nrows,		/* Will store the current number of 	*/
			ncols,		/* rows and columns in the raster.	*/

			n=0,		/* Number of cells processed.		*/

			row,col,	/* Counts through each row and column 	*/
					/* of the input raster.			*/

			wind_row,	/* Counts through each row and column	*/
			wind_col;	/* of the local neighbourhood window.	*/

    double 		mean=0.0,	/* Mean for Moran's I calculation.	*/
			var=0.0,	/* Variance for Moran's I calculation.	*/
			loc_var=0.0,	/* Average local variance 		*/
			covar[4],	/* Covariance for Moran's I calculation.*/

			lvar();		/* Function to calculate local variance.*/

    covar[0] = 0.0;			/* Initialise all four covariance values*/
    covar[1] = 0.0;
    covar[2] = 0.0;
    covar[3] = 0.0;

    /*--------------------------------------------------------------------------*/
    /*             GET DETAILS OF INPUT RASTER AND RESERVE MEMORY		*/
    /*--------------------------------------------------------------------------*/ 

    G_get_window(&region);		/* Fill out the region structure (the	*/
					/* geographical limits etc.)		*/
					
    nrows = G_window_rows();		/* Find out the number of rows and 	*/
    ncols = G_window_cols();		/* columns of the raster.		*/

    row_in = (CELL *) G_malloc(ncols*sizeof(CELL)*wsize);
					/* Reserve `wsize' rows of memory.	*/

    row_out = G_allocate_cell_buf();	/* Initialise output row buffer. 	*/

    window = (CELL *) G_malloc(wsize*wsize);	
					/* Reserve enough memory for local wind.*/

					/* Find the planimetric resolution.	*/
    if (region.ew_res != region.ns_res)
    {
	G_warning("E-W and N-S grid resolutions are different. Taking average.");
	resoln = (region.ns_res + region.ew_res)/2;
    }
    else
	resoln = region.ns_res;
       

    /*--------------------------------------------------------------------------*/
    /*          PROCESS INPUT RASTER AND WRITE OUT RASTER LINE BY LINE		*/
    /*--------------------------------------------------------------------------*/ 

    if (rast_out_name != NULL)
    {
    	G_zero_cell_buf(row_out);	
    	for (wind_row=0; wind_row<EDGE; wind_row++)
	    G_put_map_row(fd_out,row_out);	/* Write out the edge cells as zeros.	*/
    
    	for (wind_row=0; wind_row<wsize-1; wind_row++)
    	    G_get_map_row(fd_in,row_in+(wind_row*ncols),wind_row);
					/* Read in enough of the first rows to	*/
					/* allow window to be examined.		*/

        for (row=EDGE; row<(nrows-EDGE); row++)
    	{ 
	    G_get_map_row(fd_in,row_in+((wsize-1)*ncols),row+EDGE); 

	    for (col=EDGE; col<(ncols-EDGE); col++)
	    {
	    	for (wind_row=0; wind_row<wsize; wind_row++)
		    for (wind_col=0; wind_col<wsize; wind_col++)
		    	*(window+(wind_row*wsize)+wind_col) = 
				*(row_in+(wind_row*ncols)+col+wind_col-EDGE);

	    	*(row_out+col) = moran_local(window);
		loc_var += lvar(window);
	
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

       	/*------------------------------------------------------------------------------*/
    	/*                CREATE SUPPORT FILES (COLOUR, CATEGORIES ETC)                 */
    	/*------------------------------------------------------------------------------*/

	G_close_cell(fd_out);
    }


    /*--------------------------------------------------------------------------*/
    /*          CALCULATE SINGLE MORAN'S I STATISTIC FOR WHOLE RASTER		*/
    /*--------------------------------------------------------------------------*/ 

    /* STEP ONE - MEAN */

    for (row=1; row<nrows-1; row++)
    {
	G_get_map_row(fd_in,row_in,row);

	for (col=1; col<ncols-1; col++)
	    if ((NoZero) && (*(row_in + col) == 0))
		; /* Do nothing */
	    else
	    {	
	     	mean += *(row_in + col);
		n++;
	    }
    }
    mean /= n;


    /* STEP TWO - VARIANCE */

    for (row=1; row<nrows-1; row++)
    {
	G_get_map_row(fd_in,row_in,row);

	for (col=1; col<ncols-1; col++)
	    if ((NoZero) && (*(row_in + col) == 0))
	    ;	/* Do nothing */
	    else
	    	var += (*(row_in + col) - mean)*(*(row_in + col) - mean);
    }
    var /= n;

    /* STEP THREE - COVARIANCE */

    for (row=1; row<nrows-1; row++)
    {
	G_get_map_row(fd_in,row_in,          row-1);
	G_get_map_row(fd_in,row_in + ncols,  row  );
	G_get_map_row(fd_in,row_in + 2*ncols,row+1);

	for (col=1; col<ncols-1; col++)
    	{
	     /* Covariance measured in [0] -, [1] \, [2] |, [3] / directions. */
	    if ((NoZero) && (*(row_in + ncols + col) == 0))
	    ;	/* Do nothing */
	    else
	    {
	     	covar[0] += 
		    (*(row_in + ncols + col) - mean)*(*(row_in + ncols + col-1) - mean) +
	     	    (*(row_in + ncols + col) - mean)*(*(row_in + ncols + col+1) - mean);

	     	covar[1] += 
		    (*(row_in + ncols + col) - mean)*(*(row_in + col-1) - mean) +
	     	    (*(row_in + ncols + col) - mean)*(*(row_in + ncols*2 + col+1) - mean);

	     	covar[2] +=
		    (*(row_in + ncols + col) - mean)*(*(row_in + col) - mean) +
	     	    (*(row_in + ncols + col) - mean)*(*(row_in + ncols*2 + col) - mean);

	     	covar[3] +=
		    (*(row_in + ncols + col) - mean)*(*(row_in + col+1) - mean) +
	     	    (*(row_in + ncols + col) - mean)*(*(row_in + ncols*2 + col-1) - mean);
	    }
	}
    }
    covar[0] /= 2*n;
    covar[1] /= 2*n;
    covar[2] /= 2*n;
    covar[3] /= 2*n;

    G_unopen_cell(fd_in);

    /*----------------------------------------------------------------*/
    /*                    Report Results of calulations               */
    /*----------------------------------------------------------------*/

    if (Brief)
	printf("Moran = %f\n", (covar[0] + covar[1] + covar[2] + covar[3])/(4*var));
    else
    {
    	printf("===========================================================\n");
    	printf("                  STATISTICAL SUMMARY \n");
    	printf(" --------------------------------------------------------- \n");
    	printf(" Number of cells in distribution           : %d\n",n);
    	printf(" Mean of distribution is                   : %.2f\n",mean);
    	printf(" Variance of distribution is               : %.2f\n\n",var);

    	printf(" Covariance of distribution in - direction : %.2f\n",covar[0]);
    	printf(" Covariance of distribution in \\ direction : %.2f\n",covar[1]);
    	printf(" Covariance of distribution in | direction : %.2f\n",covar[2]);
    	printf(" Covariance of distribution in / direction : %.2f\n\n",covar[3]);

    	printf(" Moran's I (-) is                          : %.2f\n",covar[0]/var);
    	printf(" Moran's I (\\) is                          : %.2f\n",covar[1]/var);
    	printf(" Moran's I (|) is                          : %.2f\n",covar[2]/var);
    	printf(" Moran's I (/) is                          : %.2f\n",covar[3]/var);
    	printf(" Moran's I (average) is                    : %.2f\n\n",
			(covar[0] + covar[1] + covar[2] + covar[3])/(4*var));

	if (rast_out_name != NULL)
   	    printf(" Local variance is                         : %.2f\n\n",loc_var/n);

    	printf("============================================================\n");
    }

    /*--------------------------------------------------------------------------*/
    /*         FREE MEMORY USED TO STORE RASTER ROWS AND LOCAL WINDOW		*/
    /*--------------------------------------------------------------------------*/ 

    free(row_in);
    free(row_out);
    free(window);
}
