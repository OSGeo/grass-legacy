/************************************************************************/
/*			  	  autocorr()				*/
/*	    Function to calulate Moran's I at all possible lags		*/
/*									*/
/************************************************************************/

#include "lags.h"

autocorr()
{
    /*------------------------------------------------------------------*/
    /*				INITIALISE			     	*/
    /*------------------------------------------------------------------*/

    CELL    *raster,		/* Buffers hold one raster row.		*/
	    *moran,		/* Moran measure.			*/
	    *samples;		/* Number of samples at each lag.	*/

    int	    row,col,
            nrows,ncols;	/* Number of cell rows and columns.	*/

    int     n=0,		/* Sample size of i or j.		*/
	    zi,zj;		/* The two raster values.		*/

    double  zibar=0.0,		/* Moment parameters of i and j.	*/
	    zjbar=0.0,
	    vari=0.0,
	    varj=0.0,
	    var=0.0,
    	    covar=0.0;		/* Covaraince measure			*/

    int	    xoffset,		/* 2D lag vector displacement.		*/
	    yoffset;	

    nrows = G_window_rows();	/* Find dimensions of raster.		*/
    ncols = G_window_cols();

				/* Reserve memory for entire raster.	*/
    raster = (CELL*) G_malloc(nrows*ncols*sizeof(CELL));

    moran   = G_allocate_cell_buf();
    samples = G_allocate_cell_buf();
	


    /*------------------------------------------------------------------*/
    /*		    CALCULATE AUTOCORRELATION STATISTICS 		*/
    /*------------------------------------------------------------------*/


    printf("STAGE ONE - Reading in data\n");

    for (row=0;row<nrows; row++)
	G_get_map_row(fd_in,raster+ncols*row,row);


    printf("STAGE TWO - Calculating statistics\n");

    for(yoffset=-1*(nrows/2);yoffset<(nrows/2);yoffset++)
    {
    	for(xoffset=-1*(ncols/2);xoffset<(ncols/2);xoffset++)
	{
	    G_percent(yoffset+(nrows/2),nrows,1);


	    /* First pass - find number of samples and their mean */
	    /* -------------------------------------------------- */

	    for (row=0;row<nrows;row++)
	 	if ((row+yoffset<nrows) && (row+yoffset>=0))
                    for (col=0;col<ncols;col++)
		   	if ((col+xoffset<ncols) && (col+xoffset>=0))
		 	{
			    zi = *(raster + row*ncols + col);
			    zj = *(raster + (row+yoffset)*ncols + col+xoffset);
			    
			    if (vario)
				var += (zi-zj)*(zi-zj);
			    else
			    {
			    	zibar += zi;
			   	zjbar += zj;
			    }
			    n++;
			}

	    if (vario)			/* For variogram, we can leave loop */
	    {				/* before calculating co-variance.  */
	    	moran[xoffset+(ncols/2)] = var/(2*n);
		var = 0.0;
		n = 0;
		continue;
	    }

	if (vario)
	   fprintf(stderr,"Error: shouldnt be here.");

	    zibar /= n;
	    zjbar /= n;
	    
	    /* Second pass - find variance and co-variance  */
	    /* -------------------------------------------- */

	    for (row=0;row<nrows;row++)
	 	if ((row+yoffset<nrows) && (row+yoffset>=0))
                    for (col=0;col<ncols;col++)
		   	if ((col+xoffset<ncols) && (col+xoffset>=0))
		 	{
			    zi = *(raster + row*ncols + col);
			    zj = *(raster + (row+yoffset)*ncols + col+xoffset);
			    covar += (zi - zibar) * (zj - zjbar);
			    vari  += (zi - zibar) * (zi - zibar);

			    if ( (row + yoffset*2 >=nrows) ||
				 (row + yoffset*2 < 0)     ||
				 (col + xoffset*2 >=ncols) ||
				 (col + xoffset*2 < 0))
			    	varj  += (zj - zjbar) * (zj - zjbar);
			    	
			}
	    var = (vari + varj);	/* Note that varj is in fact the */
					/* var of var(i U j) - var (i)   */

            if (var != 0.0)
	    	moran[xoffset+(ncols/2)] = 1000.0*covar/var;
	    else
	    	moran[xoffset+(ncols/2)] = 1000;

	    zibar= 0.0;
	    zjbar= 0.0;
	    n    = 0;
	    covar= 0.0;
	    vari = 0.0;
	    varj = 0.0;
	   
	}

	/* Output raster row */
	/* ----------------- */

	G_put_map_row(fd1_out,moran);
    }

    free(raster);
}
