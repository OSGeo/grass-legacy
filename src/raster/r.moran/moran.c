/*****************************************************************************/
/***                                                                       ***/
/***                      moran() and moran_local()  		           ***/
/***     Calculates local and global Moran spatial autocorrelation stats.  ***/
/***           Jo Wood, Project ASSIST, V2.0 21st October, 1994            ***/
/***                                                                       ***/
/*****************************************************************************/

#include "moran.h"


CELL moran_local(z)
CELL *z;
{
    /*----------------------------------------------------------------------*/
    /*                                INITIALISE			    */      
    /*----------------------------------------------------------------------*/ 

    int		offset,			/* Counts through local cells.	    */
		row,col,
		n;			/* Number of cells in local window. */

    CELL	total;			/* Total of cells in local window.  */

    double	zbar  =0.0,		/* Mean of cells in local window.   */
		var   =0.0,		/* Variance of local cells.	    */
		covarH=0.0,		/* Covariance of cells in window.   */
		covarV=0.0;

    n = wsize*wsize;

    /*----------------------------------------------------------------------*/
    /*          CALCULATE MEAN, VARIANCE AND COVARIANCE OF SAMPLES          */  
    /*----------------------------------------------------------------------*/

    /* 1st pass for the mean */

    for (offset=0; offset<n; offset++)
    	zbar += *(z + offset);
        
    zbar /= (double)n;

    /* 2nd pass for variance */

    for (offset=0; offset<n; offset++)
    	var += (*(z+offset)-zbar)*(*(z+offset)-zbar);

    var /= (double)n;

    /* 3rd pass for the vertical covariance */

    for (row=0; row<wsize; row++)
	for (col=0;col<wsize-1; col++)
	    covarV += (*(z + row*wsize+col)-zbar) * (*(z + row*wsize+col+1)-zbar);
   
    /* 4th pass for the horizontal covariance */ 

    for (row=0; row<wsize-1; row++)
	for (col=0;col<wsize; col++)
	    covarH += (*(z + row*wsize+col)-zbar) * (*(z + (row+1)*wsize+col)-zbar);

    covarH /= (double)n-wsize;
    covarV /= (double)n-wsize;

    if (var == 0)
	return(100);
    else
    	return(100.0*(covarH+covarV)/(2*var));
}
