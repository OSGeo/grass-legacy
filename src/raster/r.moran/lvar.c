/*****************************************************************************/
/***                                                                       ***/
/***                      	    lvar()  		          	   ***/
/***     Calculates local and global Moran spatial autocorrelation stats.  ***/
/***           Jo Wood, Project ASSIST, V2.1 3rd April, 1995	           ***/
/***                                                                       ***/
/*****************************************************************************/

#include "moran.h"


double lvar(z)
CELL *z;
{
    /*----------------------------------------------------------------------*/
    /*                                INITIALISE			    */      
    /*----------------------------------------------------------------------*/ 

    int		offset,			/* Counts through local cells.	    */
		row,col,
		n;			/* Number of cells in local window. */

    CELL	total;			/* Total of cells in local window.  */

    double	zbar = 0.0,		/* Mean of cells in local window.   */
		lvar = 0.0;		/* Variance of local cells.	    */

    n = wsize*wsize;

    /*----------------------------------------------------------------------*/
    /*                CALCULATE MEAN, AND VARIANCE OF SAMPLES               */  
    /*----------------------------------------------------------------------*/

    /* 1st pass for the mean */

    for (offset=0; offset<n; offset++)
    	zbar += *(z + offset);
        
    zbar /= (double)n;

    /* 2nd pass for variance */

    for (offset=0; offset<n; offset++)
    	lvar += (*(z+offset)-zbar)*(*(z+offset)-zbar);
    

    lvar /= (double)n;
    return(lvar);
}
