/**************************************************************************/
/***                       inter_spline()				***/
/*** Calls spline interpolation but adds constraint between contours.   ***/
/*** Jo Wood, V1.0, December, 1991, V1.2 March 20th, 1992		***/
/*** V2.0 Modified to conform to GRASS module strcture, 23rd July, 1995 ***/
/***                                                                    ***/
/**************************************************************************/

#include "spline.h"

inter_spline(ncells,dem_prof,weight_prof,profile)

int	ncells;
CELL	*dem_prof;
int	*weight_prof,
	profile;

{
    /*------------------------------------------------------------------*/
    /*				INITIALISE				*/
    /*------------------------------------------------------------------*/
    
    int		*x;	/* Three arrays holding tabulated */
    CELL	*z;  	/* functions of form z = f(x) 	  */
    float	*zdash;	/* and zdash = f'(x).		  */

    int		col,
		count=0,
		overshoot=0,
		overshoot_max=0,
		elev, weight;

    float	slope_left  = 0.99e31,
		slope_right = 0.99e31;

    /* Reserve memory for arrays */

    x = 	(int *)   malloc((ncells+1)*sizeof(int));
    z = 	(CELL *)  malloc((ncells+1)*sizeof(CELL));
    zdash = 	(float *) malloc((ncells+1)*sizeof(float));


    /*------------------------------------------------------------------*/
    /*				INTERPOLATE				*/
    /*------------------------------------------------------------------*/
  
    		/* Initialise spline routine by calculating the second
	 		derivatives of the interpolating function.      */

    contract(x,z,dem_prof,ncells,&count);
    init_spline(x,z,count,slope_left,slope_right,zdash);

		/* Gather weights for constrained & unconstrained case.	*/
    for (col=0;col<ncells;col++)
    {
	spline(x,z,zdash,count,col,&elev,weight_prof+col);

	if (*(weight_prof+col) >=100000) /* Set contour weights to 0	*/
	    *(weight_prof+col) = 0;

		/* Cells are further apart for diagonal profiles, 
	   		so reduce weighting be a factor of root two 	*/
	if (profile == 2 || profile == 4)
	    *(weight_prof+col)*=0.707;
    }

    /* For constrained case, add points to the profile where they would	*/
    /* be exceeded in the unconstrained case.				*/

    if ((interval) && (!truncation))
    {
    	for (col=0;col<ncells;col++)
    	{
	    overshoot =spline(x,z,zdash,count,col,&elev,&weight);
		
	    if (abs(overshoot) > abs(overshoot_max))
		overshoot_max = overshoot;

	    if ((overshoot) && (abs(overshoot) < abs(overshoot_max)))
	    {
		*(dem_prof + col) = elev - overshoot;
		contract(x,z,dem_prof,ncells,&count);

		/* Re-initialise with new point. */
printf("Re itiitalising at column %d (of %d)  (overshoot of %d), max overshoot =%d\n",col,ncells,overshoot,overshoot_max);

		init_spline(x,z,count,slope_left,slope_right,zdash);
		overshoot=0;
		overshoot_max=0;
		col=0;
	    }
	}
    }

		/* Gather weights for constrained & unconstrained case.	*/

    init_spline(x,z,count,slope_left,slope_right,zdash);

    for (col=0;col<ncells;col++)	
    {
	overshoot = 0;
	overshoot = spline(x,z,zdash,count,col,dem_prof + col,&weight);
	
	if (overshoot && truncation)
	   *(dem_prof + col) -= overshoot;
    }

    /*------------------------------------------------------------------*/
    /*				FREE MEMORY				*/
    /*------------------------------------------------------------------*/

    free(x);
    free(z);
    free(zdash);
}




/*----------------------------------------------------------------------*/
/*			CONTRACT SPARSE ARRAY				*/
/*----------------------------------------------------------------------*/

contract(x,z,prof,ncells,count)
    int	 *x;
    CELL *z,
    	 *prof;
    int	 ncells;
    int  *count;
{
    int col;
    *count=0;
	 
    /*** First convert sparse row into array that just 
         holds non zero values. NOTE this means that  
	 'genuine' zero values will be lost.          ***/

    for (col=0;col<ncells;col++)
	if (*(prof+col) != 0)
	{
	  (*count)++;
	  *(x+ *count) = col;
	  *(z+ *count) = *(prof+col);
	}

}
