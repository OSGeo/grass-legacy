/*****************************************************************************/
/***                                                                       ***/
/***                                stats()                                ***/
/***     Returns various local statistics 				   ***/
/***	 that define a local trend surface. 			    	   ***/
/***     Jo Wood, Department of Geography, V2.0 15th December, 1994        ***/
/***                                                                       ***/
/*****************************************************************************/

#include "param.h"
#include <math.h>


CELL 
stats (
    int ptype,		/* Type of terrain parameter to calculate */
    float *coeff,		/* Set of six quadratic coefficents.	  */
    CELL *z,			/* Array storing local window.		  */
    double *w			/* Array storing weights.		  */
)

{

    /* Quadratic function in the form of
	
		z = ax^2 + by^2 + cxy + dx + ey +f			  */

    double	a=C_A,		
		b=C_B,
		c=C_C,
		d=C_D,
		e=C_E,
		f=C_F;		

    int		row,col,		/* Counts through local window.	*/
		x,y,			/* Local window coordinates.	*/
		obs,model;		/* Observed and modelled values.*/

    double	resid = 0.0,		/* Residuals.			*/
		covar = 0.0,
		wsum  = 0.0,
		mean  = 0.0,
		var   = 0.0;
    switch(ptype)
    {
	case RESID:
/*		for (row=0; row<wsize; row++)
		    for (col=0; col<wsize; col++)	
		    {
			y = row-EDGE;
			x = col-EDGE;
			wsum += *(w+row*wsize+col);

			obs   = *(z+row*wsize+col);
			model = a*x*x + b*y*y + c*x*y + d*x + e*y + f;

		    	resid += (*(w+row*wsize+col))*(*(w+row*wsize+col))*(obs-model)*(obs-model); 

		fprintf(stdout, "[%d][%d] obs=%d model=%d\n",x,y,obs,model);
		    }
		resid /= (wsum*wsum);
fprintf(stdout, "resid = %lf obs = %d, mod=%d\n",resid,obs,model);

		return (sqrt(100*resid/(wsize*wsize)));
*/
		return(f - *(z+EDGE*wsize+EDGE));
		break;

	case MORAN:
		*(w + EDGE*wsize + EDGE) = 0;	/* Set central weight to zero */

		for (row=0; row<wsize; row++)
		    for (col=0; col<wsize; col++)		
		    {
			y = row-EDGE;
			x = col-EDGE;

			mean += a*x*x + b*y*y + c*x*y + d*x + e*y + f; 
			/* mean += *(z+row*wsize+col);*/
			wsum += *(w+row*wsize+col);
		    }
		mean /= (wsize*wsize);

		for (row=0; row<wsize; row++)
		    for (col=0; col<wsize; col++)	
		    {
			y = row-EDGE;
			x = col-EDGE;
			obs = *(z + row*wsize+col);

			model  = a*x*x + b*y*y + c*x*y + d*x + e*y + f;
			/* model = *(z+row*wsize+col); */
			var   += (model - mean)*(model - mean);
			/*covar += (model - mean)*(f - mean)*(*(w+row*wsize+col));*/
			covar += (model - mean)*(*(w+row*wsize+col));
fprintf(stdout, "[%d][%d] obs=%d model=%d\n",x,y,obs,model);
		    }
		var /= (wsize*wsize);

		return (1000*covar*((0-mean)/var)/wsum);
		break;
		
	default:
		return((CELL)0);
    }
}
