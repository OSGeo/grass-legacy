/* changed line 58 for Linux - Markus Neteler (Jan.1998)
/*****************************************************************************/
/***                                                                       ***/
/***                                param()                                ***/
/***     Returns a terrain parameter based on the 6 quadratic coefficents  ***/
/***	 that define a local trend surface. 			    	   ***/
/***     Jo Wood, Department of Geography, V2.0 15th December, 1994        ***/
/***                                                                       ***/
/*****************************************************************************/

#include "param.h"
#include <math.h>


CELL 
param (
    int ptype,		/* Type of terrain parameter to calculate */
    float *coeff		/* Set of six quadratic coefficents.	  */
)
{

    /* Quadratic function in the form of
	
		z = ax^2 + by^2 + cxy + dx + ey +f			  */

    double	a=C_A*zscale,		/* Rescale coefficients if a 	  */
		b=C_B*zscale,		/* Z scaling is required.	  */
		c=C_C*zscale,
		d=C_D*zscale,
		e=C_E*zscale,
		f=C_F;			/* f does not need rescaling as   */
					/* it is only used for smoothing. */

    switch(ptype)
    {
	case ELEV:
		return(RINT(f));
		break;

	case SLOPE:
		return(RINT(atan(sqrt(d*d + e*e))*RAD2DEG));
		break;

	case ASPECT:
		return(RINT(atan2(e,d)*RAD2DEG));
		break;

	case PROFC:
		if ((d == 0) && (e == 0))
		    return((CELL)0);
		else
		    return(RINT(-200.0*resoln*wsize*(a*d*d + b*e*e + c*e*d) /
			   	      ((e*e + d*d) * pow(1.0 + d*d + e*e,1.5)) ));
		break;

	case PLANC:
		if ((d == 0) && (e == 0))
		    return((CELL)0);
		else
		    return(RINT(200.0*resoln*wsize*(b*d*d + a*e*e - c*d*e) /
		 		    /*  powf(e*e + d*d,1.5) )); */
		 		        pow(e*e + d*d,1.5) ));
		break;

	case LONGC:
		if ((d == 0) && (e ==0))
		    return((CELL) 0);
		else	
		    return(RINT(-200.0*resoln*wsize*(a*d*d + b*e*e + c*d*e)/(d*d + e*e)));
		break;

	case CROSC:
		if ((d == 0) && (e ==0))
		    return((CELL) 0);
		else	
		    return(RINT(-200.0*resoln*wsize*(b*d*d + a*e*e - c*d*e)/(d*d + e*e)));
		break;

	case MINIC:
		return(RINT(200.0*resoln*wsize*(-a-b-sqrt((a-b)*(a-b) + c*c))));
		break;

	case MAXIC:
		return(RINT(200.0*resoln*wsize*(-a-b+sqrt((a-b)*(a-b) + c*c))));
		break;
		
	default:
		return((CELL)0);
    }
}
