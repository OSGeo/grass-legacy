/* changes line  37 for Linux - Markus Neteler (Jan. 1998)
/*****************************************************************************/
/***                                                                       ***/
/***                              feature()                                ***/
/***     Returns a terrain feature based on the 6 quadratic coefficents    ***/
/***	 that define a local trend surface. 			    	   ***/
/***     Jo Wood, Department of Geography, V2.1 30th March, 1995           ***/
/***                                                                       ***/
/*****************************************************************************/

#include "param.h"
#include <math.h>


CELL feature(coeff)
    float *coeff;		/* Set of six quadratic coefficents. 	*/

{

    /* Quadratic function in the form of
	
		z = ax^2 + by^2 + cxy + dx + ey +f			*/

    double	a=C_A*zscale,	/* Scale parameters if necessary.	*/
		b=C_B*zscale,
		c=C_C*zscale,
		d=C_D*zscale,
		e=C_E*zscale,
		f=C_F*zscale;

    double maxic,minic,		/* Minimium and maximum curvature.	*/
	   slope,		/* Slope.				*/
	   crosc;		/* Cross-sectional curvature.		*/

    minic = 20*wsize*resoln*(-a-b-sqrt((a-b)*(a-b) + c*c));
    maxic = 20*wsize*resoln*(-a-b+sqrt((a-b)*(a-b) + c*c));
/*    slope = RAD2DEG*atan(sqrtf((d*d) + (e*e)));  */
    slope = RAD2DEG*atan(sqrt((d*d) + (e*e)));     
    crosc = -20*wsize*resoln*(b*d*d + a*e*e - c*d*e)/(d*d + e*e);


    /* Case 1: Surface is sloping. Cannot be a peak,pass or pit. Therefore
	       calculate the cross-sectional curvature to characterise as
	       channel, ridge or planar.				   */

    if (slope > slope_tol)
    {
	if (crosc > curve_tol)
	    return(RIDGE);

	if (crosc < -curve_tol)
	    return(CHANNEL);

	else
	    return(FLAT);
    }


    /* Case 2: Surface has (approximately) vertical slope normal. Feature
	       can be of any type.					  */

    if (maxic > curve_tol)
    {
	if (minic > curve_tol)
	    return(PEAK);

	if (minic < -curve_tol)
	    return(PASS);

	else
	    return (RIDGE);
    }
    else
    	if (minic < -curve_tol)
    	{
	    if (maxic < -curve_tol)
	    	return (PIT);
	
	    else
	   	return (CHANNEL);
    	}

    return (FLAT);
}
