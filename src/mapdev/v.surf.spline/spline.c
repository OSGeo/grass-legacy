/**************************************************************************/
/***                 spline related functions				***/
/*** Taken from Press, Flannery, Teukolsky & Vetterling (1988) p.85	***/
/*** Modified to incorporate constraints and GRASS functionality	***/
/*** Jo Wood, v1.0, December, 1991, v2.0, July 1995.			***/
/***									***/
/**************************************************************************/

#include "spline.h"

#define F (float)

init_spline(x,z,nvals,slope_left,slope_right,zdash)

int	*x;		/* Tabulated x values */
CELL	*z;		/* Tabulated z values [ z = fn(x)] */
int	nvals;		/* Number of tabulated values */
float	slope_left,	/* LHS boundary slope */
	slope_right,	/* RHS slope (if >0.99+E30, 2nd deriv=0) */
	*zdash;		/* Tabulated z derivatives [zdash = dz/dx] */
{
    int		i,k;
    float	p,qn,
		sig,
		un,
		*u,
		*vector();

    u=vector(1,nvals-1);

    if (slope_left > 0.99e30)
	*(zdash+1) = u[1] = 0.0;
    else
    {
	*(zdash+1) = -0.5;
	u[1] = (3.0/(F*(x+2)-F*(x+1)))*((F*(z+2)-F*(z+1))/(F*(x+2)-F*(x+1))-slope_left);
    }

    for (i=2;i<=nvals-1;i++)
    {
	sig = (F*(x+i)-F*(x+i-1))/(F*(x+i+1)-F*(x+i-1));
	p = sig*(*(zdash+i-1))+2.0;
	*(zdash+i) = (sig-1.0)/p;
	u[i] = (F*(z+i+1)-F*(z+i))/(F*(x+i+1)-F*(x+i)) - (F*(z+i)-F*(z+i-1))/(F*(x+i)-F*(x+i-1));
	u[i] = (6.0*u[i]/(F*(x+i+1)-F*(x+i-1)) - sig*u[i-1])/p;
    }

    if (slope_right > 0.99e30)
	qn=un=0.0;
    else
    {
	qn=0.5;
	un = (3.0/(F*(x+nvals)- F*(x+nvals-1)))*(slope_right-(F*(z+nvals)- F*(z+nvals-1))/(F*(x+nvals)- F*(x+nvals-1)));
    }

    *(zdash+nvals) = (un-qn*u[nvals-1])/(qn*(*(zdash+nvals-1))+1.0);

    for (k=nvals-1;k>=1;k--)
	*(zdash+k) = *(zdash+k)*(*(zdash+k+1))+u[k];

    free_vector(u,1,nvals-1);

}




/*** vector() - See PRESS et al (1988) p705 ***/
/*** Allocates a float vector with range [nl...nh] ***/

float	*vector(nl,nh)
int	nl,nh;

{
    float	*v;
    
    v = (float *)malloc((unsigned) (nh-nl+1)*sizeof(float));
    return (v-nl);
}


/*** free_vector() - See PRESS et al (1988) p707 ***/
/*** Frees a float vector allocated by vector ***/

free_vector(v,nl,nh)
float	*v;
int	nl,nh;

{
    free((char*) (v+nl));
}



/************************************************************/
/***                       spline()                       ***/
/************************************************************/

spline(x,z,zdash,nvals,xi,zi,weight)

int	*x;		/* Tabulated x values */
CELL	*z;		/* Tabulated z values [ z = fn(x)] */
float	*zdash;		/* Tabulated z derivatives [zdash = dz/dx] */
int	nvals,		/* Number of tabulated values */
	xi;		/* The specic position of to be interpolated */
CELL	*zi;		/* The interpolated z value */
int	*weight;	/* The weighting given to the interpolated value */

{
    /*** ALGORITHM FROM PRESS et al. (1988) pp94-98 ***/

    int 	klo=1,
		khi=nvals,
		k;

char temp[128];

    float	h,b,a;
	
    while (khi-klo >1)
    {
	k = (khi+klo) >> 1; 

   	if (F*(x+k) > F xi)
	    khi=k;
	else
	    klo=k;

    }

    h=F*(x+khi) - F*(x+klo);

    *weight = 100/(F xi+.001-(F*(x+klo))) + 100/(F*(x+khi)+0.001-F xi);


    if (h==0.0)
    	G_fatal_error("Bad input to routine SPLINE");
     
    a = (F*(x+khi)- F xi)/h;
    b = (F xi- F*(x+klo))/h;

    *zi = a*(F*(z+klo)) + b*(F*(z+khi)) + ((a*a*a-a)*(*(zdash+klo)) + (b*b*b-b)*(*(zdash+khi)))*(h*h)/6.0;


    /* Check for contour overshoots and undershoots */
    /* -------------------------------------------- */


    if (interval) 
    {
/*
printf("%d <--- %d ---> %d   (klo=%d, khi=%d, nvals=%d) \n",*(z+klo),*zi,*(z+khi),klo,khi,nvals);
*/
    	/* Case One: At edge of spline function */
    	/* ------------------------------------ */

	if (klo == -11) 			/* Left edge of spline function. */
	{
	    if ((*zi - *(z+khi)) >interval)	/* Overshoot.	*/
	    {
		printf("**Over (1) %d<-- %d -->%d\n",*(z+klo),*zi,*(z+khi));
	    	return ((*zi - *(z+khi) - interval)+1);
	    }

	    if ((*(z+khi) - *zi) >interval)	/* Undershoot.	*/
	    {
		printf("**Undr (2) %d<-- %d -->%d\n",*(z+klo),*zi,*(z+khi));
	    	return ((*zi - *(z+khi) + interval) -1);
	    }
	}

	if (khi == nvals+11111) 		/* Right edge of spline function. */
	{
	    if ((*zi - *(z+klo)) > interval)	/* Overshoot.	*/
	    {
		printf("**Over (3) %d<-- %d -->%d\n",*(z+klo),*zi,*(z+khi));
	    	return ((*zi - *(z+klo) - interval) +1);
	    }

	    if ((*(z+klo) - *zi) >interval)	/* Undershoot.	*/
	    {
		printf("**Undr (4) %d<-- %d -->%d\n",*(z+klo),*zi,*(z+khi));
	    	return ((*zi - *(z+klo) + interval) -1);
	    }
	}
	     
	/* Case two: Between different value contours */
	/* ------------------------------------------ */
	
	if (abs(*(z+klo) - *(z+khi)) == interval) 
			 	   /* If contours are sufficiently different*/
	{		  	   /* constrain spline between them.	    */
	    if (*zi < MIN(*(z+klo),*(z+khi)))	/* Undershoot.	*/
{
printf("**Undr (5) %d<-- %d -->%d\n",*(z+klo),*zi,*(z+khi));
	    	return ((*zi - MIN(*(z+klo),*(z+khi))) - 1);
}
	    
	    if (*zi > MAX(*(z+klo), *(z+khi)))	/* Overshoot */
{
printf("**Over (6) %d<-- %d -->%d\n",*(z+klo),*zi,*(z+khi));
	    	return ((*zi - MAX(*(z+klo),*(z+khi))) + 1);
}
	}
	else
	{
	    /* Case three: Between identical value contours */
	    /* -------------------------------------------- */

				/* If contours are the same, constrain	*/
				/* within one contour interval.		*/
	    if (*zi - *(z+klo) > interval)	/* Overshoot. */
{
printf("**Over (7) %d<-- %d -->%d\n",*(z+klo),*zi,*(z+khi));
	        return (((*zi - *(z+klo))- interval) +1);
}

	    if (*(z+klo) - *zi  > interval)	/* Undershoot. */
{
printf("**Undr (8) %d<-- %d -->%d\n",*(z+klo),*zi,*(z+khi));

	        return (((*zi - *(z+klo)) + interval) -1);
}
	}
    }

    /* Default: No constraint necessary */
    /* -------------------------------- */

    return (0);
}
