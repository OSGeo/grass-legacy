/************************************************************************/
/*									*/
/*   	     measures - Various co-occurrence matrix measures		*/
/* 	  Jo Wood, V1.0, December 1992, V1.1 October, 15th, 1995	*/
/*									*/
/************************************************************************/

#include "lags.h"


/************************************************************************/
/* init_matrix() - Fill a co-occurence matrix with zeros		*/
/************************************************************************/

init_matrix(cocr,glevs)
int	*cocr,			/* Pointer to co-occurrence matrix.	*/
	glevs;			/* Dimension of matrix.			*/
{
    int posn;			/* Position in matrix.			*/

    for (posn=0; posn <glevs*glevs; posn++)
  	*(cocr + posn) = 0;
}


/************************************************************************/
/* quant() - Quantize z value in glevs grey levels 			*/
/************************************************************************/

quant(n,zmin,zmax,glevs)
CELL	n,			/* Value to quantize.		  	*/
	zmin,zmax;		/* Range from which z is drawn.		*/
int 	glevs;			/* Number of classes to quantize into.	*/
{		
    return ((glevs-1)*(float)(n-zmin)/(zmax-zmin)); 
}




double contrast(x,y,z)
int x,y;
double z;

{
   return((x-y)*(x-y)*z);
}

double asmoment(z)
double z;
{
    return(z*z);
}

double entropy(z)
double z;
{
    if (z != 0.0)
	return (-1*(z*log(z)));
    else 
	return (0.0);
}

double asymmetry(z1,z2)
double z1,z2;
{
    return ((z1-z2)*(z1-z2));
}

double idmoment(x,y,z)
int x,y;
double z;
{
    return((1/(1+((x-y)*(x-y))))*z);
}
