/****************************************************************/
/* ludcomp() - Function to perform LU decomposition of a matrix.*/
/*		Used to find inverse of the matrix when solving */
/*		sets of linear equations.			*/
/*		Adapted from Press et al (1988), pp.43-45.	*/
/* 		V.1.0, Jo Wood, 9th December, 1994.  	  	*/
/****************************************************************/

#include <math.h>
#include "param.h"
#include "local_proto.h"


int 
ludcomp (
    float **a,	/* Matrix to be decomposed. 		*/
    int n,		/* Size of side of matrix 		*/
    int *index	/* Row permutation effected by pivoting */
)

{
    int	    i, imax,j,k;
    float   big,dum,sum,temp,
    	    *vv;


    vv = vector(1,n);

    for (i=1; i<=n; i++)    /* Loop over rows, get scaling info.*/
    {
	big = 0.0;
	for (j=1; j<=n; j++)
	    if ((temp=fabs(a[i][j])) > big)
		big = temp;
	
	if (big == 0.0)
	{
	    fprintf(stderr,"Singular matrix - can't perform LU decomposition\n");
	    exit(-1);
	}

	vv[i] = 1.0/big;    /* Save the scaling. */
    }


    for (j=1; j<=n; j++)
    {
	for (i=1; i<j; i++)
	{
	    sum=a[i][j];
	    for (k=1; k<i; k++)
		sum -= a[i][k] * a[k][j];
	    a[i][j] = sum;
	}

	big=0.0;

	for (i=j; i<=n; i++)
	{
	    sum=a[i][j];
	    for (k=1; k<j; k++)
	    	sum -= a[i][k] * a[k][j];
	    a[i][j] = sum;

	    if ( (dum=vv[i]*fabs(sum)) >= big)
	    {
		big = dum;
		imax = i;
	    }
	}

	if (j != imax)
	{
	    for (k=1; k<=n; k++)
	    {
		dum = a[imax][k];
		a[imax][k] = a[j][k];
		a[j][k] = dum;
	    }
	    vv[imax] = vv[j];
	}

	index[j] = imax;
	if (a[j][j] == 0.0)
	    a[j][j] = TINY;

	if (j != n)
	{
	    dum = 1.0/(a[j][j]);

	    for (i=j+1; i<=n; i++)
		a[i][j] *= dum;
	}
    }

    free_vector(vv,1,n);

    return 0;
}


