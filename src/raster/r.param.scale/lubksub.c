/****************************************************************/
/* lubksub() - Function to perform forward and back substitution*/
/*		on an LU decomposed matrix. Used for solving a  */
/*		set of linear equations.			*/
/*		Adapted from Press et al (1988), p.44.		*/
/* 		V.1.0, Jo Wood, 10th December, 1994.  	  	*/
/****************************************************************/


lubksub(a,n,index,b)
    int     n,		/* Size of side of matrix 		*/
	    *index;	/* Row permutation effected by pivoting */
    float   **a,	/* Matrix to be decomposed. 		*/
	    *b;		/* Input as RHS vector, output as soln.	*/

{
    int    i, ii=0, ip, j;
    float  sum;

    for (i=1; i<=n; i++)
    {
	ip    = index[i];
	sum   = b[ip];
	b[ip] = b[i];

	if (ii)
	    for (j=ii; j<=i-1; j++)
		sum -= a[i][j] * b[j];
	else
	    if (sum)
		ii=i;

	b[i] = sum;
    }

    for (i=n; i>=1; i--)
    {
	sum = b[i];
	for (j=i+1; j<=n; j++)
	    sum -= a[i][j] * b[j];
	
	b[i] = sum/a[i][i];
    }
}

