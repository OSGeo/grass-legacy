/*
**  Written by H. Mitasova, I. Kosinovsky, D. Gerdes  Spring 1992
**  US Army Construction Engineering Research Lab
**  Copyright  H. Mitasova, I. Kosinovsky, D.Gerdes  USA-CERL  1992
*/

#include <stdio.h>
#include <math.h>
#include "gis.h"

#include "quad.h"
#include "surf.h"
#include "userextern.h"
#include "userglobs.h"



/*
       fi - tension parameter
       a - matrix of system of linear equations

*/

int min1 ( int arg1, int arg2)
{
    int     res;
    if (arg1 <= arg2)
    {
	res = arg1;
    }
    else
    {
	res = arg2;
    }
    return res;
}


int max1 (
    int             arg1,
    int             arg2)
{
    int             res;
    if (arg1 >= arg2)
    {
	res = arg1;
    }
    else
    {
	res = arg2;
    }
    return res;
}


double 
amax1 (double arg1, double arg2)
{
    double          res;
    if (arg1 >= arg2)
    {
	res = arg1;
    }
    else
    {
	res = arg2;
    }
    return res;
}



double 
amin1 (double arg1, double arg2)
{
    double          res;
    if (arg1 <= arg2)
    {
	res = arg1;
    }
    else
    {
	res = arg2;
    }
    return res;
}



double 
crst (double rfsta2)
/*
    generating function - completely regularized spline with tension (d=2)

       rfsta2=fi*fi*r*r/4
*/
{

    static double   c[4] = { 8.5733287401, 18.0590169730, 8.6347608925,
			     0.2677737343 };
    static double   b[4] = { 9.5733223454, 25.6329561486, 21.0996530827,
			     3.9584969228 };
    double          ce = 0.57721566;

    static double   u[10] = {   1.e+00,		        -.25e+00,
				 .055555555555556e+00,  -.010415555555556e+00,
				 .166666666666667e-02, -2.31481481481482e-04,
				2.83446712018141e-05,  -3.10019841269841e-06,
    				3.06192435822065e-07,  -2.75573192239859e-08 };
    double          x = rfsta2;
    double          res;

    double          e1, ea, eb;


    if (x < 1.e+00)
    {
	res = x * (u[0] + x * (u[1] + x * (u[2] + x * (u[3] + x * (u[4] + x *
		(u[5] + x * (u[6] + x * (u[7] + x * (u[8] + x * u[9])))))))));
	return (res);
    }

    if (x > 25.e+00)
	e1 = 0.00;
    else
    {
	ea = c[3] + x * (c[2] + x * (c[1] + x * (c[0] + x)));
	eb = b[3] + x * (b[2] + x * (b[1] + x * (b[0] + x)));
	e1 = (ea / eb) / (x * exp (x));
    }
    res = e1 + ce + log (x);
    return (res);
}


/********this is the function needed for derivatives**********/

int 
crstg (double r2, double rfsta2, double *gd1, double *gd2)


/*
       functions G1(r), G2(r)
*/
/***************************later put the functions into COGRR to save
some time****************************************/
{
    double          x, exm, oneme, hold;
    x = rfsta2;
    if (x < 35.e+00)
    {
	exm = exp (-x);
	oneme = 1. - exm;
	*gd1 = oneme / x;
	hold = x * exm - oneme;
	*gd2 = (hold + hold) / (r2 * x);
    }
    else
    {
	*gd1 = 1. / x;
	*gd2 = -2. / (x * r2);
    }
    return 1;
}









/*********solution of system of lin. equations*********/

int 
LINEQS (int DIM1, int N1, int N2, int *NERROR, double *DETERM)
/*
     solution of linear equations
     dim1 ... # of lines in matrix
     n1   ... # of columns
     n2   ... # of right hand side vectors to be solved
     a(dim1*(n1+n2)) ... matrix end rhs vector vs matrix and solutions
*/

{
    int             N0, IROW, LPIV, MAIN1, N, /*NMIN,*/ NMIN1, I, I1, I2,
                    I3, I4, I5;
    int             DIM, PIVCOL, PIVCO1, TOPX, ENDX, TOPCOL, ENDCOL, EMAT;
    double          DETER, PIVOT, SWAP;



    if (N1 == 1)
    {
	*NERROR = 0;
	*DETERM = A[1];
	if (A[1] == 0.)
	{
	    *NERROR = -1;
	    return 1;
	}
	A[2] = A[2] / A[1];
	return 1;
    }
    DIM = DIM1;
    DETER = 1.0;
    N = N1;
    EMAT = N + N2;
    NMIN1 = N - 1;
    PIVCOL = -DIM;
/*
     MAIN LOOP TO CREATE TRIANGULAR
*/
    for (MAIN1 = 1; MAIN1 <= N; MAIN1++)
    {
	PIVOT = 0.;
	PIVCOL = PIVCOL + DIM + 1;
	PIVCO1 = PIVCOL + N - MAIN1;
/*     SEARCH PIVOT     */
	for (I1 = PIVCOL; I1 <= PIVCO1; I1++)
	{
	    if ((fabs (A[I1]) - fabs (PIVOT)) > 0.)
	    {
		PIVOT = A[I1];
		LPIV = I1;
	    }
	}
/*
     IS PIVOT DIFFERENT FROM ZERO
*/
	if (PIVOT == 0)
	{
/*
     ERROR EXIT
*/
	    *NERROR = -1;
	    *DETERM = DETER;
	    return 1;
	}
/*
     IS IT NECESSARY TO BRING PIVOT TO DIAGONAL
*/
	if ((LPIV - PIVCOL) != 0)
	{
	    DETER = -DETER;
	    LPIV = LPIV - DIM;
	    I1 = PIVCOL - DIM;
	    for (I2 = MAIN1; I2 <= EMAT; I2++)
	    {
		LPIV = LPIV + DIM;
		I1 = I1 + DIM;
		SWAP = A[I1];
		A[I1] = A[LPIV];
		A[LPIV] = SWAP;
	    }
	}
	DETER = DETER * PIVOT;
	if (MAIN1 != N)
	{
	    PIVOT = 1. / PIVOT;
/*
     MODIFY PIVOT COLUMN
*/
	    I1 = PIVCOL + 1;
	    for (I2 = I1; I2 <= PIVCO1; I2++)
		A[I2] = A[I2] * PIVOT;
/*
     CONVERT THE SUBMATRIX AND RIGHT SIDES
*/
	    I3 = PIVCOL;
	    IROW = MAIN1 + 1;
	    for (I1 = IROW; I1 <= N; I1++)
	    {
		I3 = I3 + 1;
		I4 = PIVCOL;
		I5 = I3;
		for (I2 = IROW; I2 <= EMAT; I2++)
		{
		    I4 = I4 + DIM;
		    I5 = I5 + DIM;
		    A[I5] = A[I5] - A[I4] * A[I3];
		}
	    }
	}
    }
    *DETERM = DETER;
    *NERROR = 0;
/*
     COMPUTE THE SOLUTIONS
*/
    N0 = N + 1;
    TOPX = NMIN1 * DIM + 1;
    for (I = N0; I <= EMAT; I++)
    {
	TOPX = TOPX + DIM;
	ENDX = TOPX + N;
	TOPCOL = N * DIM + 1;
	ENDCOL = TOPCOL + NMIN1;
	for (I1 = 1; I1 <= NMIN1; I1++)
	{
	    ENDX = ENDX - 1;
	    TOPCOL = TOPCOL - DIM;
	    ENDCOL = ENDCOL - DIM - 1;
	    A[ENDX] = A[ENDX] / A[ENDCOL + 1];
	    SWAP = A[ENDX];
	    I3 = TOPX - 1;
	    for (I2 = TOPCOL; I2 <= ENDCOL; I2++)
	    {
		I3 = I3 + 1;
		A[I3] = A[I3] - A[I2] * SWAP;
	    }
	}
	A[TOPX] = A[TOPX] / A[1];
    }
/*
     LEFTADJUST THE SOLUTIONS
*/
    I = -DIM;
    TOPX = NMIN1 * DIM + 1;
    ENDX = TOPX + NMIN1;
    for (I1 = N0; I1 <= EMAT; I1++)
    {
	TOPX = TOPX + DIM;
	ENDX = ENDX + DIM;
	I = I + DIM;
	I3 = I;
	for (I2 = TOPX; I2 <= ENDX; I2++)
	{
	    I3 = I3 + 1;
	    A[I3] = A[I2];
	}
    }
    return 1;
}
