/*
*  PRUNES A COORDINATE STRING OF ALL POINTS IN WHICH THE X AND Y
*  COORDINATES ARE WITHIN A CERTAIN TOLERANCE OF THE X AND Y
*  COORDINATES OF THE PREVIOUS POINT.  THE PURPOSE OF THIS ROUTINE
*  IS TO ELIMINATE IDENTICAL POINTS FOR A COORDINATE STRING WHICH IS
*  TO BE WRITTEN TO AN ASCII FILE.  IT IS NOT DESIGNED TO BE USED
*  FOR LINE GENERALIZATION.  A TOLERANCE IS NEEDED, HOWEVER, BECAUSE
*  OF THE UNRELIABILITY OF FLOATING POINT EQUALITY/INEQUALITY COMPARISONS,
*  AS WELL AS THE FACT THAT THE ULTIMATE EQUALITY OF POINTS DEPENDS UPON 
*  THE PRECISION WITH WHICH THEY ARE WRITTEN TO THE ASCII FILE.
*  
*
*  WRITTEN BY:
*  CHRIS EMMERICH, AUTOMETRIC INC., 12/27/89
*/

prune_points (x,y,ncoord,tol)

double x[],y[];                  /* COORDINATE ARRAYS */
int *ncoord;                     /* # OF COORDINATE PAIRS */
double tol;                      /* TOLERANCE FOR DUPLICATE POINTS */

{
    double fabs();
    int ic,jc;

    /* SHUFFLE ARRAY TO ELIMINATE DUPLICATE POINTS */

    ic = 1;
    jc = 0;

    while (ic < *ncoord)
    {
        if (fabs(x[ic] - x[jc]) >= tol || fabs(y[ic] - y[jc]) >= tol)
        {                  /* NON-DUPLICATE POINTS */
           jc++;
           if (jc < ic)  /* MOVE THE CURRENT POINT INTO THE ARRAY POSITION */
           {             /* AT WHICH THE LAST DUPLICATE POINT WAS FOUND */
              x[jc] = x[ic];
              y[jc] = y[ic];
           }
        }
        ic++;
    }

    *ncoord = jc + 1;
    return (0);
}
