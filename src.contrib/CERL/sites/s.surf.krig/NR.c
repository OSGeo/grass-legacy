
/*-

****************  PLEASE READ  *****************

In-Reply-To: William Skelly's message of Thu, 14 Jan 93 13:20:41 EST
Subject: Numerical Recipes Copywrite.

Thanks for your note.  I am attaching a copy of our info sheet
regarding uses of the NR routines. In a case like yours, where the use
is for a noncommercial research purpose, and where the number of
routines used is small (in your case, only two, from what you say) we
usually waive the stated fee.  However, it is a matter of concern to
us that your distribution preserve our copyright (by including the
copyright notice in each routine) and also make clear to your users
that they have permission to use the included routines *only* with
your software, *not* to transplant it to other programs (unless, of
course, they are licensed for all NR -- which currently costs only $50
per screen for educational use).

     Sincerely, Bill Press

Numerical Recipes Software P.O. Box 243 Cambridge, MA 02238
FAX: 617-863-1739

Necessary Permissions for Including Numerical Recipes

We have several levels of permissions.  The simplest is if you are
simply using individual Recipes bound into independently executable
software that you develop, and not separable from that software by
a user.  In such cases the user must not be able to use the
Recipes as a program library, indeed will normally not be aware that
they are being used in the program.  There is no fee for this level
of permission.  We require only that you obtain from us in advance
permission for commercial use, and that the executable software contain
the embedded copyright notice "Copyright (C) 1986 Numerical Recipes
Software" (for FORTRAN) or "Copyright (C) 1987, 1988 Numerical Recipes
Software" (for C).  This notice need not, and should not, be displayed
to the user.

The next level of permission is if you want to make available
to your users a subset of Recipes in object, or individually callable,
form, but not including source code.  For fewer than 20 Recipes used
in this manner, we charge a flat fee of \$40 per Recipe for each
use (unlimited number of copies).  The copyright notice is required,
as above, and we also request that the printed manual contain words
to the effect "the procedures [your procedure identifiers] are based
on routines in Numerical Recipes: The Art of Scientific Computing,
published by Cambridge University Press, and are used by permission."
We are reluctant to license the use of more than 20 Recipes in this
manner, but will occasionally do so on an individually negotiated basis.

Finally, there is the case where our source code, or substantially
identical code, is to be made available to users.  In the case, we
charge a flat fee of \$100 per Recipe if the source code is to be
distributed in machine-readable form, or \$50 per Recipe if the
distribution is not machine readable (i.e., hardcopy listing only).
The above copyright notice and permission acknowledgment provisions
apply.  In general, we will not license the use of more than 20
Recipes in this manner for a fixed fee.  Proposals to translate
substantially all of the Numerical Recipes into other computer
languages, or embed them in language-like software, are negotiated on
an individual basis.

A special case is that we allow, without fee, reproduction of up to
five Recipes in any textbook, in printed form only, provided that
advance permission is obtained, and that the appropriate copyright
notice and permission acknowledgment is included.

At any permission level, if you use the Numerical Recipes in software
of your own, you are bound by the Disclaimer of Warranty that we
distribute with all forms of our software, including its specific
disclaimer of merchantability or fitness for any purpose.  Also note
that no business of other legal relationship is created between us and
your customers by your use of our software.

*/


/*-
NUMERICAL RECIPES ROUTINES BELOW
ludcmp routine - finds inverse of matrix A
Parameters passed:
*  a			INPUT: array of floats
*  n			INPUT: number of search points + 1 lagrange number
*  indx			pointer to an array of integers
*  d			not used outside the nr routine
*  What this does:
*  This function calculates the inverse matrix.
*/

#include <stdio.h>
#include <math.h>
#include "krig.h"
#define TINY 1.0e-20;
void 
ludcmp (float **a, int n, int *indx, float *d)
{
  int i, imax, j, k;
  float big, dum, sum, temp;
  float *vv;
  /* void free_vector (); */

  vv = vector (1, n);
  *d = 1.0;
  for (i = 1; i <= n; i++)
  {
    big = 0.0;
    for (j = 1; j <= n; j++)
      if ((temp = fabs (a[i][j])) > big)
	big = temp;
    if (big == 0.0)
      fprintf (stderr,"Singular matrix in routine LUDCMP");
    vv[i] = 1.0 / big;
  }
  for (j = 1; j <= n; j++)
  {
    for (i = 1; i < j; i++)
    {
      sum = a[i][j];
      for (k = 1; k < i; k++)
	sum -= a[i][k] * a[k][j];
      a[i][j] = sum;
    }
    big = 0.0;
    for (i = j; i <= n; i++)
    {
      sum = a[i][j];
      for (k = 1; k < j; k++)
	sum -= a[i][k] * a[k][j];
      a[i][j] = sum;
      if ((dum = vv[i] * fabs (sum)) >= big)
      {
	big = dum;
	imax = i;
      }
    }
    if (j != imax)
    {
      for (k = 1; k <= n; k++)
      {
	dum = a[imax][k];
	a[imax][k] = a[j][k];
	a[j][k] = dum;
      }
      *d = -(*d);
      vv[imax] = vv[j];
    }
    indx[j] = imax;
    if (a[j][j] == 0.0)
      a[j][j] = TINY;

    if (j != n)
    {
      dum = 1.0 / (a[j][j]);
      for (i = j + 1; i <= n; i++)
	a[i][j] *= dum;
    }
  }
  /* free_vector (vv, 1, n); */
}
#undef TINY

/*-lubksb routine - multiplies inverse of matrix A (from lucmp) by matrix b
*                   and returns matrix x (weights) in "b".
*  Parameters passed:
*  a			INPUT: an array of n x n values
*  n			INPUT: number of search points + 1 lagrange parameter
*  indx			an pointer to an integer array
*  b			RETURNED: the solution vector
*/
/*
 * What this does: This function multiplies two matrices, and therefore
 * can be used without lucmp if the same a matrix is to be used.
 */

void 
lubksb (float **a, int n, int *indx, float b[])
{
  int i, ii = 0, ip, j;
  float sum;

  for (i = 1; i <= n; i++)
  {
    ip = indx[i];
    sum = b[ip];
    b[ip] = b[i];
    if (ii)
      for (j = ii; j <= i - 1; j++)
      {
	sum -= a[i][j] * b[j];
	/*
	 * fprintf(stderr,"\nsum=%f   b[%d]= %f   a[%d][%d]=%f
	 * ",sum,i,b[j],i,j,a[i][j]);
	 */
      }
    else if (sum)
      ii = i;
    b[i] = sum;
  }
  for (i = n; i >= 1; i--)
  {
    sum = b[i];
    for (j = i + 1; j <= n; j++)
    {
      sum -= a[i][j] * b[j];
      /*
       * fprintf(stderr,"\nsum=%f   b[%d]= %f   a[%d][%d]=%f
       * ",sum,i,b[j],i,j,a[i][j]);
       */
    }
    b[i] = sum / a[i][i];
    /*
     * fprintf(stderr,"\nb[%d]= %f  sum= %f  a[%d][%d]=%f
     * ",i,b[i],sum,i,i,a[i][i]);
     */
  }
}
