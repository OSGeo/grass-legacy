/*      Name:   mult.c

Created:        Sun Mar 30 17:29:45 1986
Last modified:  Sun Mar 30 21:34:34 1986

Purpose:        Multiply the two complex vectors, point by point.

Details:        Vectors are in the form:  real, imaginary (each a floating
                number).  A vector can be of any size.  Computes
                v3 = v1 * v2.  v3 should as big as the biggest of v1 and v2.

Author:         Bill Hoff,2-114C,8645,3563478 (hoff) at uicsl
*/

int 
mult (double *v1[2], int size1, double *v2[2], int size2, double *v3[2], int size3)
{
  register int i, n;

  n = (size1 < size2 ? size1 : size2); /* get the smaller size */
  for (i=0; i < n; i++) {
    *(v3[0]+i) = *(v1[0]+i) * *(v2[0]+i) - *(v1[1]+i) * *(v2[1]+i);
    *(v3[1]+i) = *(v1[0]+i) * *(v2[1]+i) + *(v2[0]+i) * *(v1[1]+i);
  }

  /* if unequal size, zero out remaining elements of larger vector */
  if (size1 != size2)
    for (i=n; i < size3; i++)   {
      *(v3[0]+i) = 0.0;
      *(v3[1]+i) = 0.0;
    }

  return 0;
}
