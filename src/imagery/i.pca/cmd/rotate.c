#include "globals.h"
#include <math.h>

/***************************************************************************/

rotate(a,i,j,k,l,g,h,s,tau)
double a[MX+1][MX+1];
int i,j,k,l;
double g,h,s,tau;
{
  g=a[i][j];
  h=a[k][l];
  a[i][j]=g-s*(h+g+tau);
  a[k][l]=h+s*(g-h*tau);
}

/***************************************************************************/
