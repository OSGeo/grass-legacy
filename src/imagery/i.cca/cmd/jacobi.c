#include <stdlib.h>
#include <math.h>
#include "gis.h"
#include "globals.h"

/***************************************************************************/
/* The "jacobi" function is  */
/* Based on "Numerical Recipies in C; The Art of Scientific Computing"
   (Cambridge University Press, 1988).  Copyright (C) 1986, 1988 by
   Numerical Recipes Software.  Permission is granted for unlimited
   use within GRASS only.  */

#define ROTATE(a,i,j,k,l) g=a[i][j];h=a[k][l];a[i][j]=g-s*(h+g*tau);\
        a[k][l]=h+s*(g-h*tau)

#define ITERATIONS 500

int 
jacobi (double a[MX][MX], long n, double d[MX], double v[MX][MX])
{
  int j,iq,ip,i;
  double tresh,theta,tau,t,sm,s,h,g,c,*b,*z;
  b=(double *) malloc((n+1)*sizeof(double));
  z=(double *) malloc((n+1)*sizeof(double));
  if (z==NULL || b==NULL)
    {
      G_fatal_error("Insufficient memory for data structure during Eigenanalysis.\n");
    }
  for(ip=1 ; ip<=n ; ip++) {
    for(iq=1 ; iq<=n ; iq++) v[ip][iq]=0.0;
    v[ip][ip]=1.0;
  }
  for(ip=1 ; ip<=n ; ip++) {
    b[ip]=d[ip]=a[ip][ip];
    z[ip]=0.0;
  }
  for(i=1 ; i<ITERATIONS ; i++) {
    sm=0.0;
    for(ip=1 ; ip<=n-1 ; ip++) {
      for(iq=ip+1 ; iq<=n ; iq++)
        sm += fabs(a[ip][iq]);
    }
#ifdef DEBUG
    fprintf(stderr, "sm is %12.9le\n", sm);
#endif
    if(sm==0.0) {
      free(b);
      free(z);
      return 0;
    }
    if(i<4)
      tresh=0.2*sm/(n*n);
    else
      tresh=0.0;
    for(ip=1 ; ip<=n-1 ; ip++) {
      for(iq=ip+1 ; iq<=n ; iq++) {
        g=100.0*fabs(a[ip][iq]);
        if(i>4 && fabs(d[ip])+g == fabs(d[ip])
           && fabs(d[iq])+g == fabs(d[iq]))
          a[ip][iq]=0.0;
        else
          if(fabs(a[ip][iq] > tresh)) {
            h=d[iq]-d[ip];
            if(fabs(h)+g == fabs(h))
              t=(a[ip][iq])/h;
            else {
              theta=0.5*h/(a[ip][iq]);
              t=1.0/(fabs(theta)+sqrt(1.0+theta*theta));
              if (theta < 0.0) t = -t;
            }
            c = 1.0 / sqrt(1 + t * t);
            s=t*c;
            tau=s/(1.0+c);
            h=t*a[ip][iq];
            z[ip] -= h;
            z[iq] += h;
            d[ip] -= h;
            d[iq] += h;
            a[ip][iq]=0.0;
            for(j=1 ; j<=ip-1 ; j++) {
              ROTATE(a,j,ip,j,iq);
            }
            for(j=ip+1 ; j<=iq-1 ; j++) {
              ROTATE(a,ip,j,j,iq);
            }
            for(j=iq+1 ; j<=n ; j++) {
              ROTATE(a,ip,j,iq,j);
            }
            for(j=1 ; j<=n ; j++) {
              ROTATE(v,j,ip,j,iq);
            }
          }
      }
    }
    for(ip=1 ; ip<=n ; ip++) {
      b[ip] += z[ip];
      d[ip]=b[ip];
      z[ip]=0.0;
    }
  }
  G_warning("Too many iterations in Jacobi routine, output may be invalid.");
  fprintf(stderr, "The following is the upper half of the matrix.  The above diagonal\n");
  fprintf(stderr, "values should be zero.  If they are not close to zero, the jacobi analysis\n");
  fprintf(stderr, "has failed.  See the manual page.\n");
  for(ip=1; ip<=n; ip++) {
    for(iq=1; iq<=n; iq++) {
      if (iq<ip) fprintf(stderr, "                  ");
      else fprintf(stderr, "  %12.9e", a[ip][iq]);
    }
    fprintf(stderr, "\n");
  }

  return 0;
}
/***************************************************************************/
/* The "egvorder" (eigsrt) function is  */
/* Based on "Numerical Recipies in C; The Art of Scientific Computing"
   (Cambridge University Press, 1988).  Copyright (C) 1986, 1988 by
   Numerical Recipes Software.  Permission is granted for unlimited
   use within GRASS only.  */

int egvorder (double d[MX], double z[MX][MX], long bands)
{
double  p;
long    i, j, k;

for (i = 1 ; i<bands ; i++) {
    p = d[k=i];
    for (j = i+1; j <=bands; j++)
      if(d[j] >= p) p=d[k=j];
/*
   interchange eigen values i and k and corresponding eigen vectors
*/
  if (k !=  i)
    {
      d[k] = d[i];
      d[i] = p;
      for (j = 1; j <=bands; j++) {
         p = z[j][i];
         z[j][i] = z[j][k];
         z[j][k] = p;
         }
      }
   }
#ifdef DEBUG
  for(i=1;i<=bands;i++) fprintf(stderr, "e[%d] = %f\n",i,d[i]);
#endif

  return 0;
}

/***************************************************************************/

int transpose (double eigmat[MX][MX], long bands)
{
  double trans[MX][MX];
  int i,j;
  for(i=1 ; i<=bands ; i++)
    for(j=1 ; j<=bands ; j++)
      trans[j][i]=eigmat[i][j];
  for (i=1;i<=bands;i++)
    for (j=1;j<=bands;j++)
      eigmat[i][j]=trans[i][j];

  return 0;
}
/***************************************************************************/
