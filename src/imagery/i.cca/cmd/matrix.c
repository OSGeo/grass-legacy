
#include "gis.h"
#include <math.h>
#include "globals.h"


/****************************************************************************/

product(vector,factor,matrix1,bands)
double vector[MX];
double  factor;
double matrix1[MX][MX];
int  bands;
{
  int i,j;
  for (i = 1 ; i <= bands ; i++)
    for (j = 1 ; j <= bands ; j++)
      {
        matrix1[i][j] = (double) factor * (vector[i] * vector[j]);
      }
}

/******************************************************************************/

setdiag(eigval,bands,l)
double eigval[MX];
int   bands;
double l[MX][MX];
{
  int i,j;
  for (i = 1 ; i <= bands ; i++)
    for (j = 1 ; j <= bands ; j++)
      if (i==j) l[i][j] = eigval[i]; else l[i][j] = 0.0;
}

/****************************************************************************/

getsqrt(w,bands,l,eigmat)
double w[MX][MX];
int bands;
double l[MX][MX];
double eigmat[MX][MX];
{
  int i,j;
  double tmp[MX][MX];
  for (i = 1 ; i <= bands ; i++) l[i][i] = 1.0 / sqrt(l[i][i]);
  matmul(tmp,eigmat,l,bands);
  transpose(eigmat,bands);
  matmul(w,tmp,eigmat,bands);
}

/****************************************************************************/

solveq(q,bands,w,p)
double q[MX][MX];
int bands;
double w[MX][MX],p[MX][MX];
{
  int i,j;
  double tmp[MX][MX];
  matmul(tmp,w,p,bands);
  matmul(q,tmp,w,bands);
}


/***************************************************************************/

matmul(res,m1,m2,dim)
double res[MX][MX];
double m1[MX][MX];
double m2[MX][MX];
int  dim;
{
  int i,j,k;
  double sum;
  for (i = 1 ; i <= dim ; i++)
    {
      for (j = 1 ; j <= dim ; j++)
        {
          sum = 0.0;
          for (k = 1 ; k <= dim ; k++) sum += m1[i][k] * m2[k][j];
          res[i][j] = sum;
        }
    }

}
