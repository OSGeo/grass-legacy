#include "gis.h"
#include "Vect.h"

/* Usage: geary_moran(catarray,&gr,&mc,n) */

geary_moran (catarray, gr, mc, n)
  double catarray[], *gr, *mc;
  int n;
{
  int i, j, sum = 0;
  double denom = 0, num = 0, mean = 0, diff = 0, tmpi, tmpj;
  extern int **c;
  extern struct Categories cats;

  for (i = 1; i <= n; ++i)
  {
    tmpi = catarray[i];
    for (j = 1; j <= n; ++j)
    {
      tmpj = catarray[j];
      diff += (double) (c[i][j]) * (tmpi - tmpj) * (tmpi - tmpj);
      sum += c[i][j];
    }
    mean += tmpi;
  }
  mean /= n;
  num = (n - 1) * diff;

  diff = 0.0;
  for (i = 1; i <= n; ++i)
  {
    tmpi = catarray[i];
    diff += (tmpi - mean) * (tmpi - mean);
  }
  denom = 2.0 * sum * diff;
  if (denom)
    *gr = num / denom;
  else
    *gr = 0;

  diff = 0.0;
  for (i = 1; i <= n; ++i)
  {
    tmpi = catarray[i];
    for (j = 1; j <= n; ++j)
    {
      tmpj = catarray[j];
      diff += c[i][j] * (tmpi - mean) * (tmpj - mean);
    }
  }

  num = n * diff;
  denom /= 2;
  if (denom)
    *mc = num / denom;
  else
    *mc = 0;

  return 1;
}
