#include <stdio.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"

#define FUDGE 10

/* Usage: std_err(catarray,&siggrr,&sigmcr,&siggrn,&sigmcn,n) */

std_err (catarray, siggrr, sigmcr, siggrn, sigmcn, n)
  double catarray[], *siggrr, *sigmcr, *siggrn, *sigmcn;
  int n;
{
  int i, j, k, sum = 0;
  double sumcij = 0, sumcj = 0, dum = 0, mean = 0, m4 = 0, s4 = 0;
  double diff = 0, tmpi, tmpj, tmpk;
  extern int **c;
  extern struct Categories cats;

  for (j = 1; j <= n; ++j)
    for (i = 1; i <= n; ++i)
      sumcij += c[i][j];

  for (i = 1; i <= n; ++i)
  {
    for (j = 1; j <= n; ++j)
    {
      sumcj = 0;
      for (k = 1; k <= n; ++k)
	sumcj += c[i][k];
      dum += c[i][j] * (sumcj - 1);
    }
  }
  tmpi = (sumcij + dum / 2.0) * (n - 1);
  tmpj = sumcij * sumcij / 2.0;
  tmpk = (n + 1) * sumcij * sumcij / 4.0;
  *siggrn = (tmpi - tmpj) / tmpk;

  *sigmcn = (sumcij + dum) * (-4 * n);
  *sigmcn += (2 * n * n * sumcij + 3 * sumcij * sumcij);
  *sigmcn /= sumcij * sumcij * (n * n - 1);
  *sigmcn -= (-1. / (n - 1)) * (-1. / (n - 1));

  /* Now we need constants for randomization assumption */
  for (i = 1; i <= n; ++i)
    mean += catarray[i];
  mean /= (double) n;

  for (i = 1; i <= n; ++i)
  {
    diff = catarray[i] - mean;
    m4 += pow (diff, (double) 4.0);
    s4 += pow (diff, (double) 2.0);
  }
  m4 /= n;
  s4 /= n;
  s4 *= s4;

  tmpi = sumcij * sumcij * (-(n - 1) * (n - 1) * m4 / s4 + (n * n - 3)) / 2.0;
  tmpj = sumcij * (n - 1) * (-(n - 1) * m4 / s4 + n * n - 3 * n + 3);
  tmpk = (sumcij + dum) * (n - 1);
  tmpk *= ((n * n - n + 2) * m4 / s4 - (n * n + 3 * n - 6)) / 2.0;
  *siggrr = (tmpi + tmpj + tmpk);	/* numerator is done */
  *siggrr /= (n * (n - 2) * (n - 3) * sumcij * sumcij / 2.0);	/* divide by denom */

  tmpi = 2.0 * n * (n * n - 3 * n + 3) * sumcij;
  tmpj = -4.0 * (sumcij + dum) * n * n + 3 * n * sumcij * sumcij;
  tmpk = -(m4 / s4) * (2.0 * (n * n - n) * sumcij - 8.0 * n * (sumcij + dum) + 6 * sumcij * sumcij);
  *sigmcr = tmpi + tmpj + tmpk;	/* numerator is done */
  *sigmcr /= ((n - 1) * (n - 2) * (n - 3) * sumcij * sumcij);	/* divide by denom */
  *sigmcr -= 1. / ((n - 1) * (n - 1));

  return 1;
}
