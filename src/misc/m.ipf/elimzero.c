/*
 * Copyright (C) 1994. James Darrell McCauley.  (darrell@mccauley-usa.com)
 * 	                                        http://mccauley-usa.com/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

#include<math.h>
#include"gis.h"
double **elimzero ( n,m)
int n; 
int **m;
{
  double **expectedvalue, tmp, k = 0.0;
  int nsamples = 0, i, j;
  double sumsq = 0.0;

  if ((expectedvalue = (double **) G_malloc ((n+1)*sizeof (double *)))
      == NULL)
    G_fatal_error ("elimzero: problems allocating memory 1");
  else
    for (i = 0; i <= n; ++i)
      if ((expectedvalue[i] = (double *) G_malloc ((n+1) * sizeof (double))) == NULL)
	G_fatal_error ("elimzero: problems allocating memory 2");

  for (i = 0; i <= n; ++i)
    for (j = 0; j <= n; ++j)
      expectedvalue[i][j] = 0;
  /*-
   *         First calculate sum (N=sum_{i,j} X_{i,j}), 
   *         sum of squares (N=sum_{i,j} X_{i,j}^2), and
   *         marginals (X_{i,\cdot}, X_{\cdot,j})
   */
  for (i = 0; i < n; ++i)
  {
    for (j = 0; j < n; ++j)
    {
      sumsq += (double)(m[i][j] * m[i][j]);
      nsamples += m[i][j];
      expectedvalue[i][n] += m[i][j];
      expectedvalue[n][j] += m[i][j];
    }
    expectedvalue[n][n] += expectedvalue[i][n];
  }

  /*-
   *        N * lambda_{i,j} 
   */
  for (i = 0; i < n; ++i)
    for (j = 0; j < n; ++j)
      expectedvalue[i][j] = (double)expectedvalue[i][n] * expectedvalue[n][j] / (double)nsamples;

  /* now do k */
  for (i = 0; i < n; ++i)
    for (j = 0; j < n; ++j)
      k += (expectedvalue[i][j] - m[i][j]) *
	(expectedvalue[i][j] - m[i][j]);
  k = (double)(nsamples * nsamples - sumsq) / k;
 
  /* the following save a few computations */
  tmp = (double) nsamples / (nsamples + k);
  k /= (double) nsamples; /* the ratio k/N */

  /* finally, the Bayesian estimator \hat{p}_{i,j} */
  for (i = 0; i < n; ++i)
    for (j = 0; j < n; ++j)
      expectedvalue[i][j] = tmp * ((double)m[i][j] + expectedvalue[i][j] * k);

  /* zero out collector bins */
  for (i = 0; i < n; ++i)
    expectedvalue[i][n] = expectedvalue[n][i] = 0.0;
  expectedvalue[n][n] = 0.0;

  /* recalculate marginals */
  for (i = 0; i < n; ++i)
  {
    for (j = 0; j < n; ++j)
    {
      expectedvalue[i][n] += expectedvalue[i][j];
      expectedvalue[n][j] += expectedvalue[i][j];
    }
    expectedvalue[n][n] += expectedvalue[i][n];
  }

  return expectedvalue;
}

