/*
 * Copyright (C) 1994. James Darrell McCauley.  (darrell@mccauley-usa.com)
 * 	                                        http://mccauley-usa.com/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

#include<math.h>
#include"gis.h"

double **ipf (n, m, stop, showstop)
  int n,showstop;
  double **m, stop;
{
  int i, j, iteration = 0;
  double **normalized, tmp = 999.0, delta = -1.0;

  if ((normalized = (double **) malloc ((n + 1) * sizeof (double *)))
      == NULL)
    G_fatal_error ("ipf: problems allocating memory 1");
  else
    for (i = 0; i <= n; ++i)
      if ((normalized[i] = (double *) malloc ((n + 1) * sizeof (double)))
	  == NULL)
	G_fatal_error ("ipf: problems allocating memory 2");
      else if (i != n)
	for (j = 0; j < n; ++j)
	  normalized[i][j] = m[i][j];

  do
  {
    /* zero out collector bins */
    for (i = 0; i < n; ++i)
      normalized[i][n] = normalized[n][i] = 0.0;
    normalized[n][n] = 0.0;

    /* calculate row marginals */
    for (i = 0; i < n; ++i)
      for (j = 0; j < n; ++j)
	normalized[i][n] += normalized[i][j];

    /* divide by row marginals */
    for (i = 0; i < n; ++i)
      for (j = 0; j < n; ++j)
	normalized[i][j] /= normalized[i][n];

    /* calculate new column marginals */
    for (i = 0; i < n; ++i)
      for (j = 0; j < n; ++j)
	normalized[n][j] += normalized[i][j];

    /* divide by column marginals */
    for (i = 0; i < n; ++i)
      for (j = 0; j < n; ++j)
	normalized[i][j] /= normalized[n][j];

    iteration++;
    for (i = 0, delta = -1.0; i < n; ++i)
    {
      tmp = fabs (1.00000 - normalized[i][n]);
      delta = (tmp > delta) ? tmp : delta;
    }

  }
  while (do_i_stop (stop, iteration, delta, showstop));

  /* zero out collector bins */
  for (i = 0; i < n; ++i)
    normalized[i][n] = normalized[n][i] = 0.0;
  normalized[n][n] = 0.0;

  /* calculate final marginals */
  for (i = 0; i < n; ++i)
  {
    for (j = 0; j < n; ++j)
    {
      normalized[i][n] += normalized[i][j];
      normalized[n][j] += normalized[i][j];
    }
    normalized[n][n] += normalized[i][j];
  }

  return normalized;
}

int do_i_stop (stop, iteration, delta,showstop)
  double stop, delta;
  int iteration,showstop;
{
  if ((double) iteration >= floor (stop) ||
      stop - floor (stop) > delta)
  {
    if (showstop)
      fprintf (stderr,
               "stopped after %d iterations, delta = %g\n\n", 
               iteration, delta);
    return 0;
  }
  return 1;
}
