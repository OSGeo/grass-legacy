#include<stdio.h>
#include<math.h>
#include"cdh.h"

double *mod_maxlik_ratio (x, n)
  double *x;
  int n;
{
  static double y[2];
  double mean = 0.0;
  double e1, m1, s1, s2, s3;
  double sum1 = 0.0, sum2 = 0.0, sum3 = 0.0, sum4 = 0.0, sum5 = 0.0;
  int i;

  for (i = 0; i < n; ++i)
  {
    sum1 += x[i];
    sum2 += log (x[i]);
  }
  s1 = (n * sum2 - mean * mean) / (n * (n - 1));
  mean = sum1 / n;
  m1 = sum2 / n;

  for (i = 0; i < n; ++i)
  {
    sum4 += (log (x[i]) - m1) * (log (x[i]) - m1);
    sum3 += pow (x[i] - mean, 3.0);
  }
  s2 = sum4 / n;

  if (sum3 < 0.0)
  {
#ifdef NOISY
    for (i = 0; i < n; ++i)
    {
      fprintf (stdout," %g", x[i]);
      if (i % 4)
	fprintf (stdout,"\n");
    }
    fprintf (stdout," THIRD SAMPLE MOMENT ABOUT THE MEAN IS LESS THAN ZERO\n");
    fprintf (stdout," HENCE WE ACCEPT THE NULL HYPOTHESIS OF NORMALITY\n");
#endif /* NOISY */
    y[0] = 0.0;
  }
  else
  {
    for (i = 0; i < n; ++i)
      sum5 += (x[i] - mean) * (x[i] - mean);
    s1 = sqrt (sum5 / n);
    s3 = sqrt (s2);
    e1 = exp (m1);
    y[0] = s1 / (s3 * e1);
  }
#ifdef NOISY
  fprintf (stdout,"  TEST22 LR(NL) =%10.4f\n", y[0]);
#endif				/* NOISY */
  return y;
}
