#include <math.h>
#include <stdlib.h>
#include "gis.h"
#include "univar.h"

UNIV univariate (Z *z, int nsites, int verbose, int field)

/*
 * Calculates the mean, standard deviation, quartiles, median, and
 * coefficient of variation for {\tt z}. Return value is a {\tt UNIV}
 * struct.
 */
{
  int i;
  double *zee;
  UNIV stats;

  if (verbose)
    fprintf (stderr, "Calculating statistics (decfield: %i) ...", field);

  zee = (double *) G_malloc (nsites * sizeof (double));
  if (zee == NULL)
    G_fatal_error ("Memory allocation error 1 (univar.c)");

  stats.n = nsites;
  stats.m = stats.s = stats.skw = stats.kur = stats.mse = stats.mav = 0;

  for (i = 0; i < stats.n; ++i)
  {
    zee[i] = z[i].z;
    stats.m += zee[i];
    stats.mse += zee[i]*zee[i];
    stats.mav += fabs(zee[i]);
  }
  stats.m /= stats.n;
  stats.mse /= stats.n;
  stats.mav /= stats.n;

  for (i = 0; i < stats.n; ++i)
  {
    stats.s += pow((zee[i] - stats.m), 2.0);
    stats.skw += pow((zee[i] - stats.m), 3.0);
    stats.kur += pow((zee[i] - stats.m), 4.0);
  }

  stats.skw /= stats.n;
  stats.skw /= pow(stats.s/(stats.n),1.5);

  stats.kur /= stats.n;
  stats.kur /= pow(stats.s/(stats.n),2.0);
  stats.kur -= 3.0;

  stats.s = sqrt(stats.s/(stats.n-1));
  stats.cv = 100 * (stats.s / stats.m);
  qsort (zee, stats.n, sizeof (double), dblcompare);
  stats.min = zee[0];
  stats.med = median (zee, stats.n);
  stats.max = zee[stats.n - 1];
  stats.q1 = firstquartile (zee, stats.n);
  stats.q3 = thirdquartile (zee, stats.n);

  free (zee);
  if (verbose)
    G_percent (1, 1, 1);
  return stats;
}

int dblcompare (i, j)
  double *i, *j;

/* \verb|qsort()| comparison function for an array of doubles. */
{
  if (*i - *j < 0)
    return -1;
  else if (*i - *j > 0)
    return 1;
  else
    return 0;
}

double median (arr, n)
  double arr[];
  int n;

/*
 * Returns the median of a sorted array ({\tt arr}) of {\tt n} doubles.
 */
{
  if (!(n % 2))			/* then there is an even number of
				 * elements in set */
#ifdef RAND_MED
    /* random()&01 will produce a random binary value. */
    return (arr[(int) n / 2 + (int) (random () & 01)]);
#else
    /* this averages the hi and lo */
    return ((arr[(int) n / 2] + arr[(int) n / 2 - 1]) / 2.0);
#endif
  else
    return (arr[(int) ((n - 1) / 2)]);
}

double firstquartile (arr, n)
  double arr[];
  int n;

/*
 * Returns the first quartile of a sorted array ({\tt arr}) of {\tt n}
 * doubles.
 */
{
  if (!(n % 4))
#ifdef RAND_MED
    /* random()&01 will produce a random binary value. */
    return (arr[(int) n / 4 + (int) (random () & 01)]);
#else
    /* this averages the hi and lo */
    return ((arr[(int) n / 4] + arr[(int) n / 4 - 1]) / 2.0);
#endif
  else
    return (arr[(int) ((n - 1) / 4)]);
}

double thirdquartile (arr, n)
  double arr[];
  int n;

/*
 * Returns the third quartile of a sorted array ({\tt arr}) of {\tt n}
 * doubles.
 */
{
  if (!(n % 4))
#ifdef RAND_MED
    /* random()&01 will produce a random binary value. */
    return (arr[(int) (3 * n / 4) + (int) (random () & 01)]);
#else
    /* this averages the hi and lo */
    return ((arr[(int) 3 * n / 4] + arr[(int) 3 * n / 4 - 1]) / 2.0);
#endif
  else
    return (arr[(int) (3 * (n - 1) / 4)]);
}
