#include <math.h>
#include <stdlib.h>
#include "gis.h"
#include "univar.h"
#include "cell_site.h"

static double median (const Cell_Site *arr, int n);
static double firstquartile (const Cell_Site *arr, int n);
static double thirdquartile (const Cell_Site *arr, int n);

UNIV univariate (const Cell_Site *z, int nsites)

/*
 * Calculates the mean, standard deviation, quartiles, median, and
 * coefficient of variation for {\tt z}. Return value is a {\tt UNIV}
 * struct.
 */
{
  int i;
  UNIV stats;

  stats.n = nsites;
  stats.m = stats.s = stats.skw = stats.kur = stats.mse = stats.mav = 0;

  for (i = 0; i < stats.n; ++i)
  {
    stats.m   += z[i].datum;
    stats.mse += z[i].datum*z[i].datum;
    stats.mav += fabs(z[i].datum);
  }
  stats.m   /= stats.n;
  stats.mse /= stats.n;
  stats.mav /= stats.n;

  for (i = 0; i < stats.n; ++i)
  {
    stats.s   += pow((z[i].datum - stats.m), 2.0);
    stats.skw += pow((z[i].datum - stats.m), 3.0);
    stats.kur += pow((z[i].datum - stats.m), 4.0);
  }

  stats.skw /= stats.n;
  if (stats.n > 1)
      stats.skw /= pow(stats.s/(stats.n),1.5);

  stats.kur /= stats.n;
  if (stats.n > 1)
      stats.kur /= pow(stats.s/(stats.n),2.0);
  stats.kur -= 3.0;

  if (stats.n > 1)
      stats.s  = sqrt(stats.s/(stats.n-1));
  stats.cv = 100 * (stats.s / stats.m);
  stats.min = z[0].datum;
  stats.med = median (z, stats.n);
  stats.max = z[stats.n - 1].datum;
  stats.q1 = firstquartile (z, stats.n);
  stats.q3 = thirdquartile (z, stats.n);

  return stats;
}

static double median (const Cell_Site *arr, int n)

/*
 * Returns the median of a sorted array ({\tt arr}) of {\tt n} doubles.
 */
{
  if (!(n % 2))			/* then there is an even number of
				 * elements in set */
#ifdef RAND_MED
    /* random()&01 will produce a random binary value. */
    return arr[n / 2 + (int) (random () & 01)].datum;
#else
    /* this averages the hi and lo */
    return (arr[n / 2].datum + arr[n / 2 - 1].datum) / 2.0;
#endif
  else
    return arr[(n - 1) / 2].datum;
}

static double firstquartile (const Cell_Site *arr, int n)

/*
 * Returns the first quartile of a sorted array ({\tt arr}) of {\tt n}
 * doubles.
 */
{
  if (!(n % 4))
#ifdef RAND_MED
    /* random()&01 will produce a random binary value. */
    return arr[n / 4 + (int) (random () & 01)].datum;
#else
    /* this averages the hi and lo */
    return (arr[n / 4].datum + arr[n / 4 - 1].datum) / 2.0;
#endif
  else
    return arr[(n - 1) / 4].datum;
}

static double thirdquartile (const Cell_Site *arr, int n)

/*
 * Returns the third quartile of a sorted array ({\tt arr}) of {\tt n}
 * doubles.
 */
{
  if (!(n % 4))
#ifdef RAND_MED
    /* random()&01 will produce a random binary value. */
    return arr[(3 * n / 4) + (int) (random () & 01)].datum;
#else
    /* this averages the hi and lo */
    return (arr[3 * n / 4].datum + arr[3 * n / 4 - 1].datum) / 2.0;
#endif
  else
    return arr[(3 * (n - 1) / 4)].datum;
}

