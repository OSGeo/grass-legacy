#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "gis.h"
#include "polish.h"
#include "local_proto.h"

static double median ( double *, int);
static int empty ( double);

int 
median_polish (
    double ***y,
    double ***yold,
    int *iter,
    int p,
    int q,
    int maxdepth,
    double thresh,	/* (p x q x maxdepth) gridded data. */
    int verbose
) 

/*-The median polish algorithm, described in
 * @Book{ cressie91,
 *   author =       "Noel A. C. Cressie",
 *   title =        "Statistics for Spatial Data",
 *   publisher =    "John Wiley \& Sons",
 *   year =         "1991",
 *   series =       "Wiley Series in Probability and Mathematical Statistics",
 *   address =      "New York, NY"
 * }
 * is implemented here.  This function handles the case where more than one
 * point may occupy a node on the overlay grid.  The extra rows and columns,
 * {p+q+1} cells, must be initialized to zero, while other cells contain
 * data values in array {y}. The array {yold} does not have to be initialized.
 */
{
  int i, k, el, m, n;
  double *tmparray, med;

  n = (q > p) ? q : p;
  tmparray = (double *) G_malloc (maxdepth * n * sizeof (double));

  n = 0;                        /* iteration counter */

  if (verbose)
    fprintf (stderr, "Median polish iterations ...         ");

  do
  {
    for (k = 0; k <= p; ++k)    /* save current data */
      for (el = 0; el <= q; ++el)
	for (m = 0; m < maxdepth; ++m)
	  yold[k][el][m] = y[k][el][m];

#ifdef DIAG
    myprint (y, yold);
#endif

    if (!(n%2))                     /* do this loop first */
    {
#ifdef DIAG
      fprintf(stderr,"DIAG: first alternative\n");
#endif
      for (k = 0; k <= p; ++k)
      {
	for (el = 0, i = 0; el < q; ++el)
	  for (m = 0; m < maxdepth; ++m)
	    if (!empty (y[k][el][m]))
	      tmparray[i++] = y[k][el][m];
	if (i)
        {
	  qsort (tmparray, i, sizeof (double), dblcompare);
	  med = median (tmparray, i);
	
	  for (el = 0; el < q; ++el) /* Subtract Median */
	    for (m = 0; m < maxdepth; ++m)
	      if (!empty (y[k][el][m]))
		y[k][el][m] -= med;
	  y[k][q][0] += med;       /* accumulate effect */
	}
      }
    }
    else
    {
#ifdef DIAG
      fprintf(stderr,"DIAG: second alternative\n");
#endif
      for (el = 0; el <= q; ++el)
      {
	for (k = 0, i = 0; k < p; ++k)
	  for (m = 0; m < maxdepth; ++m)
	    if (!empty (y[k][el][m]))
	      tmparray[i++] = y[k][el][m];
	if (i)
        {
	  qsort (tmparray, i, sizeof (double), dblcompare);
	  med = median (tmparray, i);

	  /* Subtract Median from column el */
	  for (k = 0; k < p; ++k)
	    for (m = 0; m < maxdepth; ++m)
	      if (!empty (y[k][el][m]))
		y[k][el][m] -= med;
	  y[p][el][0] += med;       /* accumulate effect */
	}
      }
    }
    n++;			/* increment iteration count */
    if(verbose)
      iteration(n);
  } while (!converged (y, yold, k, el, maxdepth, thresh));
  *iter = n;
  if(verbose)
    fprintf (stderr, "\n");

  return 0;
}

static double median (		/* returns the median of arr */
  double arr[],			/* sorted array of doubles */
  int n)			/* number of elements in array */
{
  if (!(n % 2))			/* then there is an even number of elements
				 * in set */
#ifdef RAND_MED
    /* random()&01 will produce a random binary value. */
    return (arr[(int) n / 2 + (int) (random () & 01)]);
#else
    /* this averages the hi and lo */
    return ((arr[(int) n / 2] + arr[(int) n / 2 - 1]) / 2.0);
#endif

  return (arr[(int) ((n - 1) / 2)]);
}


/* this if the comparison function for the qsort */
int dblcompare (const void *ii, const void *jj)
{
  const double *i = ii, *j = jj;
  if (*i - *j < 0)
    return -1;
  else if (*i - *j > 0)
    return 1;

  return 0;
}

/* checks to see if cell is used */
static int empty ( double c)
{
  if (c == EMPTY_CELL)
    return 1;

    return 0;
}

int converged (double ***y, double ***yold, int p, int q, int d, double thresh)
{
  int k, el, m;

  for (k = 0; k < p; ++k)
    for (el = 0; el < q; ++el)
      for (m = 0; m < d; ++m)
	if (!empty(yold[k][el][0]) &&
	    fabs (y[k][el][m] - yold[k][el][m]) > fabs (thresh))
	  return 0;
  return 1;
}
