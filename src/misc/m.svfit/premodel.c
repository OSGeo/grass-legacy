#include<stdio.h>
#include<math.h>
#include "svfit.h"


/*-void pre_model (double **a, int *m, int *n, double *b, HGN *wrk_list) */

PARAM pre_model (a, m, n, b, wrk_list, range, nwork, model, weighted)
  int *m, *n, nwork, model, weighted;
  double **a, *b, range;
  HGN *wrk_list;

/*
 * Pre-modeling: prepares design matrix {\tt a[0..m][0..n]} and vector
 * {\tt b[0..m]} (prior to QR factorization) from {\tt wrk\_list}. Values
 * are assigned to {\tt m} and {\tt n}. The {\tt parameters.\-model} number
 * is returned (zero if it does not exist).
 */
{
  int i, j;
  PARAM parameters;

  parameters.model=model;
  parameters.range=range;
  *m = 0;

  switch (parameters.model)
  {
  case LINEAR:
    *n = 2;
    while (wrk_list[*m].h <= parameters.range && *m < nwork)
    {
      a[*m][0] = 1.0;
      a[*m][1] = wrk_list[*m].h;
      b[*m] = wrk_list[*m].g;
      (*m)++;
    }
    (*m)--;
    break;
  case SPHERICAL:
    *n = 2;
    while (wrk_list[*m].h <= parameters.range && *m < nwork)
    {
      a[*m][0] = 1.0;
      a[*m][1] = 1.5 * wrk_list[*m].h / parameters.range - 0.5
	* pow (wrk_list[*m].h / parameters.range, 3.0);
      b[*m] = wrk_list[*m].g;
      (*m)++;
    }
    (*m)--;
    break;
  case EXPONENTIAL:
    *n = 2;
    while (wrk_list[*m].h <= parameters.range && *m < nwork)
    {
      a[*m][0] = 1.0;
      /* a[*m][1] = 1 - exp (-3.0*wrk_list[*m].h / parameters.range); */
      a[*m][1] = 1 - exp (-wrk_list[*m].h / parameters.range);
      b[*m] = wrk_list[*m].g;
      (*m)++;
    }
    (*m)--;
    break;
  case GAUSSIAN:
    *n = 2;
    while (wrk_list[*m].h <= parameters.range && *m < nwork)
    {
      a[*m][0] = 1.0;
      a[*m][1] = 1 - exp (-3.0 * pow (wrk_list[*m].h / parameters.range, 2.0));
      b[*m] = wrk_list[*m].g;
      (*m)++;
    }
    (*m)--;
    break;
  case QUADRATIC:
    *n = 2;
    while (wrk_list[*m].h <= parameters.range && *m < nwork)
    {
      a[*m][0] = 1.0;
      a[*m][1] = pow (wrk_list[*m].h, 2.0)
	/ (1 + pow (-wrk_list[*m].h, 2.0) / parameters.range);
      b[*m] = wrk_list[*m].g;
      (*m)++;
    }
    (*m)--;
    break;
  case HOLE_EFFECT:
    *n = 2;
    /* while (wrk_list[*m].h <= parameters.range && *m < nwork)  */
    while ( *m < nwork) 
    {
      a[*m][0] = 1.0;
      a[*m][1] = 1 - parameters.range * sin (wrk_list[*m].h / parameters.range)
	/ wrk_list[*m].h;
      b[*m] = wrk_list[*m].g;
      (*m)++;
    }
    (*m)--;
    break;
  case POWER:
    /*
     * have users enter a power? constrain it through the origin? use
     * nuggest from another model (linear)
     */
    parameters.c2 = -1.0;
    while (parameters.c2 <= 0 || parameters.c2 > 2.0)
    {
      do 
        printf("Enter lambda (0<lambda<2): ");
      while (scanf ("%lf",&parameters.c2) != 1);
    }
    *n = 2;
    while (wrk_list[*m].h <= parameters.range && *m < nwork)
    {
      a[*m][0] = 1.0;
      a[*m][1] = pow (wrk_list[*m].h, parameters.c2);
      b[*m] = wrk_list[*m].g;
      (*m)++;
    }
    (*m)--;
    break;
  default:
    parameters.model = 0;
    break;
  }

  if (weighted) /* do weighted least squares fitting */
  {
    for (i = 0; i < *m; ++i)
    {
      b[i] *= wrk_list[i].n;
      for (j = 0; j < *n; ++j)
        a[i][j] *= wrk_list[i].n;
    }
  }
  return parameters;
}
