#include "gis.h"
#include "svfit.h"
#include "graphics.h"

#define MAX_NUM 3 /* maximum number of parameters to models */

int domodel (wrk_list)
  HGN *wrk_list;

/*
 * Allocates memory for design matrix, solution vector, and LHS vector.
 * Calls \verb|select_model()| to determine which model should be fit. It
 * it returns a nonzero value, \verb|solvex()| is called to do QR
 * factorization. Memory is \verb|free()|'ed before the function returns
 * the model selected/fitted.
 */
{
  double **a, *b, *x;
  int i, m, n;
  extern int nwork;
  extern PARAM parameters;

  if ((parameters.model = select_model ()) != 0)
  {
    if (parameters.model > 99)
    {
      parameters.model -= 100;
      manual_model (wrk_list);
    }
    else
    {
      a = (double **) G_malloc (nwork * sizeof (double *));
      if (a == NULL)
	G_fatal_error ("Memory allocation error 1 (domodel.c)");
      for (i = 0; i < nwork; ++i)
      {
	a[i] = (double *) G_malloc (MAX_NUM * sizeof (double));
	if (a[i] == NULL)
	  G_fatal_error ("Memory allocation error 2 (domodel.c)");
      }
      b = (double *) G_malloc (nwork * sizeof (double));
      if (b == NULL)
	G_fatal_error ("Memory allocation error 3 (domodel.c)");
      x = (double *) G_malloc (MAX_NUM * sizeof (double));
      if (x == NULL)
	G_fatal_error ("Memory allocation error 4 (domodel.c)");

      pre_model (a, &m, &n, b, wrk_list);
      if (m > n)
	x = solvex (a, n, m, b);
      else
	G_warning ("Not enough points - range too small");

      update_parameters (x);
      display_parameters ();
      if (AUTOPLOT)
        plot_model (wrk_list);

      for (i = 0; i < nwork; ++i)
	free (a[i]);
      free (a);
      free (b);
      free (x);
    }
  }

  return parameters.model;
}
