#include "gis.h"
#include "svfit.h"

void display_parameters (parameters)
  PARAM parameters;
/*
 * displays semivariogram model parameters, stored in the 
 * structure {\tt parameters}. Does not
 * check to see if a model has been defined (i.e., if {\tt parameters} has
 * anything but garbage stored in it).
 */
{
  extern char *modelnames[NMODELS];
  extern char *estimatornames[NESTIMATORS];

  /* fprintf (stdout, "Estimator: %s\n", 
estimatornames[parameters.estimator]); */
  fprintf (stdout, "  model = %s\n", modelnames[parameters.model]);
/*-
  if (parameters.omni == 1)
    fprintf (stdout, "Omnidirectional: lag = %g +_ %g\n \n",
	     parameters.dist_interval, parameters.distance_tol);
  else
  {
    fprintf (stdout, "Directional: angle = %g +_ %g\n",
	     parameters.direction, parameters.angle_tol);
    fprintf (stdout, "               lag = %g +_ %g\n",
	     parameters.dist_interval, parameters.distance_tol);
  }
*/

  if (parameters.range < 0)
    fprintf (stdout, "  range = undefined\n");
  else
    fprintf (stdout, "  range = %g\n", parameters.range);

  if (parameters.sill < 0)
    fprintf (stdout, "   sill = undefined\n");
  else
    fprintf (stdout, "   sill = %g\n", parameters.sill);

  fprintf (stdout, " nugget = %g\n", parameters.c0);

  if (parameters.model == RBF)
    fprintf (stdout, "tension = %g\n", parameters.c1);
  else 
    fprintf (stdout, "     c1 = %g\n", parameters.c1);

  if (parameters.c0 < 0)
    G_warning("negative nugget");

  return;
}
