#include<math.h>
#include "svfit.h"

/*-void update_parameters(double *x); */

void update_parameters (x, parameters)
  double *x;
  PARAM *parameters;

/*
 * Updates the global parameter structure {\tt parameters} with the
 * solution vector {\tt x}, depending upon which model is chosen.
 */
{

  switch (parameters->model)
  {
  case LINEAR:
    parameters->c0 = x[0];
    parameters->c1 = x[1];
    parameters->sill = x[0] + parameters->range * x[1];
    break;
  case SPHERICAL:
    parameters->c0 = x[0];
    parameters->c1 = x[1];
    parameters->sill = x[0] + x[1];
    break;
  case EXPONENTIAL:
    parameters->c0 = x[0];
    parameters->c1 = x[1];
    parameters->sill = x[0] + x[1] * 0.9502129;
    break;
  case GAUSSIAN:
    parameters->c0 = x[0];
    parameters->c1 = x[1];
    parameters->sill = x[0] + x[1];	/* ?? */
    break;
  case QUADRATIC:
    parameters->c0 = x[0];
    parameters->c1 = x[1];
    parameters->sill = x[0] + x[1];	/* ?? */
    break;
  case HOLE_EFFECT:
    parameters->c0 = x[0];
    parameters->c1 = x[1];
    parameters->sill = x[0] + x[1];	/* ?? */
    break;
  case POWER:
    parameters->c0 = x[0];
    parameters->c1 = x[1];
    parameters->sill = -1.0;	/* undefined */
    break;
  default:
    break;
  }
  return;
}
