#include<math.h>
#include "svfit.h"

/*-void plot_model ( HGN *list, PARAM parameters, int nwork) */

void plot_model (list, parameters, nwork, save, verbose)
  char * save;
  int nwork, verbose;
  HGN *list;
  PARAM parameters;

/*
 * Driver function for plotting semivariogram models with their data.
 * The current model is determined from 
 * \verb|parameters.model| and the appropriate function is called.
 */
{

  switch (parameters.model)
  {
  case LINEAR:
    plot_linear (list, parameters, nwork, verbose, save);
    break;
  case SPHERICAL:
    plot_spherical (list, parameters, nwork, verbose, save);
    break;
  case EXPONENTIAL:
    plot_exponential (list, parameters, nwork, verbose, save);
    break;
  case GAUSSIAN:
    plot_gaussian (list, parameters, nwork, verbose, save);
    break;
  case QUADRATIC:
    plot_quadratic (list, parameters, nwork, verbose, save);
    break;
  case HOLE_EFFECT:
    plot_hole_effect (list, parameters, nwork, verbose, save);
    break;
#ifdef OTHERS
  case POWER:
    plot_power (list, parameters, nwork, verbose, save);
    break;
  case RBF:
    plot_crst (list, parameters, nwork, verbose, save);
    break;
#endif
  default:
    break;
  }
  return;
}
