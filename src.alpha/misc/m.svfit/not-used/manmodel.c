#include<stdio.h>
#include<math.h>
#include "semivar.h"

/*-void manual_model (double **a, int *m, int *n, double *b,
                  HGN_LIST *wrk_list) */

void manual_model (wrk_list)
  HGN_LIST *wrk_list;

/*
 * Manual Modeling: user enters parameters of selected semivariogram
 * models, plots and screen display is updated.
 */
{
  extern int nwork;
  extern PARAM parameters;

  V_clear ();
    V_ques (&parameters.c0, 'd', 4, 20, 5);
    V_line (4, "nugget");
  if (parameters.model != RBF)
  {
    if (parameters.model = POWER)
    {
      V_line (5, "0 < exponent < 2");
      V_ques (&parameters.c1, 'd', 5, 20, 5);
    }
    else
    {
      V_line (5, "sill");
      V_ques (&parameters.sill, 'd', 5, 20, 5);
    }
    V_line (6, "range");
    V_ques (&parameters.range, 'd', 6, 20, 5);
  }
  else
  {
    V_ques (&parameters.c1, 'd', 5, 20, 5);
    V_line (5, "tension");
  }

  switch (parameters.model)
  {
  case LINEAR:
    V_line (0, "g(h)=nugget + slope * h, h < range");
    V_line (1, "g(h)=sill, h > range");
    break;
  case SPHERICAL:
    V_line (0,
      "g(h)=nugget + c * ( 1.5*h/range - 0.5*(h/range)^3 ), h < range ");
    V_line (1, "g(h)=nugget + c , h > range");
    break;
  case EXPONENTIAL:
    V_line (0, "g(h)=nugget + c * ( 1 - exp(-h/range) ) ");
    break;
  case GAUSSIAN:
    break;
  case QUADRATIC:
    V_line (0, "g(h)=nugget + c * h * ( 1 + h^2/range ) ");
    break;
  case HOLE_EFFECT:
    V_line (0, "g(h)=nugget + c * ( 1 - range * sin(h/range) / h ");
    break;
  case POWER:
    V_line (0, "g(h)=nugget + c * h^k ");
    break;
  case RBF:
    V_line (0, "R(p)=-[ E1(p) + ln(p) + Ce ]");
    V_line (1, "    where p = (tension*h/2)^2");
    break;
  default:
    parameters.model = 0;
    break;
  }

  do
  {
    V_intrpt_ok ();
    if (!V_call ())
      return;
    switch (parameters.model)
    {
    case LINEAR:
      parameters.c1 = (parameters.sill - parameters.c0) / parameters.range;
      break;
    case SPHERICAL:
      parameters.c1 = parameters.sill - parameters.c0;
      break;
    case EXPONENTIAL:
      parameters.c1 = (parameters.sill - parameters.c0) / 0.9502129;
      break;
    case GAUSSIAN:
      parameters.c1 = parameters.sill - parameters.c0;
      break;
    case QUADRATIC:
      parameters.c1 = parameters.sill - parameters.c0;
      break;
    case HOLE_EFFECT:
      parameters.c1 = parameters.sill - parameters.c0;
      break;
    case POWER:
      parameters.sill = -1;	/* undefined */
      break;
    case NUGGET:
      parameters.sill = -1;	/* undefined */
      parameters.c1 = -1;	/* undefined */
      parameters.range = -1;	/* undefined */
      break;
    case RBF:
      parameters.sill = -1;	/* undefined */
      parameters.range = -1;	/* undefined */
      break;
    default:
      break;
    }
    display_parameters ();
    plot_model (wrk_list);
  } while (parameters.range != 0);

  return;
}
