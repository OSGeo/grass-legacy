#include "gis.h"

/* int select_model () */

int select_model ()
/*
 * Menu function to selection a semivariogram model. Integer value that
 * corresponds to known models (see {\tt semivar.h}) is returned.
 */
{
  int set = 0;
  int manual=0;

  V_clear ();
  vee_constants ();
  V_line (0, "Choose a variogram model");
  V_line (4, "Model Selection");
  V_ques (&set, 'i', 4, 30, 1);
  /* consult semivar.h for numbers */
  V_line (7, "  1   Linear");
  V_line (8, "  2   Spherical");
  V_line (9, "  3   Exponential");
  V_line (10, "  4   Gaussian");
  V_line (11, "  5   Quadratic");
  V_line (12, "  6   Hole Effect");
  V_line (13, "  7   Power");
  V_line (14, "  8   Nugget");
  V_line (15, "  9   RBF of Mitas/Mitasova");
  V_line (17, "Set parameters manually?");
  V_ques (&manual, 'i', 17, 30, 1);

  do
  {
    if (!V_call ())
      exit (0);
  /* } while (set <= 0 && set > 7 && manual != 'y' && manual != 'n' ); */
  } while (set <= 0 && set > 9 );

  if (manual) set +=100;
  return set;
}
