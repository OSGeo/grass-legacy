#include "gis.h"
#include "Vect.h"
#define FUDGE 10

int c_matrix (struct Map_info *Map)
{
  int i, j, k, m;
  P_LINE *Line;
  extern int **c;

  /* allocate memory first */
  c = (int **) G_malloc ((Map->n_atts + FUDGE) * sizeof (int *));
  for (i = 0; i <= Map->n_atts + FUDGE; i++)
  {
    c[i] = (int *) G_malloc ((Map->n_atts + FUDGE) * sizeof (int));
    if (c[i] == NULL)
      G_fatal_error ("Insufficent mem for allocation of connectivity matrix");
  }

  /* just to make sure */
  for (i = 0; i <= Map->n_atts + FUDGE; ++i)
    for (j = 0; j <= Map->n_atts + FUDGE; ++j)
      c[i][j] = 0;

  /* now compute connectivity matrix */
  for (i = 1; i <= Map->n_lines; ++i)	/* for each line */
  {
    Line = &(Map->Line[i]);	/* just set pointer */

    if (Line->left && Line->right)	/* if line sits between 2 areas */
    {
      k = V2_area_att (Map, Line->left);	/* get the attributes of */
      m = V2_area_att (Map, Line->right);	/* those areas */
      if (k && m)		/* we don't care about "no data" */
	c[k][m] = c[m][k] = 1;	/* update symetric matrix */
    }
  }

  return Map->n_atts;
}
