#include <math.h>
#include "gis.h"
#include "s_struct.h"
#include "quaddefs.h"

int *count ( Z *quads, int nquads, double radius, Z *z, int nz, int verbose)

/*
 * counts the number of sites in the z struct of nz sites that fall within
 * nquads quads of a certain radius
 */
{

  int i,j,*counts=NULL;

#ifndef lint
  counts = (int *) G_malloc (nquads * sizeof (int));
#endif
  if (counts == NULL)
    G_fatal_error ("cannot allocate memory for counts");

  for (j = 0; j < nquads; ++j)
    counts[j] = 0.0;

  /* this may save us time on the hypot call */
  for (j = 0; j < nz; ++j)
    z[j].z = 1.0;

  if (verbose)
    fprintf (stderr, "Counting sites in quadrats ...      ");

  for (i = 0; i < nquads; ++i)
  {
    for (j = 0; j < nz; ++j)
    {
      if (z[j].z > 0)
      {
	if (hypot (z[j].x - quads[i].x, z[j].y - quads[i].y) <= radius)
	{
	  counts[i]++;
	  z[j].z = -1.0;
	}
      }
    }
    if (verbose)
      G_percent (i, nquads, 1);
  }

  if (verbose)
    G_percent (1, 1, 1);

  return counts;
}
