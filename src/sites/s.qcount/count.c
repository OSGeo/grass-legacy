/*
 * Copyright (C) 1994-1995. James Darrell McCauley. (darrell@mccauley-usa.com)
 * 	                          http://mccauley-usa.com/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "gis.h"
#include "site.h"
#include "quaddefs.h"

int *count_sites (SITE_XYZ *quads, int nquads, int *counts, double radius, 
    SITE_XYZ *z, int nz)
/*
 * counts the number of sites in the z struct of nz sites that fall within
 * nquads quads of a certain radius
 */
{

  int i,j;
  void *tmp;

  if (counts == NULL) {
    tmp = G_malloc (nquads * (sizeof(int)));
    if (tmp == NULL)
      G_fatal_error ("cannot allocate memory for counts");
    counts = (int *) tmp;
    for (j = 0; j < nquads; ++j)
      counts[j] = 0;
  }

  /* this may save us time on the hypot call */
  for (j = 0; j < nz; ++j)
    z[j].z = 1.0;

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
  }

  return counts;
}
/* vim: softtabstop=2 shiftwidth=2 expandtab */
