/*
 * Copyright (C) 1993-1994. James Darrell McCauley. (darrell@mccauley-usa.com)
 * 	                          http://mccauley-usa.com/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

#include <string.h>
#include "gis.h"
#include "kcv.h"

int readsites (
  FILE *fdsite,
  int verbose,
  Z **xyd,
  struct Cell_head window)
{
  char *dum;
  int i, alloced = 1000;
  double east, north;

  G_sleep_on_error (0);

  if (verbose)
    fprintf (stderr, "Reading sites list ...              ");

  /* allocate chunk of memory */
  (*xyd) = (Z *) G_malloc (alloced * sizeof (Z));
  if ((*xyd) == NULL)
    G_fatal_error ("cannot allocate memory");
  i = 0;
  while (G_get_site (fdsite, &east, &north, &dum) > 0 && i < alloced)
  {
    if (east >= window.west && east <= window.east &&
	north <= window.north && north >= window.south)
    {
      if (i == alloced)
      {
	alloced += 1000;
	(*xyd) = (Z *) G_realloc (*xyd, alloced * sizeof (Z));
	if ((*xyd) == NULL)
	  G_fatal_error ("cannot allocate memory");
      }
      strcpy ((*xyd)[i].desc, dum);
      (*xyd)[i].partition = 0;
      (*xyd)[i].x = east;
      (*xyd)[i++].y = north;
    }
  }
  fclose (fdsite);
  G_sleep_on_error (1);
  if (verbose)
    G_percent (1, 1, 1);
  return i;
}
