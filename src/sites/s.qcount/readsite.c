/*
 * Copyright (C) 1994-1995. James Darrell McCauley. (darrell@mccauley-usa.com)
 * 	                          http://mccauley-usa.com/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

#include "gis.h"
#include "s_struct.h"
#include "quaddefs.h"

int readsites (fdsite, verbose, xyd, window)
  FILE *fdsite;
  int verbose;
  Z **xyd;
  struct Cell_head window;
{
  char *dum;
  int i, c, alloced = 1000;
  Site *s;

  G_sleep_on_error (0);

  s = G_site_new_struct (c, 2, 0, 0);

  if (verbose)
    fprintf (stderr, "Reading sites list ...              ");

  /* allocate chunk of memory */
#ifndef lint
  (*xyd) = (Z *) G_malloc (alloced * sizeof (Z));
#endif
  if ((*xyd) == NULL)
    G_fatal_error ("cannot allocate memory for input sites list");
  i = 0;
  while (G_site_get (fdsite, s) == 0)
  {
    if (s->east >= window.west && s->east <= window.east &&
	s->north <= window.north && s->north >= window.south)
    {
      if (i == alloced)
      {
	alloced += 1000;
#ifndef lint
	(*xyd) = (Z *) G_realloc (*xyd, alloced * sizeof (Z));
#endif
	if ((*xyd) == NULL)
          G_fatal_error ("cannot allocate memory for input sites list");
      }
      (*xyd)[i].x = s->east;
      (*xyd)[i++].y = s->north;
    }
  }
  fclose (fdsite);
  G_sleep_on_error (1);
  if (verbose)
    G_percent (1, 1, 1);
  return i;
}
