/*
 * Copyright (C) 1994. James Darrell McCauley.  (darrell@mccauley-usa.com)
 * 	                                        http://www.usol.com/~mccauley/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

#include "gis.h"
#include "methods.h"

int readsites (fdsite, verbose, xyz)
  FILE *fdsite;
  int verbose;
  Z ** xyz;

/* Reads a sites list into {\tt xyz}, returning the number of sites found.  */
{
  char *dum;
  int i, n, c, d, allocated=1000;
  double east, north, ndesc, atof ();
  char desc[80];
  Site *s;
  extern struct Cell_head window;

  G_sleep_on_error (0);

  if (verbose)
    fprintf (stderr, "Reading sites list ...              ");


  if (G_site_describe (fdsite, &n, &c, &i, &d)!=0)
    G_fatal_error("failed to guess format");
  s = G_site_new_struct (c, n, i, d);

  if (d==0)
  {
    fprintf(stderr,"\n");
    G_warning("I'm finding records that do not have a floating point attributes (fields prefixed with '%').");
  }

  /* allocate chunk of memory */
  (*xyz) = (Z *) G_malloc (allocated * sizeof (Z));
  if ((*xyz)==NULL) G_fatal_error("cannot allocate memory");

  i = 0;
  /* while (G_get_site (fdsite, &east, &north, &dum) > 0) */
  while (G_site_get (fdsite, s) == 0) 
  {
    if (i == allocated)
    {
      allocated+=1000;
      (*xyz) = (Z *) G_realloc ((*xyz), allocated * sizeof (Z));
      if ((*xyz)==NULL) G_fatal_error("cannot allocate memory");
    }
    i++;
  }
  fclose (fdsite);
  G_sleep_on_error (1);
  if (verbose)
    G_percent (1, 1, 1);
  return i;
}
