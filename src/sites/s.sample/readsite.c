#include "gis.h"
#include "methods.h"

Site **readsites (fdsite, verbose, nsites, window)
  FILE *fdsite;
  int verbose;
  int *nsites;
  struct Cell_head window;
{
  char *dum;
  int i, c, j, alloced = 1000;
  int dims = 0, cat = 0, strs = 0, dbls = 0;
  Site **sites;

  G_sleep_on_error (0);

  if (verbose)
    fprintf (stderr, "Reading sites list ...              ");

  /* allocate chunk of memory */
#ifndef lint
  sites = (Site **) G_malloc (alloced * sizeof (Site *));
#endif
  if (sites == NULL)
    G_fatal_error ("cannot allocate memory");

  if (G_site_describe (fdsite, &dims, &cat, &strs, &dbls) != 0)
    G_fatal_error ("failed to guess format");
  if (dbls < 1)
    dbls = 1;

  i = 0;
  if ((sites[i] = G_site_new_struct (c, dims, strs, dbls)) == NULL)
    G_fatal_error ("cannot allocate memory");

  while (G_site_get (fdsite, sites[i]) == 0)
  {
  /*  if (sites[i]->east >= window.west &&
	sites[i]->east < window.east &&
	sites[i]->north <= window.north &&
	sites[i]->north > window.south)
*/
    if (G_site_in_region(sites[i],&window))
    {
      if (i == alloced - 1)
      {
	alloced += 1000;
	if ((sites = (Site **) G_realloc (sites, alloced * sizeof (Site *)))
	    == NULL)
	  G_fatal_error ("cannot allocate memory");
      }
      if ((sites[++i] = G_site_new_struct (c, dims, strs, dbls)) == NULL)
	G_fatal_error ("cannot allocate memory");
    }
  }
  fclose (fdsite);
  G_sleep_on_error (1);
  if (verbose)
    G_percent (1, 1, 1);
  *nsites = i;
  return sites;
}

