#include "gis.h"
#include "s_struct.h"

int readsites (fdsite, verbose, xyd, window)
  FILE *fdsite;
  int verbose;
  Z **xyd;
  struct Cell_head window;
{
  char *dum;
  int i, alloced = 1000;
  double east, north;

  G_sleep_on_error (0);

  if (verbose)
    fprintf (stderr, "Reading sites list ...              ");

  /* allocate chunk of memory */
#ifndef lint
  (*xyd) = (Z *) G_malloc (alloced * sizeof (Z));
#endif
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
#ifndef lint
	(*xyd) = (Z *) G_realloc (alloced * sizeof (Z));
#endif
	if ((*xyd) == NULL)
	  G_fatal_error ("cannot allocate memory");
      }
      G_strcpy ((*xyd)[i].desc, dum);
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
