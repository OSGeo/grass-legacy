#include "gis.h"
#include "sv.h"

int readsites (fdsite, all, label, verbose, xyz)
  FILE *fdsite;
  int all, label, verbose;
  Z ** xyz;

/* Reads a sites list into {\tt xyz}, returning the number of sites found.  */
{
  char *dum;
  int i;
  double east, north, ndesc, atof ();
  char desc[80];
  struct Cell_head window;
  G_get_window (&window);
  G_sleep_on_error (0);

  if (verbose)
    fprintf (stderr, "Reading sites list ...              ");

  /* allocate chunk of memory */
  (*xyz) = (Z *) G_malloc (1000 * sizeof (Z));
  if ((*xyz)==NULL) G_fatal_error("cannot allocate memory");
  i = 0;
  while (G_get_site (fdsite, &east, &north, &dum) > 0)
  {
    if (all || (east >= window.west && east <= window.east &&
		north <= window.north && north >= window.south))
    {
      if (label)
      {
	sscanf (dum, "#%*d %s", desc);
	ndesc = atof (G_strip (desc));
	if (ndesc == 0.0)
	{
	  G_warning ("possible non-numeric description field; setting to zero");
	  ndesc = 0;
	}
        (*xyz)[i].z = ndesc;
      }
      else
      {
        while (dum[0] == '#' || dum[0] == ' ' ) dum++;
 	sscanf (dum, "%lf ", &(*xyz)[i].z);
      }
      (*xyz)[i].x=east;
      (*xyz)[i++].y=north;
    }
  }
  fclose (fdsite);
  G_sleep_on_error (1);
  if (verbose)
    G_percent (1, 1, 1);
  return i;
}
