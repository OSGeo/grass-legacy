#include "stdio.h"
#include "stdlib.h"
#include "gis.h"
#include "idsfft.h"

#define SITE_BLOCK 512

int readsites (fdsite, all, verbose, xyz)
  FILE *fdsite;
  int all, verbose;
  Z **xyz;

/* Reads a sites list into {\tt xyz}, returning the number of sites found.  */
{
  char *dum;
  int i, nxyz, n;
  double east, north;
  struct Cell_head window;
  G_get_window (&window);
  G_sleep_on_error (0);

  if (verbose)
    fprintf (stderr, "Reading sites list ...              ");

  *xyz = NULL;
  nxyz = 0;
  i = 0;
  while (G_get_site (fdsite, &east, &north, &dum) > 0)
  {
    if (all || (east >= window.west && east <= window.east &&
		north <= window.north && north >= window.south))
    {
      if (sscanf(dum, "%d", &n) != 1)
          G_fatal_error("error in site file");
      if (i == nxyz) {
          nxyz += SITE_BLOCK;
          if ((*xyz = (Z *)realloc(*xyz, nxyz * sizeof(Z))) == NULL) {
              G_fatal_error("cannot allocate memory");
          }
      }
      (*xyz)[i].x=east;
      (*xyz)[i].y=north;
      (*xyz)[i].z=n;
      i ++;
    }
  }
  fclose (fdsite);
  G_sleep_on_error (1);
  if (verbose)
    G_percent (1, 1, 1);
  return i;
}
