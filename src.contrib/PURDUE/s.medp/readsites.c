/*-int readsites (fdsite,all,label) // returns the number of sites read
 * FILE *fdsite;                    // ascii file that contains sites
 * int all;                         // flag that says whether to use the
 *                                  // default or current region
 * read sites, stores in external data array, and closes site file.
 */

#include "gis.h"
#include "polish.h"

int readsites (fdsite, all, verbose)
  FILE *fdsite;
  int all, verbose;
{
  int i, nsites;
  double east, north;
  char *dum;
  extern int *cats;
  extern char **desc;
  extern struct Cell_head window;
  extern float *datax, *datay;

  nsites = 0;

  if (verbose)
    fprintf (stderr, "Reading sites list ...              ");
  /* allocate chunk of memory */
  datax = (float *) G_malloc (LIMIT * sizeof (float));
  datay = (float *) G_malloc (LIMIT * sizeof (float));
  if (datax == NULL || datay == NULL)
      G_fatal_error ("Insufficent memory for allocation of data structure-1");
  
  desc = (char **) G_malloc (LIMIT * sizeof (char *));
  for (i = 0; i <= LIMIT; i++)    {
    desc[i] = (char *) G_malloc (40 * sizeof (char));
    if (desc[i] == NULL)
      G_fatal_error ("Insufficent memory for allocation of data structure-2");
  }
  cats = (int *) G_malloc (LIMIT * sizeof (int));
  if (cats == NULL)
    G_fatal_error ("Insufficent memory for allocation of data structure-3");

  i = 0;
  /*
   * this silently reads only LIMIT sites, then quits. I really should put in
   * a check and allocate more memory if necessary. It would be ideal if
   * there was a {\em quick} way of determining the number of sites in a file
   */
  while (G_get_site (fdsite, &east, &north, &dum) > 0 && i < LIMIT)
  {
    if (all || (east >= window.west && east <= window.east &&
		north <= window.north && north >= window.south))
    {
      sscanf (dum, "#%d ", &cats[i]);
      sscanf (dum, "#%*d %s", desc[i]);
      datax[i] = east;
      datay[i++] = north;
    }
  }
  fclose (fdsite);
  if (verbose)
    G_percent (1, 1, 1);
  return i;
}
