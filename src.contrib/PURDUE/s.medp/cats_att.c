#include <stdlib.h>
#include "gis.h"
#include "Vect.h"
#include "dig_atts.h"
#include "local_proto.h"

static int intcompare (const void *ii, const void *jj)
{
  const int *i = ii, *j = jj;
  return (*i - *j);
}

void write_cats_att (FILE *fd_att, FILE *fd_cats, int n, int verbose)
{
  int i = 0;
  extern int *cats;
  extern char **desc;
  extern float *datax, *datay;


  /*
   * from /usr/grass4/src/raster/r.support/modcats.c G_write_vector_cats
   * (name, &cats);
   * 
   * should probably use library function
   */


  fprintf (fd_cats,
	   "# %d categories\ndescription\n\n0.00 0.00 0.00 0.00\n", n);
  fprintf (fd_cats, "0:no data\n");

  qsort (cats, n, sizeof (int), intcompare);

  if (verbose)
    fprintf (stderr, "Writing category file ...           ");
  fprintf (fd_cats, "%d:%s\n", cats[i], desc[i++]);
  while (i < n)
  {
    if (verbose)
      G_percent (i, n, 10);
    if (cats[i] != cats[i - 1])
      fprintf (fd_cats, "%d:%s\n", cats[i], desc[i]);
    i++;
  }
  if (verbose)
    G_percent (1, 1, 2);

  if (verbose)
    fprintf (stderr, "Writing attribute file ...          ");
  for (i = 0; i < n; ++i)
  {
    if (verbose)
      G_percent (i, n, 2);
    write_att (fd_att, 'P', datax[i],datay[i], cats[i]);
  }
  if (verbose)
    G_percent (1, 1, 2);

}
