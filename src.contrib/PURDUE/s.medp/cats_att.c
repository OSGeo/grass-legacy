#include "gis.h"
#include "Vect.h"
#include "dig_structs.h"

void write_cats_att (fd_att, fd_cats, n,  verbose)
  FILE *fd_att, *fd_cats;
  int n,  verbose;
{
  int i = 0;
  extern int *cats;
  extern char **desc;
  extern int intcompare ();
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

static int intcompare (i, j)
  int *i, *j;
{
  return (*i - *j);
}
