#include <math.h>
#include "gis.h"

int 
make_histo (int **p, int np, int nsites)
{
  int i, j;


  /* minimum number of sites per partition */
  j = (int) floor ((double) nsites / np);

  *p = (int *) G_malloc (np * sizeof (int));
  if (*p == NULL)
    G_fatal_error ("Memory allocation error");
  for (i = 0; i < np; ++i)
    (*p)[i] = j;
  i = np * j;
  j = 0;
  while (i++ < nsites)
    (*p)[j++]++;

  /* return max number of sites per partition */
  return (int) ceil ((double) nsites / np);
}
