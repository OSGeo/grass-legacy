#include <stdio.h>
#include "polish.h"

/* georesid.c - georeference residuals. Puts residuals (and effects?) into
   a global array along with their x and y coordinates */

georesid(y, p, q, maxdepth, tmpx, tmpy, effects, no_sites, row_g, col_g, all_g)
     double ***y, *tmpx, *tmpy, **row_g, **col_g, *all_g;
     int p, q, maxdepth, effects, no_sites;
{
  int k, el, j, d, alloc, tmp;
  extern float *datax;
  extern float *datay;
  extern int *cats;
  extern char **desc;

  /* Memory management---add room for row, column, and all effects */
  alloc = no_sites + p+q+2;
  if (alloc > LIMIT)
  {
    datax = (float *) G_realloc (datax, alloc * sizeof (float));
    datay = (float *) G_realloc (datay, alloc * sizeof (float));
    desc = (char **) G_realloc (desc, alloc * sizeof (char *));
    for (j = 0; j <= alloc; j++)  
      desc[j] = (char *) G_realloc (desc[j], 40 * sizeof (char));
    cats = (int *) G_realloc (cats, alloc * sizeof (int));
  }
  /* Associate coordinates with residuals */
  for (j = 0; j < no_sites; ++j) {
    sscanf(desc[j], "|%d|%d|%d|%d|", &tmp,&k,&el,&d);
    datax[j] = (float) tmpx[tmp];
    datay[j] = (float) tmpy[tmp];
    sprintf(desc[j],"%g", y[k][el][d]);
    cats[j] = j;
  }

  if (effects)
  {
    for(k=0; k<p; ++k)
    {
      datax[j] = row_g[k][0];
      datay[j] = row_g[k][1];
      sprintf(desc[j], "%g Row", y[k][q][0]);
      cats[j] = j++;
    }
    for(el=0; el<q; ++el)
    {
      datax[j] = col_g[el][0];
      datay[j] = col_g[el][1];
      sprintf(desc[j], "%g Column", y[p][el][0]);
      cats[j] = j++;
    }
    datax[j] = all_g[0];
    datay[j] = all_g[1];
    sprintf(desc[j], "%g All", y[p][q][0]);
    cats[j] = j;
  }
  return j;
}
