#include "gis.h"

double
perimeter(np,x,y)
  register int np;
  register double *x, *y;
 {
  register double tot_perim;
  register int i;
  register int ii, jj;

  tot_perim = 0.0;
  for (i=0; i < (np-1); i++) 
  {
     if ( i==((np-1)-1) )
     {
      ii = i;
      jj = 0;
     }
     else
     {
      ii = i;
      jj = i + 1;
     }
     tot_perim = tot_perim + G_distance(*(x+jj), *(y+jj), *(x+ii), *(y+ii));
  }
  return(tot_perim);
 }
