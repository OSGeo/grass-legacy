#include <math.h>

double
perimeter(np,x,y)
  register int np;
  register double *x, *y;
 {
  register double tot_perim;
  register double hyp;
  register int i;
  register int ii, jj;
  extern double hypot();

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
    hyp = hypot(  ((*(x+jj))-(*(x+ii))), ((*(y+jj))-(*(y+ii)))  );
    tot_perim = tot_perim + hyp;
   }
  return(tot_perim);
 }
