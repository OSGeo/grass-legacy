#include <math.h>

double
perimeter(np,x,y)
  register int np;
  register double *x, *y;
 {
  register double tot_perim;
  register double hyp;
  register int i;
  extern double hypot();
  register double *xptr1, *xptr2, *yptr1, *yptr2;

  xptr1 = x;
  yptr1 = y;
  xptr2 = x + 1;
  yptr2 = y + 1;
  tot_perim = 0.0;

  for (i=1; i < np; i++) 
   {
    hyp = hypot(*xptr1-*xptr2, *yptr1-*yptr2);
    tot_perim += hyp;
    xptr1++ ; xptr2++ ; yptr1++; yptr2++;
   }
  return(tot_perim);
 }
