
double
area_calc(np,x,y)
  register int np;
  register double *x,*y; 
 {
  register double total_area;
  register int i;
  register int jj, kk;

  total_area = 0.0;
  for (i=0; i < (np-1); i++) 
   {
    if (i==0)
      jj = (np-1)-1;
    else
      jj = i - 1;
    if (i==((np-1)-1))
      kk = 0;
    else
      kk = i + 1;
    total_area = total_area +  ( (*(x+i)) * ( (*(y+jj)) - (*(y+kk)) ) );
   }
  total_area = (double) 0.5 * total_area;

  return (total_area);
 }
