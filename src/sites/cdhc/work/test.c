#include<stdio.h>
#include<stdlib.h>
main()
{
 double *xcopy;
 int i,n=10; 
 static int dcmp();

  if ((xcopy = (double *) malloc (n * sizeof (double))) == NULL)
    fprintf (stderr, "Memory error\n"), exit (-1);

  for (i = 0; i < n; ++i)
  {
    xcopy[i] = rand();
    fprintf (stdout,"before %d %g\n",i,xcopy[i]);
  }
 
  qsort (xcopy, n, sizeof (double), dcmp);
 
  for (i = 0; i < n; ++i)
    fprintf (stdout," after %d %g\n",i,xcopy[i]);
}

#ifdef OLD
int dcmp (a, b)
char *a, *b;
/* const void *a, *b; for ANSI */
 
{
  int result = 0;               /* integer to be returned */
  double diff, *x, *y;
 
  x=(double *) a;
  y=(double *) b;

  fprintf (stdout,"%g %g\n", x,y);

  if ((diff = (double *) a - (double *) b) < 0.0)
    result = -1;
  else if (diff > 0.0)
    result = 1;
  return result;
}
#else
static int 
dcmp (i, j)
    double *i, *j;
{
    if (*i < *j)
        return  -1;
 
    if (*i > *j)
        return 1;
 
    return 0;
}
#endif /*OLD*/
