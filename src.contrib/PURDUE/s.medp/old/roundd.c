#include<stdio.h>

/*-test program
main()
{
  double roundd();

  printf("%g\t%g\n",1000/3., roundd( (double) (1000/3.), 2) );
}
 */

double roundd (d, i)
  double d;
  int i;
{
  char str[128];
  double atof ();

  sprintf (str, "%.*f%c", i, d, NULL);
  return atof (str);
}
