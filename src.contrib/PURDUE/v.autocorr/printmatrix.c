#include<stdio.h>
printmatrix (str, n)
  FILE *str;
  int n;
{
  int i, j;
  extern int **c;

  fprintf (str, "Connectivity Matrix\n");
  for (i = 1; i <= n; ++i)
  {
    for (j = 1; j <= n; ++j)
    {
      fprintf (str, "%d ", c[i][j]);
    }
    fprintf (str, "\n");
  }
}
