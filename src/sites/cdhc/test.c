#include<stdio.h>
main()
{
  static double h[2][3]={ {0.0, 1.0, 2.0}, {4.0, 6.0, 8.0} };
  int i,j;
  for(i=0;i<2;++i)
  {
    for(j=0;j<3;++j)
      fprintf (stdout,"%.1f ",h[i][j]);
    fprintf (stdout,"\n");
  }
}
