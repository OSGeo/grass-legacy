#include<stdio.h>
#include "polish.h"

main()
{ int i,j;
  double ***y, ***yold; 
  float tmp;
  FILE *inp;

  inp = fopen("coalash.dat","r");

  y=(double ***) malloc(25*sizeof(double **));
  yold=(double ***) malloc(25*sizeof(double **));
  for(i=0; i<24; ++i)
  {
    y[i]= (double **) malloc(19*sizeof(double *)); 
    yold[i]= (double **) malloc(19*sizeof(double *)); 
    for(j=0; j<19; ++j)
    {
      y[i][j]= (double *) malloc(1*sizeof(double));
      yold[i][j]= (double *) malloc(1*sizeof(double));
    }
  }

  for(i=0; i<23; ++i)
    for(j=15; j>=0; --j)
       y[i][j][0]=EMPTY_CELL;
  for(i=0; i<=23; ++i)
       y[i][16][0]=0.0;
  for(j=0; j<=16; ++j)
       y[23][j][0]=0.0;

  while(fscanf(inp,"%d %d %f\n", &i, &j, &tmp)!=EOF)
     if (tmp != 0)
	y[j-1][i-1][0]=tmp;
  fclose(inp);

  i=0;
  median_polish (y, yold, &i, 23, 16, 1, 0.50);

  myprint (y, yold);
  fprintf(stderr,"%d iterations\n", i);
}

myprint(y,yold)
double ***y, ***yold;
{
  int i,j;

    for (i = 23; i >= 0; --i)
    {
        for (j = 0; j < 17; ++j)
          if (y[i][j][0]!= EMPTY_CELL)
          fprintf(stderr, "%02.2f\t", y[i][j][0]);
          else
          fprintf(stderr, "      \t");
        fprintf(stderr, "\n");
    }
 if (y[23][16][0] == 9.82) getchar();
}
