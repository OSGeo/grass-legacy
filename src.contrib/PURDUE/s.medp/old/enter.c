#include<stdio.h>
main()
{ int i,j;
  float y[24][17];
  FILE *out;

  out = fopen("ca.dat","w");

  for(i=1; i<23; ++i)
    for(j=23; j>0; --j)
       y[i][j]=0;

  for(i=1; i<23; ++i)
  {
    for(j=23; j>0; --j)
    {
       printf("Enter x=%d y=%d: ",i,j);
       scanf("%f", &(y[i][j]));
       if (y[i][j]==-1)
         j=0;
       if (y[i][j]==-999)
         j=0,i=23;
    }
    fprintf(stderr,"(Saving data...");
    for(j=23; j>0; --j)
    {
      if (y[i][j]==0 && y[i][j]==-1)
        fprintf(out,"    ");
      else 
        fprintf(out,"%.2f ",y[i][j]);
    }
    fprintf(out,"\n");
    fprintf(stderr,"done)\n");
  }
  fclose(out);
}
