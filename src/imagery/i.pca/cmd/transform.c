#include "gis.h"
#include "globals.h"

/***************************************************************************/

transform(DATA,NN,eigmat,bands,max)
double *DATA[MX];
long  NN;
double eigmat[MX+1][MX+1];
long bands;
CELL *max;
{
  int i,j,k;
  double sum[MX],min[MX];
  for (i=0 ; i<bands ; i++) max[i]=min[i]=0;
  for(i=0 ; i<NN ; i++)
    {
      for(j=1 ; j<=bands ; j++)
        {
          sum[j-1]=0.0;
          for(k=1 ; k<=bands ; k++)
            {
              sum[j-1] += eigmat[j][k] * (*(DATA[k-1]+i));
            }
        }
      for(j=0 ; j<bands ; j++)
        {
          if (sum[j]<min[j]) min[j]=sum[j];
          if (sum[j]>max[j]) max[j]=sum[j];
          *(DATA[j]+i) = sum[j];
        }
    }
  for (i=0 ; i<bands ; i++)
    if (min[i]<0)
      {
#ifdef DEBUG
        printf("Principal componet %d was shifted by +%f units.\n",i+1,-min[i]);
#endif
        for (j=0 ; j<NN ; j++) *(DATA[i]+j) -= min[i];
      }
  printf("Transform completed.\n");
}

