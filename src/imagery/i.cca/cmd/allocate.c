
#include "gis.h"
#include "globals.h"

/***************************************************************************/

allocate(DATA,NN,bands)
double *DATA[MX];
int NN;
int bands;
{
  int i;
  for (i = 1 ; i <= bands ; i++)
    {
      DATA[i] = (double *) G_malloc(NN * sizeof(double));
      if (DATA[i] == NULL)
        {
          printf("Insufficient memory for allocation of data sturctures\n");
          exit(-1);
        }
    }
}


/*****************************************************************************/

freemem(DATA,bands)
double *DATA[MX];
int   bands;
{
  int i;
  for (i=1 ; i<=bands; i++)
    {
      free(DATA[i]);
    }
}
