#include <stdio.h>
#include "globals.h"

/***************************************************************************/

allocate(DATA,NN,bands)
double *DATA[MX];
long   NN,bands;
{
  int i;
  for (i=0 ; i<bands; i++)
    {
      DATA[i] = (double *) G_malloc(NN*sizeof(double));
      if (DATA[i] == NULL)
        {
          G_fatal_error("Insufficient memory for allocation of data sturctures\n");
        }
    }
}


/***************************************************************************/

freemem(DATA,bands)
long   bands;
double *DATA[MX];
{
  int i;
  for (i=0 ; i<bands; i++)
    {
      free((char *)DATA[i]);
    }
}
