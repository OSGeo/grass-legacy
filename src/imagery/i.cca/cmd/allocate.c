#include <stdlib.h>
#include "gis.h"
#include "globals.h"

/***************************************************************************/

int 
allocate (double *DATA[MX], int NN, int bands)
{
  int i;
  for (i = 1 ; i <= bands ; i++)
    {
      DATA[i] = (double *) G_malloc(NN * sizeof(double));
      if (DATA[i] == NULL)
        {
          fprintf(stderr, "Insufficient memory for allocation of data structures\n");
          exit(-1);
        }
    }

    return 0;
}


/*****************************************************************************/

int freemem (double *DATA[MX], int bands)
{
  int i;
  for (i=1 ; i<=bands; i++)
    {
      free(DATA[i]);
    }

    return 0;
}
