#include "globals.h"

readbands(nbands, cur)
    int nbands, cur;
{
  register int i;

  for (i=0; i<nbands; i++)
    if (G_get_map_row_nomask(Bandfd[i],Bandbuf[i],cur) < 0)
      G_fatal_error("Error reading cell file in readbands");
}
