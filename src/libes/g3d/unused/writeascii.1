
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include "G3d.h"

/*---------------------------------------------------------------------------*/

void
G3d_writeAscii (map, fname)

     void *map; 
     char *fname;

{
  FILE *fp;
  double d1 = 0;
  double *d1p;
  float *f1p;
  int x, y, z;
  int rows, cols, depths, typeIntern;

  G3d_getCoordsMap (map, &rows, &cols, &depths);
  typeIntern = G3d_tileTypeMap (map);

  d1p = &d1; f1p = (float *) &d1;

  if (fname == NULL)
    fp = stdout;
  else
    if ((fp = fopen (fname, "w")) == NULL) 
      G3d_fatalError ("G3d_writeAscii: can't open file to write\n");

  for (z = 0; z < depths; z++) 
    for (y = 0; y < cols; y++) {
      fprintf (fp, "z y x %d %d (%d - %d)\n", z, y, 0, rows - 1);
      for (x = 0; x < rows; x++) {
	G3d_getValueRegion (map, x, y, z, d1p, typeIntern);
	
	if (typeIntern == G3D_FLOAT)
	  fprintf (fp, "%.18f ", *f1p);
	else
	  fprintf (fp, "%.50lf ", d1);
      }
      fprintf (fp, "\n");
    }

  if (fp != stdout) fclose (fp);
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
