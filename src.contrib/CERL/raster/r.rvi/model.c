/******************************************************************************
NAME:                        model.c 
 
PURPOSE    To calculate ground feature of each pixel by remote sensing data
	   using rvi regression model.
	After run r.rational.regression, a rvi model is coefficient vector
	amodel[7] and with the definition of:
			  a[0]x1 + a[1]x2 + a[2]x3 + a[6]
	ground feature = ---------------------------------
			  a[3]x1 + a[4]x2 + a[5]x3 + 1.0
 
******************************************************************************/
#include "gis.h"
#include "globals.h"

void model(amodel, rowbuffer, columns)
     double amodel[7];
     CELL *rowbuffer[NBANDS];
     int columns;
{
  int    sample;		/* sample indicator                      */
  double   band1;		
  double   band2;
  double   band3;
	
  for (sample= 0;sample < columns; sample++)
    {
      band1 = (double) rowbuffer[0][sample];
      band2 = (double) rowbuffer[1][sample];
      band3 = (double) rowbuffer[2][sample];
	  rowbuffer[0][sample] = 
   ( amodel[0]*band1 + amodel[1]*band2 + amodel[2]*band3 + amodel[6] ) /
   ( amodel[3]*band1 + amodel[4]*band2 + amodel[5]*band3 + 1.0 ); 
    }
}
