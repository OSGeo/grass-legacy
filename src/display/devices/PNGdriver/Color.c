/*
 * Identify a color that has been set in the reset_color() (found in Reset_clr.c
 * file in this directory).  Subsequent graphics calls will use this color.
 *
 * Called by:
 *      Color() in ../lib/Color.c
 */

#include "png.h"

extern int colorTable[];
 
int color(number)
	int number ;
{
  currentColor = colorTable[number];
  /*fprintf(stderr,"color: Setter farge til %d\n",number);*/
  return(0);
}
