/*- void in_region (x,y) 	// returns 1 if x,y are in window, else 0
 * double x,y;       		
 */

#include<stdio.h>
#include "gis.h"

extern struct Cell_head window;

int 
in_region (double x, double y)
{
  if ( x >= window.west && x <= window.east &&
       y >= window.south && y <= window.north)
    return 1;
  else
  {
    return 0;
  }
}
