/* "pt_in_WIND" will confirm whether the point (n,e) is within the "WIND"    */
/* area or not.  If the point (n,e) is within the "WIND" area then this      */
/* function will return a 1.  If the point (n,e) is NOT within the "WIND"    */
/* area then this function will return a 0.                                  */
#include "distance.h"
int
pt_in_WIND(n,e)
  double n;
  double e;
 {
  int point_in;

  if (n<=window.north && n>=window.south &&
      e<=window.east &&  e>=window.west    )
    point_in = 1;
  else
    point_in = 0;
  return(point_in);
 }
