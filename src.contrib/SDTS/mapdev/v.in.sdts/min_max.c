#include <stdio.h>
#include "sdts_in.h"

#ifdef FOO
get_min_max (axis, min, max, coord)
  int axis;
  int *min, *max;
  int coord;
{
    static int first_x = 1;
	static int first_y = 1;

	/*DEBUG*/
    if (axis == 1)
	{
   			 if(first_x) {
				 *min = *max = coord;
				 first_x = 0;
			 }
			 else {
				*min = (*min > coord) ? coord : *min;
				*max = (*max < coord) ? coord : *max;
			 }
	 }
     else
	 {
   			 if(first_y) {
				 *min = *max = coord;
				 first_y = 0;
			 }
			 else {
				*min = (*min > coord) ? coord : *min;
				*max = (*max < coord) ? coord : *max;
			 }
	}		
}
#endif

get_min_max (axis, coord, bounds)
  int axis;
  int coord;
  struct Bounds *bounds;
{
    static int first_x = 1;
	static int first_y = 1;

	/*DEBUG*/
    if (axis == 1)
	{
   			 if(first_x) {
				 bounds->min_x = bounds->max_x = coord;
				 first_x = 0;
			 }
			 else {
				bounds->min_x = (bounds->min_x > coord) ? coord : bounds->min_x;
				bounds->max_x = (bounds->max_x < coord) ? coord : bounds->max_x;
			 }
	 }
     else
	 {
   			 if(first_y) {
				 bounds->min_y = bounds->max_y = coord;
				 first_y = 0;
			 }
			 else {
				bounds->min_y = (bounds->min_y > coord) ? coord : bounds->min_y;
				bounds->max_y = (bounds->max_y < coord) ? coord : bounds->max_y;
			 }
	}		
}
