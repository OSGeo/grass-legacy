/****************************************************************/
/*								*/
/*	make_point.c	in	~/src/Gtraj			*/
/*								*/
/*	This function allocates memory space for a new point,	*/
/*	initializes the various fields using the values of      */
/*      the parameters passed and returns the address of this   */
/*      new point so that it could be attached in the linked    */
/*      list.                                                   */
/*                                                              */
/****************************************************************/

#include "point.h"
#define NULL 0

#define         NEW_PT_X                NEW_PT->x        
#define         NEW_PT_Y                NEW_PT->y
#define		NEW_PT_DISTANCE		NEW_PT->distance
#define         NEW_PT_ORIENTATION      NEW_PT->orientation
#define		NEW_PT_LOW_ANGLE	NEW_PT->low_angle
#define		NEW_PT_HIGH_ANGLE	NEW_PT->high_angle
#define         NEXT_NEW_PT             NEW_PT->next

POINT *make_point(distance, orientation, y, x,
					low_angle, high_angle)

        double distance ,orientation, low_angle, high_angle;
        int x,y;

{       POINT *NEW_PT;
        char *malloc();

	NEW_PT = (POINT *) malloc(sizeof(POINT));

	NEW_PT_X	   = x;
	NEW_PT_Y	   = y;
	NEW_PT_ORIENTATION = orientation;
	NEW_PT_DISTANCE    = distance;
	NEW_PT_LOW_ANGLE   = low_angle;
	NEW_PT_HIGH_ANGLE  = high_angle;
	NEXT_NEW_PT        = NULL;
 
        return(NEW_PT);

}

/**************** END OF FUNCTION "MAKE_POINT" ******************/
