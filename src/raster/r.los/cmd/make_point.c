/****************************************************************/
/*								*/
/*	make_point.c	in	~/src/Glos			*/
/*								*/
/*	This function allocates memory space for a new point,	*/
/*	initializes the various fields using the values of	*/
/*	the parameters passed and returns the address of this	*/
/*	new point so that it could be attached in the linked	*/
/*	list.							*/
/*								*/
/****************************************************************/

#include "point.h"
#define NULL 0

#define		NEW_PT_X		NEW_PT->x	
#define		NEW_PT_Y		NEW_PT->y
#define		NEW_PT_ORIENTATION	NEW_PT->orientation
#define		NEW_PT_INCLINATION	NEW_PT->inclination
#define		NEXT_NEW_PT		NEW_PT->next
                                            

struct point *make_point(orientation,inclination,y,x)
        double orientation,inclination;
        int x,y;
 
{       struct point *NEW_PT;
        char *G_malloc();
 
        NEW_PT = (struct point *) G_malloc(sizeof(struct point));
        NEW_PT_ORIENTATION = orientation;
        NEW_PT_INCLINATION = inclination;
        NEW_PT_Y           = y;
        NEW_PT_X           = x;
        NEXT_NEW_PT        = NULL;
 
        return(NEW_PT);
}

/********* END OF FUNCTION "MAKE_POINT" *************************/
