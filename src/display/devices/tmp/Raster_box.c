/*
*
*  Written by the GRASS Team in the Winter of 88.
*/

#include	"igraphics.h"

extern int WNO ;
extern unsigned long VSI_PLANE_MASK ;

Raster_box_abs( x1, y1, x2, y2, short_array)
	int  x1, y1 ;
	int  x2, y2 ;
	short  short_array[] ;
{
	register  int y ;
	int  top, bottom ;
	short  left, right ;

	if ( y1<y2)
	{
		top = y1 ;
		bottom = y2 ;
	}
	else
	{
		top = y2 ;
		bottom = y1 ;
	}

	if ( x1<x2)
	{
		left = x1 ;
		right = x2 ;
	}
	else
	{
		left = x2 ;
		right = x1 ;
	}


	for ( y = top; y <= bottom; ++y) 
	{
		putline16 (WNO, (int)VSI_PLANE_MASK, left, (short)y,
			right, (short)y, short_array) ;
	}

}


