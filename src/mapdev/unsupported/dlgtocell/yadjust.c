/* @(#)yadjust.c	2.2   9/21/87 */
#include <stdio.h>
#include "dlg.h"

/* FIRST  change all Y grid coordinates to array coordinates.
 *        (add .5 to all Y grid coordinates)
 *        X array coordinates are simply (int)(GRID_X)
 * THEN   add .0001 to all integer y values 
 *        This takes care of all sorts of evils and time later on  
 */

area_yadjust(yarray, num_verticies)
	double yarray[] ;
	int num_verticies ;
{
	register int row, incr ;
	register double *pointer ;

	for(pointer = yarray, incr=0; incr<=num_verticies; incr++, pointer++)
	{
		if (*pointer == ISLAND_MARKER)
			continue ;
		*pointer += .5 ;
		row = *pointer ;
		if (*pointer == (double)row )
			*pointer += .0001 ;
	}
}

line_yadjust(yarray, num_verticies)
	double yarray[] ;
	int num_verticies ;
{
	register int row, incr ;
	register double *pointer ;

	for(pointer = yarray+1, incr=0; incr<=num_verticies; incr++, pointer+=2)
	{
		*pointer += .5 ;
		row = *pointer ;
		if (*pointer == (double)row )
			*pointer += .0001 ;
	}
}

line_xadjust(xarray, num_verticies)
	double xarray[] ;
	int num_verticies ;
{
	register int row, incr ;
	register double *pointer ;

	for(pointer = xarray, incr=0; incr<=num_verticies; incr++, pointer+=2)
	{
		*pointer += .5 ;
		row = *pointer ;
		if (*pointer == (double)row )
			*pointer += .0001 ;
	}
}
