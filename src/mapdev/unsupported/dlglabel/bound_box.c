/*  @(#)bound_box.c	2.1  6/26/87  */
#include "dlg.h"

/*
 * b_box_area   Establishes bounding box for areas
 *
 */

b_box_areas()
{
	int i ;
	for(i=1; i<=tot_areas; i++)
		b_box_area(i) ;
}

b_box_area(i)
	int i ;
{
	int b ;
	int l ;

	l = abs(area[i].lines[0]) ;
	area[i].N = line[l].N ;
	area[i].S = line[l].S ;
	area[i].E = line[l].E ;
	area[i].W = line[l].W ;
	for(b=1; b<area[i].n_lines; b++)
	{
		if ( ! (l = abs(area[i].lines[b])))		/*  rest are islands  */
			break;
		if (area[i].N < line[l].N )
			area[i].N = line[l].N ;
		if (area[i].S > line[l].S )
			area[i].S = line[l].S ;
		if (area[i].E < line[l].E )
			area[i].E = line[l].E ;
		if (area[i].W > line[l].W )
			area[i].W = line[l].W ;
	}
}
