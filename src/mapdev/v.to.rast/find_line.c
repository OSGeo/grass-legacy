/* @(#)find_line.c	2.2   9/21/87 */
#include "gis.h"

/*
#define	DEBUG
*/

find_line(num_verticies, xarr, yarr)
	double *xarr, *yarr ;
	int num_verticies ;
{
    int node ;

    int  x,  y,  x_end,  y_end ;
    int  xinc,  yinc,  error ;
    int  delta_x,  delta_y ;

    /* adjust Y grid coordinates to Y array coordinates */
    /*
    line_yadjust( num_verticies, yarr) ;
    */


    store_xy( ifloor (xarr[0]) , ifloor (yarr[0]) ) ;

    num_verticies-- ;    /* Adjustment for the number of vert pairs */
    for (node=0; node<num_verticies; node++)
    {
	x = ifloor (xarr[node]);
	x_end = ifloor (xarr[node+1]);
	y = ifloor (yarr[node]);
	y_end = ifloor (yarr[node+1]);

	if (x == x_end && y == y_end)
	{
	    store_xy (x, y);
	    continue ;
	}

	bres_line (x, y, x_end, y_end);
    }

}		/*  main()  */


static  int  first_time = 1 ;

store_xy( col, row)
	int  col, row ;
{
	row_put (row, col);
}

