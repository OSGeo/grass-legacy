#include "raster.h"
#include "graph.h"


/*!
 * \brief draw a closed polygon
 *
 * The <b>number</b> relative positions in the <b>x</b> and <b>y</b>
 * arrays outline a closed polygon which is filled with the current color. The
 * first position is relative to the starting current location; the succeeding
 * positions are then relative to the previous position. The current position is
 * updated to the position of the last point.
 *
 *  \param xarray x
 *  \param yarray y
 *  \param number
 *  \return int
 */

int R_polygon_rel( int *xarray,int *yarray , int number )
{
	int n ;
	_send_ident(POLYGON_REL) ;
	n = number ;
	_send_int(&n) ;
	_send_int_array(number,xarray) ;
	_send_int_array(number,yarray) ;

	return 0;
}
