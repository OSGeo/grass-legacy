#include "raster.h"
#include "graph.h"


/*!
 * \brief draw a closed polygon
 *
 * The <b>number</b> absolute positions in the <b>x</b> and <b>y</b> arrays
 * outline a closed polygon which is filled with the current color. The current
 * position is left updated to the position of the last point.
 *
 *  \param xarray x
 *  \param yarray y
 *  \param number
 *  \return int
 */

int R_polygon_abs(int *xarray,int *yarray , int number )
{
	int n ;
	_send_ident(POLYGON_ABS) ;
	n = number ;
	_send_int(&n) ;
	_send_int_array(number,xarray) ;
	_send_int_array(number,yarray) ;

	return 0;
}
