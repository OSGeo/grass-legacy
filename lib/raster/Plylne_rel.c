#include "raster.h"
#include "graph.h"


/*!
 * \brief draw an open polygon
 *
 * The <b>number</b> relative positions in the <b>x</b> and <b>y</b>
 * arrays are used to generate a multisegment line (often curved). The first
 * position is relative to the starting current location; the succeeding
 * positions are then relative to the previous position. The current position is
 * updated to the position of the last point. This line is drawn with the current
 * color.
 * <b>Note.</b> No line is drawn between the last point and the first point.
 *
 *  \param xarray x
 *  \param yarray y
 *  \param number
 *  \return int
 */

int R_polyline_rel(int *xarray,int *yarray, int number )
{
	int n ;
	_send_ident(POLYLINE_REL) ;
	n = number ;
	_send_int(&n) ;
	_send_int_array(number,xarray) ;
	_send_int_array(number,yarray) ;

	return 0;
}
