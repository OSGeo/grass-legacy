#include "raster.h"
#include "graph.h"


/*!
 * \brief draw a series of dots
 *
 * Pixels at the <b>num</b> absolute positions in the <b>x</b> and
 * <b>y</b> arrays are turned to the current color. The current position is
 * left updated to the position of the last dot.
 *
 *  \param xarray x
 *  \param yarray y
 *  \param number
 *  \return int
 */

int R_polydots_abs(int *xarray,int *yarray ,int number )
{
	int n ;
	_send_ident(POLYDOTS_ABS) ;
	n = number ;
	_send_int(&n) ;
	_send_int_array(number,xarray) ;
	_send_int_array(number,yarray) ;

	return 0;
}
