#include "raster.h"
#include "graph.h"


/*!
 * \brief draw line
 *
 * Draw a line using the current color, selected via <i>R_color</i>, from the 
 * current location to the location specified by <b>x,y.</b> The current location
 * is updated to <b>x,y.</b>
 *
 *  \param x
 *  \param y
 *  \return int
 */

int R_cont_abs(int x,int y)
{
	int z ;
	_send_ident(CONT_ABS) ;
	z = x ;
	_send_int(&z) ;
	z = y ;
	_send_int(&z) ;

	return 0;
}
