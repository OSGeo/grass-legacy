#include "raster.h"
#include "graph.h"


/*!
 * \brief draw line
 *
 * Draw a line using the
 * current color, selected via <i>R_color</i>, from the current location to
 * the relative location specified by <b>x</b> and <b>y.</b> The current
 * location is updated:
  \code
   Newx = Oldx + x;
   Newy = Oldy + y;
  \endcode
 *
 *  \param x
 *  \param y
 *  \return int
 */

int R_cont_rel(int x,int y)
{
	int z ;
	_send_ident(CONT_REL) ;
	z = x ;
	_send_int(&z) ;
	z = y ;
	_send_int(&z) ;

	return 0;
}
