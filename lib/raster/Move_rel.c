#include "raster.h"
#include "graph.h"


/*!
 * \brief move current location
 *
 * Shift the current screen location by the values in <b>dx</b> and <b>dy</b>:
 \code
   Newx = Oldx + dx;
   Newy = Oldy + dy;
 \endcode
 * Nothing is drawn on the screen.
 *
 *  \param x dx
 *  \param y dy
 *  \return int
 */

int R_move_rel(int x,int y )
{
	int z ;
	_send_ident(MOVE_REL) ;
	z = x ;
	_send_int(&z) ;
	z = y ;
	_send_int(&z) ;

	return 0;
}
