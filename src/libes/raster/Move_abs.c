#include "raster.h"
#include "graph.h"


/*!
 * \brief move current location
 *
 * Move the current location to the absolute screen coordinate <b>x,y.</b>
 * Nothing is drawn on the screen.
 *
 *  \param x
 *  \param y
 *  \return int
 */

int R_move_abs(int x,int y)
{
	int z ;
	_send_ident(MOVE_ABS) ;
	z = x ;
	_send_int(&z) ;
	z = y ;
	_send_int(&z) ;

	return 0;
}
