#include "raster.h"
#include "graph.h"


/*!
 * \brief get mouse location using pointer
 *
 * A cursor is put on the screen at the location
 * specified by the coordinate found at the <b>wx,wy</b> pointers. This cursor
 * tracks the mouse (or other pointing device) until one of three mouse buttons
 * are pressed. Upon pressing, the cursor is removed from the screen, the current
 * mouse coordinates are returned by the <b>wx</b> and <b>wy</b> pointers,
 * and the mouse button (1 for left, 2 for middle, and 3 for right) is returned
 * in the <b>button</b> pointer.
 *
 *  \param wx
 *  \param wy
 *  \param button
 *  \return int
 */

int R_get_location_with_pointer(int *wx,int *wy,int *button )
{
	int z ;
	_send_ident(GET_LOCATION_WITH_POINTER) ;
	z = *wx ;
	_send_int(&z) ;
	z = *wy ;
	_send_int(&z) ;
	z = *button ;
	_send_int(&z) ;
	_get_int(wx) ;
	_get_int(wy) ;
	_get_int(button) ;

	return 0;
}
