#include "raster.h"
#include "graph.h"


/*!
 * \brief get mouse location using a line
 *
 * Similar to
 * <i>R_get_location_with_pointer</i> except the pointer is replaced by a
 * line which has one end fixed at the coordinate identified by the <b>x,y</b>
 * values. The other end of the line is initialized at the coordinate identified
 * by the <b>nx,ny</b> pointers. This end then tracks the mouse until a button
 * is pressed. The mouse button (1 for left, 2 for middle, and 3 for right) is
 * returned in the <b>button</b> pointer.
 *
 *  \param cx
 *  \param cy
 *  \param wx
 *  \param wy
 *  \param button
 *  \return int
 */

int R_get_location_with_line(
	int cx,int cy,
	int *wx,int *wy,
	int *button)
{
	int z ;
	_send_ident(GET_LOCATION_WITH_LINE) ;
	z = cx ;
	_send_int(&z) ;
	z = cy ;
	_send_int(&z) ;
	z = *wx ;
	_send_int(&z) ;
	z = *wy ;
	_send_int(&z) ;
	_get_int(wx) ;
	_get_int(wy) ;
	_get_int(button) ;

	return 0;
}
