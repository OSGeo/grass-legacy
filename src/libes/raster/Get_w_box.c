#include "raster.h"
#include "graph.h"


/*!
 * \brief get mouse location using a box
 *
 * Identical to
 * <i>R_get_location_with_line</i> except a rubber-band box is used instead
 * of a rubber-band line.
 *
 *  \param cx
 *  \param cy
 *  \param wx
 *  \param wy
 *  \param button
 *  \return int
 */

int R_get_location_with_box(int cx,int cy,int *wx,int *wy,int *button)
{
	int z ;
	_send_ident(GET_LOCATION_WITH_BOX) ;
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
