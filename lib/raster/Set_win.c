#include "raster.h"
#include "graph.h"


/*!
 * \brief set text clipping frame
 *
 * Subsequent calls to <i>R_text</i> will have text strings
 * clipped to the screen frame defined by <b>top, bottom, left, right.</b>
 *
 *  \param t top
 *  \param b bottom
 *  \param l left
 *  \param r right
 *  \return int
 */

int R_set_window(int t,int b,int l,int r )
{
	int i ;
	_send_ident(SET_WINDOW) ;
	i = t ;
	_send_int(&i) ;
	i = b ;
	_send_int(&i) ;
	i = l ;
	_send_int(&i) ;
	i = r ;
	_send_int(&i) ;

	return 0;
}
