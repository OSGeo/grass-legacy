#include "raster.h"
#include "graph.h"


/*!
 * \brief set text size
 *
 * Sets text pixel width and height to <b>width</b> and <b>height.</b>
 *
 *  \param width
 *  \param height
 *  \return int
 */

int R_text_size( int width,int height )
{
	int z ;
	_send_ident(TEXT_SIZE) ;
	z = width ;
	_send_int(&z) ;
	z = height ;
	_send_int(&z) ;

	return 0;
}
