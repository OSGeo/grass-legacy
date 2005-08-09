#include "raster.h"
#include "graph.h"


/*!
 * \brief change the width of line
 *
 * Changes the <b>width</b> of line to be used in subsequent draw commands.
 *
 *  \param width
 *  \return int
 */

int R_line_width(int width)
{
	int i ;
	i = width ;
	_send_ident(LINE_WIDTH) ;
	_send_int(&i) ;

	return 0;
}
