#include "raster.h"
#include "graph.h"


/*!
 * \brief write text
 *
 * Writes <b>text</b> in the current color and font, at the current text
 * width and height, starting at the current screen location.
 *
 *  \param sometext
 *  \return int
 */

int R_text(char *sometext )
{
	_send_ident(TEXT) ;
	_send_text(sometext) ;

	return 0;
}
