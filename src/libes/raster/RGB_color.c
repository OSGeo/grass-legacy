#include "raster.h"
#include "graph.h"


/*!
 * \brief select color
 *
 * When in
 * float mode (see <i>R_color_table_float</i>), this call selects the color
 * most closely matched to the <b>red, grn</b>, and <b>blue</b> intensities
 * requested. These values must be in the range of 0-255.
 *
 *  \param red
 *  \param grn
 *  \param blue
 *  \return int
 */

int R_RGB_color(unsigned char red,unsigned char grn,unsigned char blu )
{
	unsigned char f ;

	_send_ident(RGB_COLOR) ;
	f = red ; _send_char(&f) ;
	f = grn ; _send_char(&f) ;
	f = blu ; _send_char(&f) ;

	return 0;
}
