#include "raster.h"
#include "graph.h"


/*!
 * \brief define single color
 *
 * Sets color number <b>num</b> to the
 * intensities represented by <b>red, grn</b>, and <b>blue.</b>
 *
 *  \param red
 *  \param grn
 *  \param blu
 *  \param num number
 *  \return int
 */

int R_reset_color(
	unsigned char red,unsigned char grn,unsigned char blu,
	int number )
{
	int i ;
	unsigned char f ;

	i = number ;
	if (i < 0)
		i = 256 - i ;

	_send_ident(RESET_COLOR) ;
	f = red ; _send_char(&f) ;
	f = grn ; _send_char(&f) ;
	f = blu ; _send_char(&f) ;
	_send_int(&i) ;

	return 0;
}
