#include "raster.h"
#include "graph.h"


/*!
 * \brief select color
 *
 * Selects the <b>color</b> to be
 * used in subsequent draw commands.
 *
 *  \param index
 *  \return int
 */

int R_color(int index)
{
	int i ;
	i = index ;
	_send_ident(COLOR) ;
	_send_int(&i) ;

	return 0;
}


/*!
 * \brief select standard color
 *
 * Selects the
 * standard <b>color</b> to be used in subsequent draw commands.  The
 * <b>color</b> value is best retrieved using <i>D_translate_color.</i>
 * See Display_Graphics_Library.
 *
 *  \param index
 *  \return int
 */

int R_standard_color(int index)
{
	int i ;
	i = index ;
	_send_ident(STANDARD_COLOR) ;
	_send_int(&i) ;

	return 0;
}
