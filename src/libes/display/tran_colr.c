/* Takes a color name in ascii, returns the color number for that color.
 *    returns 0 if color is not known.
 */

#include <string.h>
#include "display.h"
#include "colors.h"


/*!
 * \brief color name to number
 *
 * Takes a
 * color <b>name</b> in ascii and returns the color number for that color.
 * Returns 0 if color is not known. The color number returned is for lines and
 * text, not raster graphics.
 *
 *  \param name
 *  \return int
 */

int D_translate_color(const char *str )
{
	if (! strcmp(str, "red"))	return(RED) ;
	if (! strcmp(str, "orange"))	return(ORANGE) ;
	if (! strcmp(str, "yellow"))	return(YELLOW) ;
	if (! strcmp(str, "green"))	return(GREEN) ;
	if (! strcmp(str, "blue"))	return(BLUE) ;
	if (! strcmp(str, "indigo"))	return(INDIGO) ;
	if (! strcmp(str, "violet"))	return(VIOLET) ;
	if (! strcmp(str, "white"))	return(WHITE) ;
	if (! strcmp(str, "black"))	return(BLACK) ;
	if (! strcmp(str, "gray"))	return(GRAY) ;
	if (! strcmp(str, "brown"))	return(BROWN) ;
	if (! strcmp(str, "magenta"))	return(MAGENTA) ;
	if (! strcmp(str, "aqua"))	return(AQUA) ;

	if (! strcmp(str, "grey"))	return(GRAY) ;

	return(0) ;
}
