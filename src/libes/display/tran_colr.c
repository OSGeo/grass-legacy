/* Takes a color name in ascii, returns the color number for that color.
 *    returns 0 if color is not known.
 */

#include "colors.h"

D_translate_color(str)
	char *str ;
{
	if (! strcmp(str, "red"))
		return(RED) ;
	if (! strcmp(str, "orange"))
		return(ORANGE) ;
	if (! strcmp(str, "yellow"))
		return(YELLOW) ;
	if (! strcmp(str, "green"))
		return(GREEN) ;
	if (! strcmp(str, "blue"))
		return(BLUE) ;
	if (! strcmp(str, "cyan"))
		return(BLUE) ;
	if (! strcmp(str, "indigo"))
		return(INDIGO) ;
	if (! strcmp(str, "violet"))
		return(VIOLET) ;
	if (! strcmp(str, "purple"))
		return(VIOLET) ;
	if (! strcmp(str, "white"))
		return(WHITE) ;
	if (! strcmp(str, "black"))
		return(BLACK) ;
	if (! strcmp(str, "gray"))
		return(GRAY) ;
	if (! strcmp(str, "grey"))
		return(GRAY) ;
	if (! strcmp(str, "brown"))
		return(BROWN) ;
	if (! strcmp(str, "magenta"))
		return(MAGENTA) ;
	if (! strcmp(str, "aqua"))
		return(AQUA) ;
	if (! strcmp(str, "gray"))
		return(GRAY) ;
	
	return(0) ;
}
