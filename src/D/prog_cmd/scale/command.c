/*  %W%  %G%  */

#include "options.h"

set_default_options()
{
	color1 = D_translate_color("black") ;
	color2 = D_translate_color("white") ;
}

stash_away(pos, option)
	int pos ;
	char *option ;
{
	int new_colr ;

	switch(pos)
	{
	case COLOR1:
		new_colr = D_translate_color(option) ;
		if (new_colr == 0)
			goto color_error ;
		color1 = new_colr ;
		break ;
	case COLOR2:
		new_colr = D_translate_color(option) ;
		if (new_colr == 0)
			goto color_error ;
		color2 = new_colr ;
		break ;
	default:
		printf("Unknown option\n") ;
		return(-1) ;
		break ;
	}
	return(0) ;

color_error:
	printf("Don't know the color %s\n", option) ;
	printf("Available colors:\n") ;
	printf("  red      orange      yellow     green\n") ;
	printf("  blue     indigo      violet     gray\n") ;
	printf("  brown    magenta     white      black\n") ;
	return(-1) ;
}
