/*  %W%  %G%  */

#include "options.h"

set_default_options()
{
	textcolor = D_translate_color("white") ;
	backcolor = D_translate_color("black") ;
	size = 3.0 ;
}

stash_away(pos, option)
	int pos ;
	char *option ;
{
	int new_colr ;

	switch(pos)
	{
	case SIZE:
		if (! sscanf(option,"%f",&size) )
			return(-1) ;
		break ;
	case BACKCOLOR:
		new_colr = D_translate_color(option) ;
		if (new_colr == 0)
			return(-1) ;
		else
			backcolor = new_colr ;
		break ;
	case TEXTCOLOR:
		new_colr = D_translate_color(option) ;
		if (new_colr == 0)
		{
			printf("Don't know the color %s\n", option) ;
			printf("Available colors:\n") ;
			printf("  red      orange      yellow      green\n") ;
			printf("  blue     indigo      violet      gray\n") ;
			printf("  white    black\n") ;
			return(-1) ;
		}
		else
			textcolor = new_colr ;
		break ;
	case FONT:
		R_font(option) ;
		break ;
	default:
		printf("Unknown option\n") ;
		return(-1) ;
		break ;
	}
	return(0) ;
}
