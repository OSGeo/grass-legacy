/* %W%  %G%  */

#include "options.h"

set_default_options()
{
	strcpy(name, "") ;
	color1 = D_translate_color("blue") ;
	color2 = D_translate_color("yellow") ;
}

stash_away(pos, option)
	int pos ;
	char *option ;
{
	int new_colr ;

	switch(pos)
	{
	case NAME:
		strcpy (name, option);
		G_strip (name);
		if (*name == 0)
			return -1;
		break ;
	case COLOR1:
		new_colr = D_translate_color(option) ;
		if (new_colr == 0)
		{
			tell_about_colors(option) ;
			return(-1) ;
		}
		else
			color1 = new_colr ;
		break ;
	case COLOR2:
		new_colr = D_translate_color(option) ;
		if (new_colr == 0)
		{
			tell_about_colors(option) ;
			return(-1) ;
		}
		else
			color2 = new_colr ;
		break ;
	default:
		printf("Unknown option\n") ;
		return(-1) ;
		break ;
	}
	return(0) ;
}

static
tell_about_colors(s)
	char *s ;
{
	printf("Don't know the color %s\n", s) ;
	printf("Available colors:\n") ;
	printf("  red      orange      yellow      green\n") ;
	printf("  blue     indigo      violet      gray\n") ;
	printf("  white    black\n") ;
}
