/*  %W%  %G%  */

#include "options.h"

set_default_options()
{
	strcpy(textcolor, "white");
	strcpy(backcolor, "black");
	init_font("romant");
	size = 6.0 ;
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
		/*
		new_colr = D_translate_color(option) ;
		if (new_colr == 0)
			return(-1) ;
		else
			backcolor = new_colr ;
		*/

		strcpy(backcolor, option);
		break ;

	case TEXTCOLOR:
		/*
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
		*/

		strcpy(textcolor, option);
		break ;

	case FONT:
		init_font(option) ;
		break ;
	default:
		printf("Unknown option\n") ;
		return(-1) ;
		break ;
	}
	return(0) ;
}
