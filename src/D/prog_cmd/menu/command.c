/* %W%  %G%  */

#include "options.h"

set_default_options()
{
	dividercolor = D_translate_color("black") ;
	backcolor    = D_translate_color("gray") ;
	textcolor    = D_translate_color("white") ;
	size = 3 ;
	top = 100 - 10 ;  /* 10% from top */
	left = 10 ;
}

stash_away(pos, option)
	int pos ;
	char *option ;
{
	int new_colr ;

	switch(pos)
	{
	case SIZE:
		if (! sscanf(option,"%d",&size) )
			return(-1) ;
		break ;
	case BCOLOR:
	case TCOLOR:
	case DCOLOR:
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
		{
			switch (pos)
			{
			case BCOLOR: backcolor = new_colr ; break;
			case TCOLOR: textcolor = new_colr ; break;
			case DCOLOR: dividercolor = new_colr ; break;
			}
		}
		break ;
	default:
		printf("Unknown option\n") ;
		return(-1) ;
		break ;
	}
	return(0) ;
}
