/*  %W%  %G%  */

#include <stdio.h>
#include "options.h"

set_default_options()
{
	color = D_translate_color("white") ;
	size = 1 ;
	type = TYPE_X ;
	infile = stdin ;
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
	case COLOR:
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
			color = new_colr ;
		break ;
	case TYPE:
		if (! strcmp(option, "x"))
			type = TYPE_X ;
		else if (! strcmp(option, "+"))
			type = TYPE_PLUS ;
		else if (! strcmp(option, "box"))
			type = TYPE_BOX ;
		else if (! strcmp(option, "diamond"))
			type = TYPE_DIAMOND ;
		break ;
	case INFILE:
		infile = fopen(option, "r") ;
		if (infile == NULL)
		{
			fprintf(stderr, "File %s not available\n", option) ;
			return(-1) ;
		}
		break ;
	default:
		printf("Unknown option\n") ;
		return(-1) ;
		break ;
	}
	return(0) ;
}
