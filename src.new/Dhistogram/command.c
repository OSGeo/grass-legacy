/*  %D%  %J%  */

#include "options.h"

set_default_options()
{
	strcpy(map_name, "") ;
	color = D_translate_color("white") ;
	type = COUNT;
        style = BAR ;
        nodata = NO;
}

stash_away(pos, option)
	int pos ;
	char *option ;
{
	int new_colr ;

        switch(pos)
	{
	case NAME:
		if (! sscanf(option,"%s",map_name) )
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
		if (strcmp(option,"count") == 0)
                   type = COUNT;
                else if (strcmp(option,"area") == 0)
		   type = AREA;
                else
                   {
                   printf("type option is bad\n");
                   return(-1);
                   }
                break;

	case STYLE:
		if (strcmp(option,"bar") == 0)
                   style = BAR;
                else if (strcmp(option,"pie") == 0)
		   style = PIE;
                else
                   {
                   printf("style option is bad\n");
                   return(-1);
                   }
		break ;
	case NODATA:
		if (strcmp(option,"yes")==0 || strcmp(option,"YES")==0)
                   nodata = YES;
		else if (strcmp(option,"no")==0 || strcmp(option,"NO")==0)
         	   nodata = NO; 
                else
 		   { 
                   printf("nodata option is bad\n");
                   return(-1);
                   }
		break;
	default:
		printf("Unknown option\n") ;
		return(-1) ;
		break ;
	}
	return(0) ;
}
