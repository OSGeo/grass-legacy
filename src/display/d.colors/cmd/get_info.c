#include <stdio.h>
#include "gis.h"

get_map_info(name,mapset)
    char *name, *mapset;
{
    struct Colors colors ;
    struct Categories categories ;
    char buff[128] ;

    if(!name) 
	exit(0);
    if (*name == NULL)
	exit(0) ;

/* Reading color lookup table */
    if (G_read_cats(name, mapset, &categories) == -1)
    {
	sprintf(buff,"category file for [%s] not available", name) ;
	G_fatal_error(buff) ;
    }

/* Reading color lookup table */
    if (G_read_colors(name, mapset, &colors) == -1)
    {
	sprintf(buff,"color file for [%s] not available", name) ;
	G_fatal_error(buff) ;
    }

/* Set the colors for the display */
    if(!D_set_colors(&colors))
    {
	printf ( "Note, [%s] has more colors than the graphics device\n",
	    name);
	if (!G_yes("Continue? ", 1)) return;
    };

    interact(&categories, &colors, name, mapset) ; 

/* Wrapup graphics */
    R_flush() ;
}
