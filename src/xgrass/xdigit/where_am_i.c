/*  @(#)where_am_i.c    2.1  6/26/87  */
#include "digit.h"
#include "dig_head.h"

where_am_i(map)
    struct Map_info *map;
{
    int i;
    int found;
    int button;
    int screen_x, screen_y;
    double thresh;
    double ux, uy;
    char buffer[64];

    double fabs();

    screen_y  = 0;
    screen_x = 0;

    show_select_dialog(NULL, "abort", "Where am I?", 0);
    thresh =  map->head.map_thresh;

    for(;;)
    {

	get_location_with_pointer(&screen_x, &screen_y, &button);
	screen_to_utm (screen_x, screen_y, &ux, &uy);

	switch(button)
	{
	    case FIND:
		sprintf(buffer,"EAST: %12.2lf   NORTH: %12.2lf", ux, uy);
		write_info(1, buffer);
		break;
	    case DONE:
		return(0);
		break;
	    default:
		break;
	}
    }
}
