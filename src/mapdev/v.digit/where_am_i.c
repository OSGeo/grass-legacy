/*  @(#)where_am_i.c    2.1  6/26/87  */
#include <math.h>
#include "digit.h"
#include "raster.h"
#include "Map_proto.h"
#include "dig_curses.h"
#include "local_proto.h"
#include "glocale.h"

int where_am_i (struct Map_info *map)
{
    int i;
    int found;
    int button;
    int screen_x, screen_y;
    double thresh;
    double ux, uy;
    char buffer[64];

    screen_y  = 0;
    screen_x = 0;

    _Clear_base ();
    _Write_base(10, _("Buttons:"));
    _Write_base(11, _("   Left:   Where am i"));
    _Write_base(12, _("   Middle: Quit"));
     Write_base(13, _("   Right:  Quit"));

    thresh =  map->head.map_thresh;

    for(;;)
    {

	R_get_location_with_pointer(&screen_x, &screen_y, &button);
	screen_to_utm (screen_x, screen_y, &ux, &uy);

	switch(button)
	{
	    case LEFTB:
		sprintf(buffer,_("EAST: %12.2f   NORTH: %12.2f"), ux, uy);
		Write_info(1, buffer);
		break;
	    case MIDDLEB:
#ifdef FOO
		/* Check to see if we are pointing to an area */
		Write_info(2, "Checking areas");
		if ( i = dig_point_to_area(ux, uy) )
		{
		    sprintf(buffer,_("AREA: %d"), i);
		    Write_area(i, &area[i]);
		    Write_info(2, buffer);
		}
		else
		    Write_info(2, _("No Area found"));

		/* Check to see if we are pointing to a node */
		Write_info(3, _("Checking nodes"));
		found = 0;
		for (i=1; i<=map->n_nodes; i++)
		{
		    if ( (fabs(map->Node[i].x-ux) < thresh)
		      && (fabs(map->Node[i].y-uy) < thresh) )
		    {
			found++;
			sprintf(buffer,"NODE: %d", i);
			Write_node(i, &(map->Node[i]));
			Write_info(3, buffer);
		    }
		}
		if (! found)
		    Write_info(3, _("No Nodes found"));

		/* Check to see if we are pointing to a line */
		/*  checking for lines takes a long time  
		*     may have to comment out or find a faster way.
		*/
		Write_info(4, _("Checking lines"));
		if ( i = dig_point_to_line(ux, uy) )
		 {
		    sprintf(buffer,_("LINE: %d"), i);
		    Write_info(i, &(map->Line[i]));
		    Write_info(4, buffer);
		 }
		else
		    Write_info(4, _("No Lines found"));
		break;

#endif
	    case RIGHTB:
		return(0);
		break;
	    default:
		break;
	}
    }
}
