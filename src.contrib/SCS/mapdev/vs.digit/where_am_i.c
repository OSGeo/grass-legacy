/*  @(#)where_am_i.c    2.1  6/26/87  */
#include "digit.h"
#include "dig_head.h"
#include "popup.h"

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
    int menu_left, menu_top;
    int ret, chr, first = 1;

    menu_left = Next_l + 1;
    menu_top = Next_t + 15;

    screen_y  = 0;
    screen_x = 0;
    buttons[0] = " ";
    buttons[1] = "Buttons:\0";
    buttons[2] = "Left:   where am i";
    buttons[3] = "Middle: quit";
    buttons[4] = "Right:  quit";
    buttons[5] = "  ";
    buttons[6] = '\0';

    G_clear_screen ();
    Dchoose(MEN.name) ;
    popup_butns(
                menu_top,
                menu_left,
                buttons,
		"where",
		1
                ) ;
    Dchoose(DIG.name) ;

    thresh =  map->head.map_thresh;

    for(;;)
    {
	R_get_location_with_pointer(&screen_x, &screen_y, &button);
	screen_to_utm (screen_x, screen_y, &ux, &uy);

	switch(button)
	{
	    case 1:
		if (first)
		 {
                 sprintf(buffer,"EAST:%12.2lf  NORTH:%13.2lf .", ux, uy);
                 message[0] = (char *) malloc (strlen (buffer) + 1);
                 sprintf(message[0],"%s\0", buffer);
		 }
                else
		 {
                 sprintf(message[0],"EAST:%12.2lf  NORTH:%13.2lf .", ux, uy);
		 }
                message[1] = '\0';

                Dchoose(MEN.name) ;
                popup_messg( "info", first) ;

                Dchoose(DIG.name) ;
		first = 0;
		break;
	    case 2:
	    case 3:
		if (!first) erase_popup("info");
		erase_popup("where");
		return(0);
		break;
	    default:
		break;
	}
    }
}
