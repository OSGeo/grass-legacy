/*
**  Written by Dave Gerdes  6/1988
**  US Army Construction Engineering Research Lab
*/
/*  modified by R.L.Glenn 12/1991
**  USDA Tech. Infor. Sys. Division
*/

#include "digit.h"
#include "dig_head.h"
#include "wind.h"
#include "gis.h"
#include "popup.h"

remove_block (Map)
    struct Map_info *Map;
{
    char buffer[64] ;
    double x, y ;
    int screen_x, screen_y ;
    double ux1, uy1 ;
    double ux2, uy2 ;
    double sx1, sy1 ;
    double sx2, sy2 ;
    int button ;
    int cur_screen_x, cur_screen_y ;
    double N, S, E, W;
    double delta;
    int yn;
    int cnt;
    char buf[1024];
    register int i;
    int menu_left, menu_top;
    int ret, chr;

    menu_left = Next_l + 1;
    menu_top = Next_t;

    G_clear_screen ();
    buttons[0] = " ";
    buttons[1] = "Buttons:\0";
    buttons[2] = "Left:   Establish a corner";
    buttons[3] = "Middle: Abort";
    buttons[4] = "Right:  Accept window";
    buttons[5] = "  ";
    buttons[6] = '\0';

    Dchoose(MEN.name) ;
    popup_butns( menu_top, menu_left, buttons, "rm_blk", 1) ;
    Dchoose(DIG.name) ;

    cur_screen_x = (int)D_west ;
    cur_screen_y = (int)D_south ;
/*
    screen_x = (int)D_east ;
    screen_y = (int)D_north ;
*/
    screen_x = cur_screen_x + 10 ;
    screen_y = cur_screen_y + 10 ;

    sx1 = ux1 = U_west ;
    sy1 = uy1 = U_south ;
    sx2 = ux2 = U_east ;
    sy2 = uy2 = U_north ;
top:

    while (1)
    {
	R_get_location_with_box(cur_screen_x, cur_screen_y, &screen_x, &screen_y, &button) ;
	G_clear_screen ();

	switch (button) {
	    case 1:
		if ( cur_screen_x == screen_x  &&  cur_screen_y == screen_y)
		{
                    sprintf (buf,"Block is too small to use .") ;
                    message[0] = (char *) malloc (strlen (buf) + 1);
                    sprintf(message[0],"%s", buf);
                    message[1] = " ";
                    message[2] = '\0';

                    Dchoose(MEN.name) ;
                    popup_messg( "warning", 1) ;
	            sleep(2);
                    erase_popup("warning");
                    Dchoose(DIG.name) ;
		    continue ;
		}
		cur_screen_x = screen_x ;
		cur_screen_y = screen_y ;
		screen_to_utm ( cur_screen_x, cur_screen_y, &ux1, &uy1) ;
		break;

	    case 2:
                erase_popup("rm_blk");
		return (0);
		break;
	    case 3:
		screen_to_utm ( screen_x, screen_y, &ux2, &uy2) ;
		    goto foo;
		break;
	}

    }

foo:
    erase_popup("rm_blk");
    sprintf(buf,
            "You are about to remove a block of lines\nOK? ");
    if (!mouse_yes_no (buf))
	return (0);
    cnt = 0;

    N = GREATER (uy1, uy2);
    S = LESSER  (uy1, uy2);
    E = GREATER (ux1, ux2);
    W = LESSER  (ux1, ux2);
    for (i = 1 ; i <= Map->n_lines ; i++)
    {
	if (!LINE_ALIVE (&(Map->Line[i])))
	    continue;
	if (_line_in_window (&(Map->Line[i]), N, S, E, W))
	{
	    if (_line_really_in_window (Map, &(Map->Line[i]), N, S, E, W))
	    {
		_remove_line (Map, i);
		cnt++;
	    }
	}
    }
    sprintf (buf, " %d lines removed.", cnt);
    message[0] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[0],"%s", buf);
    message[1] = " ";
    message[2] = '\0';

    Dchoose(MEN.name) ;
    popup_messg( "warning", 1) ;
    sleep(2);
    erase_popup("warning");
    Dchoose(DIG.name) ;
    return (cnt);
}
