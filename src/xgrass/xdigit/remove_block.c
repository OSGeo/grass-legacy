/*
**  Written by Dave Gerdes  6/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "dig_head.h"
#include "wind.h"
#include "gis.h"

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
    double tmpx, tmpy;
    int button = 0;
    int cur_screen_x, cur_screen_y ;
    double N, S, E, W;
    double delta;
    int yn;
    int cnt;
    char buf[1024];
    register int i;
    int box;

    cur_screen_x = (int)D_west ;
    cur_screen_y = (int)D_south ;

    screen_x = cur_screen_x + 10 ;
    screen_y = cur_screen_y + 10 ;

    sx1 = ux1 = U_west ;
    sy1 = uy1 = U_south ;
    sx2 = ux2 = U_east ;
    sy2 = uy2 = U_north ;

    show_select_dialog("accept", "abort", "Select block:", 1);
    box = 0;
    fill_pix();
    while (1)
    {
	get_location_with_box(&cur_screen_x, &cur_screen_y, &screen_x, &screen_y, &button) ;
	switch (button) {
	    case FIND:
		if ( cur_screen_x == screen_x  ||  cur_screen_y == screen_y)
		    continue ;
		screen_to_utm ( cur_screen_x, cur_screen_y, &ux1, &uy1) ;
		screen_to_utm ( screen_x, screen_y, &ux2, &uy2) ;
		box = 1;
		break;

	    case DONE:
		if (box)
		    copy_pix();
		return (0);
		break;
	    case ACCEPT:
		if (box)
		{   
		    copy_pix();
		    goto foo;
		}
		else
		    make_monolog (1,"No block selected");
		    show_select_dialog("accept", "abort", "Select block:", 1);
		break;
	    default:
		break;
	}

    }

foo:
    if(!mouse_yes_no("You are about to remove a block of lines. OK? "))
	return (0);
    sprintf (buf, " Please wait while lines are deleted...");
	write_info (1, buf);

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
    make_monolog (1, buf);
    return (cnt);
}
